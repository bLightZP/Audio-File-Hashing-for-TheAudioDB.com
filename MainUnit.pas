unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FolderBrowser, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls,
  ComCtrls, tntclasses, tntsysutils;

type
  TMainForm = class(TForm)
    FolderList: TTntListBox;
    AddFolderButton: TTntButton;
    FolderBrowser: TFolderBrowser;
    ClearButton: TTntButton;
    LabeOutput: TTntLabel;
    OutputFormatCB: TTntComboBox;
    HashButton: TTntButton;
    LabelTip: TTntLabel;
    ProgressBar: TProgressBar;
    PanelStatus: TTntPanel;
    procedure AddFolderButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure HashButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  HashIdle        = 0;
  HashProcessing  = 1;
  HashComplete    = 2;
  HashStatusCount = 3;

  HashStatus      : Array[HashIdle..HashStatusCount-1] of String =
    ('waiting to begin','processing:','operation complete');


Type
  THashRecord =
  Record
    hrFileName : WideString;
    hrHash1    : Int64;
    hrHash2    : Int64;
  End;
  PHashRecord = ^THashRecord;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


procedure CalcGabestHash(const Stream: TStream; var Hash1,Hash2 : Int64); overload;
var
  Hash1Ofs : Int64;
  Hash2Ofs : Int64;
  sSize    : Int64;
const
  HashPartSize = 1 shl 16; // 64 KiB

  procedure HashFromStream(const Stream: TStream; var Hash: Int64);
  var
    I      : Integer;
    Buffer : Array[0..HashPartSize div SizeOf(Int64)-1] of Int64;
  begin
    Stream.ReadBuffer(Buffer[0], SizeOf(Buffer));
    For I := Low(buffer) to High(buffer) do Inc(Hash, Buffer[i]);
  end;

begin
  Hash1    := 0;
  Hash2    := 0;
  Hash1Ofs := 0;
  Hash2Ofs := 0;

  sSize := Stream.Size;

  // The hash offset position within the file is determined by the file size to support smaller file
  // sizes, while allowing larger TAG data (embedded images) to be changed without affecting both hashes (on files over 2048KiB).

  // 256KiB - 2048KiB
  If (sSize >= 1 shl 18) and (sSize < 1 shl 21) then
  Begin
    Hash1Ofs := 1 shl 17;               // Hash1 offset is  128KiB from the start of the file
    Hash2Ofs := Stream.Size-(1 shl 17); // Hash2 offset is  128KiB from the end of the file
  End
    else
  // 2048KiB - MAX
  If (sSize >= 1 shl 21) then
  Begin
    Hash1Ofs := 1 shl 20;               // Hash1 offset is 1024KiB from the start of the file
    Hash2Ofs := Stream.Size-(1 shl 20); // Hash2 offset is 1024KiB from the end of the file
  End;

  If Hash1Ofs > 0 then
  Begin
    // Hash1:
    Stream.Position:= Hash1Ofs;
    HashFromStream(Stream, Hash1);

    // Hash2:
    Stream.Position:= Hash2Ofs;
    HashFromStream(Stream, Hash2);
  End;

  // use "IntToHex(Hash1, 16);" to get a string and "StrToInt64('$' + hash);" to get your Int64 back
end;


procedure CalcGabestHash(const FileName: WideString; var Hash1,Hash2 : Int64); overload;
var
  Stream: TStream;
begin
  Stream := TTNTFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Try
    CalcGabestHash(Stream,Hash1,Hash2);
  Except
    Hash1 := 0;
    Hash2 := 0;
  End;
  Stream.Free;
end;


procedure ScanForAudioFiles(srcPath : WideString; fileList : TTNTStringList);
var
  sRec  : TSearchRecW;
  S     : String;
begin
  If WideFindFirst(srcPath+'*.*',faAnyFile,sRec) = 0 then
  Begin
    Repeat
      If (sRec.Attr and faDirectory = faDirectory) then
      Begin
        ScanForAudioFiles(srcPath+sRec.Name,fileList);
      End
        else
      If (sRec.Attr and faVolumeID = 0) then
      Begin
        S := Lowercase(ExtractFileExt(sRec.Name));
        If (S = '.mp3') or (S = '.flac') then fileList.Add(srcPath+sRec.Name);
      End;
    Until WideFindNext(sRec) <> 0;
    WideFindClose(sRec);
  End;
end;


procedure SaveHashOutput(outList : TList; savePath : WideString; saveFormat : Integer);
var
  I       : Integer;
  sList   : TStringList;
  fStream : TTNTFileStream;
  outFile : WideString;
begin
  If outList.Count > 0 then
  Begin
    sList   := TStringList.Create;
    outFile := '';

    Case saveFormat of
      0 : // CSV
      Begin
        outFile := savePath+'AudioHash.csv';
        For I := 0 to outList.Count-1 do with PHashRecord(outList[I])^ do
        Begin
          sList.Add('"'+hrFileName+'",'+IntToHex(hrHash1,16)+','+IntToHex(hrHash2,16));
        End;
      End;
      1 : // XML
      Begin
        outFile := savePath+'AudioHash.xml';
        sList.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');
        sList.Add('<audiohash>');
        For I := 0 to outList.Count-1 do with PHashRecord(outList[I])^ do
        Begin
          sList.Add('<entry filename="'+hrFileName+'" hash1="'+IntToHex(hrHash1,16)+'" hash2="'+IntToHex(hrHash2,16)+'" />');
        End;
        sList.Add('</audiohash>');
      End;
    End;

    // Save file
    If outFile <> '' then
    Begin
      Try
        fStream := TTNTFileStream.Create(outFile,fmCreate);
      Except
        fStream := nil
      End;
      If fStream <> nil then sList.SaveToStream(fStream);
    End;
    sList.Free;
  End;
end;


procedure TMainForm.AddFolderButtonClick(Sender: TObject);
var
  S : WideString;
begin
  If FolderBrowser.Execute('') = True then
  Begin
    S := FolderBrowser.Directory;
    If S[Length(S)] <> '\' then S := S+'\';
    FolderList.Items.Add(S);
  End;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  LabelTip.Caption    := 'TIPS:'#10'1. Folders are scanned recursively.'#10#10'2. Each folder listed is designated as a root folder where an output file is saved.';
  PanelStatus.Caption := HashStatus[HashIdle];
end;


procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  FolderList.Clear;
  PanelStatus.Caption  := HashStatus[HashIdle];
  ProgressBar.Position := 0;
end;


procedure TMainForm.HashButtonClick(Sender: TObject);
var
  I,I1   : Integer;
  fList  : TTNTStringList;
  oList  : TList;
  sPath  : WideString;
  fHash1 : Int64;
  fHash2 : Int64;
  nEntry : PHashRecord;

begin
  If FolderList.Count > 0 then
  Begin
    fList := TTNTStringList.Create;
    oList := TList.Create;
    For I := 0 to FolderList.Count-1 do
    Begin
      ProgressBar.Position := 0;
      PanelStatus.Caption  := HashStatus[HashProcessing]+' '+FolderList.Items[I];

      ScanForAudioFiles(FolderList.Items[I],fList);

      If fList.Count > 0 then
      Begin
        For I1 := 0 to fList.Count-1 do
        Begin
          New(nEntry);
          CalcGabestHash(fList[I1],nEntry^.hrHash1,nEntry^.hrHash1);
          If nEntry^.hrHash1 > 0 then
          Begin
            nEntry^.hrFileName := fList[I1];
          End
          Else Dispose(nEntry);
        End;
        SaveHashOutput(oList,FolderList.Items[I],OutputFormatCB.ItemIndex);

        For I1 := 0 to oList.Count-1 do Dispose(PHashRecord(oList[I1]));
        oList.Clear;
        fList.Clear;
      End;
    End;
    oList.Free;
    fList.Free;
  End;
  PanelStatus.Caption := HashStatus[HashComplete];
end;


end.
