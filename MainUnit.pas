unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FolderBrowser, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls,
  ComCtrls, tntclasses, tntsysutils, XPMan;

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
    XPManifest: TXPManifest;
    procedure AddFolderButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure HashButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  misc_utils_unit, MediaInfoDLL;

var
  MediaInfoLoaded : Boolean = False;


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
  LabelTip.Caption    := 'TIPS:'#10'1. Folders are scanned recursively.'#10#10'2. Each listed folder is designated as a root folder where an output file is saved.';
  PanelStatus.Caption := HashStatus[HashIdle];

  MediaInfoLoaded := MediaInfoDLL_Load('MediaInfo.dll');
  If MediaInfoLoaded = True then
  Begin
    MediaInfo_Option(0, 'Internet', 'No');
    MediaInfo_Option(0, 'ParseSpeed', '0');
    MediaInfo_Option(0, 'ReadByHuman', '0');
  End;
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
        ProgressBar.Max := fList.Count-1;
        For I1 := 0 to fList.Count-1 do
        Begin
          ProgressBar.Position := I1;
          New(nEntry);
          ResetHashRecord(nEntry);

          // Calculate Hashes
          CalcGabestHash(fList[I1],nEntry^.hrHash1,nEntry^.hrHash2);

          // Validate successful hashing
          If nEntry^.hrHash1 <> 0 then
          Begin
            // Get File Path
            nEntry^.hrFilePath := WideExtractFilePath(fList[I1]);

            // Get File Name
            nEntry^.hrFileName := WideExtractFileName(fList[I1]);

            // Get File Size
            nEntry^.hrFileSize := GetFileSize(fList[I1]);

            // Get file Extension
            nEntry^.hrFileExt  := Lowercase(ExtractFileExt(nEntry^.hrFileName));
            If Length(nEntry^.hrFileExt) > 0 then If nEntry^.hrFileExt[1] = '.' then Delete(nEntry^.hrFileExt,1,1);

            // Get TAG/ID3 data
            If MediaInfoLoaded = True then MediaInfo_ProccessTAGdata(fList[I1],nEntry);

            oList.Add(nEntry);
          End
          Else Dispose(nEntry);
        End;
        SaveHashOutput(oList,FolderList.Items[I],OutputFormatCB.ItemIndex);
        ProgressBar.Position := ProgressBar.Max;

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


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  If MediaInfoLoaded = True then
  Begin
    MediaInfoDLL_UnLoad;
  End;
end;

end.
