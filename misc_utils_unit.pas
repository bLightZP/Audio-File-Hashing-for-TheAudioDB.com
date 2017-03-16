unit misc_utils_unit;

interface


uses windows, sysutils, tntsysutils, classes, tntclasses;


const
  HashIdle        = 0;
  HashScanning    = 1;
  HashProcessing  = 2;
  HashComplete    = 3;
  HashStatusCount = 4;

  HashStatus      : Array[HashIdle..HashStatusCount-1] of String =
    ('waiting to begin','scanning:','processing:','operation complete');


Type
  THashRecord =
  Record
    hrFilePath     : WideString;
    hrFileName     : WideString;
    hrFileExt      : WideString;
    hrFileSize     : Int64;
    hrHash1        : Int64;
    hrHash2        : Int64;
    hrImage        : Boolean;
    hrTrack        : Widestring;
    hrGenre        : WideString;
    hrTrackNumber  : WideString;
    hrPerformer    : WideString;
    hrAlbum        : WideString;
    hrRecordedDate : WideString;
    hrWrittenBy    : WideString;
    hrEncodedApp   : WideString;
    hrEncodedLib   : WideString;
    hrComment      : WideString;
    hrURL          : WideString;
    hrCopyright    : WideString;
  End;
  PHashRecord = ^THashRecord;


procedure ResetHashRecord(HashRec : PHashRecord);
procedure CalcGabestHash(const Stream: TStream; var Hash1,Hash2 : Int64); overload;
procedure CalcGabestHash(const FileName: WideString; var Hash1,Hash2 : Int64); overload;
procedure ScanForAudioFiles(srcPath : WideString; fileList : TTNTStringList);
procedure SaveHashOutput(outList : TList; savePath : WideString; saveFormat : Integer);
function  GetFileSize(FileName : Widestring) : Int64;
function  MediaInfo_ProccessTAGdata(FileName : WideString; HashRec : PHashRecord) : Boolean;
function  EncodeTextTags(S : WideString; AddSuffix : Boolean) : WideString;



implementation

uses
  MediaInfoDLL, forms;


function EncodeTextTags(S : WideString; AddSuffix : Boolean) : WideString;
var
  S1 : WideString;
begin
  If AddSuffix = True then S1 := ';' else S1 := '';
  S := TNT_WideStringReplace(S,'&' ,'&amp'  +S1,[rfReplaceAll]);
  S := TNT_WideStringReplace(S,'''','&apos' +S1,[rfReplaceAll]);
  S := TNT_WideStringReplace(S,'"' ,'&quot' +S1,[rfReplaceAll]);
  S := TNT_WideStringReplace(S,'<' ,'&lt'   +S1,[rfReplaceAll]);
  S := TNT_WideStringReplace(S,'>' ,'&gt'   +S1,[rfReplaceAll]);
  //S := TNT_WideStringReplace(S,',' ,'&comma'+S1,[rfReplaceAll]);
  //S := TNT_WideStringReplace(S,'|' ,'&pipe' +S1,[rfReplaceAll]);

  Result := S;
end;


function MediaInfo_ProccessTAGdata(FileName : WideString; HashRec : PHashRecord) : Boolean;
var
  miHandle       : Cardinal;
  sImage         : String;
  sDecoded       : String;
  fStream        : TMemoryStream;
  S              : String;

begin
  Result   := False;
  miHandle := MediaInfo_New;
  If MediaInfo_Open(miHandle, PWideChar(FileName)) = 1 then With HashRec^ do
  Begin
    hrImage        :=            MediaInfo_Get(miHandle, Stream_General, 0, 'Cover_Data'         , Info_Text, Info_Name) <> '';
    hrTrack        := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Track'              , Info_Text, Info_Name)),True);
    hrGenre        := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Genre'              , Info_Text, Info_Name)),True);
    hrTrackNumber  := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Track/Position'     , Info_Text, Info_Name)),True);
    hrPerformer    := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Performer'          , Info_Text, Info_Name)),True);
    hrAlbum        := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Album'              , Info_Text, Info_Name)),True);
    hrRecordedDate := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Recorded_Date'      , Info_Text, Info_Name)),True);
    hrWrittenBy    := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'WrittenBy'          , Info_Text, Info_Name)),True);
    hrEncodedApp   := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Encoded_Application', Info_Text, Info_Name)),True);
    hrEncodedLib   := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Encoded_Library'    , Info_Text, Info_Name)),True);
    hrComment      := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Comment'            , Info_Text, Info_Name)),True);
    hrURL          := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Track/Url'          , Info_Text, Info_Name)),True);
    hrCopyright    := EncodeTextTags(UTF8Decode(MediaInfo_Get(miHandle, Stream_General, 0, 'Copyright'          , Info_Text, Info_Name)),True);
    MediaInfo_Close(miHandle);
    Result := True;
  End;
end;


procedure ResetHashRecord(HashRec : PHashRecord);
begin
  With HashRec^ do
  Begin
    hrFilePath     := '';
    hrFileName     := '';
    hrFileExt      := '';
    hrFileSize     := -1;
    hrHash1        := 0;
    hrHash2        := 0;
    hrImage        := False;
    hrTrack        := '';
    hrGenre        := '';
    hrTrackNumber  := '';
    hrPerformer    := '';
    hrAlbum        := '';
    hrRecordedDate := '';
    hrWrittenBy    := '';
    hrEncodedApp   := '';
    hrEncodedLib   := '';
    hrComment      := '';
    hrURL          := '';
    hrCopyright    := '';
  End;
end;


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

  If Hash1Ofs <> 0 then
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
        If (sRec.Name <> '.') and (sRec.Name <> '..') then
        Begin
          Application.ProcessMessages;
          ScanForAudioFiles(srcPath+sRec.Name+'\',fileList);
        End;
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
  S       : String;
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
          S := //'"'+UTF8Encode(EncodeTextTags(hrFilePath,True))+'",'+
               '"'+UTF8Encode(EncodeTextTags(hrFileName,True))+'",'+
               '"'+UTF8Encode(EncodeTextTags(hrFileExt,True))+'",'+IntToStr(hrFileSize)+','+IntToHex(hrHash1,16)+','+IntToHex(hrHash2,16)+','+{BoolToStr(hrImage,True)+','+}
               // Meta-Data:
               '"'+UTF8Encode(hrTrack)       +'",'+
               '"'+UTF8Encode(hrGenre)       +'",'+
               '"'+UTF8Encode(hrTrackNumber) +'",'+
               '"'+UTF8Encode(hrPerformer)   +'",'+
               '"'+UTF8Encode(hrAlbum)       +'",'+
               '"'+UTF8Encode(hrRecordedDate)+'",'+
               '"'+UTF8Encode(hrWrittenBy)   +'",'+
               '"'+UTF8Encode(hrEncodedApp)  +'",';
               //'"'+UTF8Encode(hrEncodedLib)  +'",'+
               //'"'+UTF8Encode(hrComment)     +'",'+
               //'"'+UTF8Encode(hrURL)         +'",'+
               //'"'+UTF8Encode(hrCopyright)   +'"';
          sList.Add(S);
        End;
      End;
      1 : // XML
      Begin
        outFile := savePath+'AudioHash.xml';
        sList.Add('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');
        sList.Add('<audiohash>');
        For I := 0 to outList.Count-1 do With PHashRecord(outList[I])^ do
        Begin
          S := #9'<entry'+
                       //' filepath="'+UTF8Encode(EncodeTextTags(hrFilePath,True))+'"'+
                       ' filename="'+UTF8Encode(EncodeTextTags(hrFileName,True))+'"'+
                       ' fileext="' +UTF8Encode(EncodeTextTags(hrFileExt,True))+'"'+
                       ' filesize="'+IntToStr(hrFileSize)+'"'+
                       ' hash1="'   +IntToHex(hrHash1,16)+'"'+
                       ' hash2="'   +IntToHex(hrHash2,16)+'" '{+
                       ' image="'   +BoolToStr(hrImage,True)+'" '};

          // Meta-Data:
          If hrTrack        <> '' then S := S+'Track="'       +UTF8Encode(hrTrack)       +'" ';
          If hrGenre        <> '' then S := S+'Genre="'       +UTF8Encode(hrGenre)       +'" ';
          If hrTrackNumber  <> '' then S := S+'TrackNumber="' +UTF8Encode(hrTrackNumber) +'" ';
          If hrPerformer    <> '' then S := S+'Performer="'   +UTF8Encode(hrPerformer)   +'" ';
          If hrAlbum        <> '' then S := S+'Album="'       +UTF8Encode(hrAlbum)       +'" ';
          If hrRecordedDate <> '' then S := S+'RecordedDate="'+UTF8Encode(hrRecordedDate)+'" ';
          If hrWrittenBy    <> '' then S := S+'WrittenBy="'   +UTF8Encode(hrWrittenBy)   +'" ';
          If hrEncodedApp   <> '' then S := S+'EncodedApp="'  +UTF8Encode(hrEncodedApp)  +'" ';
          //If hrEncodedLib   <> '' then S := S+'EncodedLib="'  +UTF8Encode(hrEncodedLib)  +'" ';
          //If hrComment      <> '' then S := S+'Comment="'     +UTF8Encode(hrComment)     +'" ';
          //If hrURL          <> '' then S := S+'URL="'         +UTF8Encode(hrURL)         +'" ';
          //If hrCopyright    <> '' then S := S+'Copyright="'   +UTF8Encode(hrCopyright)   +'" ';

          S := S+'/>';

          sList.Add(S);
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
      If fStream <> nil then
      Begin
        sList.SaveToStream(fStream);
        fStream.Free;
      End;
    End;
    sList.Free;
  End;
end;


function GetFileSize(FileName : Widestring) : Int64;
var
  sRec        : TSearchRecW;
  nLen        : Integer;
  FFileHandle : THandle;
  i64         : Int64;
  fData       : WIN32_FILE_ATTRIBUTE_DATA;
begin
  Result := -1;
  nLen := Length(FileName);
  If (nLen > 0) then
  Begin
    If Char(FileName[nLen]) in ['\','/'] then FileName := Copy(FileName,1,nLen-1);

    If Win32PlatformIsUnicode = False then
    Begin
      If GetFileAttributesExA(PChar(String(FileName)),GetFileExInfoStandard,@fData) = True then
        If fData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
      Begin
        Int64Rec(Result).Lo := fData.nFileSizeLow;
        Int64Rec(Result).Hi := fData.nFileSizeHigh;
      End;
    End
      else
    Begin
      If GetFileAttributesExW(PWideChar(FileName),GetFileExInfoStandard,@fData) = True then
        If fData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
      Begin
        Int64Rec(Result).Lo := fData.nFileSizeLow;
        Int64Rec(Result).Hi := fData.nFileSizeHigh;
      End;
    End;
  End;
end;


end.
