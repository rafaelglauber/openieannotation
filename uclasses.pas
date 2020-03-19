unit UClasses;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows, JwaTlHelp32,{$ENDIF}
  {$IFDEF LINUX}process,{$ENDIF}
  Dialogs,
  Classes, SysUtils, contnrs, Regexpr, FileUtil, LazUTF8Classes;

type

  TLoadForm = (lfAll, lfChunk, lfAnnotation);

  TFormatFile = (ffTabs, ffIberlef);

  TOIESystem = (osDependentIE, osPragmaticOIE, osInferPortOIE, osDptOIE, osArgOE);

  TStringArray = array of String;
  TAnsiStringArray = array of AnsiString;

  TEqualCalc   = (ecFull, ecPartial, ecText);

  { TOpenIEAnnotation }

  TOpenIEAnnotation = class
  private
    FArg1: String;
    FArg2: String;
    FRel: String;
    FMinimal: Boolean;
    FValidFact: Boolean;
    FValidMinimal: Boolean;
    function RemoveDoubleSpaces(text: String): String;
    function RemoveSpecialChar(text: String): String;
   public
     constructor Create(arg1, rel, arg2: String; minimal: Boolean; validFact: Boolean; validMinimal: Boolean); overload;
     property Arg1: String read FArg1;
     property Rel: String read FRel;
     property Arg2: String read FArg2;
     property Minimal: Boolean read FMinimal;
     property ValidFact: Boolean read FValidFact write FValidFact;
     property ValidMinimal: Boolean read FValidMinimal write FValidMinimal;
     function ToStringFile: String;
     function ToString: ansistring; override;
     function EqualsEx(another: TOpenIEAnnotation; ttype: TEqualCalc): Boolean;
  end;

  { TChunk }

  TChunk = class
  function GetText: String;
  function GetChunkType: String;
  private
    FType: String;
    FStart: Integer;
    FEnd: Integer;
    FText: String;
    function getMaxLen(): Integer;
  public
    constructor Create(ctype: String; startc, endc: Integer; text: String); overload;
    //destructor Destroy; override;
    property Text: String read GetText;
    property ChunkType: String read GetChunkType;
    function IncStart: String;
    function IncEnd: String;
    function DecStart: String;
    function DecEnd: String;
    function ToStringFile: String;
    function ToString: ansistring; override;
  end;

  { TOpenIERecord }

  TOpenIERecord = class
    function GetSentenceAndPos: String;
    function GetSentenceAndPosAndDepForBrat: String;
  private
    FID: Integer;
    FSentence: String;
    FTokens: TStringListUTF8;
    FPOSs: TStringListUTF8;
    FDEPs: TStringListUTF8;
    FChunks: TObjectList;
    FAnnotations: TObjectList;
  public
    constructor Create; overload;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Sentence: String read FSentence write FSentence;
    property SentenceAndPos: String read GetSentenceAndPos;
    property Chunks: TObjectList read FChunks;
    property Annotations: TObjectList read FAnnotations;
    property Tokens: TStringListUTF8 read FTokens;
    property POSs: TStringListUTF8 read FPOSs;
    property DEPs: TStringListUTF8 read FDEPs;
    procedure AddFact(arg1, rel, arg2: String; minimal, validFact, validMinimal: Boolean);
    procedure UpdateFact(index: Integer; arg1, rel, arg2: String; minimal, validFact, validMinimal: Boolean);
    procedure RemoveFact(index: Integer);
    function ToString: ansistring; override;
    function IsDuplicatedFact(arg1, rel, arg2: String): Boolean;
    function IsValidFact(arg1, rel, arg2: String): Boolean;
  end;

  { TAnnotationFile }

  TAnnotationFile = class
    function GetCurrentSentence: TOpenIERecord;
  private
    FFileName: String;
    FRecords: TObjectList;
    FUpdated: Boolean;
    FCurrentSentenceID: Integer;
    function GetSentenceCount: Integer;
    function Split(base: String): TStringArray;
    function RemoveLastTab(base: String): String;
    procedure LoadFile;
    class function LoadExtractionsFromFile(system: TOIESystem; fileName: String): TObjectList;
  public
    constructor Create(fileName: String); overload;
    constructor Create; overload;
    destructor Destroy; override;
    property Updated: Boolean read FUpdated write FUpdated;
    property CurrentSentenceID: Integer read FCurrentSentenceID write FCurrentSentenceID;
    property CurrentSentence: TOpenIERecord read GetCurrentSentence;
    property Records: TObjectList read FRecords;
    property SentenceCount: Integer read GetSentenceCount;
    procedure SaveFile;
    procedure SetFileName(fileName: String);
    function GetFileName: String;
    procedure SetRecords(value: TObjectList);
    procedure ExportToFile(format: TFormatFile; fileName: String);
    function GetAllAnnotations: TObjectList;
    function GetRecordByIndex(index: Integer): TOpenIERecord;
    function GetRecordById(id: Integer): TOpenIERecord;
    function GetFactProperties(rec: Integer; ann: TOpenIEAnnotation): TOpenIEAnnotation;
    procedure AddFactBySentenceID(id: Integer; arg1, rel, arg2: String);
    class procedure ConvertOIEToAnnotationFile(system: TOIESystem; sourceFileName: String);
    class procedure PrepareFileConvertion(baseFileName: String; out finalFileName: String; out f1: String; out f2: String);
  end;

  { TObjectListHelper }

  TObjectListHelper = class helper for TObjectList
    procedure RemoveAll;
    function ContainAnnotation(annotation: TOpenIEAnnotation; ttype: TEqualCalc): Boolean;
    function GetRecordByID(id: Integer): TOpenIERecord;
  end;


  function KillTask(ExeFileName: string): Integer;

implementation

const
  SECNAME_SENTID = 'SENTID';
  SECNAME_SENT   = 'SENT';
  SECNAME_TOKEN  = 'TOKEN';
  SECNAME_POS    = 'POS';
  SECNAME_DEP    = 'DEP';
  SECNAME_CHUNK  = 'CHUNK';
  SECNAME_OPENIE = 'OPENIE';
  COMMENT_SYMBOL = '#';

  {$IFDEF WINDOWS}
  function KillTaskWindows(ExeFileName: string): Integer;
  const
   PROCESS_TERMINATE = $0001;
  var
    ContinueLoop: BOOL;
    FSnapshotHandle: THandle;
    FProcessEntry32: TProcessEntry32;
  begin
    Result := 0;
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    while Integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
           UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
           UpperCase(ExeFileName))) then
           Result := Integer(TerminateProcess(
                              OpenProcess(PROCESS_TERMINATE,
                                          BOOL(0),
                                          FProcessEntry32.th32ProcessID),
                                          0));
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  function KillTaskLinux(ExeFileName: string): Integer;
  begin
    Application.Terminate;
  end;
  {$ENDIF}

  function KillTask(ExeFileName: string): Integer;
  begin
    {$IFDEF WINDOWS}
    Result:= KillTaskWindows(ExeFileName);
    {$ENDIF}
    {$IFDEF LINUX}
    Result:= KillTaskLinux(ExeFileName);
    {$ENDIF}
  end;

{ TObjectListHelper }

procedure TObjectListHelper.RemoveAll;
begin
  while Self.Count > 0 do
    Self.Remove(Self.Last);
end;

function TObjectListHelper.ContainAnnotation(annotation: TOpenIEAnnotation;
  ttype: TEqualCalc): Boolean;
var
  I: Integer;
begin
  // Init
  Result:= False;
  //
  for I:= 0 to Self.Count -1 do
    if (Self[I] as TOpenIEAnnotation).EqualsEx(annotation, ttype) then
    begin
      Result:= True;
      Break;
    end;
end;

function TObjectListHelper.GetRecordByID(id: Integer): TOpenIERecord;
var
  I: Integer;
begin
  Result:= Nil;
  for I:= 0 to Self.Count -1 do
    if (Self[I] as TOpenIERecord).ID = id then
    begin
      Result:= (Self[I] as TOpenIERecord);
      Break;
    end;
end;

{ TOpenIERecord }

constructor TOpenIERecord.Create;
begin
  FID:= -1;
  FSentence:= '';
  FTokens:= TStringListUTF8.Create;
  FPOSs:= TStringListUTF8.Create;
  FDEPs:= TStringListUTF8.Create;
  FChunks:= TObjectList.Create(True);
  FAnnotations:= TObjectList.Create(True);
end;

destructor TOpenIERecord.Destroy;
begin
  if Assigned(FTokens) then
    FreeAndNil(FTokens);

  if Assigned(FPOSs) then
    FreeAndNil(FPOSs);

  if Assigned(FDEPs) then
    FreeAndNil(FDEPs);

  if Assigned(FChunks) then
  begin
    FChunks.RemoveAll;
    FreeAndNil(FChunks);
  end;

  if Assigned(FAnnotations) then
  begin
    FAnnotations.RemoveAll;
    FreeAndNil(FAnnotations);
  end;

  inherited Destroy();
end;

procedure TOpenIERecord.AddFact(arg1, rel, arg2: String; minimal, validFact, validMinimal: Boolean);
begin
  FAnnotations.Add(TOpenIEAnnotation.Create(arg1, rel, arg2, minimal, validFact, validMinimal));
end;

procedure TOpenIERecord.UpdateFact(index: Integer; arg1, rel, arg2: String;
  minimal, validFact, validMinimal: Boolean);
begin
  if Assigned(FAnnotations) then
  begin
    (FAnnotations.Items[index] as TOpenIEAnnotation).FArg1:= arg1;
    (FAnnotations.Items[index] as TOpenIEAnnotation).FArg2:= arg2;
    (FAnnotations.Items[index] as TOpenIEAnnotation).FRel:= rel;
    (FAnnotations.Items[index] as TOpenIEAnnotation).FMinimal:= minimal;
    (FAnnotations.Items[index] as TOpenIEAnnotation).FValidFact:= validFact;
    (FAnnotations.Items[index] as TOpenIEAnnotation).FValidMinimal:= validMinimal;
  end;
end;

procedure TOpenIERecord.RemoveFact(index: Integer);
begin
  if Assigned(FAnnotations) then
    FAnnotations.Remove(FAnnotations.Items[index]);
end;

function TOpenIERecord.ToString: ansistring;
begin
  Result:= IntToStr(FID)
           + ',"' +
           StringReplace(StringReplace(FSentence, ',', ';', [rfReplaceAll]), '"', '''', [rfReplaceAll])
           + '"';
end;

function TOpenIERecord.IsDuplicatedFact(arg1, rel, arg2: String): Boolean;
var
  I: Integer;
begin
  Result:= False;
  if Assigned(FAnnotations) then
    for I:= 0 to FAnnotations.Count - 1 do
      if ((FAnnotations[I] as TOpenIEAnnotation).Arg1 = arg1) and
         ((FAnnotations[I] as TOpenIEAnnotation).Arg2 = arg2) and
         ((FAnnotations[I] as TOpenIEAnnotation).Rel = rel) then
       begin
         Result:= True;
         Break;
       end;
end;

function TOpenIERecord.IsValidFact(arg1, rel, arg2: String): Boolean;
begin
  Result:= not ((arg1 = '') or (rel = '') or (arg2 = ''))
end;

function TOpenIERecord.GetSentenceAndPos: String;
var
  I: Integer;
begin
  Result:= '';
  for I:= 0 to FTokens.Count - 1 do
    Result:= Result + FTokens[I] + '/' + FPOSs[I] + ' ';
end;

function TOpenIERecord.GetSentenceAndPosAndDepForBrat: String;
var
  I: Integer;
begin
  Result:= '';
  for I:= 0 to FTokens.Count - 1 do
    Result:= Result + FTokens[I] + '/' + FPOSs[I] + ' ';
  Result:= Copy(Result, 1, Length(Result)-1);
  for I:= 0 to FDEPs.Count -1 do
    Result:= Result + LineEnding + FDEPs[I];
end;

{ TChunk }

function TChunk.getMaxLen(): Integer;
begin
  //Result:= FTokens.Count - 1;
  Result:= -1;
end;

function TChunk.GetChunkType: String;
begin
  Result:= FType;
end;

constructor TChunk.Create(ctype: String; startc, endc: Integer; text: String);
begin
  FType:= ctype;
  FStart:= startc;
  FEnd:= endc;
  FText:= text;
end;

function TChunk.GetText: String;
//var
//  I: Integer;
begin
  Result:= FText;
  {
  Result := '';
  for I := 0 to FTokens.Count - 1 do
  begin
    if (I >= FStart) and (I <= FEnd) then
      Result:= Result + FTokens[I] + ' ';
  end;
  if Result <> '' then
    Result:= Copy(Result, 1, Length(Result)-1);
   }
end;

function TChunk.IncStart: String;
begin
  if (FStart < getMaxLen()) and (FStart < FEnd) then
    Inc(FStart);
  Result := getText();
end;

function TChunk.IncEnd: String;
begin
  if (FEnd < getMaxLen()) then
    Inc(FEnd);
  Result := getText();
end;

function TChunk.DecStart: String;
begin
  if (FStart > 0) then
    Dec(FStart);
  Result := getText();
end;

function TChunk.DecEnd: String;
begin
  if (FEnd > 0) and (FEnd > FStart) then
    Dec(FEnd);
  Result := getText();
end;

function TChunk.ToStringFile: String;
begin
  Result:= FType + #9 + IntToStr(FStart) + #9 + IntToStr(FEnd) + #9 + GetText;
end;

function TChunk.ToString: ansistring;
begin
  Result:= FType + ',"' + GetText() + '"';
end;

{ TOpenIEAnnotation }

function TOpenIEAnnotation.RemoveDoubleSpaces(text: String): String;
var
  P : Integer;
begin
  Result := text;
  repeat
    P := Pos('  ', Result);  // that's two spaces
    if P > 0 then
      Delete(Result, P + 1, 1);
  until P = 0;
  Result:= Trim(Result);
end;

function TOpenIEAnnotation.RemoveSpecialChar(text: String): String;
const
  CHAR1 = ')';
  CHAR2 = '!';
  CHAR3 = '?';
  CHAR4 = '.';
  CHAR5 = ',';
  CHAR6 = '(';
  CHAR7 = '_';
  CHAR8 = '@';
  CHAR9 = 'nÃ£o';
  CHAR0 = 'NÃ£o';
begin
  Result:= text;
  //
  Result:= StringReplace(Result, CHAR1, '', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR2, '', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR3, '', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR4, '', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR5, '', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR6, '', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR7, ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR8, ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR9, ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, CHAR0, ' ', [rfReplaceAll]);
  Result:= Trim(Result);
end;

constructor TOpenIEAnnotation.Create(arg1, rel, arg2: String;
  minimal: Boolean; validFact: Boolean; validMinimal: Boolean);
begin
  FArg1:= RemoveDoubleSpaces(RemoveSpecialChar(arg1));
  FArg2:= RemoveDoubleSpaces(RemoveSpecialChar(arg2));
  FRel := RemoveDoubleSpaces(RemoveSpecialChar(rel));
  FMinimal:= minimal;
  FValidFact:= validFact;
  FValidMinimal:= validMinimal;
end;

function TOpenIEAnnotation.ToStringFile: String;
begin
  Result:= FArg1 + #9 + FRel + #9 + FArg2  + #9 +
           BoolToStr(FMinimal, '1', '0')   + #9 +
           BoolToStr(FValidFact, '1', '0') + #9 +
           BoolToStr(FValidMinimal, '1', '0');
end;

function TOpenIEAnnotation.ToString: ansistring;
begin
  Result:= '"' + FArg1 + '","' + FRel + '","' + FArg2 + '","' +
           BoolToStr(FMinimal, '1', '0') + '","' +
           BoolToStr(FValidFact, '1', '0') + '","' +
           BoolToStr(FValidMinimal, '1', '0') + '"';
end;

function TOpenIEAnnotation.EqualsEx(another: TOpenIEAnnotation; ttype: TEqualCalc): Boolean;
begin
  Result:= False;

  if not Assigned(another) then
    Exit;

  case ttype of
    ecFull: Result:= (Self.FArg1 = another.FArg1) and
                     (Self.FArg2 = another.FArg2) and
                     (Self.FRel = another.FRel) and
                     (Self.FMinimal = another.Minimal);
    ecPartial: Result:= (Self.FArg1 = another.FArg1) and
                     (Self.FArg2 = another.FArg2) and
                     (Self.FRel = another.FRel);
    ecText: Result:= (Self.FArg1 + ' ' + Self.FRel + ' ' + Self.FArg2) =
                     (another.FArg1 + ' ' + another.FRel + ' ' + another.FArg2);
  end;
end;

{ TAnnotationFile }

procedure TAnnotationFile.LoadFile;
var
  f: TStringListUTF8;
  I: Integer;
  currentSection: String;
  sentId, sent, token, pos: String;
  chunk, dep, openie: TStringListUTF8;

  // Create a new record from file
  function newRecordFromFile(): TOpenIERecord;
  var
    buffer: TStringArray;
    K: Integer;
  begin
    // Create result object
    Result:= TOpenIERecord.Create();
    Result.ID:= StrToInt(sentId);
    Result.Sentence:= sent;
    // Get tokens
    buffer := Split(token);
    for K:= Low(buffer) to High(buffer) do
      Result.Tokens.Add(buffer[K]);
    // Get POSs
    buffer := Split(pos);
    for K:= Low(buffer) to High(buffer) do
      Result.POSs.Add(buffer[K]);
    // Get DEPs
    Result.DEPs.Text:= dep.Text;
    // Get chunks
    for K:= 0 to chunk.Count - 1 do
    begin
      buffer:= Split(chunk[K]);
      if Length(buffer) = 4 then
        Result.Chunks.Add(TChunk.Create(buffer[0],
                                        StrToInt(buffer[1]),
                                        StrToInt(buffer[2]),
                                        buffer[3]))
       else
         Result.Chunks.Add(TChunk.Create(buffer[0],
                                         StrToInt(buffer[1]),
                                         StrToInt(buffer[2]),
                                         'ERROR'));

    end;
    // Get openie
    for K:= 0 to openie.Count - 1 do
    begin
      buffer:= Split(openie[K]);

      if Length(buffer) < 3 then
        Continue;

      // Initialize valid propertie
      if Length(buffer) = 3 then
      begin
        SetLength(buffer, 6);
        buffer[3]:= '1';
        buffer[4]:= '1';
        buffer[5]:= '1';
      end;
      if Length(buffer) = 4 then
      begin
        SetLength(buffer, 6);
        buffer[4]:= '1';
        buffer[5]:= '1';
      end;
      if Length(buffer) = 5 then
      begin
        SetLength(buffer, 6);
        buffer[5]:= '1';
      end;
      if Length(buffer) < 6 then
        ShowMessage('hug debug ' + #13#13 + IntToStr(Length(buffer)) + #13#10 + openie[K]);
      Result.Annotations.Add(
          TOpenIEAnnotation.Create(buffer[0],
                                   buffer[1],
                                   buffer[2],
                                   buffer[3]='1',
                                   buffer[4]='1',
                                   buffer[5]='1'));
    end;
  end;

begin
  if not FileExists(FFileName) then
    raise Exception.Create('File not exists <' + FFileName + '>!');

  // Init current section
  currentSection := SECNAME_SENTID;
  // Init others variables
  sentId:= '';
  sent:= '';
  token:= '';
  pos:= '';
  chunk:= TStringListUTF8.Create;
  dep:= TStringListUTF8.Create;
  openie:= TStringListUTF8.Create;
  f:= TStringListUTF8.Create();
  try
    f.LoadFromFile(FFileName);

    if (f.Count = 0) then
      raise Exception.Create('File is empty <' + FFileName + '>!');

    for I:= 0 to f.Count - 1 do
    begin
      if (Trim(f[I]) = '') then
        Continue;
      //raise Exception.Create('Not supported empty lines <line ' + IntToStr(I + 1) + '>!');

      // Don't load comment lines
      if (f[I][1] <> COMMENT_SYMBOL) then
      begin
        // Set current section
        if (f[I] = SECNAME_SENT) or
           (f[I] = SECNAME_TOKEN) or
           (f[I] = SECNAME_POS) or
           (f[I] = SECNAME_DEP) or
           (f[I] = SECNAME_CHUNK) or
           (f[I] = SECNAME_OPENIE) then
           currentSection:= f[I]
        else if (f[I] = SECNAME_SENTID) then
        begin
          currentSection:= f[I];
          if (sentId <> '') then
          begin
            Self.FRecords.Add(newRecordFromFile());
            // clear variables
            sentId:= '';
            sent:= '';
            token:= '';
            pos:= '';
            chunk.Clear;
            dep.Clear;
            openie.Clear;
          end;
        end
        else
        begin
          if (currentSection = SECNAME_SENTID) then
            sentId:= f[I]
          else if (currentSection = SECNAME_SENT) then
            sent := f[I]
          else if (currentSection = SECNAME_TOKEN) then
            token := f[I]
          else if (currentSection = SECNAME_POS) then
            pos := f[I]
          else if (currentSection = SECNAME_CHUNK) then
            chunk.Add(f[I])
          else if (currentSection = SECNAME_DEP) then
            dep.Add(f[I])
          else if (currentSection = SECNAME_OPENIE) then
            openie.Add(f[I]);
        end;
      end;
    end;
    // Insert the last record
    Self.FRecords.Add(newRecordFromFile());
  finally
    FreeAndNil(f);
    FreeAndNil(chunk);
    FreeAndNil(dep);
    FreeAndNil(openie);
  end;
end;

class function TAnnotationFile.LoadExtractionsFromFile(system: TOIESystem; fileName: String): TObjectList;
var
  f: TStringListUTF8;
  //fa: TStringList;
  I, p0, p1: Integer;
  buffer: String;
  a1, rel, a2: String;
  rec: TOpenIERecord;
  values: TStringArray;
  currentID, aux: Integer;
const
  DEPENDENTIE_BEGIN_SENT = 'Sentença';
  DEPENDENTIE_BEGIN_ID   = ' ';
  DEPENDENTIE_END_ID     = ':';
  DEPENDENTIE_BEGIN_ARG1 = ' arg1:';
  DEPENDENTIE_BEGIN_REL  = '| rel:';
  DEPENDENTIE_BEGIN_ARG2 = '| arg2:';
  DEPENDENTIE_POS_MATCH  = 1;
  ARGOE_BEGIN_ID = 'SENTID_';
begin
  // init result
  Result:= TObjectList.Create;
  rec:= Nil;

  if system = osDependentIE then
  begin
    f:= TStringListUTF8.Create;
    try
      f.LoadFromFile(fileName);
      for I:= 0 to f.Count - 1 do
      begin
        if Trim(f[I]) = '' then
        begin
          if Assigned(rec) then
          begin
            Result.Add(rec);
            rec:= Nil;
          end;
        end
        else
        begin
          // Line with sentence
          if Pos(DEPENDENTIE_BEGIN_SENT, f[I]) = DEPENDENTIE_POS_MATCH then
          begin
            rec:= TOpenIERecord.Create;
            // get ID
            p0:= Pos(DEPENDENTIE_BEGIN_ID, f[I]);
            p1:= Pos(DEPENDENTIE_END_ID, f[I]);
            buffer:= Trim(Copy(f[I], p0+1, p1-1-p0));
            rec.ID:= StrToInt(buffer);
            // get Sentence
            buffer:= Trim(Copy(f[I], p1+1, Length(f[I])));
            rec.Sentence:= buffer;
          end
          else
          // Line with extracted triples
          if Pos(DEPENDENTIE_BEGIN_ARG1, f[I]) = DEPENDENTIE_POS_MATCH then
          begin
            // get Arg1
            p0:= Pos(DEPENDENTIE_BEGIN_ARG1, f[I]);
            p1:= Pos(DEPENDENTIE_BEGIN_REL, f[I]);
            a1:= Trim(Copy(f[I], p0+6, p1-1-p0-5));
            p0:= Pos(DEPENDENTIE_BEGIN_REL, f[I]);
            p1:= Pos(DEPENDENTIE_BEGIN_ARG2, f[I]);
            rel:= Trim(Copy(f[I], p0+6, p1-1-p0-5));
            p0:= Pos(DEPENDENTIE_BEGIN_ARG2, f[I]);
            p1:= Length(f[I]);
            a2:= Trim(Copy(f[I], p0+7, p1));
            rec.AddFact(a1, rel, a2, true, true, true);
          end;
        end;
      end;
    finally
      FreeAndNil(f);
    end;
  end
  else
  if system = osDptOIE then
  begin
    f:= TStringListUTF8.Create;
    try
      f.LoadFromFile(fileName);
      for I:= 1 to f.Count - 1 do
      begin
        values:= StringReplace(f[I], '"', '', [rfReplaceAll]). Split([';']);

        // Line with sentence
        if Trim(values[0]) <> '' then
        begin
          if Assigned(rec) then
            Result.Add(rec);
          rec:= TOpenIERecord.Create;
          // get ID
          rec.ID:= StrToInt(Trim(values[0]));
          // get Sentence
          rec.Sentence:= Trim(values[1]);
        end;
        // Line with extracted triples
        if Trim(values[3]) <> '' then
          // get rel
          rec.AddFact(values[3], values[4], values[5], true, true, true);
      end;
      // Last sentence
      if Assigned(rec) then
        Result.Add(rec);
    finally
      FreeAndNil(f);
    end;
  end
  else
  if system = osPragmaticOIE then
  begin
    f:= TStringListUTF8.Create;
    try
      f.LoadFromFile(fileName);
      for I:= 0 to f.Count - 1 do
      begin

        values:= StringReplace(f[I], '"', '', [rfReplaceAll]). Split([';']);

        // Line with sentence
        if Trim(values[0]) <> '' then
        begin
          if Assigned(rec) then
            Result.Add(rec);
          rec:= TOpenIERecord.Create;
          // get ID
          rec.ID:= StrToInt(values[0]);
          // get Sentence
          rec.Sentence:= Trim(values[1]);
        end;
        // Line with extracted triples
        if Trim(values[2]) <> '' then
          // get rel
          rec.AddFact(values[2], values[3], values[4], true, true, true);
      end;
      // Last sentence
      if Assigned(rec) then
        Result.Add(rec);
    finally
      FreeAndNil(f);
    end;
  end
  else
  if system = osArgOE then
  begin
    f:= TStringListUTF8.Create;
    try
      f.LoadFromFile(fileName);

      currentID:= 0;

      for I:= 0 to f.Count - 1 do
      begin

        values:= StringReplace(f[I], '"', '', [rfReplaceAll]). Split([#9]);

        aux:= StrToInt(StringReplace(values[0], ARGOE_BEGIN_ID, '', [rfReplaceAll]));

        // Line with sentence
        if aux <> currentID then
        begin
          if Assigned(rec) then
            Result.Add(rec);
          rec:= TOpenIERecord.Create;
          // get ID
          rec.ID:= aux;
          currentID:= aux;
          // get Sentence
          rec.Sentence:= '#';
        end;
        // Line with extracted triples
        if Length(values) = 4 then
          // get rel
          rec.AddFact(values[1], values[2], values[3], true, true, true);
      end;
      // Last sentence
      if Assigned(rec) then
        Result.Add(rec);
    finally
      FreeAndNil(f);
    end;
  end;

end;

class procedure TAnnotationFile.PrepareFileConvertion(baseFileName: String; out
  finalFileName: String; out f1: String; out f2: String);
begin
  f1:= baseFileName;
  f1:= StringReplace(f1, ExtractFileDir(f1), '', []);
  f1:= Copy(f1, 2, Length(f1));
  f2:= f1 + '.ann';
  finalFileName:= ExtractFileDir(ParamStr(0)) + DirectorySeparator + 'convert' + DirectorySeparator + f2;
  CopyFile(baseFileName, ExtractFileDir(ParamStr(0)) + DirectorySeparator + 'convert' + DirectorySeparator + f1, false);
  f1:= 'convert/' + f1;
  f2:= 'convert/' + f2;
end;

function TAnnotationFile.GetCurrentSentence: TOpenIERecord;
var
  I: Integer;
begin
  Result := Nil;
  if FCurrentSentenceID > 0 then
    if FRecords.Count > 0 then
      for I:= 0 to FRecords.Count - 1 do
        if ((FRecords[I] as TOpenIERecord).ID = FCurrentSentenceID) then
        begin
          Result:= (FRecords[I] as TOpenIERecord);
          Break;
        end;
end;

function TAnnotationFile.Split(base: String): TStringArray;
var
  f: TStringListUTF8;
  I: Integer;
begin
  SetLength(Result, 0);
  f:= TStringListUTF8.Create;
  try
    f.Delimiter:= #9;
    f.StrictDelimiter:= True;
    f.DelimitedText:= base;
    for I:= 0 to f.Count - 1 do
    begin
      SetLength(Result, I + 1);
      Result[I]:= f[I];
    end;
  finally
    FreeAndNil(f);
  end;
end;

function TAnnotationFile.RemoveLastTab(base: String): String;
begin
  Result:= base;
  if Pos(#9, Result) > 0 then
    Result:= Copy(Result, 1, Length(Result)-1);
end;

function TAnnotationFile.GetSentenceCount: Integer;
begin
  Result:= FRecords.Count;
end;

constructor TAnnotationFile.Create(fileName: String);
begin
  FCurrentSentenceID:= -1;
  FUpdated:= False;
  FFileName:= fileName;
  FRecords:= TObjectList.Create(True);
  loadFile();
  if FRecords.Count > 0 then
    FCurrentSentenceID := (FRecords[0] as TOpenIERecord).ID;
end;

constructor TAnnotationFile.Create;
begin
  FCurrentSentenceID:= -1;
  FUpdated:= False;
  FFileName:= '';
  FRecords:= TObjectList.Create(True);
end;

destructor TAnnotationFile.Destroy;
begin
  if Assigned(FRecords) then
  begin
    FRecords.RemoveAll;
    FreeAndNil(FRecords);
  end;
  inherited Destroy;
end;

procedure TAnnotationFile.SaveFile;
var
  f: TStringListUTF8;
  I, J: Integer;
  buffer: String;
begin
  f:= TStringListUTF8.Create;
  try
    for I:= 0 to FRecords.Count - 1 do
    begin
      // ID
      f.Add(SECNAME_SENTID);
      f.Add(IntToStr((FRecords[I] as TOpenIERecord).ID));
      // Sentence
      f.Add(SECNAME_SENT);
      f.Add((FRecords[I] as TOpenIERecord).Sentence);
      // Tokens
      f.Add(SECNAME_TOKEN);
      buffer:= '';
      for J:= 0 to (FRecords[I] as TOpenIERecord).Tokens.Count - 1 do
        buffer:= buffer + (FRecords[I] as TOpenIERecord).Tokens[J] + #9;
      f.Add(RemoveLastTab(buffer));
      // POS
      f.Add(SECNAME_POS);
      buffer:= '';
      for J:= 0 to (FRecords[I] as TOpenIERecord).POSs.Count - 1 do
        buffer:= buffer + (FRecords[I] as TOpenIERecord).POSs[J] + #9;
      f.Add(RemoveLastTab(buffer));
      // Chunk
      f.Add(SECNAME_CHUNK);
      for J:= 0 to (FRecords[I] as TOpenIERecord).Chunks.Count - 1 do
        f.Add(((FRecords[I] as TOpenIERecord).Chunks[J] as TChunk).ToStringFile);
      // DEP
      f.Add(SECNAME_DEP);
      for J:= 0 to (FRecords[I] as TOpenIERecord).DEPs.Count - 1 do
        f.Add((FRecords[I] as TOpenIERecord).DEPs[J]);

      // OPEN IE
      f.Add(SECNAME_OPENIE);
      for J:= 0 to (FRecords[I] as TOpenIERecord).Annotations.Count - 1 do
        f.Add(((FRecords[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).ToStringFile);
      // Save to file...
      f.SaveToFile(FFileName);
      //
      FUpdated:= False;
    end;
  finally
    FreeAndNil(f);
  end;
end;

procedure TAnnotationFile.SetFileName(fileName: String);
begin
  FFileName:= fileName;
end;

function TAnnotationFile.GetFileName: String;
begin
  Result:= FFileName;
end;

procedure TAnnotationFile.SetRecords(value: TObjectList);
begin
  FRecords.Clear;
  FRecords.Assign(value);
end;

procedure TAnnotationFile.ExportToFile(format: TFormatFile; fileName: String);
var
  I,J: Integer;
  f: TStringListUTF8;
begin
  if format = ffTabs then
  begin
    f:= TStringListUTF8.Create;
    try
      for I:= 0 to FRecords.Count - 1 do
      begin
        f.Add(IntToStr((FRecords[I] as TOpenIERecord).ID) + #9 + (FRecords[I] as TOpenIERecord).Sentence);
        for J:= 0 to (FRecords[I] as TOpenIERecord).Annotations.Count - 1 do
          f.Add(((FRecords[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).ToStringFile);
      end;
      f.SaveToFile(fileName);
    finally
      FreeAndNil(f);
    end;
  end
  else
  if format = ffIberlef then
  begin
    f:= TStringListUTF8.Create;
    f.Add('SENTENCE_ID' + #9 +
          'RELATION_ID' + #9 +
          'SENTENCE' + #9 +
          'ARGUMENT_1' + #9 +
          'ARGUMENT_1_CATEGORY1' + #9 +
          'RELATION' + #9 +
          'ARGUMENT_2' + #9 +
          'ARGUMENT_2_CATEGORY');
    try
      for I:= 0 to FRecords.Count - 1 do
      begin
        for J:= 0 to (FRecords[I] as TOpenIERecord).Annotations.Count - 1 do
          f.Add(IntToStr((FRecords[I] as TOpenIERecord).ID) + #9 +
                IntToStr(J+1) + #9 +
                (FRecords[I] as TOpenIERecord).Sentence + #9 +
                ((FRecords[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).Arg1 + #9 +
                'none' + #9 +
                ((FRecords[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).Rel + #9 +
                ((FRecords[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).Arg2 + #9 +
                'none'
          );
      end;
      f.SaveToFile(fileName);
    finally
      FreeAndNil(f);
    end;
  end
  else
    raise Exception.Create('Format is not valid!');
end;

class procedure TAnnotationFile.ConvertOIEToAnnotationFile(system: TOIESystem;
  sourceFileName: String);
var
  triples: TObjectList;
  //f: TStringListUTF8;
  //I, J: Integer;
  annFile: TAnnotationFile;
begin


  triples:= TAnnotationFile.LoadExtractionsFromFile(system, sourceFileName);

  annFile:= TAnnotationFile.Create;
  try
    annFile.SetFileName(sourceFileName + '.ann');
    annFile.SetRecords(triples);
    annFile.SaveFile;
  finally
    FreeAndNil(annFile);
  end;

  {
  f:= TStringListUTF8.Create;
  try
    for I:= 0 to triples.Count -1 do
    begin
      f.Add(IntToStr((triples[I] as TOpenIERecord).ID) + #9 + (triples[I] as TOpenIERecord).Sentence);
      for J:= 0 to (triples[I] as TOpenIERecord).Annotations.Count -1 do
        f.Add( ((triples[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).Arg1 + #9 +
               ((triples[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).Rel + #9 +
               ((triples[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation).Arg2);
    end;
    f.SaveToFile(sourceFileName + '.txt');
  finally
    FreeAndNil(f);
  end;
  }
end;

function TAnnotationFile.GetAllAnnotations: TObjectList;
var
  I, J: Integer;
begin
  Result:= TObjectList.Create(False);
  for I:= 0 to FRecords.Count -1 do
    for J:= 0 to (FRecords[I] as TOpenIERecord).Annotations.Count -1 do
      Result.Add((FRecords[I] as TOpenIERecord).Annotations[J] as TOpenIEAnnotation);
end;

function TAnnotationFile.GetRecordByIndex(index: Integer): TOpenIERecord;
begin
  if index > FRecords.Count -1 then
    Result:= Nil
  else
    Result:= FRecords.Items[index] as TOpenIERecord;
end;

function TAnnotationFile.GetRecordById(id: Integer): TOpenIERecord;
var
  I: Integer;
begin
  Result:= Nil;
  if Assigned(FRecords) then
  begin
    for I:= 0 to FRecords.Count -1 do
      if (FRecords.Items[I] as TOpenIERecord).FID  = id then
      begin
        Result:= FRecords.Items[I] as TOpenIERecord;
        Break;
      end;
  end;
end;

function TAnnotationFile.GetFactProperties(rec: Integer; ann: TOpenIEAnnotation): TOpenIEAnnotation;
var
  I: Integer;
begin
  Result:= Nil;
  for I:= 0 to Records.GetRecordByID(rec).Annotations.Count -1 do
  begin
    if (Records.GetRecordByID(rec).Annotations[I] as TOpenIEAnnotation).EqualsEx(ann, ecPartial) then
    begin
      Result:= (Records.GetRecordByID(rec).Annotations[I] as TOpenIEAnnotation);
      Break;
    end;
  end;
end;

procedure TAnnotationFile.AddFactBySentenceID(id: Integer; arg1, rel,
  arg2: String);
begin
  GetRecordById(id).AddFact(arg1, rel, arg2, true, true, true);
  //GetRecordByIndex(id).AddFact(arg1, rel, arg2, true, true, true);
end;

end.

