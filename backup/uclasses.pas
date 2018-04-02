unit UClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TObjectListHelper }

  TObjectListHelper = class helper for TObjectList
    procedure RemoveAll;
  end;

  TLoadForm = (lfAll, lfChunk, lfAnnotation);

  TFormatFile = (ffTabs);

  TStringArray = array of String;

  { TOpenIEAnnotation }

  TOpenIEAnnotation = class
  private
    FArg1: String;
    FArg2: String;
    FRel: String;
    FMinimalist: Boolean;
   public
     constructor Create(arg1, rel, arg2: String; minimalist: Boolean); overload;
     property Arg1: String read FArg1;
     property Rel: String read FRel;
     property Arg2: String read FArg2;
     property Minimalist: Boolean read FMinimalist;
     function ToStringFile: String;
     function ToString: ansistring; override;
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
  private
    FID: Integer;
    FSentence: String;
    FTokens: TStringList;
    FPOSs: TStringList;
    FChunks: TObjectList;
    FAnnotations: TObjectList;
  public
    constructor Create(id: Integer); overload;
    destructor Destroy(); override;
    function setSentence(sent: String): TOpenIERecord;
    function setTokens(tokens: TStringList): TOpenIERecord;
    function setPOSs(poss: TStringList): TOpenIERecord;
    function setChunks(chunks: TObjectList): TOpenIERecord;
    function setAnnotations(annotations: TObjectList): TOpenIERecord;
    property ID: Integer read FID;
    property Sentence: String read FSentence;
    property SentenceAndPos: String read GetSentenceAndPos;
    property Chunks: TObjectList read FChunks;
    property Annotations: TObjectList read FAnnotations;
    property Tokens: TStringList read FTokens;
    property POSs: TStringList read FPOSs;
    procedure AddFact(arg1, rel, arg2: String; minimalist: Boolean);
    procedure RemoveFact(index: Integer);
    function ToString: ansistring; override;
    function IsDuplicateFact(arg1, rel, arg2: String): Boolean;
  end;

  { TAnnotationFile }

  TAnnotationFile = class
    function GetCurrentSentence: TOpenIERecord;
  private
    FFileName: String;
    FRecords: TObjectList;
    FUpdated: Boolean;
    FCurrentSentenceID: Integer;
    function Split(base: String): TStringArray;
    procedure LoadFile;
  public
    constructor Create(fileName: String); overload;
    destructor Destroy(); override;
    property Updated: Boolean read FUpdated write FUpdated;
    property CurrentSentenceID: Integer read FCurrentSentenceID write FCurrentSentenceID;
    property CurrentSentence: TOpenIERecord read GetCurrentSentence;
    property Records: TObjectList read FRecords;
    procedure SaveFile;
    procedure SetFileName(fileName: String);
    procedure ExportToFile(format: TFormatFile; fileName: String);
  end;

implementation

const
  SECNAME_SENTID = 'SENTID';
  SECNAME_SENT   = 'SENT';
  SECNAME_TOKEN  = 'TOKEN';
  SECNAME_POS    = 'POS';
  SECNAME_CHUNK  = 'CHUNK';
  SECNAME_OPENIE = 'OPENIE';
  COMMENT_SYMBOL = '#';

{ TObjectListHelper }

procedure TObjectListHelper.RemoveAll;
var
  I: Integer;
begin
  for I:= 0 to Self.Count -1 do
    Self.Remove(Self.Items[I]);
end;

{ TOpenIERecord }

constructor TOpenIERecord.Create(id: Integer);
begin
  FID:= id;
end;

destructor TOpenIERecord.Destroy();
begin
  if Assigned(FTokens) then
    FreeAndNil(FTokens);

  if Assigned(FPOSs) then
    FreeAndNil(FPOSs);

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

function TOpenIERecord.setSentence(sent: String): TOpenIERecord;
begin
  FSentence:= sent;
  Result:= Self;
end;

function TOpenIERecord.setTokens(tokens: TStringList): TOpenIERecord;
begin
  FTokens:= tokens;
  Result:= Self;
end;

function TOpenIERecord.setPOSs(poss: TStringList): TOpenIERecord;
begin
  FPOSs:= poss;
  Result:= Self;
end;

function TOpenIERecord.setChunks(chunks: TObjectList): TOpenIERecord;
begin
  FChunks:= chunks;
  Result:= Self;
end;

function TOpenIERecord.setAnnotations(annotations: TObjectList): TOpenIERecord;
begin
  FAnnotations:= annotations;
  Result:= Self;
end;

procedure TOpenIERecord.AddFact(arg1, rel, arg2: String; minimalist: Boolean);
begin
  if not Assigned(FAnnotations) then
    FAnnotations:= TObjectList.Create(True);
  FAnnotations.Add(TOpenIEAnnotation.Create(arg1, rel, arg2, minimalist));
end;

procedure TOpenIERecord.RemoveFact(index: Integer);
begin
  if Assigned(FAnnotations) then
    FAnnotations.Remove(FAnnotations.Items[index]);
end;

function TOpenIERecord.ToString: ansistring;
begin
  Result:= IntToStr(FID) + ',"' + StringReplace(FSentence, ',', ';', [rfReplaceAll]) + '"';
end;

function TOpenIERecord.IsDuplicateFact(arg1, rel, arg2: String): Boolean;
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

function TOpenIERecord.GetSentenceAndPos: String;
var
  I: Integer;
begin
  Result:= '';
  for I:= 0 to FTokens.Count - 1 do
    Result:= Result + FTokens[I] + '/' + FPOSs[I] + ' ';
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

constructor TOpenIEAnnotation.Create(arg1, rel, arg2: String;
  minimalist: Boolean);
begin
  FArg1:= arg1;
  FArg2:= arg2;
  FRel:= rel;
  FMinimalist:= minimalist;
end;

function TOpenIEAnnotation.ToStringFile: String;
var
  min: String;
begin
  if FMinimalist then
    min:= '1'
  else
    min:= '0';
  Result:= FArg1 + #9 + FRel + #9 + FArg2 + #9 + min;
end;

function TOpenIEAnnotation.ToString: ansistring;
var
  min: String;
begin
  if FMinimalist then
    min:= '1'
  else
    min:= '0';
  Result:= '"' + FArg1 + '","' + FRel + '","' + FArg2 + '","' + min + '"';
end;

{ TAnnotationFile }

procedure TAnnotationFile.LoadFile;
var
  f: TStringList;
  I: Integer;
  currentSection: String;
  sentId, sent, token, pos: String;
  chunk, openie: TStringList;

  // Create a new record from file
  function newRecordFromFile(): TOpenIERecord;
  var
    buffer: TStringArray;
    K: Integer;
    t: TStringList;
    l: TObjectList;
  begin
    // Create result object
    Result := TOpenIERecord.Create(StrToInt(sentId)).setSentence(sent);
    // Get tokens
    buffer := Split(token);
    t:= TStringList.Create;
    try
      for K:= Low(buffer) to High(buffer) do
        t.Add(buffer[K]);
      Result.setTokens(t);
    finally
      t:= Nil;
    end;
    // Get POSs
    buffer := Split(pos);
    t:= TStringList.Create;
    try
      for K:= Low(buffer) to High(buffer) do
        t.Add(buffer[K]);
      Result.setPOSs(t);
    finally
      t:= Nil;
    end;
    // Get chunks
    l:= TObjectList.Create;
    try
      for K:= 0 to chunk.Count - 1 do
      begin
        buffer:= Split(chunk[K]);
        l.Add(TChunk.Create(buffer[0], StrToInt(buffer[1]), StrToInt(buffer[2]), buffer[3]));
      end;
      Result.setChunks(l);
    finally
      l:= Nil;
    end;
    // Get openie
    l:= TObjectList.Create;
    try
      for K:= 0 to openie.Count - 1 do
      begin
        buffer:= Split(openie[K]);
        l.Add(TOpenIEAnnotation.Create(buffer[0], buffer[1], buffer[2], buffer[3]='1'));
      end;
      Result.setAnnotations(l);
    finally
      l:= Nil;
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
  chunk:= TStringList.Create;
  openie:= TStringList.Create;
  f:= TStringList.Create();
  try
    f.LoadFromFile(FFileName);

    if (f.Count = 0) then
      raise Exception.Create('File is empty <' + FFileName + '>!');

    for I:= 0 to f.Count - 1 do
    begin
      if (Trim(f[I]) = '') then
        raise Exception.Create('Not supported empty lines <line ' + IntToStr(I + 1) + '>!');

      // Don't load comment lines
      if (f[I][1] <> COMMENT_SYMBOL) then
      begin
        // Set current section
        if (f[I] = SECNAME_SENT) or
           (f[I] = SECNAME_TOKEN) or
           (f[I] = SECNAME_POS) or
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
    FreeAndNil(openie);
  end;
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
  f: TStringList;
  I: Integer;
begin
  SetLength(Result, 0);
  try
    f:= TStringList.Create;
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

constructor TAnnotationFile.Create(fileName: String);
begin
  FCurrentSentenceID:= -1;
  FUpdated:= False;
  FFileName:= fileName;
  FRecords:= TObjectList.Create;
  loadFile();
  if FRecords.Count > 0 then
  begin
    FCurrentSentenceID := (FRecords[0] as TOpenIERecord).ID;
  end;
end;

destructor TAnnotationFile.Destroy();
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
  f: TStringList;
  I, J: Integer;
  buffer: String;
begin
  f:= TStringList.Create;
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
      f.Add(buffer);
      // POS
      f.Add(SECNAME_POS);
      buffer:= '';
      for J:= 0 to (FRecords[I] as TOpenIERecord).POSs.Count - 1 do
        buffer:= buffer + (FRecords[I] as TOpenIERecord).POSs[J] + #9;
      f.Add(buffer);
      // Chunk
      f.Add(SECNAME_CHUNK);
      for J:= 0 to (FRecords[I] as TOpenIERecord).Chunks.Count - 1 do
        f.Add(((FRecords[I] as TOpenIERecord).Chunks[J] as TChunk).ToStringFile);
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

procedure TAnnotationFile.ExportToFile(format: TFormatFile; fileName: String);
var
  I,J: Integer;
  f: TStringList;
begin
  if format = ffTabs then
  begin
    f:= TStringList.Create;
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
    raise Exception.Create('Format is not valid!');
end;

end.

