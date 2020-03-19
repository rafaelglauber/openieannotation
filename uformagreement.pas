unit uformagreement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, utypes, contnrs, UClasses;

type

  { TFormAgreement }

  TFormAgreement = class(TForm)
    buttonClearListFile: TSpeedButton;
    buttonRun: TSpeedButton;
    buttonUnify: TSpeedButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabelCurrentFile: TLabel;
    listAnnotationFile: TListBox;
    buttonAddFile: TSpeedButton;
    memoAgreementInfo: TMemo;
    OpenDialogAnnotation: TOpenDialog;
    RadioGroupMode: TRadioGroup;
    RadioGroupEqual: TRadioGroup;
    SaveDialogAnnotation: TSaveDialog;
    procedure buttonAddFileClick(Sender: TObject);
    procedure buttonClearListFileClick(Sender: TObject);
    procedure buttonUnifyClick(Sender: TObject);
    procedure buttonRunClick(Sender: TObject);
    procedure listAnnotationFileClick(Sender: TObject);
  private
    procedure CreateAgreeFile;
    procedure CreateAgreeFileRound2;
    procedure CreateUnifyFile(fileName: String);
    function ValidateFiles(anns: TObjectList): Boolean;
  public
    class procedure Show;
  end;

var
  FormAgreement: TFormAgreement;

implementation

uses uutils;

{$R *.lfm}

{ TFormAgreement }

procedure TFormAgreement.buttonClearListFileClick(Sender: TObject);
begin
  listAnnotationFile.Clear;
  memoAgreementInfo.Lines.Text:= 'none';
  LabelCurrentFile.Caption:= 'Current file';
end;

procedure TFormAgreement.buttonUnifyClick(Sender: TObject);
begin
  if mrYes = MessageDlg(MSG_CAPTION_QUESTION, MSG_MESSAGE_UNIFY_FILES,
                        mtConfirmation, [mbYes, mbNo], 0) then
    if SaveDialogAnnotation.Execute then
      CreateUnifyFile(SaveDialogAnnotation.FileName);
end;

procedure TFormAgreement.buttonRunClick(Sender: TObject);
begin
  if listAnnotationFile.Count > 1 then
  begin
    if RadioGroupMode.ItemIndex = 0 then
    begin
      CreateAgreeFile();
      if FileExists(FILE_AGREEMENT_STEP1) then
        ExecN2oieProcess(joAgree1, []);
      if FileExists(FILE_RETURN_STEP1) then
        memoAgreementInfo.Lines.LoadFromFile(FILE_RETURN_STEP1);
    end
    else
    begin
      CreateAgreeFileRound2;
      if FileExists(FILE_AGREEMENT_STEP2) then
        ExecN2oieProcess(joAgree2, []);
      if FileExists(FILE_RETURN_STEP2) then
        memoAgreementInfo.Lines.LoadFromFile(FILE_RETURN_STEP2);
    end;
  end
  else
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_MIN_NUM_FILES, mtError, [mbCancel], 0);
end;

procedure TFormAgreement.listAnnotationFileClick(Sender: TObject);
begin
  if listAnnotationFile.ItemIndex > -1 then
    LabelCurrentFile.Caption:= listAnnotationFile.Items[listAnnotationFile.ItemIndex];
end;

procedure TFormAgreement.CreateAgreeFile;
var
  I: Integer;
  anns: TObjectList;
  ttype: TEqualCalc;

  procedure CreateFile;
  var
    r: TStringList;
    J, K: Integer;
    allOnFile: TObjectList;
    all: TObjectList;
    negative: Integer;
  begin
    r:= TStringList.Create;
    all:= TObjectList.Create(False);
    try
      // Load all open ie annotations...
      for J:= 0 to anns.Count - 1 do
      begin
        allOnFile:= (anns[J] as TAnnotationFile).GetAllAnnotations;
        try
          for K:= 0 to allOnFile.Count -1 do
            if not (all.ContainAnnotation(allOnFile[K] as TOpenIEAnnotation, ttype)) then
              all.Add(allOnFile[K] as TOpenIEAnnotation);
        finally
          FreeAndNil(allOnFile);
        end;
      end;
      // Initialize negative variable
      negative:= all.Count;

      // Check agreement in each file
      for J:= 0 to all.Count - 1 do
      begin
        r.Add('');
        for K:= 0 to anns.Count -1 do
        begin
          allOnFile:= (anns[K] as TAnnotationFile).GetAllAnnotations;
          try
            if (allOnFile.ContainAnnotation(all[J] as TOpenIEAnnotation, ttype)) then
              r[r.Count-1]:= r[r.Count-1] + IntToStr(J+1) + #9
            else
            begin
              Inc(negative);
              r[r.Count-1]:= r[r.Count-1] + IntToStr(negative) + #9;
            end;
          finally
            FreeAndNil(allOnFile);
          end;
        end;
      end;
      // Remove final tab caracter
      for J:= 0 to r.Count -1 do
        r[J]:= Copy(r[J], 1, Length(r[J]) - 1);
      // Save agreement file
      r.SaveToFile(FILE_AGREEMENT_STEP1);
    finally
      FreeAndNil(r);
      FreeAndNil(all);
    end;
  end;

begin
  ttype:= ecFull;
  if RadioGroupEqual.ItemIndex = 1 then
    ttype:= ecPartial
  else if RadioGroupEqual.ItemIndex = 2 then
    ttype:= ecText;

  // Remove integration files with java program
  if FileExists(FILE_AGREEMENT_STEP1) then
    DeleteFile(FILE_AGREEMENT_STEP1);

  if FileExists(FILE_RETURN_STEP1) then
    DeleteFile(FILE_RETURN_STEP1);

  memoAgreementInfo.Lines.Text:= 'none';

  anns:= TObjectList.Create(True);
  try
    for I:= 0 to listAnnotationFile.Count - 1 do
      anns.Add(TAnnotationFile.Create(listAnnotationFile.Items[I]));

    if not ValidateFiles(anns) then
      MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGEM_INVALID_FILES, mtError, [mbCancel], 0)
    else
      CreateFile;

    anns.RemoveAll;
  finally
    FreeAndNil(anns);
  end;
end;

procedure TFormAgreement.CreateAgreeFileRound2;
var
  I: Integer;
  anns: TObjectList;
  procedure CreateFile;
  var
    r: TStringList;
    J, K, W: Integer;
    line: String;
    fileBase: TAnnotationFile;
    recordBase: TOpenIERecord;
    annotationBase: TOpenIEAnnotation;
    anotherAnnotation: TOpenIEAnnotation;
  begin
    r:= TStringList.Create;
    try
      // file base
      fileBase:= anns[0] as TAnnotationFile;
      //
      for J:= 0 to fileBase.Records.Count -1 do
      begin
        recordBase:= fileBase.GetRecordByIndex(J);
        for K:= 0 to recordBase.Annotations.Count - 1 do
        begin
          annotationBase:= recordBase.Annotations[K] as TOpenIEAnnotation;
          // Init line
          line:= BoolToStr((recordBase.Annotations[K] as TOpenIEAnnotation).ValidFact, '1', '0') + #9;
          for W:= 1 to anns.Count - 1 do
          begin
            anotherAnnotation:= (anns[W] as TAnnotationFile).GetFactProperties(recordBase.ID, annotationBase);
            if not Assigned(anotherAnnotation) then
            begin
              MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGEM_INVALID_FILES, mtError, [mbCancel], 0);
              Abort;
            end
            else
              line:= line + BoolToStr(anotherAnnotation.ValidFact, '1', '0') + #9;
          end;
          //
          r.Add(Copy(line, 1, Length(line) - 1));
        end;
      end;
      // Save agreement file
      r.SaveToFile(FILE_AGREEMENT_STEP2);
    finally
      FreeAndNil(r);
    end;
  end;

begin
  // Remove integration files with java program
  if FileExists(FILE_AGREEMENT_STEP1) then
    DeleteFile(FILE_AGREEMENT_STEP1);

  if FileExists(FILE_RETURN_STEP2) then
    DeleteFile(FILE_RETURN_STEP2);

  memoAgreementInfo.Lines.Text:= 'none';

  anns:= TObjectList.Create(True);
  try
    for I:= 0 to listAnnotationFile.Count - 1 do
      anns.Add(TAnnotationFile.Create(listAnnotationFile.Items[I]));

    if not ValidateFiles(anns) then
      MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGEM_INVALID_FILES, mtError, [mbCancel], 0)
    else
      CreateFile;

    anns.RemoveAll;
  finally
    FreeAndNil(anns);
  end;

end;

procedure TFormAgreement.CreateUnifyFile(fileName: String);
var
  anns: TObjectList;
  I: Integer;

  procedure UnifyFile;
  var
    J, K, W: Integer;
    annotationFile: TAnnotationFile;
    activeRecord: TOpenIERecord;
    activeAnnotation: TOpenIEAnnotation;
  begin
    // First file is the reference (correct annotation formats - pos, dep, chunker...)
    annotationFile:= (anns[0] as TAnnotationFile);
    try
      for W:= 1 to anns.Count -1 do
      begin
        for J:= 0 to annotationFile.Records.Count -1 do
        begin
          // Active record
          activeRecord:= (anns[W] as TAnnotationFile).GetRecordById(J+1);

          // Caso o arquivo de extração possua a sentença
          if Assigned(activeRecord) then
          begin
            for K:= 0 to activeRecord.Annotations.Count -1 do
            begin
              // Active annotation
              activeAnnotation:= (activeRecord.Annotations[K] as TOpenIEAnnotation);

              if not annotationFile.GetRecordByIndex(J).IsDuplicatedFact(
                   activeAnnotation.Arg1,
                   activeAnnotation.Rel,
                   activeAnnotation.Arg2) then
                   if annotationFile.GetRecordByIndex(J).IsValidFact(
                        activeAnnotation.Arg1,
                        activeAnnotation.Rel,
                        activeAnnotation.Arg2) then
                      annotationFile.GetRecordByIndex(J).AddFact(
                          activeAnnotation.Arg1,
                          activeAnnotation.Rel,
                          activeAnnotation.Arg2,
                          activeAnnotation.Minimal,
                          True,
                          True);
            end;
          end
          else
            memoAgreementInfo.Lines.Add('Record: ' + IntToStr(J) + ' file: ' + annotationFile.GetFileName);
        end;
      end;
      annotationFile.SetFileName(fileName);
      annotationFile.SaveFile;
    finally
      annotationFile:= Nil;
      activeRecord:= Nil;
      activeAnnotation:= Nil;
    end;
  end;

begin
  anns:= TObjectList.Create(True);
  try
    for I:= 0 to listAnnotationFile.Count - 1 do
      anns.Add(TAnnotationFile.Create(listAnnotationFile.Items[I]));

    //if not ValidateFiles(anns) then
    //  MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGEM_INVALID_FILES, mtError, [mbCancel], 0)
    //else
      UnifyFile;

    anns.RemoveAll;
  finally
    FreeAndNil(anns);
  end;
end;

function TFormAgreement.ValidateFiles(anns: TObjectList): Boolean;
var
  aux: Integer;
  I: Integer;
begin
  Result:= anns.Count > 1;
  if Result then
  begin
    aux:= -1;
    for I:= 0 to anns.Count - 1 do
    begin
      if (aux = -1) then
        aux:= (anns[I] as TAnnotationFile).SentenceCount
      else
        if (aux <> (anns[I] as TAnnotationFile).SentenceCount) then
        begin
          Result:= False;
          Break;
        end;
    end;
  end;
end;

class procedure TFormAgreement.Show;
begin
  with FormAgreement do
  begin
    ShowModal;
  end;
end;

procedure TFormAgreement.buttonAddFileClick(Sender: TObject);
var
  fileName: String;
  I: Integer;
  test: Boolean;
begin
  // Open File
  if OpenDialogAnnotation.Execute then
  begin
    fileName := OpenDialogAnnotation.FileName;
    test:= False;
    for I:= 0 to listAnnotationFile.Count - 1 do
      if listAnnotationFile.Items[I] = fileName then
        test:= True;
    //
    if test then
    begin
      MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_DUPLICATED_FILE, mtError, [mbCancel], 0);
      Exit;
    end
    else
      listAnnotationFile.Items.Add(fileName);
  end;
end;

end.

