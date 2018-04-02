unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, Grids, Buttons, UClasses, HtmlView;

type

  { TMainForm }

  TMainForm = class(TForm)
    buttonClearArguments: TSpeedButton;
    buttonAddArgument1: TSpeedButton;
    buttonDeleteCurrentFact: TSpeedButton;
    buttonAddFact: TSpeedButton;
    buttonAddArgument2: TSpeedButton;
    buttonAddRelationship: TSpeedButton;
    checkBoxMinimalist: TCheckBox;
    editArg1: TEdit;
    editArg2: TEdit;
    editRel: TEdit;
    gridFact: TStringGrid;
    MI_AGREE: TMenuItem;
    MI_KAPPA: TMenuItem;
    viewCurrentSentence: THtmlViewer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labelCurrentFile: TLabel;
    MainMenu: TMainMenu;
    memoCurrentPOSSentence: TMemo;
    MI_EXP_RELONLY: TMenuItem;
    MI_EXPORT: TMenuItem;
    MI_SAVE: TMenuItem;
    MI_FILE: TMenuItem;
    MI_OPEN_FILE: TMenuItem;
    MI_SAVE_AS: TMenuItem;
    MI_SEPARATE1: TMenuItem;
    MI_EXIT: TMenuItem;
    OpenDialogAnnotationFile: TOpenDialog;
    buttonShowSentences: TSpeedButton;
    gridChunk: TStringGrid;
    SaveDialogExportTXT: TSaveDialog;
    SaveDialogAnnotationFile: TSaveDialog;
    procedure buttonAddArgument1Click(Sender: TObject);
    procedure buttonAddArgument2Click(Sender: TObject);
    procedure buttonAddFactClick(Sender: TObject);
    procedure buttonAddRelationshipClick(Sender: TObject);
    procedure buttonClearArgumentsClick(Sender: TObject);
    procedure buttonDeleteCurrentFactClick(Sender: TObject);
    procedure buttonShowSentencesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridChunkClick(Sender: TObject);
    procedure gridFactClick(Sender: TObject);
    procedure memoCurrentPOSSentenceChange(Sender: TObject);
    procedure MI_EXP_RELONLYClick(Sender: TObject);
    procedure MI_KAPPAClick(Sender: TObject);
    procedure MI_OPEN_FILEClick(Sender: TObject);
    procedure MI_EXITClick(Sender: TObject);
    procedure MI_SAVEClick(Sender: TObject);
    procedure MI_SAVE_ASClick(Sender: TObject);
    procedure viewCurrentSentenceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FAnnotationFile: TAnnotationFile;
    FFormTitle: String;
    procedure InitForm;
    procedure LoadFile;
    procedure SaveFile(fileName: String = '');
    procedure LoadFormValues(mode: TLoadForm = lfAll);
    function CheckUpdate: Boolean;
    function ValidateFact: Boolean;
    function ValidateDelFact: Boolean;
    function GetCurrentChunkText: String;
  public

  end;

resourcestring
  MSG_CAPTION_QUESTION       = 'Question';
  MSG_CAPTION_WARNING        = 'Warning';
  MSG_CAPTION_ERRO           = 'Error';
  MSG_MESSAGE_PENDENT_UPDATE = 'Do you want to save pending changes?';
  MSG_MESSAGE_NO_PENDINGS    = 'No changes is current file.';
  MSG_MESSAGE_NO_VALUE       = 'The field needs a value.';
  MSG_MESSAGE_NO_ANNOTATIONF = 'There are no annotation in progress.';
  MSG_MESSAGE_NO_CURRENTSENT = 'A sentence for annotation has not been selected.';
  MSG_MESSAGE_NO_FACTS       = 'There are no facts to remove it.';
  MSG_MESSAGE_DUPLICATED     = 'This fact is duplicated, please modify!';

var
  MainForm: TMainForm;

implementation

uses
  uformsentlist;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.InitForm;
var
  I: Integer;
begin
  editArg1.Clear;
  editArg2.Clear;
  editRel.Clear;
  viewCurrentSentence.Clear;
  memoCurrentPOSSentence.Clear;

  gridChunk.RowCount:= 1;
  gridFact.RowCount:= 1;
  FormSentences.gridSentences.RowCount:= 1;
  //for I:= 1 to gridChunk.RowCount - 3 do
  //  gridChunk.DeleteRow(I);
  //for I:= 1 to gridFact.RowCount - 3 do
  //  gridFact.DeleteRow(I);
end;

procedure TMainForm.LoadFile;
var
  fileName: String;
begin
  // Open File
  if OpenDialogAnnotationFile.Execute then
  begin
    fileName := OpenDialogAnnotationFile.FileName;
    labelCurrentFile.Caption:= fileName;
    if Assigned(FAnnotationFile) then
      FreeAndNil(FAnnotationFile);
    FAnnotationFile := TAnnotationFile.Create(fileName);
  end;
end;

procedure TMainForm.SaveFile(fileName: String = '');
begin
  if fileName <> '' then
    FAnnotationFile.SetFileName(fileName);
  FAnnotationFile.SaveFile;
end;

procedure TMainForm.LoadFormValues(mode: TLoadForm = lfAll);
var
  currentSentence: TOpenIERecord;
  I: Integer;
begin
  if Assigned(FAnnotationFile) then
  begin
    // Set caption form
    if FAnnotationFile.Updated then
      Self.Caption:= FFormTitle + '*'
    else
      Self.Caption:= FFormTitle;
    // Load current sentence
    currentSentence:= FAnnotationFile.GetCurrentSentence();
    //
    if Assigned(currentSentence) then
    begin
      if (mode = lfAll) then
      begin
        viewCurrentSentence.LoadFromString('<p>' + currentSentence.Sentence + '</p>');
        memoCurrentPOSSentence.Text:= currentSentence.SentenceAndPos;
      end;
      // Load chunks
      if Assigned(currentSentence.Chunks) and (mode in [lfAll, lfChunk])  then
      begin
        gridChunk.RowCount:= 1;
        for I:= 0 to currentSentence.Chunks.Count - 1 do
        begin
          gridChunk.RowCount:= gridChunk.RowCount + 1;
          gridChunk.Rows[gridChunk.RowCount - 1].CommaText:=
                           (currentSentence.Chunks[I] as TChunk).ToString;
        end;
      end;
      // Load annotations
      if Assigned(currentSentence.Annotations) and (mode in [lfAll, lfAnnotation]) then
      begin
        gridFact.RowCount:= 1;
        for I:= 0 to currentSentence.Annotations.Count - 1 do
        begin
          gridFact.RowCount:= gridFact.RowCount + 1;
          gridFact.Rows[gridFact.RowCount - 1].CommaText:=
                           (currentSentence.Annotations[I] as TOpenIEAnnotation).ToString;
        end;
      end;
    end;
    // Load sentence list
    if Assigned(FAnnotationFile.Records) and (mode = lfAll) then
    begin
      FormSentences.gridSentences.RowCount:= 1;
      for I:= 0 to FAnnotationFile.Records.Count - 1 do
      begin
        FormSentences.gridSentences.RowCount:= FormSentences.gridSentences.RowCount + 1;
        FormSentences.gridSentences.Rows[FormSentences.gridSentences.RowCount - 1].CommaText:=
            (FAnnotationFile.Records[I] as TOpenIERecord).ToString;
      end;
    end;
    currentSentence:= Nil;
  end;
end;

function TMainForm.CheckUpdate: Boolean;
begin
  // Check updated state
  Result:= Assigned(FAnnotationFile) and FAnnotationFile.Updated;
end;

function TMainForm.ValidateFact: Boolean;
begin
  //
  Result:= True;
  if not Assigned(FAnnotationFile) then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_ANNOTATIONF, mtError, [mbCancel], 0);
    FocusControl(editArg1);
    Result:= False;
  end
  else if FAnnotationFile.CurrentSentenceID <= 0 then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_CURRENTSENT, mtError, [mbCancel], 0);
    FocusControl(editArg1);
    Result:= False;
  end
  else if editArg1.Text = '' then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_VALUE, mtError, [mbCancel], 0);
    FocusControl(editArg1);
    Result:= False;
  end;
  if editArg2.Text = '' then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_VALUE, mtError, [mbCancel], 0);
    FocusControl(editArg2);
    Result:= False;
  end;
  if editRel.Text = '' then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_VALUE, mtError, [mbCancel], 0);
    FocusControl(editRel);
    Result:= False;
  end
  else if FAnnotationFile.GetCurrentSentence.IsDuplicateFact(editArg1.Text, editRel.Text, editArg2.Text) then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_DUPLICATED, mtError, [mbCancel], 0);
    FocusControl(editArg1);
    Result:= False;
  end;

end;

function TMainForm.ValidateDelFact: Boolean;
begin
  //
  Result:= True;
  if not Assigned(FAnnotationFile) then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_ANNOTATIONF, mtError, [mbCancel], 0);
    FocusControl(gridFact);
    Result:= False;
  end
  else if FAnnotationFile.CurrentSentenceID <= 0 then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_CURRENTSENT, mtError, [mbCancel], 0);
    FocusControl(gridFact);
    Result:= False;
  end
  else if gridFact.RowCount <= 1 then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_NO_FACTS, mtError, [mbCancel], 0);
    FocusControl(gridFact);
    Result:= False;
  end;
end;

function TMainForm.GetCurrentChunkText: String;
begin
  if gridChunk.RowCount > 1 then
    Result:= gridChunk.Rows[gridChunk.Row][1]
  else
    Result:= '';
end;

// Form Events
procedure TMainForm.MI_EXITClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MI_OPEN_FILEClick(Sender: TObject);
begin
  if CheckUpdate() then
    MI_SAVE_AS.Click;
  LoadFile();
  InitForm();
  LoadFormValues();
end;

procedure TMainForm.MI_SAVEClick(Sender: TObject);
begin
  if CheckUpdate() then
  begin
    if mrYes = MessageDlg(MSG_CAPTION_QUESTION, MSG_MESSAGE_PENDENT_UPDATE,
                          mtConfirmation, [mbYes, mbNo], 0) then
    begin
      SaveFile;
      LoadFormValues;
    end;
  end
  else
    MessageDlg(MSG_CAPTION_WARNING, MSG_MESSAGE_NO_PENDINGS, mtWarning, [mbOK], 0);
end;

procedure TMainForm.MI_SAVE_ASClick(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
  begin
    if mrYes = MessageDlg(MSG_CAPTION_QUESTION, MSG_MESSAGE_PENDENT_UPDATE,
                          mtConfirmation, [mbYes, mbNo], 0) then
      if SaveDialogAnnotationFile.Execute then
      begin
        SaveFile(SaveDialogAnnotationFile.FileName);
        LoadFormValues;
      end;
  end
  else
    MessageDlg(MSG_CAPTION_WARNING, MSG_MESSAGE_NO_PENDINGS, mtWarning, [mbOK], 0);
end;

procedure TMainForm.viewCurrentSentenceKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = Word('C')) or (Key = Word('c'))) and (Shift = [ssCtrl]) then
    viewCurrentSentence.CopyToClipboard;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if checkUpdate() then
    MI_SAVE_AS.Click;
  FormSentences.Free;
  FormSentences:= Nil;
  CloseAction:= caFree;
end;

procedure TMainForm.buttonShowSentencesClick(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    FAnnotationFile.CurrentSentenceID := TFormSentences.getSelectedSentenceID();
  InitForm();
  LoadFormValues();
end;

procedure TMainForm.buttonAddFactClick(Sender: TObject);
begin
  if ValidateFact() then
  begin
    FAnnotationFile.Updated:= True;
    FAnnotationFile.CurrentSentence.AddFact(editArg1.Text, editRel.Text, editArg2.Text, checkBoxMinimalist.Checked);
    LoadFormValues(lfAnnotation);
  end;
end;

procedure TMainForm.buttonAddRelationshipClick(Sender: TObject);
begin
  if editRel.Text <> '' then
    editRel.Text := editRel.Text + ' ' + GetCurrentChunkText()
  else
    editRel.Text := GetCurrentChunkText()
end;

procedure TMainForm.buttonAddArgument1Click(Sender: TObject);
begin
  if editArg1.Text <> '' then
    editArg1.Text := editArg1.Text + ' ' + GetCurrentChunkText()
  else
    editArg1.Text := GetCurrentChunkText()
end;

procedure TMainForm.buttonAddArgument2Click(Sender: TObject);
begin
  if editArg2.Text <> '' then
    editArg2.Text := editArg2.Text + ' ' + GetCurrentChunkText()
  else
    editArg2.Text := GetCurrentChunkText()
end;

procedure TMainForm.buttonClearArgumentsClick(Sender: TObject);
begin
  editArg1.Text:= '';
  editArg2.Text:= '';
  editRel.Text:= '';
  checkBoxMinimalist.Checked:= True;
end;

procedure TMainForm.buttonDeleteCurrentFactClick(Sender: TObject);
var
  index: Integer;
begin
  if ValidateDelFact then
  begin
    index := gridFact.Row - 1;
    FAnnotationFile.Updated:= True;
    FAnnotationFile.CurrentSentence.RemoveFact(index);
    LoadFormValues(lfAnnotation);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    FreeAndNil(FAnnotationFile);
  //
  MainForm:= Nil;
  Halt();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  InitForm();
  FFormTitle:= Self.Caption;
end;

procedure TMainForm.gridChunkClick(Sender: TObject);
begin

end;

procedure TMainForm.gridFactClick(Sender: TObject);
var
  arg1, rel, arg2: String;
  currentSentence: TOpenIERecord;
  sentence: String;
const
  REL_COLOR   = '#ffcc99';
  ARG1_COLOR  = '#33cccc';
  ARG2_COLOR  = '#ffff99';
  BEGIN_SPAN1 = '<span style="background-color: ';
  BEGIN_SPAN2 = ';">';
  END_SPAN    = '</span>';
begin
  if Assigned(FAnnotationFile) then
  if gridFact.RowCount > 1 then
  begin
    arg1:= gridFact.Rows[gridFact.Row][0];
    rel:= gridFact.Rows[gridFact.Row][1];
    arg2:= gridFact.Rows[gridFact.Row][2];
    if (arg1 <> '') and (rel <> '') and (arg2 <> '') then
    begin
      // Load current sentence
      currentSentence:= FAnnotationFile.GetCurrentSentence();
      if Assigned(currentSentence) then
      begin
        sentence:= currentSentence.Sentence;
        sentence:= StringReplace(sentence, arg1, BEGIN_SPAN1 + ARG1_COLOR + BEGIN_SPAN2 + arg1 + END_SPAN, []);
        sentence:= StringReplace(sentence, rel, BEGIN_SPAN1 + REL_COLOR + BEGIN_SPAN2 + rel + END_SPAN, []);
        sentence:= StringReplace(sentence, arg2, BEGIN_SPAN1 + ARG2_COLOR + BEGIN_SPAN2 + arg2 + END_SPAN, []);
        viewCurrentSentence.LoadFromString('<p>' + sentence + '</p>');
        currentSentence:= Nil;
      end;
    end;
  end;
end;

procedure TMainForm.memoCurrentPOSSentenceChange(Sender: TObject);
begin

end;

procedure TMainForm.MI_EXP_RELONLYClick(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    if SaveDialogExportTXT.Execute then
      FAnnotationFile.ExportToFile(ffTabs, SaveDialogExportTXT.FileName);
end;

procedure TMainForm.MI_KAPPAClick(Sender: TObject);
begin
  //
end;

end.
