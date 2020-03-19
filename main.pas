unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, Grids, Buttons, ExtCtrls, UClasses, utypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    buttonClearArguments: TSpeedButton;
    buttonAddArgument1: TSpeedButton;
    buttonDeleteCurrentFact: TSpeedButton;
    buttonAddFact: TSpeedButton;
    buttonAddArgument2: TSpeedButton;
    buttonAddRelationship: TSpeedButton;
    buttonShowSentences1: TSpeedButton;
    buttonUpdateFact: TSpeedButton;
    checkValidFact: TCheckBox;
    checkValidMinimal: TCheckBox;
    checkBoxMinimal: TCheckBox;
    editArg1: TEdit;
    editArg2: TEdit;
    editRel: TEdit;
    gridFact: TStringGrid;
    GroupBox1: TGroupBox;
    labelCurrentSentence: TLabel;
    MI_ARGOE: TMenuItem;
    MI_PRAGMATICOIE: TMenuItem;
    MI_INFERPORTOIE: TMenuItem;
    MI_DPTOIE: TMenuItem;
    MI_DEPENDENTIE: TMenuItem;
    MI_OIE_FILES: TMenuItem;
    MI_IBERLEF19: TMenuItem;
    MI_IMPORT_SENT: TMenuItem;
    MI_CLOSE_FILE: TMenuItem;
    MI_AGREE: TMenuItem;
    MI_CALC_AGREE: TMenuItem;
    OpenDialogOIEFile: TOpenDialog;
    OpenDialogSentenceFile: TOpenDialog;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labelCurrentFile: TLabel;
    MainMenu: TMainMenu;
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
    viewCurrentSentence: TStaticText;
    procedure buttonAddArgument1Click(Sender: TObject);
    procedure buttonAddArgument2Click(Sender: TObject);
    procedure buttonAddFactClick(Sender: TObject);
    procedure buttonAddRelationshipClick(Sender: TObject);
    procedure buttonClearArgumentsClick(Sender: TObject);
    procedure buttonDeleteCurrentFactClick(Sender: TObject);
    procedure buttonShowSentences1Click(Sender: TObject);
    procedure buttonUpdateFactClick(Sender: TObject);
    procedure buttonShowSentencesClick(Sender: TObject);
    procedure checkValidFactChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridChunkClick(Sender: TObject);
    procedure gridFactClick(Sender: TObject);
    procedure gridFactSelection(Sender: TObject; aCol, aRow: Integer);
    procedure MI_ARGOEClick(Sender: TObject);
    procedure MI_CLOSE_FILEClick(Sender: TObject);
    procedure MI_DEPENDENTIEClick(Sender: TObject);
    procedure MI_DPTOIEClick(Sender: TObject);
    procedure MI_EXP_RELONLYClick(Sender: TObject);
    procedure MI_CALC_AGREEClick(Sender: TObject);
    procedure MI_IBERLEF19Click(Sender: TObject);
    procedure MI_IMPORT_SENTClick(Sender: TObject);
    procedure MI_OPEN_FILEClick(Sender: TObject);
    procedure MI_EXITClick(Sender: TObject);
    procedure MI_PRAGMATICOIEClick(Sender: TObject);
    procedure MI_SAVEClick(Sender: TObject);
    procedure MI_SAVE_ASClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    FAnnotationFile: TAnnotationFile;
    FFormTitle: String;
    procedure InitForm;
    procedure LoadFile;
    procedure SaveFile(fileName: String = '');
    procedure LoadFormValues(mode: TLoadForm = lfAll);
    function CheckUpdate: Boolean;
    function ValidateFact: Boolean;
    function ValidateDelUpFact: Boolean;
    function GetCurrentChunkText: String;
    procedure SetMode;
    procedure updateFactView;
  public

  end;


var
  MainForm: TMainForm;

implementation

uses
  uformsentlist, uformagreement, uutils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.InitForm;
//var
//  I: Integer;
begin
  editArg1.Clear;
  editArg2.Clear;
  editRel.Clear;
  viewCurrentSentence.Caption:= '';

  gridChunk.RowCount:= 1;
  gridFact.RowCount:= 1;
  FormSentences.gridSentences.RowCount:= 1;
  labelCurrentFile.Caption:= 'NO FILE';
  labelCurrentSentence.Caption:= 'NO SENTENCE';
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
    // Set current file label
    labelCurrentFile.Caption:= FAnnotationFile.GetFileName;
    // Load current viewCurrentSentence
    currentSentence:= FAnnotationFile.GetCurrentSentence();
    //
    labelCurrentSentence.Caption:= 'CurrentSentID: ' + IntToStr(currentSentence.ID);
    //
    if Assigned(currentSentence) then
    begin
      if (mode = lfAll) then
      begin
        viewCurrentSentence.Caption:= currentSentence.Sentence;
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
      //
    end;
    // Load viewCurrentSentence list
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
  else if FAnnotationFile.GetCurrentSentence.IsDuplicatedFact(editArg1.Text, editRel.Text, editArg2.Text) then
  begin
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGE_DUPLICATED, mtError, [mbCancel], 0);
    FocusControl(editArg1);
    Result:= False;
  end;

end;

function TMainForm.ValidateDelUpFact: Boolean;
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

procedure TMainForm.SetMode;
begin
  // Clear edit controls
  editArg1.Text:= '';
  editArg2.Text:= '';
  editRel.Text:= '';
  // If Evaluation mode
  if RadioGroup1.ItemIndex = 1 then
  begin
    // Annotation
    editArg1.Enabled:= False;
    editArg2.Enabled:= False;
    editRel.Enabled:= False;
    checkBoxMinimal.Enabled:= False;
    buttonClearArguments.Enabled:= False;
    buttonAddArgument1.Enabled:= False;
    buttonAddArgument2.Enabled:= False;
    buttonAddRelationship.Enabled:= False;
    buttonAddFact.Enabled:= False;
    buttonDeleteCurrentFact.Enabled:= False;
    // Evaluation
    checkValidFact.Enabled:= True;
    checkValidMinimal.Enabled:= True;
    checkValidFact.Checked:= True;
    checkValidMinimal.Checked:= True;
  end
  // If Annotation mode
  else
  begin
    // Annotation
    editArg1.Enabled:= True;
    editArg2.Enabled:= True;
    editRel.Enabled:= True;
    checkBoxMinimal.Enabled:= True;
    buttonClearArguments.Enabled:= True;
    buttonAddArgument1.Enabled:= True;
    buttonAddArgument2.Enabled:= True;
    buttonAddRelationship.Enabled:= True;
    buttonAddFact.Enabled:= True;
    buttonDeleteCurrentFact.Enabled:= True;
    // Evaluation
    checkValidFact.Enabled:= False;
    checkValidMinimal.Enabled:= False;
    checkValidFact.Checked:= True;
    checkValidMinimal.Checked:= True;
  end;
  gridFactClick(gridFact);
end;

procedure TMainForm.updateFactView;
var
  arg1, rel, arg2, min, vf, vm: String;
  //currentSentence: TOpenIERecord;
  //viewCurrentSentence: String;
//const
  //REL_COLOR   = '#ffcc99';
  //ARG1_COLOR  = '#33cccc';
  //ARG2_COLOR  = '#ffff99';
  //BEGIN_SPAN1 = '<span style="background-color: ';
  //BEGIN_SPAN2 = ';">';
  //END_SPAN    = '</span>';
begin
  if Assigned(FAnnotationFile) then
  if gridFact.RowCount > 1 then
  begin
    arg1:= gridFact.Rows[gridFact.Row][0];
    rel:= gridFact.Rows[gridFact.Row][1];
    arg2:= gridFact.Rows[gridFact.Row][2];
    min:= gridFact.Rows[gridFact.Row][3];
    vf:= gridFact.Rows[gridFact.Row][4];
    vm:=  gridFact.Rows[gridFact.Row][5];
    if (arg1 <> '') and (rel <> '') and (arg2 <> '') then
    begin
      // Set argument edits
      editArg1.Text:= arg1;
      editArg2.Text:= arg2;
      editRel.Text:= rel;
      checkBoxMinimal.Checked:= min = '1';
      checkValidFact.Checked:= vf = '1';
      checkValidMinimal.Checked:= vm = '1';
      // Load current viewCurrentSentence
      //currentSentence:= FAnnotationFile.GetCurrentSentence();
      //if Assigned(currentSentence) then
      //begin
        //viewCurrentSentence:= currentSentence.Sentence;
        //viewCurrentSentence:= StringReplace(viewCurrentSentence, arg1, BEGIN_SPAN1 + ARG1_COLOR + BEGIN_SPAN2 + arg1 + END_SPAN, []);
        //viewCurrentSentence:= StringReplace(viewCurrentSentence, rel, BEGIN_SPAN1 + REL_COLOR + BEGIN_SPAN2 + rel + END_SPAN, []);
        //viewCurrentSentence:= StringReplace(viewCurrentSentence, arg2, BEGIN_SPAN1 + ARG2_COLOR + BEGIN_SPAN2 + arg2 + END_SPAN, []);
        //viewCurrentSentence.LoadFromString('<p>' + viewCurrentSentence + '</p>');
        //currentSentence:= Nil;
      //end;
    end;
  end;
end;

// Form Events
procedure TMainForm.MI_EXITClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MI_PRAGMATICOIEClick(Sender: TObject);
begin
  if OpenDialogOIEFile.Execute then
    TAnnotationFile.ConvertOIEToAnnotationFile(osPragmaticOIE, OpenDialogOIEFile.FileName);
end;

procedure TMainForm.MI_OPEN_FILEClick(Sender: TObject);
begin
  if CheckUpdate() then
    MI_SAVE_AS.Click;
  LoadFile();
  InitForm();
  LoadFormValues();
  //
  RadioGroup1.ItemIndex:= 0;
  SetMode;
  gridFactClick(gridFact);
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

procedure TMainForm.RadioGroup1Click(Sender: TObject);
begin
  SetMode;
end;


//procedure TMainForm.viewCurrentSentenceKeyDown(Sender: TObject; var Key: Word;
//  Shift: TShiftState);
//begin
//  if ((Key = Word('C')) or (Key = Word('c'))) and (Shift = [ssCtrl]) then
//    viewCurrentSentence.CopyToClipboard;
//end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if checkUpdate() then
    MI_SAVE_AS.Click;
  FormSentences.Free;
  FormSentences:= Nil;
  FormAgreement.Free;
  FormAgreement:= Nil;
  CloseAction:= caFree;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FAnnotationFile:= Nil;
end;

procedure TMainForm.buttonShowSentencesClick(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    FAnnotationFile.CurrentSentenceID := TFormSentences.getSelectedSentenceID();
  InitForm();
  LoadFormValues();
  gridFactClick(gridFact);
end;

procedure TMainForm.checkValidFactChange(Sender: TObject);
begin

end;

procedure TMainForm.buttonAddFactClick(Sender: TObject);
begin
  if ValidateFact() then
  begin
    FAnnotationFile.Updated:= True;
    FAnnotationFile.CurrentSentence.AddFact(editArg1.Text,
                                            editRel.Text,
                                            editArg2.Text,
                                            checkBoxMinimal.Checked,
                                            checkValidFact.Checked,
                                            checkValidMinimal.Checked);
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
  checkBoxMinimal.Checked:= True;
  checkValidFact.Checked:= True;
  checkValidMinimal.Checked:= True;
end;

procedure TMainForm.buttonDeleteCurrentFactClick(Sender: TObject);
var
  index: Integer;
begin
  if ValidateDelUpFact then
  begin
    index := gridFact.Row - 1;
    FAnnotationFile.Updated:= True;
    FAnnotationFile.CurrentSentence.RemoveFact(index);
    LoadFormValues(lfAnnotation);
  end;
end;

procedure TMainForm.buttonShowSentences1Click(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    FAnnotationFile.CurrentSentenceID := FAnnotationFile.CurrentSentenceID + 1;
  InitForm();
  LoadFormValues();
  gridFactClick(gridFact);
end;

procedure TMainForm.buttonUpdateFactClick(Sender: TObject);
var
  index: Integer;
begin
  if ValidateDelUpFact then
  begin
    index := gridFact.Row - 1;
    FAnnotationFile.Updated:= True;
    FAnnotationFile.CurrentSentence.UpdateFact(index,
                                               editArg1.Text,
                                               editRel.Text,
                                               editArg2.Text,
                                               checkBoxMinimal.Checked,
                                               checkValidFact.Checked,
                                               checkValidMinimal.Checked);
    LoadFormValues(lfAnnotation);
    gridFact.Row:= index + 1;
    gridFact.SetFocus;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    FreeAndNil(FAnnotationFile);
  //
  MainForm:= Nil;
  //
  KillTask('OpenIEAnnotation.exe');

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
begin
  updateFactView;
end;

procedure TMainForm.gridFactSelection(Sender: TObject; aCol, aRow: Integer);
begin
  updateFactView;
end;

procedure TMainForm.MI_ARGOEClick(Sender: TObject);
begin
  if OpenDialogSentenceFile.Execute then
    TAnnotationFile.ConvertOIEToAnnotationFile(osArgOE, OpenDialogSentenceFile.FileName);
end;

procedure TMainForm.MI_CLOSE_FILEClick(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
  begin
    InitForm;
    FreeAndNil(FAnnotationFile);
  end;
end;

procedure TMainForm.MI_DEPENDENTIEClick(Sender: TObject);
begin
  if OpenDialogSentenceFile.Execute then
    TAnnotationFile.ConvertOIEToAnnotationFile(osDependentIE, OpenDialogSentenceFile.FileName);
end;

procedure TMainForm.MI_DPTOIEClick(Sender: TObject);
begin
  if OpenDialogOIEFile.Execute then
    TAnnotationFile.ConvertOIEToAnnotationFile(osDptOIE, OpenDialogOIEFile.FileName);
end;

procedure TMainForm.MI_EXP_RELONLYClick(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    if SaveDialogExportTXT.Execute then
      FAnnotationFile.ExportToFile(ffTabs, SaveDialogExportTXT.FileName);
end;

procedure TMainForm.MI_CALC_AGREEClick(Sender: TObject);
begin
  //
  if Assigned(FAnnotationFile) then
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGEM_CLOSE_FILE, mtError, [mbCancel], 0)
  else
    TFormAgreement.Show();
end;

procedure TMainForm.MI_IBERLEF19Click(Sender: TObject);
begin
  if Assigned(FAnnotationFile) then
    if SaveDialogExportTXT.Execute then
      FAnnotationFile.ExportToFile(ffIberlef, SaveDialogExportTXT.FileName);
end;

procedure TMainForm.MI_IMPORT_SENTClick(Sender: TObject);
var
  f1, f2, fileName: String;
begin
  if Assigned(FAnnotationFile) then
    MessageDlg(MSG_CAPTION_ERRO, MSG_MESSAGEM_CLOSE_FILE, mtError, [mbCancel], 0)
  else
  begin
    if OpenDialogSentenceFile.Execute then
    begin
      {
      f1:= OpenDialogSentenceFile.FileName;
      f1:= StringReplace(f1, ExtractFileDir(f1), '', []);
      f1:= Copy(f1, 2, Length(f1));
      f2:= f1 + '.ann';
      fileName:= ExtractFileDir(ParamStr(0)) + DirectorySeparator + 'convert' + DirectorySeparator + f2;
      CopyFile(OpenDialogSentenceFile.FileName,
        ExtractFileDir(ParamStr(0)) + DirectorySeparator + 'convert' + DirectorySeparator + f1);
      f1:= 'convert/' + f1;
      f2:= 'convert/' + f2;
      }
      TAnnotationFile.PrepareFileConvertion(OpenDialogSentenceFile.FileName, fileName, f1, f2);
      ExecN2oieProcess(joConvertTxt, [f1, f2]);
      //
      FAnnotationFile:= TAnnotationFile.Create(fileName);
      InitForm();
      LoadFormValues();
      //
      RadioGroup1.ItemIndex:= 0;
      SetMode;
    end;
  end;
end;

end.
