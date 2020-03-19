program OpenIEAnnotation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Main,
  UClasses,
  uformsentlist,
  SysUtils,
  classes,
  uformagreement,
  utypes,
  process, uutils
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Open IE Annotation';
  //if FileExists('heap.trc') then
  //  DeleteFile('heap.trc');
  //SetHeapTraceOutput('heap.trc');
  //GlobalSkipIfNoLeaks:= True;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormSentences, FormSentences);
  Application.CreateForm(TFormAgreement, FormAgreement);
  Application.Run;
end.

