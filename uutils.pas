unit uutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TJavaOperation = (joAgree1, joAgree2, joConvertTxt);
  procedure ExecN2oieProcess(operation: TJavaOperation; parameters: Array of String);

implementation

//http://wiki.freepascal.org/Executing_External_Programs#The_Simplest_Example
procedure ExecN2oieProcess(operation: TJavaOperation; parameters: Array of String);
var
  AProcess: TProcess;
  I: Integer;
begin
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable:= 'java';
    AProcess.Parameters.Add('-cp');
    AProcess.Parameters.Add('"N2oie.jar;N2oie_lib/*"');
    case operation of
      joAgree1: begin
                  AProcess.Parameters.Add('br.ufba.formas.analyzer.Run');
                  AProcess.Parameters.Add('1');
                end;
      joAgree2: begin
                  AProcess.Parameters.Add('br.ufba.formas.analyzer.Run');
                  AProcess.Parameters.Add('1');
                end;
      joConvertTxt: begin
                      AProcess.Parameters.Add('br.ufba.formas.analyzer.Run');
                      AProcess.Parameters.Add('0');
                      for I:= Low(parameters) to High(parameters) do
                        AProcess.Parameters.Add(parameters[I]);
                    end;
    end;
    // We will define an option set for program execute.
    // Wait on Exit
    AProcess.Options := AProcess.Options + [poWaitOnExit];

    // Now let AProcess run the program
    AProcess.Execute;
  finally
    FreeAndNil(AProcess);
  end;
end;

end.

