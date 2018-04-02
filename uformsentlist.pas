unit uformsentlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids;

type

  { TFormSentences }

  TFormSentences = class(TForm)
    gridSentences: TStringGrid;
    procedure gridSentencesDblClick(Sender: TObject);
  private

  public
    class function getSelectedSentenceID(): Integer;
  end;

var
  FormSentences: TFormSentences;

implementation

{$R *.lfm}

{ TFormSentences }

procedure TFormSentences.gridSentencesDblClick(Sender: TObject);
begin
  Close;
end;

class function TFormSentences.getSelectedSentenceID(): Integer;
begin
  Result := -1;
  with FormSentences do
  begin
    ShowModal;
    if gridSentences.RowCount > 1 then
      Result := StrToInt(gridSentences.Rows[gridSentences.Row][0]);
  end;
end;

end.

