unit tpv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TfrmTpv }

  TfrmTpv = class(TForm)
    procedure FormChangeBounds(Sender: TObject);
  private

  public

  end;

var
  frmTpv: TfrmTpv;

implementation

{$R *.lfm}

{ TfrmTpv }

procedure TfrmTpv.FormChangeBounds(Sender: TObject);
begin
  if frmTpv.Active then
    begin
      frmTpv.Top:=125;
      frmClientes.Left:=25;
    end;

end;

end.

