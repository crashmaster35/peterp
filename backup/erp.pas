program erp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs, uniqueinstance_package, main, login_form, clients,
  datetimectrls, runtimetypeinfocontrols, lazcontrols, reportes,
  fortes324forlaz, categories, currencies, products, providers, entry_inventory,
  tpv;

{$R *.res}
var
  Res : Integer;

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Res := 1;

  while Res = 1 do
    begin
    Application.CreateForm(TfrmLogin, frmLogin);
    Application.CreateForm(TfrmMain, frmMain);
      Res := frmLogin.ShowModal;
      if Res <> 1 then
        begin
          if Res = 2 then
            begin
              exit;
            end
          else if Res = 3 then
            begin
              ShowMessage('El nombre del usuario o la contraseña son incorrectas.');
            end
          else if Res = 4 then
            begin
              ShowMessage('El usuario no tiene permiso de ingresar al sistema.');
            end
          else if Res = 5 then
            begin
              ShowMessage('El usuario se encuentra desactivado. Contacte a la oficina matriz.');
            end;
          Res := 1;
        end
      else
        begin
          Application.ShowMainForm :=false;
          frmMain.Visible:=true;
          Application.Run;
        end;
    end;
  Application.terminate;
end.

