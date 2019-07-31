unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DateUtils, Menus, ActnList, ComCtrls, BCToolBar, Utils,
  Clients, Categories, Currencies, Products, Providers, Entry_inventory, tpv;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acl_general: TActionList;
    act_clients: TAction;
    act_close: TAction;
    act_quit: TAction;
    BCToolBar1: TBCToolBar;
    imgMain: TImageList;
    img_main: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnu_provider: TMenuItem;
    mnu_inv_baja: TMenuItem;
    mnu_inv_alta: TMenuItem;
    mnu_inventario: TMenuItem;
    mnu_Products: TMenuItem;
    mnu_Moneda: TMenuItem;
    mnu_categories: TMenuItem;
    mnu_catalogs: TMenuItem;
    mnu_clients: TMenuItem;
    mnuMain: TMainMenu;
    mnu_caja: TMenuItem;
    mnu_cash: TMenuItem;
    mnu_config: TMenuItem;
    mnu_divisor1: TMenuItem;
    mnu_exit: TMenuItem;
    mnu_file: TMenuItem;
    stMainBar: TStatusBar;
    tbtnQuit: TToolButton;
    Timer1: TTimer;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tlbClients: TToolButton;
    ToolButton3: TToolButton;
    procedure act_quitExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnu_cashClick(Sender: TObject);
    procedure mnu_categoriesClick(Sender: TObject);
    procedure mnu_clientsClick(Sender: TObject);
    procedure mnu_inv_altaClick(Sender: TObject);
    procedure mnu_MonedaClick(Sender: TObject);
    procedure mnu_ProductsClick(Sender: TObject);
    procedure mnu_providerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure actualizaMenu();
    procedure desactivaMenu();
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;
  Pinto : boolean;

implementation

{$R *.lfm}

{ TfrmMain }
procedure TfrmMain.actualizaMenu();
begin
  session := loadConfig;

  if session.user.Clients = 1 then
    begin
      frmMain.mnu_clients.Enabled:=true;
      frmMain.tlbClients.Enabled:=true;
      frmMain.act_clients.Enabled:=true;
    end
  else
    begin
      frmMain.mnu_clients.Enabled:=false;
      frmMain.tlbClients.Enabled:=false;
      frmMain.act_clients.Enabled:=false;
    end;
end;

procedure TfrmMain.desactivaMenu();
begin
  frmMain.mnu_clients.Enabled:=false;
  frmMain.tlbClients.Enabled:=false;
  frmMain.act_clients.Enabled:=false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Pinto := True;
  session := loadConfig;
  desactivaMenu();
  stMainBar.Panels[0].Text:='ACUARIO ACUAEXPRESS';
end;

procedure TfrmMain.FormDeactivate(Sender: TObject);
begin
  desactivaMenu();
end;

procedure TfrmMain.act_quitExecute(Sender: TObject);
begin
  if (NOT application.Terminated) then
    application.Terminate;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  actualizaMenu();
end;

procedure TfrmMain.FormChangeBounds(Sender: TObject);
begin
  frmMain.top:=0;
  frmMain.left:=0;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if (NOT application.Terminated) then
    application.Terminate;
end;

procedure TfrmMain.mnu_cashClick(Sender: TObject);
begin
  Application.CreateForm(TfrmTpv, frmTpv);
  frmTpv.ShowModal;
end;


procedure TfrmMain.mnu_categoriesClick(Sender: TObject);
begin
  Application.CreateForm(TfrmCategories, frmCategories);
  frmCategories.ShowModal;
end;

procedure TfrmMain.mnu_clientsClick(Sender: TObject);
begin
  Application.CreateForm(TFrmClientes, frmClientes);
  frmClientes.ShowModal;
end;

procedure TfrmMain.mnu_inv_altaClick(Sender: TObject);
begin
  Application.CreateForm(TfrmEntryInversment, frmEntryInversment);
  frmEntryInversment.ShowModal;
end;

procedure TfrmMain.mnu_MonedaClick(Sender: TObject);
begin
  Application.CreateForm(TfrmCurrencies, frmCurrencies);
  frmCurrencies.ShowModal;
end;

procedure TfrmMain.mnu_ProductsClick(Sender: TObject);
begin
  Application.CreateForm(TfrmProducts, frmProducts);
  frmProducts.ShowModal;
end;

procedure TfrmMain.mnu_providerClick(Sender: TObject);
begin
  Application.CreateForm(TfrmProviders, frmProviders);
  frmProviders.ShowModal;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  stMainBar.Font.Color:=clSkyBlue;
  stMainBar.Panels[1].Text := TimeToStr(Time);
  if Pinto then
    begin
      stMainBar.Panels[1].Text := 'Hoy es ' + FormatDateTime('dddddd', Now) + '    ' + FormatDateTime('hh', Now) + ':' + FormatDateTime('nn', Now) + ':' + FormatDateTime('ss', Now) + '         ';
      Pinto := False;
    end
  else
    begin
      stMainBar.Panels[1].Text := 'Hoy es ' + FormatDateTime('dddddd', Now) + '    ' + FormatDateTime('hh', Now) + ':' + FormatDateTime('nn', Now) + ' ' + FormatDateTime('ss', Now) + '         ';
      Pinto := True;
    end;
end;




end.

