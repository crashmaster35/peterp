unit clients;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn, db, FileUtil, DateTimePicker,
  RTTICtrls, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, DbCtrls, StdCtrls, MaskEdit, Buttons, EditBtn,
  Utils, reportes;

type

  { TfrmClientes }

  TfrmClientes = class(TForm)
    btnSave: TBitBtn;
    btnNew: TBitBtn;
    btnDelete: TBitBtn;
    btnQuit: TBitBtn;
    btnReport: TBitBtn;
    cmbSexo: TComboBox;
    cldFechaNac: TDateTimePicker;
    cmbMoneda: TComboBox;
    conInfo: TSQLConnector;
    conData: TSQLConnector;
    conCurrency: TSQLConnector;
    dbcmbCP: TDBLookupComboBox;
    dbcmbAsenta: TDBLookupComboBox;
    dbgCatalog: TDBGrid;
    dsCatalog: TDataSource;
    dsInfo: TDataSource;
    dsData: TDataSource;
    dsCurrency: TDataSource;
    edtId: TEdit;
    edtSearch: TEditButton;
    edtEmail: TEdit;
    edtEmail2: TEdit;
    edtCiudad: TEdit;
    edtCalle: TEdit;
    edtNombre: TEdit;
    edtApellido: TEdit;
    edtFacebook: TEdit;
    edtTwitter: TEdit;
    edtGoogle: TEdit;
    edtEstado: TEdit;
    edtMunicipio: TEdit;
    edtNumero: TEdit;
    grpUno: TGroupBox;
    grpDos: TGroupBox;
    grpTres: TGroupBox;
    lblId: TLabel;
    lblNombre: TLabel;
    lblTwitter: TLabel;
    lblGoogle: TLabel;
    lblEmail2: TLabel;
    lblCP: TLabel;
    lblEstado: TLabel;
    lblMpio: TLabel;
    lblCiudad: TLabel;
    lblAsentamiento: TLabel;
    lblCalle: TLabel;
    lblNumero: TLabel;
    lblApellido: TLabel;
    lblFechaNac: TLabel;
    lblEmail: TLabel;
    lblSexo: TLabel;
    lblLocal: TLabel;
    lblMovil: TLabel;
    lblMoneda: TLabel;
    LblFacebook: TLabel;
    edtLocal: TMaskEdit;
    edtMovil: TMaskEdit;
    navCatalog: TDBNavigator;
    imgBack: TImage;
    pnlLeft: TPanel;
    conCatalog: TSQLConnector;
    qryCatalog: TSQLQuery;
    qryInfo: TSQLQuery;
    qryData: TSQLQuery;
    qryCurrency: TSQLQuery;
    tmCurrency: TSQLTransaction;
    trnCatalog: TSQLTransaction;
    tmInfo: TSQLTransaction;
    trnData: TSQLTransaction;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure dbcmbCPSelect(Sender: TObject);
    procedure dbgCatalogCellClick();
    procedure dbgCatalogTitleClick(Column: TColumn);
    procedure edtNombreChange(Sender: TObject);
    procedure edtSearchButtonClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgBackResize(Sender: TObject);
  private
    { private declarations }
    procedure updateData(id:Integer);
    procedure refreshGrid();
    function getCP(cp : Integer; colony : String) : Integer;
    function getCurrency(currency : String) : Integer;
  public
    { public declarations }
  end;

var
  frmClientes: TfrmClientes;
  Campo, Query, Order, Where, Direction : String;
  Operation : Char;
  Cambio : boolean;

implementation

{$R *.lfm}

{ TfrmClientes }

// Refresca la gradilla con los datos del catalogo
procedure TfrmClientes.refreshGrid();
begin
   with conCatalog do
    begin
      if Connected then
        Connected:=false;

      Connected:=true;

      if Connected then
        begin
          with qryCatalog do
            begin
              SQL.Clear;
              if Where = '' then
                SQL.Text:=Query + ' ORDER BY' + Order + Direction
              else
                SQL.Text:=Query + Where + ' ORDER BY' + Order + Direction;

              Active:=true;
            end;
          end;
    end;
end;

// Actualiza la información de los campos con los datos de la base de datos
procedure TfrmClientes.updateData(id : integer);
begin
  conInfo.HostName:=session.db.host;
  conInfo.UserName:=session.db.user;
  conInfo.Password:=session.db.pwd;

  with conInfo do
    begin
      if Connected then
        Connected:=false;

      Connected:=true;

      if Connected then
        begin
          with qryInfo do
            begin
              SQL.Clear;
              SQL.Text:='SELECT c.id, c.first_name, c.last_name, c.email, c.email2, c.birthdate, c.gender, c.street, c.number, pc.postal_code, pc.state, pc.colony, pc.municipality, pc.city, c.phone, c.mobile, c.facebook, c.twitter, c.google_plus, cu.currency as currencyN FROM `clients` AS c JOIN currencies AS cu ON (c.currency = cu.id) JOIN postal_codes AS pc ON (c.cp = pc.id) WHERE c.id = :qryID';
              params.ParamByName('qryID').AsInteger:=id;
              Active:=true;

              if NOT EOF then
                begin
                  edtId.Text:=FieldByName('id').AsString;
                  edtNombre.Text:= FieldByName('first_name').AsString;
                  edtApellido.Text:=FieldByName('last_name').AsString;
                  cldFechaNac.Date:=FieldByName('birthdate').AsDateTime;
                  cmbSexo.Text:=FieldByName('gender').AsString;
                  cmbMoneda.Text:=UpperCase(FieldByName('currencyN').AsString);
                  edtLocal.Text:=FieldByName('phone').AsString;
                  edtMovil.Text:=FieldByName('mobile').AsString;
                  edtFacebook.Text:=FieldByName('facebook').AsString;
                  edtEmail.Text:=FieldByName('email').AsString;
                  edtTwitter.Text:=FieldByName('twitter').AsString;
                  edtEmail2.Text:=FieldByName('email2').AsString;
                  edtGoogle.Text:=FieldByName('google_plus').AsString;
                  dbcmbCP.Text:=FieldByName('postal_code').AsString;
                  edtEstado.Text:=FieldByName('state').AsString;
                  edtMunicipio.Text:=FieldByName('municipality').AsString;
                  edtCiudad.Text:=FieldByName('city').AsString;
                  dbcmbAsenta.Text:=FieldByName('colony').AsString;
                  edtCalle.Text:=FieldByName('street').AsString;
                  edtNumero.Text:=FieldByName('number').AsString;
                  Cambio:=false;
                  btnSave.Enabled:=false;
                end;
            end;
          end;
    end;
end;

//Crea el formulario
procedure TfrmClientes.FormCreate(Sender: TObject);
begin
  Operation:='E';
  session := loadConfig;
  frmClientes.Top:=250;
  frmClientes.Width:= screen.Width - 50;
  frmClientes.Height:=screen.Height - 250;
  pnlLeft.Width:=Trunc(frmClientes.Width/2);

  // Manejo de la base de datos
  conCatalog.HostName:=session.db.host;
  conCatalog.UserName:=session.db.user;
  conCatalog.Password:=session.db.pwd;
  conCatalog.Transaction:=trnCatalog;

  conCatalog.Connected:=true;

  trnCatalog.DataBase:=conCatalog;

  qryCatalog.Database:=conCatalog;
  Query := 'SELECT `id` AS ID, CONCAT(`first_name`, '' '', `last_name`) AS NOMBRE, `email` AS EMAIL, `mobile` AS CELULAR, `debt` AS DEUDA FROM `clients`';
  Where := '';
  Order := ' ID';
  Direction := ' ASC';

  //qryCatalog.SQL.text:=Query + Where + ' ORDER BY' + Order + Direction;
  //qryCatalog.open;

  refreshGrid();

  dsCatalog.DataSet:=qryCatalog;

  dbgCatalog.DataSource:=dsCatalog;
  dbgCatalog.AutoFillColumns:=true;

  navCatalog.DataSource:=dsCatalog;

    // Manejo de la base de datos
  conData.HostName:=session.db.host;
  conData.UserName:=session.db.user;
  conData.Password:=session.db.pwd;
  conData.Transaction:=trnData;

  conData.Connected:=true;

  trnData.DataBase:=conData;

  qryData.Database:=conData;

  with qryData do
    begin
      SQL.text:='SELECT DISTINCT`postal_code` FROM `postal_codes` ORDER BY `postal_code`';
      open;

      while NOT EOF do
        begin
          dbcmbCP.Items.Add(FieldByName('postal_code').AsString);
          next;
        end;
    end;

     // Manejo de la base de datos
  conCurrency.HostName:=session.db.host;
  conCurrency.UserName:=session.db.user;
  conCurrency.Password:=session.db.pwd;

  conCurrency.Connected:=true;

  with qryCurrency do
    begin
      SQL.text:='SELECT * FROM `currencies` ORDER BY `currency`';
      open;

      while NOT EOF do
        begin
          cmbMoneda.Items.Add(UpperCase(FieldByName('currency').AsString));
          next;
        end;
    end;

  updateData(StrToInt(dbgCatalog.DataSource.DataSet.Fields[0].Value));

  Cambio := false;
  btnSave.Enabled:=false;
end;

// Se activa para hacer responsivo el formulario cuando la imagen de fondo de ajusta a la pantalla
procedure TfrmClientes.imgBackResize(Sender: TObject);
var
  ancho, alto, ancho1, alto1, Alto2 : Integer;
begin
  // Calcula el ancho y alto de la imagen
  Ancho := imgBack.Width;
  Alto := imgBack.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.02);

  Alto2 := Trunc((Alto / 3)) - (Alto1*4);

  // Define el alto, ancho y posicion de los grupos
  grpUno.Top:=Alto1;
  grpUno.Left:=Ancho1;
  grpUno.Width:=Ancho - (Ancho1 + Ancho1);
  grpUno.Height:=Alto2;

  grpDos.top:=grpUno.top + grpUno.Height + Alto1;
  grpDos.Left:=Ancho1;
  grpDos.Width:=Ancho - (Ancho1 + Ancho1);
  grpDos.Height:=Alto2;

  grpTres.top:=grpDos.top + grpDos.Height + Alto1;
  grpTres.Left:=Ancho1;
  grpTres.Width:=Ancho - (Ancho1 + Ancho1);
  grpTres.Height:=Alto2;

  // Acomoda los botones de la pantalla inferior
  btnNew.Left:=Ancho1;
  btnNew.Top:=Alto-btnNew.Height-Alto1;

  btnDelete.Left:=btnNew.Left+btnNew.Width+Ancho1;
  btnDelete.Top:=Alto-btnDelete.Height-Alto1;

  btnSave.Left:=btnDelete.Left+btnSave.Width+Ancho1;
  btnSave.Top:=Alto-btnSave.Height-Alto1;

  btnReport.Left:=btnSave.Left+btnReport.Width+Ancho1;
  btnReport.Top:=Alto-btnReport.Height-Alto1;

  btnQuit.Left:=Ancho-Alto1-btnQuit.Width;
  btnQuit.Top:=Alto-btnQuit.Height-Alto1;

  //Acomoda los botones dentro del grupo 2
  Ancho := grpUno.Width;
  Alto := grpUno.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.02);

  Alto2 := Trunc((Alto / 3)) - (Alto1*4);

  lblNombre.Top:=Alto1*2;
  lblNombre.Left:=Ancho1;

  edtNombre.Top:=lblNombre.Top+lblNombre.Height+Alto1;
  edtNombre.Left:=Ancho1;
  edtNombre.Width:=Trunc((Ancho/2.5) - (Ancho1*3));

  lblApellido.Top:=Alto1*2;
  lblApellido.Left:=edtNombre.Width+(Ancho1*2);

  edtApellido.Top:=lblApellido.Top+lblApellido.Height+Alto1;
  edtApellido.Left:=edtNombre.Width+(Ancho1*2);
  edtApellido.Width:=Trunc((Ancho/2.5) - (Ancho1*3));

  edtId.Top:=edtNombre.Top;
  edtId.Width:=Ancho1*5;

  lblId.Top:=lblApellido.Top;
  lblId.Left:=Ancho - (Ancho1 * 2) - edtId.Width;

  edtId.Left:=lblId.Left;

  lblFechaNac.Top:=edtNombre.Top + edtNombre.Height + (Alto1*3);
  lblFechaNac.Left:=Ancho1;

  cldFechaNac.Top:=lblFechaNac.Top+lblFechaNac.Height+Alto1;
  cldFechaNac.Left:=Ancho1;

  lblSexo.Top:=edtNombre.Top + edtNombre.Height + (Alto1*3);
  lblSexo.Left:=cldFechaNac.Width + (Ancho1 * 10);

  cmbSexo.Top:=lblFechaNac.Top+lblFechaNac.Height+Alto1;
  cmbSexo.Left:=cldFechaNac.Width + (Ancho1 * 10);

  lblMoneda.Top:=cmbSexo.Top+cmbSexo.Height+(Alto1*3);
  lblMoneda.Left:=Ancho1;

  cmbMoneda.Top:=lblMoneda.Top+lblMoneda.Height+Alto1;
  cmbMoneda.Left:=Ancho1;
  cmbMoneda.Width:=Trunc((Ancho/3) - (Ancho1*3));

  //Acomoda los botones dentro del grupo 2
  Ancho := grpDos.Width;
  Alto := grpDos.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.02);

  Alto2 := Trunc((Alto / 3)) - (Alto1*4);

  lblLocal.Top:=Alto1*2;
  lblLocal.Left:=Ancho1;

  edtLocal.Top:=lblLocal.Top+lblLocal.Height+Alto1;
  edtLocal.Left:=Ancho1;
  edtLocal.Width:=100;

  lblMovil.Top:=Alto1*2;
  lblMovil.Left:=Trunc((Ancho/3) - (Ancho1*3)) + Ancho1;

  edtMovil.Top:=lblMovil.Top+lblMovil.Height+Alto1;
  edtMovil.Left:=lblMovil.Left;

  lblFacebook.Top:=lblMovil.Top;
  lblFacebook.Left:=Trunc((Ancho/3) - (Ancho1*3)) + Ancho1 + edtMovil.Left;

  edtFacebook.Top:=edtMovil.Top;
  edtFacebook.Left:=lblFacebook.Left;
  edtFacebook.Width:=Trunc((Ancho/3) - (Ancho1*3));

  lblEmail.Top:=edtLocal.Top+edtLocal.Height+(Alto1*2);
  lblEmail.Left:=Ancho1;

  edtEmail.Top:=lblEmail.Top+lblEmail.Height+Alto1;
  edtEmail.Left:=Ancho1;
  edtEmail.Width:=Trunc((Ancho/2) - (Ancho1*3));

  lblTwitter.Top:=lblEmail.Top;
  lblTwitter.Left:=lblFacebook.Left;

  edtTwitter.Top:=edtEmail.Top;
  edtTwitter.Left:=lblFacebook.Left;
  edtTwitter.Width:=edtFacebook.Width;

  lblEmail2.Top:=edtEmail.Top+edtEmail.Height+(Alto1*2);
  lblEmail2.Left:=Ancho1;

  edtEmail2.Top:=lblEmail2.Top+lblEmail2.Height+Alto1;
  edtEmail2.Left:=Ancho1;
  edtEmail2.Width:=edtEmail.Width;

  lblGoogle.Top:=lblEmail2.Top;
  lblGoogle.Left:=lblTwitter.Left;

  edtGoogle.Top:=edtEmail2.Top;
  edtGoogle.Left:=lblTwitter.Left;
  edtGoogle.Width:=edtTwitter.Width;

  //Acomoda los botones dentro del grupo 3
  Ancho := grpTres.Width;
  Alto := grpTres.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.02);

  Alto2 := Trunc((Alto / 3)) - (Alto1*4);

  lblCP.Top:=Alto1*2;
  lblCP.Left:=Ancho1;

  dbcmbCP.Top:=lblCP.Top+lblCP.Height+Alto1;
  dbcmbCP.Left:=Ancho1;
  dbcmbCP.Width:=Trunc((Ancho/3) - (Ancho1*4));

  lblEstado.Top:=Alto1*2;
  lblEstado.Left:=Trunc(Ancho/3) + Ancho1;

  edtEstado.Top:=lblEstado.Top+lblEstado.Height+Alto1;
  edtEstado.Left:=lblEstado.Left;
  edtEstado.Width:=dbcmbCP.Width;

  lblMpio.Top:=lblEstado.Top;
  lblMpio.Left:=(Trunc(Ancho/3) + Ancho1)*2;

  edtMunicipio.Top:=edtEstado.Top;
  edtMunicipio.Left:=lblMpio.Left;
  edtMunicipio.Width:=edtEstado.Width;

  lblCiudad.Top:=edtMunicipio.Top+edtMunicipio.Height+(Alto1*2);
  lblCiudad.Left:=Ancho1;

  edtCiudad.Top:=lblCiudad.Top+lblCiudad.Height+Alto1;
  edtCiudad.Left:=Ancho1;
  edtCiudad.Width:=Trunc((Ancho/2) - (Ancho1*3));

  lblAsentamiento.Top:=lblCiudad.Top;
  lblAsentamiento.Left:=edtCiudad.Left+edtCiudad.Width+(Ancho1*2);

  dbcmbAsenta.Top:=edtCiudad.Top;
  dbcmbAsenta.Left:=lblAsentamiento.Left;
  dbcmbAsenta.Width:=edtCiudad.Width;

  lblCalle.Top:=edtCiudad.Top+edtCiudad.Height+(Alto1*2);
  lblCalle.Left:=Ancho1;

  edtCalle.Top:=lblCalle.Top+lblCalle.Height+Alto1;
  edtCalle.Left:=Ancho1;
  edtCalle.Width:=edtCiudad.Width;

  lblNumero.Top:=lblCalle.Top;
  lblNumero.Left:=lblAsentamiento.Left;

  edtNumero.Top:=edtCalle.Top;
  edtNumero.Left:=lblAsentamiento.Left;
  edtNumero.Width:=edtCalle.Width;
end;

// Se usa para que la pantalla quede en el mismo lugar cuando se trata de crecer
procedure TfrmClientes.FormChangeBounds(Sender: TObject);
begin
  if frmClientes.Active then
    begin
      frmClientes.Top:=125;
      frmClientes.Left:=25;
    end;
end;

// Se usa para traer los datos de la dirección cuando se selecciona un codigo postal
procedure TfrmClientes.dbcmbCPSelect(Sender: TObject);
begin
  edtEstado.Caption:='';
  edtMunicipio.Caption:='';
  edtCiudad.Caption:='';
  edtCiudad.Caption:='';
  dbcmbAsenta.Clear;

  if conData.Connected then
    conData.Connected:=false;

  conData.Connected:=true;

  trnData.DataBase:=conData;

  qryData.Database:=conData;

  with qryData do
    begin
      SQL.text:='SELECT DISTINCT`state` FROM `postal_codes` WHERE postal_code = :qryPC ORDER BY `postal_code`';
      params.ParamByName('qryPC').AsInteger:=StrToInt(dbcmbCP.Text);
      open;

      if NOT EOF then
        edtEstado.Caption:= FieldByName('state').AsString;
    end;

  if conData.Connected then
    conData.Connected:=false;

  conData.Connected:=true;

  trnData.DataBase:=conData;

  qryData.Database:=conData;

  with qryData do
    begin
      SQL.text:='SELECT DISTINCT`municipality` FROM `postal_codes` WHERE postal_code = :qryPC ORDER BY `municipality`';
      params.ParamByName('qryPC').AsInteger:=StrToInt(dbcmbCP.Text);
      open;

      if NOT EOF then
        edtMunicipio.Caption:=FieldByName('municipality').AsString;
    end;

   if conData.Connected then
    conData.Connected:=false;

  conData.Connected:=true;

  trnData.DataBase:=conData;

  qryData.Database:=conData;

  with qryData do
    begin
      SQL.text:='SELECT DISTINCT`city` FROM `postal_codes` WHERE postal_code = :qryPC ORDER BY `City`';
      params.ParamByName('qryPC').AsInteger:=StrToInt(dbcmbCP.Text);
      open;

      if NOT EOF then
        edtCiudad.Caption:=UTF8ToAnsi(FieldByName('city').AsString);
    end;

  if conData.Connected then
    conData.Connected:=false;

  conData.Connected:=true;

  trnData.DataBase:=conData;

  qryData.Database:=conData;

  with qryData do
    begin
      SQL.text:='SELECT `colony` FROM `postal_codes` WHERE postal_code = :qryPC ORDER BY `colony`';
      params.ParamByName('qryPC').AsInteger:=StrToInt(dbcmbCP.Text);
      open;

      while NOT EOF do
        begin
          dbcmbAsenta.Items.Add(FieldByName('colony').AsString);
          next;
        end;
    end;
end;

// Se usa para crear un formulario en blanco cuando se selecciona el botón de nuevo en el catalogo.
procedure TfrmClientes.btnNewClick(Sender: TObject);
begin
  Operation := 'A';

  edtId.Text:='';
  edtNombre.Text:= '';
  edtApellido.Text:='';
  cldFechaNac.Date:=Date;
  cmbSexo.Text:='';
  cmbMoneda.Text:='';
  edtLocal.Text:='';
  edtMovil.Text:='';
  edtFacebook.Text:='';
  edtEmail.Text:='';
  edtTwitter.Text:='';
  edtEmail2.Text:='';
  edtGoogle.Text:='';
  dbcmbCP.Text:='';
  edtEstado.Text:='';
  edtMunicipio.Text:='';
  edtCiudad.Text:='';
  dbcmbAsenta.Text:='';
  edtCalle.Text:='';
  edtNumero.Text:='';
end;

// Se usa para salir del formulario y se revisa si hay cambios en el
procedure TfrmClientes.btnQuitClick(Sender: TObject);
begin
    if Cambio then
      begin
        if MessageDlg('Se perderán los cambios', '¿Desea continuar?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          frmClientes.close();
      end
    else
      frmClientes.close();
end;

procedure TfrmClientes.btnReportClick(Sender: TObject);
begin
  frmRpt.conReport.HostName:=session.db.host;
  frmRpt.conReport.UserName:=session.db.user;
  frmRpt.conReport.Password:=session.db.pwd;

  with frmRpt.conReport do
    begin
      if Connected then
        Connected := false;

      Connected := true;

      if Connected then
        begin
          with frmRpt.qryReport do
            begin
              SQL.Clear;
              SQL.Text:='SELECT id as NoCliente, CONCAT(first_name, '' '', last_name) as NOMBRE, birthdate as CUMPLEANOS, mobile as CELULAR, email as EMAIL FROM `clients` ORDER BY NOMBRE';
              open;
              frmRpt.rptTitle.Caption := 'Listado de Clientes';
              frmRpt.rptForm.Preview;
            end;
        end;
    end;
end;

// Función utilizada para traer los datos de la dirección con el codigo postal seleccionado
function TfrmClientes.getCP(cp : Integer; colony : String) : Integer;
begin
  with conInfo do
    begin
      if Connected then
        Connected:=false;

      Connected := true;

      if Connected then
        begin
          with qryInfo do
            begin
              SQL.Clear;
              SQL.Text:='SELECT * FROM `postal_codes` WHERE `postal_code` = :qryCP AND `colony` = :qryColony ';
              params.ParamByName('qryCP').AsInteger:=cp;
              params.ParamByName('qryColony').AsString:=colony;
              open;

              if NOT EOF then
                getCP := FieldByName('id').AsInteger;
            end;
        end;
    end;
end;

// Se utiliza para obtener el nombre de la moneda seleccionada
function TfrmClientes.getCurrency(currency : String) : Integer;
begin
  with conInfo do
    begin
      if Connected then
        Connected:=false;

      Connected := true;

      if Connected then
        begin
          with qryInfo do
            begin
              SQL.Clear;
              SQL.Text:='SELECT * FROM `currencies` WHERE `currency` = :qryCurrency ';
              params.ParamByName('qryCurrency').AsString:=currency;
              open;

              if NOT EOF then
                getCurrency := FieldByName('id').AsInteger;
            end;
        end;
    end;
end;

// Se usa al momento de querer guardar los datos, revisando primero si hubo cambios y luego si es nuevo o edicion
procedure TfrmClientes.btnSaveClick(Sender: TObject);
var
  first_name, last_name, email, email2, gender, street, number, phone, mobile, facebook, twitter, google_plus, birthdate : String;
  cp, currency : Integer;
begin
  if Cambio then
    begin
      first_name := edtNombre.Text;
      last_name := edtApellido.Text;
      email := edtEmail.Text;
      email2 := edtEmail2.Text;
      gender := cmbSexo.Text;
      street := edtCalle.Text;
      number := edtNumero.Text;
      phone := edtLocal.Text;
      mobile := edtMovil.Text;
      facebook := edtFacebook.Text;
      twitter := edtTwitter.Text;
      google_plus := edtGoogle.Text;
      birthdate := FormatDateTime('yyyy-mm-dd', cldFechaNac.Date);
      cp := getCP(StrToInt(dbcmbCP.Text), dbcmbAsenta.Text);
      currency := getCurrency(cmbMoneda.Text);
      if MessageDlg('Guardar Cliente', '¿Desea guarar los datos del cliente ' + edtNombre.Text + ' ' + edtApellido.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          with conInfo do
            begin
              if Connected then
                Connected:=false;

              Connected := true;

              if Connected then
                begin
                  with qryInfo do
                    begin
                      SQL.Clear;
                      if Operation = 'A' then
                        begin
                          SQL.Text:='INSERT INTO `clients` (first_name, last_name, email, email2, gender, street, number, phone, mobile, facebook, twitter, google_plus, birthdate, cp, currency) VALUES (:qryFirstName, :qryLastName, :qryEmail, :qryEmail2, :qryGender, :qryStreet, :qryNumber, :qryPhone, :qryMobile, :qryFacebook, :qryTwitter, :qryGooglePlus, :qryBirthdate, :qryCp, :qryCurrency)';
                          params.ParamByName('qryFirstName').AsString:=first_name;
                          params.ParamByName('qryLastName').AsString:=last_name;
                          params.ParamByName('qryEmail').AsString:=email;
                          params.ParamByName('qryEmail2').AsString:=email2;
                          params.ParamByName('qryGender').AsString:=gender;
                          params.ParamByName('qryStreet').AsString:=street;
                          params.ParamByName('qryNumber').AsString:=number;
                          params.ParamByName('qryPhone').AsString:=phone;
                          params.ParamByName('qryMobile').AsString:=mobile;
                          params.ParamByName('qryFacebook').AsString:=facebook;
                          params.ParamByName('qryTwitter').AsString:=twitter;
                          params.ParamByName('qryGooglePlus').AsString:=google_plus;
                          params.ParamByName('qryBirthdate').AsString:=birthdate;
                          params.ParamByName('qryCp').AsInteger:=cp;
                          params.ParamByName('qryCurrency').AsInteger:=currency;
                          Cambio := false;
                          btnSave.Enabled:=false;
                        end
                      else
                        begin
                          SQL.Text:='UPDATE `clients` SET first_name = :qryFirstName, last_name = :qryLastName, email = :qryEmail, email2 = :qryEmail2, gender = :qryGender, street = :qryStreet, number = :qryNumber, phone = :qryPhone, mobile = :qryMobile, facebook = :qryFacebook, twitter = :qryTwitter, google_plus = :qryGooglePlus, birthdate = :qryBirthdate, cp = :qryCp, currency = :qryCurrency WHERE id = :qryID';
                          params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                          params.ParamByName('qryFirstName').AsString:=first_name;
                          params.ParamByName('qryLastName').AsString:=last_name;
                          params.ParamByName('qryEmail').AsString:=email;
                          params.ParamByName('qryEmail2').AsString:=email2;
                          params.ParamByName('qryGender').AsString:=gender;
                          params.ParamByName('qryStreet').AsString:=street;
                          params.ParamByName('qryNumber').AsString:=number;
                          params.ParamByName('qryPhone').AsString:=phone;
                          params.ParamByName('qryMobile').AsString:=mobile;
                          params.ParamByName('qryFacebook').AsString:=facebook;
                          params.ParamByName('qryTwitter').AsString:=twitter;
                          params.ParamByName('qryGooglePlus').AsString:=google_plus;
                          params.ParamByName('qryBirthdate').AsString:=birthdate;
                          params.ParamByName('qryCp').AsInteger:=cp;
                          params.ParamByName('qryCurrency').AsInteger:=currency;
                        end;
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      edtApellido.Text:='';
                      cldFechaNac.Date:=Date;
                      cmbSexo.Text:='';
                      cmbMoneda.Text:='';
                      edtLocal.Text:='';
                      edtMovil.Text:='';
                      edtFacebook.Text:='';
                      edtEmail.Text:='';
                      edtTwitter.Text:='';
                      edtEmail2.Text:='';
                      edtGoogle.Text:='';
                      dbcmbCP.Text:='';
                      edtEstado.Text:='';
                      edtMunicipio.Text:='';
                      edtCiudad.Text:='';
                      dbcmbAsenta.Text:='';
                      edtCalle.Text:='';
                      edtNumero.Text:='';
                    end;
                end;
            end;
          showmessage('El cliente ha sido almacenado correctamente.');
          Operation:='E';
          Cambio := false;
          btnSave.Enabled:=false;
        end
      else
        begin
          edtId.Text:='';
          edtNombre.Text:= '';
          edtApellido.Text:='';
          cldFechaNac.Date:=Date;
          cmbSexo.Text:='';
          cmbMoneda.Text:='';
          edtLocal.Text:='';
          edtMovil.Text:='';
          edtFacebook.Text:='';
          edtEmail.Text:='';
          edtTwitter.Text:='';
          edtEmail2.Text:='';
          edtGoogle.Text:='';
          dbcmbCP.Text:='';
          edtEstado.Text:='';
          edtMunicipio.Text:='';
          edtCiudad.Text:='';
          dbcmbAsenta.Text:='';
          edtCalle.Text:='';
          edtNumero.Text:='';
          Operation:='E';
          Cambio := false;
          btnSave.Enabled:=false;
        end;
    end;
end;

// Se usa para borrar un registro del catalogo
procedure TfrmClientes.btnDeleteClick(Sender: TObject);
begin
  if Operation = 'A' then
    begin
      edtId.Text:='';
      edtNombre.Text:= '';
      edtApellido.Text:='';
      cldFechaNac.Date:=Date;
      cmbSexo.Text:='';
      cmbMoneda.Text:='';
      edtLocal.Text:='';
      edtMovil.Text:='';
      edtFacebook.Text:='';
      edtEmail.Text:='';
      edtTwitter.Text:='';
      edtEmail2.Text:='';
      edtGoogle.Text:='';
      dbcmbCP.Text:='';
      edtEstado.Text:='';
      edtMunicipio.Text:='';
      edtCiudad.Text:='';
      dbcmbAsenta.Text:='';
      edtCalle.Text:='';
      edtNumero.Text:='';
      Operation:='E';
      Cambio := false;
      btnSave.Enabled:=false;    end
  else
    begin
      if MessageDlg('Eliminar cliente', '¿Desea eliminar el cliente ' + edtNombre.Text + ' ' + edtApellido.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          with conInfo do
            begin
              if Connected then
                Connected:=false;

              Connected := true;

              if Connected then
                begin
                  with qryInfo do
                    begin
                      SQL.Clear;
                      SQL.Text:='DELETE FROM `clients` WHERE id = :qryID';
                      params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      edtApellido.Text:='';
                      cldFechaNac.Date:=Date;
                      cmbSexo.Text:='';
                      cmbMoneda.Text:='';
                      edtLocal.Text:='';
                      edtMovil.Text:='';
                      edtFacebook.Text:='';
                      edtEmail.Text:='';
                      edtTwitter.Text:='';
                      edtEmail2.Text:='';
                      edtGoogle.Text:='';
                      dbcmbCP.Text:='';
                      edtEstado.Text:='';
                      edtMunicipio.Text:='';
                      edtCiudad.Text:='';
                      dbcmbAsenta.Text:='';
                      edtCalle.Text:='';
                      edtNumero.Text:='';
                      Operation:='E';
                      Cambio := false;
                      btnSave.Enabled:=false;
                    end;
                end;
            end;
        end;
    end;
end;

// Se usa para ordenar en forma ascendente o descendente segun el numero de clicks al titulo de la columna en el grid
procedure TfrmClientes.dbgCatalogTitleClick(Column: TColumn);
var
  i : integer;
begin
  Campo := Column.Title.Caption;
  Order := Campo;

  if trim(Direction) = 'ASC' then
    Direction:=' DESC'
  else
    Direction:=' ASC';

  qryCatalog.SQL.Text:=Query + ' ORDER BY ' + Order + Direction;
  qryCatalog.Open;

  dbgCatalog.DataSource.DataSet.Refresh;

  edtSearch.Text:='';
  edtSearch.Caption:='';

  for i:= 0 to (dbgCatalog.Columns.Count - 1) do
    dbgCatalog.Columns[i].Title.Color:=clDefault;
  Column.Title.Color:=clHighlight;
end;

// Esta función se usa en todos los campos para seleccionar si ya hubo un cambio o no
procedure TfrmClientes.edtNombreChange(Sender: TObject);
begin
  Cambio := true;
  btnSave.Enabled:=true;
end;

// Esa función se utiliza para limpiar el grid y regresarlo a su estado original, realiza un refresh
procedure TfrmClientes.edtSearchButtonClick(Sender: TObject);
var
  i : Integer;
begin
  Query := 'SELECT `id` AS ID, CONCAT(`first_name`, '' '', `last_name`) AS NOMBRE, `email` AS EMAIL, `mobile` AS CELULAR, `debt` AS DEUDA FROM `clients`';
  Where := '';
  Order := ' ID';
  Direction := ' ASC';

  qryCatalog.SQL.text:=Query + Where + ' ORDER BY' + Order + Direction;
  qryCatalog.open;

  dbgCatalog.DataSource.DataSet.Refresh;

  edtSearch.Text:='';
  edtSearch.Caption:='';

  for i:= 0 to (dbgCatalog.Columns.Count - 1) do
    dbgCatalog.Columns[i].Title.Color:=clDefault;
end;

// Función utilizada para realizar una busqueda de filtrado en el grid
procedure TfrmClientes.edtSearchChange(Sender: TObject);
var
  number : Integer;
begin
  if (edtSearch.Text <> '') then
    begin
      number := 0;

      TryStrToInt(edtSearch.Text,number);

      qryCatalog.Close;

      if number > 0 then
        Where := ' WHERE (ID = :qryID) OR (first_name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (last_name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (email LIKE CONCAT(''%'',:qrySearch,''%''))'
      else
        Where := ' WHERE (first_name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (last_name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (email LIKE CONCAT(''%'',:qrySearch,''%''))';

      qryCatalog.SQL.Text:=Query + Where + ' ORDER BY ' + Order;

      if number > 0 then
        begin
          qryCatalog.ParamCheck:=true;
          qryCatalog.ParamByName('qryID').DataType:=ftInteger;
          qryCatalog.ParamByName('qryID').AsInteger:=number;
          qryCatalog.ParamByName('qrySearch').DataType:=ftString;
          qryCatalog.ParamByName('qrySearch').AsString:=edtSearch.Text;
          qryCatalog.Active:=true;
        end
      else
        begin
          qryCatalog.ParamCheck:=true;
          qryCatalog.ParamByName('qrySearch').DataType:=ftString;
          qryCatalog.ParamByName('qrySearch').AsString:=edtSearch.Text;
          qryCatalog.Active:=true;
        end;

      qryCatalog.Open;

      dbgCatalog.DataSource.DataSet.Refresh;
    end
  else
    begin
      qryCatalog.SQL.Text:=Query + ' ORDER BY ' + Order;
      qryCatalog.Open;
      dbgCatalog.DataSource.DataSet.Refresh;
    end;
end;

// Esta función se usa para traer los datos y mostrarlos en el formulario del catalogo al pulsar un registro.
procedure TfrmClientes.dbgCatalogCellClick();
begin
  if Cambio then
    begin
      if MessageDlg('Se perderán los cambios', '¿Desea continuar?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          updateData(StrToInt(dbgCatalog.DataSource.DataSet.Fields[0].Value));
          Cambio := false;
          btnSave.Enabled:=false;
        end;
    end
  else
    updateData(StrToInt(dbgCatalog.DataSource.DataSet.Fields[0].Value));
end;


end.

