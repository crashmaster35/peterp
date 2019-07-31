unit currencies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn, db, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, DbCtrls, StdCtrls, Buttons, EditBtn, Spin,
  Utils, reportes;

type

  { TfrmClientes }

  { TfrmCurrencies }

  TfrmCurrencies = class(TForm)
    btnSave: TBitBtn;
    btnNew: TBitBtn;
    btnDelete: TBitBtn;
    btnQuit: TBitBtn;
    btnReport: TBitBtn;
    conInfo: TSQLConnector;
    dbgCatalog: TDBGrid;
    dsCatalog: TDataSource;
    dsInfo: TDataSource;
    edtSingular: TEdit;
    edtPlural: TEdit;
    edtCodigo: TEdit;
    edtId: TEdit;
    edtSearch: TEditButton;
    edtNombre: TEdit;
    edtChange: TFloatSpinEdit;
    grpUno: TGroupBox;
    lblSingular: TLabel;
    LblPlural: TLabel;
    LblCodigo: TLabel;
    LblCambio: TLabel;
    lblId: TLabel;
    lblNombre: TLabel;
    navCatalog: TDBNavigator;
    imgBack: TImage;
    pnlLeft: TPanel;
    conCatalog: TSQLConnector;
    qryCatalog: TSQLQuery;
    qryInfo: TSQLQuery;
    trnCatalog: TSQLTransaction;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
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
  public
    { public declarations }
  end;

var
  frmCurrencies: TfrmCurrencies;
  Campo, Query, Order, Where, Direction : String;
  Operation : Char;
  Cambio : boolean;

implementation

{$R *.lfm}

{ TfrmClientes }

// Refresca la gradilla con los datos del catalogo
procedure TfrmCurrencies.refreshGrid();
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
procedure TfrmCurrencies.updateData(id : integer);
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
              SQL.Text:='SELECT c.id, c.currency, c.singular, c.plural, c.code, c.exchange FROM `currencies` AS c WHERE c.id = :qryID';
              params.ParamByName('qryID').AsInteger:=id;
              Active:=true;

              if NOT EOF then
                begin
                  edtId.Text:=FieldByName('id').AsString;
                  edtNombre.Text:= UpperCase(FieldByName('currency').AsString);
                  edtSingular.Text:=FieldByName('singular').AsString;
                  edtPlural.Text:=FieldByName('plural').AsString;
                  edtCodigo.Text:=FieldByName('code').AsString;
                  edtChange.Value:=FieldByName('exchange').AsFloat;
                  Cambio:=false;
                  btnSave.Enabled:=false;
                end;
            end;
          end;
    end;
end;

//Crea el formulario
procedure TfrmCurrencies.FormCreate(Sender: TObject);
begin
  Operation:='E';
  session := loadConfig;
  frmCurrencies.Top:=250;
  frmCurrencies.Width:= screen.Width - 50;
  frmCurrencies.Height:=screen.Height - 250;
  pnlLeft.Width:=Trunc(frmCurrencies.Width/2);

  // Manejo de la base de datos
  conCatalog.HostName:=session.db.host;
  conCatalog.UserName:=session.db.user;
  conCatalog.Password:=session.db.pwd;
  conCatalog.Transaction:=trnCatalog;

  conCatalog.Connected:=true;

  trnCatalog.DataBase:=conCatalog;

  qryCatalog.Database:=conCatalog;
  Query := 'SELECT `id` AS ID, `currency` AS MONEDA, `singular` AS SINGULAR, `plural` AS PLURAL, `code` AS CODIGO, `exchange` AS CAMBIO FROM `currencies`';
  Where := '';
  Order := ' ID';
  Direction := ' ASC';

  refreshGrid();

  dsCatalog.DataSet:=qryCatalog;

  dbgCatalog.DataSource:=dsCatalog;
  dbgCatalog.AutoFillColumns:=true;

  navCatalog.DataSource:=dsCatalog;

  updateData(StrToInt(dbgCatalog.DataSource.DataSet.Fields[0].Value));

  Cambio := false;
  btnSave.Enabled:=false;
end;

// Se activa para hacer responsivo el formulario cuando la imagen de fondo de ajusta a la pantalla
procedure TfrmCurrencies.imgBackResize(Sender: TObject);
var
  ancho, alto, ancho1, alto1, Alto2 : Integer;
begin
  // Calcula el ancho y alto de la imagen
  Ancho := imgBack.Width;
  Alto := imgBack.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.02);

  Alto2 := Trunc((Alto / 1.2)) - (Alto1*2);

  // Define el alto, ancho y posicion de los grupos
  grpUno.Top:=Alto1;
  grpUno.Left:=Ancho1;
  grpUno.Width:=Ancho - (Ancho1 + Ancho1);
  grpUno.Height:=Alto2;

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

  edtId.Top:=edtNombre.Top;
  edtId.Width:=Ancho1*5;

  lblId.Top:=lblNombre.Top;
  lblId.Left:=Ancho - (Ancho1 * 2) - edtId.Width;

  edtId.Left:=lblId.Left;

  lblSingular.Top:=edtNombre.Top + edtNombre.Height + (Alto1*3);
  lblSingular.Left:=Ancho1;

  edtSingular.Top:=lblSingular.Top + lblSingular.Height + Alto1;
  edtSingular.Left:=edtNombre.Left;
  edtSingular.Width:=Trunc((Ancho/2) - (Ancho1*4));

  lblPlural.Top:=lblSingular.Top;
  lblPlural.Left:=edtSingular.Left + edtSingular.Width + (Ancho1*2);

  edtPlural.Top:=edtSingular.Top;
  edtPlural.Left:=lblPlural.Left;
  edtPlural.Width:=edtSingular.Width;

  lblCodigo.Top:=edtSingular.Top + edtSingular.Height + (Alto1*3);
  lblCodigo.Left:=edtSingular.Left;

  edtCodigo.Top:=lblCodigo.Top+lblCodigo.Height+Alto1;
  edtCodigo.Left:=lblCodigo.Left;
  edtCodigo.Width:=Trunc((Ancho/2) - (Ancho1*4));

  lblCambio.Top:=lblCodigo.Top;
  lblCambio.Left:=edtCodigo.Left + edtCodigo.Width + (Ancho1*2);

  edtChange.Top:=lblCambio.Top+lblCambio.Height+Alto1;
  edtChange.Left:=lblCambio.Left;
  edtChange.Width:=edtCodigo.Width;
end;

// Se usa para que la pantalla quede en el mismo lugar cuando se trata de crecer
procedure TfrmCurrencies.FormChangeBounds(Sender: TObject);
begin
  if frmCurrencies.Active then
    begin
      frmCurrencies.Top:=125;
      frmCurrencies.Left:=25;
    end;
end;

// Se usa para crear un formulario en blanco cuando se selecciona el botón de nuevo en el catalogo.
procedure TfrmCurrencies.btnNewClick(Sender: TObject);
begin
  Operation := 'A';

  edtId.Text:='';
  edtNombre.Text:= '';
  edtSingular.Text:='';
  edtPlural.Text:='';
  edtCodigo.Text:='';
  edtChange.Value:=0;
end;

// Se usa para salir del formulario y se revisa si hay cambios en el
procedure TfrmCurrencies.btnQuitClick(Sender: TObject);
begin
    if Cambio then
      begin
        if MessageDlg('Se perderán los cambios', '¿Desea continuar?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          frmCurrencies.close();
      end
    else
      frmCurrencies.close();
end;

procedure TfrmCurrencies.btnReportClick(Sender: TObject);
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

// Se usa al momento de querer guardar los datos, revisando primero si hubo cambios y luego si es nuevo o edicion
procedure TfrmCurrencies.btnSaveClick(Sender: TObject);
var
  currency, singular, plural, code : String;
  exchange : double;
begin
  if Cambio then
    begin
      currency := edtNombre.Text;
      singular := edtSingular.Text;
      plural := edtPlural.Text;
      code := edtCodigo.Text;
      exchange := edtChange.Value;
      if MessageDlg('Guardar Moneda', '¿Desea guarar los datos de la moneda ' + UpperCase(edtNombre.Text) + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
                          SQL.Text:='INSERT INTO `currencies` (currency, singular, plural, code, exchange) VALUES (:qryCurrency, :qrySingular, :qryPlural, :qryCode, :qryExchange)';
                          params.ParamByName('qryCurrency').AsString:=UpperCase(currency);
                          params.ParamByName('qrySingular').AsString:=singular;
                          params.ParamByName('qryPlural').AsString:=plural;
                          params.ParamByName('qryCode').AsString:=UpperCase(code);
                          params.ParamByName('qryExchange').AsFloat:=exchange;
                          Cambio := false;
                          btnSave.Enabled:=false;
                        end
                      else
                        begin
                          SQL.Text:='UPDATE `currencies` SET currency = :qryCurrency, singular = :qrySingular, plural = :qryPlural, code = :qryCode, exchange = :qryExchange WHERE id = :qryID';
                          params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                          params.ParamByName('qryCurrency').AsString:=UpperCase(currency);
                          params.ParamByName('qrySingular').AsString:=singular;
                          params.ParamByName('qryPlural').AsString:=plural;
                          params.ParamByName('qryCode').AsString:=UpperCase(code);
                          params.ParamByName('qryExchange').AsFloat:=exchange;
                        end;
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      edtSingular.Text:='';
                      edtPlural.Text:='';
                      edtCodigo.Text:='';
                      edtChange.Value:=0;
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
          edtSingular.Text:='';
          edtPlural.Text:='';
          edtCodigo.Text:='';
          edtChange.Value:=0;
          Operation:='E';
          Cambio := false;
          btnSave.Enabled:=false;
        end;
    end;
end;

// Se usa para borrar un registro del catalogo
procedure TfrmCurrencies.btnDeleteClick(Sender: TObject);
begin
  if Operation = 'A' then
    begin
      edtId.Text:='';
      edtNombre.Text:= '';
      edtSingular.Text:='';
      edtPlural.Text:='';
      edtCodigo.Text:='';
      edtChange.Value:=0;
      Operation:='E';
      Cambio := false;
      btnSave.Enabled:=false;    end
  else
    begin
      if MessageDlg('Eliminar Moneda', '¿Desea eliminar la moneda ' + UpperCase(edtNombre.Text) + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
                      SQL.Text:='DELETE FROM `currencies` WHERE id = :qryID';
                      params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      edtSingular.Text:='';
                      edtPlural.Text:='';
                      edtCodigo.Text:='';
                      edtChange.Value:=0;
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
procedure TfrmCurrencies.dbgCatalogTitleClick(Column: TColumn);
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
procedure TfrmCurrencies.edtNombreChange(Sender: TObject);
begin
  Cambio := true;
  btnSave.Enabled:=true;
end;

// Esa función se utiliza para limpiar el grid y regresarlo a su estado original, realiza un refresh
procedure TfrmCurrencies.edtSearchButtonClick(Sender: TObject);
var
  i : Integer;
begin
  Query := 'SELECT `id` AS ID, currency AS MONEDA, `singular` AS SINGULAR, `plural` AS PLURAL, `code` AS CODIGO, `exchange` AS CAMBIO FROM `currencies`';
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
procedure TfrmCurrencies.edtSearchChange(Sender: TObject);
var
  number : Integer;
begin
  if (edtSearch.Text <> '') then
    begin
      number := 0;

      TryStrToInt(edtSearch.Text,number);

      qryCatalog.Close;

      if number > 0 then
        Where := ' WHERE (ID = :qryID) OR (currency LIKE CONCAT(''%'',:qrySearch,''%'')) OR (singular LIKE CONCAT(''%'',:qrySearch,''%'')) OR (plural LIKE CONCAT(''%'',:qrySearch,''%'')) OR (code LIKE CONCAT(''%'',:qrySearch,''%''))'
      else
        Where := ' WHERE (currency LIKE CONCAT(''%'',:qrySearch,''%'')) OR (singular LIKE CONCAT(''%'',:qrySearch,''%'')) OR (plural LIKE CONCAT(''%'',:qrySearch,''%'')) OR (code LIKE CONCAT(''%'',:qrySearch,''%''))';

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
procedure TfrmCurrencies.dbgCatalogCellClick();
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

