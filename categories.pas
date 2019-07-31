unit categories;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn, db, FileUtil,
  RTTICtrls, Forms, Controls, Graphics, Dialogs, Variants,
  ExtCtrls, DBGrids, DbCtrls, StdCtrls, Buttons, EditBtn, ExtDlgs,
  Utils, reportes;

type

  { TfrmCategories }

  TfrmCategories = class(TForm)
    btnLoadImage: TBitBtn;
    btnSave: TBitBtn;
    btnNew: TBitBtn;
    btnDelete: TBitBtn;
    btnQuit: TBitBtn;
    btnReport: TBitBtn;
    chbxActive: TCheckBox;
    conInfo: TSQLConnector;
    dbgCatalog: TDBGrid;
    dsCatalog: TDataSource;
    dsInfo: TDataSource;
    edtId: TEdit;
    edtSearch: TEditButton;
    edtNombre: TEdit;
    grpUno: TGroupBox;
    imgCategory: TImage;
    lblImagen: TLabel;
    lblId: TLabel;
    lblNombre: TLabel;
    lblDescripion: TLabel;
    memDescrip: TMemo;
    navCatalog: TDBNavigator;
    imgBack: TImage;
    opdImage: TOpenPictureDialog;
    pnlLeft: TPanel;
    conCatalog: TSQLConnector;
    qryCatalog: TSQLQuery;
    qryInfo: TSQLQuery;
    trnCatalog: TSQLTransaction;
    tmInfo: TSQLTransaction;
    procedure btnLoadImageClick(Sender: TObject);
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
  frmCategories: TfrmCategories;
  Campo, Query, Order, Where, Direction : String;
  Operation : Char;
  Cambio : boolean;

implementation

{$R *.lfm}

{ TfrmClientes }

// Refresca la gradilla con los datos del catalogo
procedure TfrmCategories.refreshGrid();
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
procedure TfrmCategories.updateData(id : integer);
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
              SQL.Text:='SELECT c.id, c.name, c.image, c.description, c.active FROM `categories` AS c WHERE c.id = :qryID';
              params.ParamByName('qryID').AsInteger:=id;
              Active:=true;

              if NOT EOF then
                begin
                  edtId.Text:=FieldByName('id').AsString;
                  edtNombre.Text:= FieldByName('name').AsString;
                  memDescrip.Text:=FieldByName('description').AsString;
                  if FieldByName('active').AsBoolean then
                    chbxActive.Checked:=true
                  else
                    chbxActive.Checked:=false;

                  if FileExists(GetCurrentDir + '\images\uploads\' + FieldByName('image').AsString) then
                    imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\uploads\' + FieldByName('image').AsString)
                  else
                    imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');

                  Cambio:=false;
                  btnSave.Enabled:=false;
                end;
            end;
          end;
    end;
end;

//Crea el formulario
procedure TfrmCategories.FormCreate(Sender: TObject);
begin
  Operation:='E';
  session := loadConfig;
  frmCategories.Top:=250;
  frmCategories.Width:= screen.Width - 50;
  frmCategories.Height:=screen.Height - 250;
  pnlLeft.Width:=Trunc(frmCategories.Width/2);

  // Manejo de la base de datos
  conCatalog.HostName:=session.db.host;
  conCatalog.UserName:=session.db.user;
  conCatalog.Password:=session.db.pwd;
  conCatalog.Transaction:=trnCatalog;

  conCatalog.Connected:=true;

  trnCatalog.DataBase:=conCatalog;

  qryCatalog.Database:=conCatalog;
  Query := 'SELECT `id` AS ID, `name` AS NOMBRE, IF (active = 1, @ACTIVO := ''ACTIVADO'', @ACTIVO := ''DESACTIVADO'') AS ACTIVO FROM `categories`';
  Where := '';
  Order := ' ID';
  Direction := ' ASC';

  refreshGrid();

  dsCatalog.DataSet:=qryCatalog;

  dbgCatalog.DataSource:=dsCatalog;
  dbgCatalog.AutoFillColumns:=true;

  navCatalog.DataSource:=dsCatalog;

  if NOT VarIsNull(dbgCatalog.DataSource.DataSet.Fields[0].Value) then
    updateData(StrToInt(dbgCatalog.DataSource.DataSet.Fields[0].Value))
  else
    begin
      imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
      Operation := 'A';
    end;

  Cambio := false;
  btnSave.Enabled:=false;
end;

// Se activa para hacer responsivo el formulario cuando la imagen de fondo de ajusta a la pantalla
procedure TfrmCategories.imgBackResize(Sender: TObject);
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

  Alto2 := Trunc((Alto / 1.5)) - (Alto1*2);

  lblNombre.Top:=Alto1*2;
  lblNombre.Left:=Ancho1;

  edtNombre.Top:=lblNombre.Top+lblNombre.Height+Alto1;
  edtNombre.Left:=Ancho1;
  edtNombre.Width:=Trunc((Ancho/2) - (Ancho1*3));

  chbxActive.Top:=edtNombre.Top;
  chbxActive.Left:=edtNombre.Left + edtNombre.Width + (Ancho1);

  edtId.Top:=edtNombre.Top;
  edtId.Width:=Ancho1*5;

  lblId.Top:=lblNombre.Top;
  lblId.Left:=Ancho - (Ancho1 * 2) - edtId.Width;

  edtId.Left:=lblId.Left;

  lblDescripion.Top:=edtNombre.Top + edtNombre.Height + (Alto1*3);
  lblDescripion.Left:=Ancho1;

  memDescrip.Top:=lblDescripion.Top+lblDescripion.Height+Alto1;
  memDescrip.Left:=Ancho1;
  memDescrip.Width:=Ancho - (Ancho1 * 2);
  memDescrip.Height:=100;

  lblImagen.Top:=memDescrip.Top+memDescrip.Height+(Alto1*3);
  lblImagen.Left:=Ancho1;

  imgCategory.Top:=lblImagen.Top+lblImagen.Height+Alto1;
  imgCategory.Left:=Ancho1;
  imgCategory.Width:=Trunc((Ancho) - (Ancho1*2));

  btnLoadImage.Top:=grpUno.Height - btnLoadImage.Height - (Alto1 * 2);
  btnLoadImage.Left:=Ancho - btnLoadImage.Width - (Ancho1 * 2);
end;

// Se usa para que la pantalla quede en el mismo lugar cuando se trata de crecer
procedure TfrmCategories.FormChangeBounds(Sender: TObject);
begin
  if frmCategories.Active then
    begin
      frmCategories.Top:=125;
      frmCategories.Left:=25;
    end;
end;

// Se usa para crear un formulario en blanco cuando se selecciona el botón de nuevo en el catalogo.
procedure TfrmCategories.btnNewClick(Sender: TObject);
begin
  Operation := 'A';

  edtId.Text:='';
  edtNombre.Text:= '';
  memDescrip.Text:='';
  imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
  chbxActive.Checked:=true;
end;

// Se usa para salir del formulario y se revisa si hay cambios en el
procedure TfrmCategories.btnQuitClick(Sender: TObject);
begin
    if Cambio then
      begin
        if MessageDlg('Se perderán los cambios', '¿Desea continuar?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          frmCategories.close();
      end
    else
      frmCategories.close();
end;

procedure TfrmCategories.btnReportClick(Sender: TObject);
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
procedure TfrmCategories.btnSaveClick(Sender: TObject);
var
  nombre, description, image, act : String;
  activo : boolean;
begin
  conInfo.HostName:=session.db.host;
  conInfo.UserName:=session.db.user;
  conInfo.Password:=session.db.pwd;

  if Cambio then
    begin
      nombre := edtNombre.Text;
      description := memDescrip.Text;
      activo := chbxActive.Checked;
      if activo then
        act := '1'
      else
        act := '0';

      if opdImage.FileName <> '' then
         image := ExtractFilename(opdImage.FileName);

      if MessageDlg('Guardar Categoria', '¿Desea guarar los datos de la categoría ' + edtNombre.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
                          if image = '' then
                            begin
                              SQL.Text:='INSERT INTO `categories` (name, description, active) VALUES (:qryName, :qryDescription, :qryActive)';
                              params.ParamByName('qryName').AsString:=nombre;
                              params.ParamByName('qryDescription').AsString:=description;
                              params.ParamByName('qryActive').AsString:=act;
                              Cambio := false;
                              btnSave.Enabled:=false;
                            end
                          else
                            begin
                              SQL.Text:='INSERT INTO `categories` (name, description, image, active) VALUES (:qryName, :qryDescription, :qryImage, :qryActive)';
                              params.ParamByName('qryName').AsString:=nombre;
                              params.ParamByName('qryDescription').AsString:=description;
                              params.ParamByName('qryActive').AsString:=act;
                              params.ParamByName('qryImage').AsString:=image;
                              Cambio := false;
                              btnSave.Enabled:=false;
                            end;
                        end
                      else
                        begin
                          if image = '' then
                            begin
                              SQL.Text:='UPDATE `categories` SET name = :qryName, description = :qryDescription, active = :qryActive WHERE id = :qryID';
                              params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                              params.ParamByName('qryName').AsString:=nombre;
                              params.ParamByName('qryActive').AsString:=act;
                              params.ParamByName('qryDescription').AsString:=description;
                            end
                          else
                            begin
                              SQL.Text:='UPDATE `categories` SET name = :qryName, description = :qryDescription, image = :qryImage, active = :qryActive WHERE id = :qryID';
                              params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                              params.ParamByName('qryName').AsString:=nombre;
                              params.ParamByName('qryDescription').AsString:=description;
                              params.ParamByName('qryActive').AsString:=act;
                              params.ParamByName('qryImage').AsString:=image;
                            end;
                        end;
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      memDescrip.Text:='';
                      imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
                    end;
                end;
            end;
          showmessage('La categoría ha sido almacenada correctamente.');
          Operation:='E';
          Cambio := false;
          btnSave.Enabled:=false;
        end
      else
        begin
          edtId.Text:='';
          edtNombre.Text:= '';
          memDescrip.Text:='';
          imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
          chbxActive.Checked:=true;
          Operation:='E';
          Cambio := false;
          btnSave.Enabled:=false;
        end;
    end;
end;

// Se usa para borrar un registro del catalogo
procedure TfrmCategories.btnDeleteClick(Sender: TObject);
begin
  if Operation = 'A' then
    begin
      edtId.Text:='';
      edtNombre.Text:= '';
      memDescrip.Text:='';
      imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
      chbxActive.Checked:=true;
      Operation:='E';
      Cambio := false;
      btnSave.Enabled:=false;    end
  else
    begin
      if MessageDlg('Eliminar Categoria', '¿Desea eliminar la categoría ' + edtNombre.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
                      SQL.Text:='DELETE FROM `categories` WHERE id = :qryID';
                      params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      memDescrip.Text:='';
                      imgCategory.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
                      chbxActive.Checked:=true;
                      Operation:='E';
                      Cambio := false;
                      btnSave.Enabled:=false;
                    end;
                end;
            end;
        end;
    end;
end;

procedure TfrmCategories.btnLoadImageClick(Sender: TObject);
begin
  if opdImage.Execute then
    begin
      CopyFile(opdImage.FileName, GetCurrentDir + '/images/uploads/' + ExtractFilename(opdImage.FileName));
      imgCategory.Picture.LoadFromFile(opdImage.FileName);
      Cambio := true;
      btnSave.Enabled:=true;
    end;
end;

// Se usa para ordenar en forma ascendente o descendente segun el numero de clicks al titulo de la columna en el grid
procedure TfrmCategories.dbgCatalogTitleClick(Column: TColumn);
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
procedure TfrmCategories.edtNombreChange(Sender: TObject);
begin
  Cambio := true;
  btnSave.Enabled:=true;
end;

// Esa función se utiliza para limpiar el grid y regresarlo a su estado original, realiza un refresh
procedure TfrmCategories.edtSearchButtonClick(Sender: TObject);
var
  i : Integer;
begin
  Query := 'SELECT `id` AS ID, `name` AS NOMBRE, `description` AS DESCRIPCION, IF (active = 1, @ACTIVO := ''ACTIVADO'', @ACTIVO := ''DESACTIVADO'') AS ACTIVO FROM `categories`';
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
procedure TfrmCategories.edtSearchChange(Sender: TObject);
var
  number : Integer;
begin
  if (edtSearch.Text <> '') then
    begin
      number := 0;

      TryStrToInt(edtSearch.Text,number);

      qryCatalog.Close;

      if number > 0 then
        Where := ' WHERE (ID = :qryID) OR (name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (description LIKE CONCAT(''%'',:qrySearch,''%''))'
      else
        begin
          if UpperCase(edtSearch.Text) = 'ACTIVADO' then
            Where := ' WHERE (name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (description LIKE CONCAT(''%'',:qrySearch,''%'')) OR (active = 1)'
          else if UpperCase(edtSearch.Text) = 'DESACTIVADO' then
            Where := ' WHERE (name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (description LIKE CONCAT(''%'',:qrySearch,''%'')) OR (active = 0)'
          else
            Where := ' WHERE (name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (description LIKE CONCAT(''%'',:qrySearch,''%''))';
        end;

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
procedure TfrmCategories.dbgCatalogCellClick();
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

