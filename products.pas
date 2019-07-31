unit products;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql56conn, db, FileUtil, DateTimePicker,
  RTTICtrls, Forms, Controls, Graphics, Dialogs, ExtCtrls, DBGrids, DbCtrls,
  StdCtrls, Buttons, EditBtn, Spin, ExtDlgs, BCLabel, Utils, reportes,
  LR_BarC, RLBarcode, Variants, Strutils;

type

  { TfrmProducts }

  TfrmProducts = class(TForm)
    chbxPedimento: TCheckBox;
    lblPercent: TBCLabel;
    btnLoadImage: TBitBtn;
    btnLoadPDF: TBitBtn;
    btnSave: TBitBtn;
    btnNew: TBitBtn;
    btnDelete: TBitBtn;
    btnQuit: TBitBtn;
    btnReport: TBitBtn;
    chbxDiscountAvail: TCheckBox;
    cldStartDate: TDateTimePicker;
    cldEndDate: TDateTimePicker;
    cmbSupercategory: TComboBox;
    cmbCategoria: TComboBox;
    conInfo: TSQLConnector;
    conData: TSQLConnector;
    conCurrency: TSQLConnector;
    dbgCatalog: TDBGrid;
    dsCatalog: TDataSource;
    dsInfo: TDataSource;
    dsData: TDataSource;
    dsCurrency: TDataSource;
    edtBarCode: TEdit;
    edtFilePdf: TEdit;
    edtBarCodeDraw: TEdit;
    edtId: TEdit;
    edtSearch: TEditButton;
    edtNombre: TEdit;
    edtSku: TEdit;
    edtPurchacePrice: TFloatSpinEdit;
    edtSellPrice: TFloatSpinEdit;
    edtStock: TFloatSpinEdit;
    edtStockMin: TFloatSpinEdit;
    edtStockMax: TFloatSpinEdit;
    grpDos: TGroupBox;
    grpUno: TGroupBox;
    imgProduct: TImage;
    lblDescription: TLabel;
    lblPurchacePrice: TLabel;
    lblCodeBarDraw: TLabel;
    lblSellPrice: TLabel;
    lblDescuento: TLabel;
    lblInicio: TLabel;
    lblFinal: TLabel;
    lblCantidad: TLabel;
    lblMinimo: TLabel;
    lblMaximo: TLabel;
    lblArchivoPDF: TLabel;
    lblId: TLabel;
    lblImagen: TLabel;
    lblNombre: TLabel;
    lblCategory: TLabel;
    lblSKU: TLabel;
    lblSuperC: TLabel;
    lblCodeBar: TLabel;
    memDescrip: TMemo;
    navCatalog: TDBNavigator;
    imgBack: TImage;
    opdImage: TOpenPictureDialog;
    opdPdf: TOpenDialog;
    pnlLeft: TPanel;
    conCatalog: TSQLConnector;
    qryCatalog: TSQLQuery;
    qryInfo: TSQLQuery;
    qryData: TSQLQuery;
    qryCurrency: TSQLQuery;
    edtDiscount: TSpinEdit;
    tmCurrency: TSQLTransaction;
    trnCatalog: TSQLTransaction;
    tmInfo: TSQLTransaction;
    trnData: TSQLTransaction;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnLoadPDFClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cmbSupercategoryChange(Sender: TObject);
    procedure conDataAfterConnect(Sender: TObject);
    procedure dbgCatalogCellClick();
    procedure dbgCatalogTitleClick(Column: TColumn);
    procedure edtBarCodeChange(Sender: TObject);
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
    function getCategoryID(category : String) : Integer;
    function existCode(cb : String) : Boolean;
  public
    { public declarations }
  end;

var
  frmProducts: TfrmProducts;
  Campo, Query, Order, Where, Direction : String;
  Operation : Char;
  Cambio : boolean;

implementation

{$R *.lfm}

{ TfrmProducts }
function TfrmProducts.existCode(cb : String) : Boolean;
begin
  conCurrency.HostName:=session.db.host;
  conCurrency.UserName:=session.db.user;
  conCurrency.Password:=session.db.pwd;

  with conCurrency do
    begin
      if Connected then
        Connected:=false;

      Connected := true;

      if Connected then
        begin
          with qryCurrency do
            begin
              SQL.Clear;
              if edtId.Text <> '' then
                begin
                  SQL.Text:='SELECT `bar_code` FROM `products` WHERE `bar_code` = :qryCodeBar AND id <> :qryID';
                  params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                  params.ParamByName('qryCodeBar').AsString:=CB;
                end
              else
                begin
                  SQL.Text:='SELECT `bar_code` FROM `products` WHERE `bar_code` = :qryCodeBar';
                  params.ParamByName('qryCodeBar').AsString:=CB;
                end;
              open;

              if NOT EOF then
                existCode := True
              else
                existCode := False;
            end;
        end;
    end;
end;

function TfrmProducts.getCategoryID(category : String) : Integer;
begin
  conCurrency.HostName:=session.db.host;
  conCurrency.UserName:=session.db.user;
  conCurrency.Password:=session.db.pwd;

  with conCurrency do
    begin
      if Connected then
        Connected:=false;

      Connected := true;

      if Connected then
        begin
          with qryCurrency do
            begin
              SQL.Clear;
              SQL.Text:='SELECT * FROM `categories` WHERE `name` = :qryCategory ';
              params.ParamByName('qryCategory').AsString:=category;
              open;

              if NOT EOF then
                getCategoryID := FieldByName('id').AsInteger;
            end;
        end;
    end;
end;

// Refresca la gradilla con los datos del catalogo
procedure TfrmProducts.refreshGrid();
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
procedure TfrmProducts.updateData(id : integer);
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
              SQL.Text:='SELECT p.`id`, p.`name`, p.`description`, p.`SKU`, p.`bar_code`, p.`category_id`, p.`supercategory`, p.`purchase_price`, p.`sell_price`, p.`discount`, p.`stock`, p.`stock_min`, p.`stock_max`, p.`discount_available`, p.`discount_start_date`, p.`discount_end_date`, p.`information_file`, p.`image`, p.`pediment`, c.name AS category FROM `products` AS p JOIN `categories` AS c ON (p.category_id = c.id) WHERE p.id = :qryID';
              params.ParamByName('qryID').AsInteger:=id;
              Active:=true;

              if NOT EOF then
                begin
                  edtId.Text:=FieldByName('id').AsString;
                  edtNombre.Text:= FieldByName('name').AsString;
                  memDescrip.Text:=FieldByName('description').AsString;
                  edtSKU.Text:=FieldByName('SKU').AsString;
                  edtBarCode.Text:=FieldByName('bar_code').AsString;
                  edtBarCodeDraw.Text:=FieldByName('bar_code').AsString;
                  cmbCategoria.Text:=FieldByName('category').AsString;
                  cmbSupercategory.Text:=FieldByName('supercategory').AsString;
                  edtPurchacePrice.Text:=FieldByName('purchase_price').AsString;
                  edtSellPrice.Text:=FieldByName('sell_price').AsString;
                  edtDiscount.Text:=FieldByName('discount').AsString;
                  edtStock.Text:=FieldByName('stock').AsString;
                  edtStockMin.Text:=FieldByName('stock_min').AsString;
                  edtStockMax.Text:=FieldByName('stock_max').AsString;
                  chbxDiscountAvail.Checked:=FieldByName('discount_available').AsBoolean;
                  cldStartDate.Date:=FieldByName('discount_start_date').AsDateTime;
                  cldEndDate.Date:=FieldByName('discount_end_date').AsDateTime;
                  edtFilePdf.Text:=FieldByName('information_file').AsString;
                  if FileExists(GetCurrentDir + '\images\uploads\' + FieldByName('image').AsString) then
                    imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\uploads\' + FieldByName('image').AsString)
                  else
                    imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
                  chbxPedimento.Checked:=FieldByName('pediment').AsBoolean;

                  Cambio:=false;
                  btnSave.Enabled:=false;
                end;
            end;
          end;
    end;
end;

//Crea el formulario
procedure TfrmProducts.FormCreate(Sender: TObject);
begin
  Operation:='E';
  session := loadConfig;
  frmProducts.Top:=250;
  frmProducts.Width:= screen.Width - 50;
  frmProducts.Height:=screen.Height - 250;
  pnlLeft.Width:=Trunc(frmProducts.Width/2);

  // Manejo de la base de datos
  conCatalog.HostName:=session.db.host;
  conCatalog.UserName:=session.db.user;
  conCatalog.Password:=session.db.pwd;
  conCatalog.Transaction:=trnCatalog;

  conCatalog.Connected:=true;

  trnCatalog.DataBase:=conCatalog;

  qryCatalog.Database:=conCatalog;
  Query := 'SELECT p.`id` AS ID, p.`name` AS PRODUCTO, p.`bar_code` AS CODIGO, p.`supercategory` AS SUPERCAT, c.`name` AS CATEGORIA, p.`sell_price` AS PRECIO, p.`stock` AS STOCK FROM `products` AS p JOIN `categories` AS c ON (p.category_id = c.id)';
  Where := '';
  Order := ' ID';
  Direction := ' ASC';

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

  cmbCategoria.Clear;

  with qryData do
    begin
      SQL.text:='SELECT `name` FROM `categories` ORDER BY `name`';
      open;

      while NOT EOF do
        begin
          cmbCategoria.Items.Add(FieldByName('name').AsString);
          next;
        end;
    end;

  if NOT VarIsNull(dbgCatalog.DataSource.DataSet.Fields[0].Value) then
      updateData(StrToInt(dbgCatalog.DataSource.DataSet.Fields[0].Value))
    else
      begin
        imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
        Operation := 'A';
      end;

  Cambio := false;
  btnSave.Enabled:=false;
end;

// Se activa para hacer responsivo el formulario cuando la imagen de fondo de ajusta a la pantalla
procedure TfrmProducts.imgBackResize(Sender: TObject);
var
  ancho, alto, ancho1, alto1, Alto2, Alto3 : Integer;
begin
  // Calcula el ancho y alto de la imagen
  Ancho := imgBack.Width;
  Alto := imgBack.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.01);

  Alto2 := Trunc((Alto*0.45 )) - (Alto1*3);
  Alto3 := Trunc((Alto*0.15 )) - (Alto1*3);

  // Define el alto, ancho y posicion de los grupos
  grpUno.Top:=Alto1;
  grpUno.Left:=Ancho1;
  grpUno.Width:=Ancho - (Ancho1 + Ancho1);
  grpUno.Height:=Alto2;

  grpDos.top:=grpUno.top + grpUno.Height + Alto1;
  grpDos.Left:=Ancho1;
  grpDos.Width:=Ancho - (Ancho1 + Ancho1);
  grpDos.Height:=Alto3;

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

  lblNombre.Top:=Alto1;
  lblNombre.Left:=Ancho1;

  edtNombre.Top:=lblNombre.Top+lblNombre.Height+Alto1;
  edtNombre.Left:=Ancho1;
  edtNombre.Width:=Trunc((Ancho/2.5) - (Ancho1*3));

  lblSKU.Top:=Alto1;
  lblSKU.Left:=edtNombre.Width+(Ancho1*2);

  edtSku.Top:=lblSKU.Top+lblSKU.Height+Alto1;
  edtSku.Left:=edtNombre.Width+(Ancho1*2);
  edtSku.Width:=Trunc((Ancho/2.5) - (Ancho1*3));

  edtId.Top:=edtNombre.Top;
  edtId.Width:=Ancho1*5;

  lblId.Top:=lblSKU.Top;
  lblId.Left:=Ancho - (Ancho1 * 2) - edtId.Width;

  edtId.Left:=lblId.Left;

  lblDescription.Top:=edtNombre.Top + edtNombre.Height + (Alto1);
  lblDescription.Left:=Ancho1;

  memDescrip.Top:=lblDescription.Top+lblDescription.Height+Alto1;
  memDescrip.Left:=Ancho1;
  memDescrip.Width:=Trunc(Ancho / 2) - (Ancho1 * 2);

  lblSuperC.Top:=lblDescription.Top;
  lblSuperC.Left:=memDescrip.Left + memDescrip.Width + (Ancho1 * 2);

  cmbSupercategory.Top:=lblSuperC.Top+lblSuperC.Height+Alto1;
  cmbSupercategory.Left:=lblSuperC.Left;
  cmbSupercategory.Width:=Trunc(Ancho / 2) - (Ancho1 * 2);

  lblCategory.Top:=cmbSupercategory.Top+cmbSupercategory.Height+(Alto1*2);
  lblCategory.Left:=lblSuperC.Left;

  cmbCategoria.Top:=lblCategory.Top+lblCategory.Height+Alto1;
  cmbCategoria.Left:=lblCategory.Left;
  cmbCategoria.Width:=cmbSupercategory.Width;

  lblCodeBar.Top:=cmbCategoria.Top+cmbCategoria.Height+Alto1;
  lblCodeBar.Left:=lblCategory.Left;

  edtBarCode.Top:=lblCodeBar.Top + lblCodeBar.Height + (Alto1*2);
  edtBarCode.Left:=lblCodeBar.Left;
  edtBarCode.Width:=cmbCategoria.Width;

  memDescrip.Height:=edtBarCode.Top-Trunc(edtBarCode.Height*2.4);

  lblPurchacePrice.Top:=edtBarCode.Top + edtBarCode.Height+Alto1;
  lblPurchacePrice.Left:=Ancho1;

  edtPurchacePrice.Top:=lblPurchacePrice.Top + lblPurchacePrice.Height+Alto1;
  edtPurchacePrice.Left:=Ancho1;
  edtPurchacePrice.Width:=Trunc((Ancho/3) - (Ancho1*3));

  lblSellPrice.Top:=lblPurchacePrice.Top;
  lblSellPrice.Left:=edtPurchacePrice.Left+edtPurchacePrice.Width+(Ancho1*2);

  edtSellPrice.Top:=edtPurchacePrice.Top;
  edtSellPrice.Left:=lblSellPrice.Left;
  edtSellPrice.Width:=edtPurchacePrice.Width;

  lblDescuento.Top:=lblSellPrice.Top;
  lblDescuento.Left:=edtSellPrice.Left+edtSellPrice.Width+(Ancho1*2);

  edtDiscount.Top:=edtSellPrice.Top;
  edtDiscount.Left:=lblDescuento.Left;
  edtDiscount.Width:=edtSellPrice.Width;

  lblPercent.Top:=edtDiscount.Top-7;
  lblPercent.Left:=edtDiscount.Left+edtDiscount.Width+Ancho1-5;

  chbxDiscountAvail.Top:=edtPurchacePrice.Top+edtPurchacePrice.Height+Alto1;
  chbxDiscountAvail.Left:=Ancho1;

  lblInicio.Top:=chbxDiscountAvail.Top;
  lblInicio.Left:=chbxDiscountAvail.Left + chbxDiscountAvail.Width + (Ancho1 * 4);

  cldStartDate.Top:=lblInicio.Top+lblInicio.Height+Alto1;
  cldStartDate.Left:=lblInicio.Left;

  lblFinal.Top:=lblInicio.Top;
  lblFinal.Left:=cldStartDate.Left + cldStartDate.Width + (Ancho1 * 4);

  cldEndDate.Top:=cldStartDate.Top;
  cldEndDate.Left:=lblFinal.Left;

  chbxPedimento.Top:=chbxDiscountAvail.Top;
  chbxPedimento.Left:=lblDescuento.Left;


  //Acomoda los botones dentro del grupo 2
  Ancho := grpDos.Width;
  Alto := grpDos.Height;

  Ancho1 := Trunc(Ancho*0.02);
  Alto1 := Trunc(Alto*0.02);

  Alto2 := Trunc((Alto / 3)) - (Alto1*4);

  lblCantidad.Top:=Alto1*2;
  lblCantidad.Left:=lblPurchacePrice.Left;

  edtStock.Top:=lblCantidad.Top+lblCantidad.Height+Alto1;
  edtStock.Left:=Ancho1;
  edtStock.Width:=edtPurchacePrice.Width;

  lblMinimo.Top:=lblCantidad.Top;
  lblMinimo.Left:=lblSellPrice.Left;

  edtStockMin.Top:=edtStock.Top;
  edtStockMin.Left:=lblMinimo.Left;
  edtStockMin.Width:=edtStock.Width;

  lblMaximo.Top:=lblMinimo.Top;
  lblMaximo.Left:=lblDescuento.Left;

  edtStockMax.Top:=edtStockMin.Top;
  edtStockMax.Left:=lblMaximo.Left;
  edtStockMax.Width:=edtStockMin.Width;

  // Fuera de los grupos
  lblImagen.Top:=grpDos.Top+grpDos.Height+(Alto1*2);
  lblImagen.Left:=Ancho1;

  imgProduct.Top:=lblImagen.Top+lblImagen.Height+Alto1;
  imgProduct.Left:=Ancho1;
  imgProduct.Width:=lblSellPrice.Left - Ancho1;
  imgProduct.Height:=Trunc((imgBack.Height - imgProduct.Top) / 1.5);

  lblCodeBarDraw.Top:=lblImagen.Top;
  lblCodeBarDraw.Left:=lblMinimo.Left + Ancho1;

  edtBarCodeDraw.Top:=imgProduct.Top;
  edtBarCodeDraw.Left:=lblCodeBarDraw.Left;
  edtBarCodeDraw.Width:=Trunc((grpDos.Width - edtBarCodeDraw.Left) / 1.5);

  lblArchivoPDF.Top:=edtBarCodeDraw.Top + edtBarCodeDraw.Height + Trunc(Alto1*10);
  lblArchivoPDF.Left:=edtBarCodeDraw.Left;

  edtFilePdf.Top:=lblArchivoPDF.Top + lblArchivoPDF.Height + Alto1;
  edtFilePdf.Left:=lblArchivoPDF.Left;
  edtFilePdf.Width:=edtBarCodeDraw.Width;

  btnLoadImage.Top:=edtBarCodeDraw.Top;
  btnLoadImage.Left:=grpDos.Left+grpDos.Width - btnLoadImage.Width;

  btnLoadPDF.Top:=edtFilePdf.Top;
  btnLoadPDF.Left:=btnLoadImage.Left;
end;

// Se usa para que la pantalla quede en el mismo lugar cuando se trata de crecer
procedure TfrmProducts.FormChangeBounds(Sender: TObject);
begin
  if frmProducts.Active then
    begin
      frmProducts.Top:=125;
      frmProducts.Left:=25;
    end;
end;

// Se usa para crear un formulario en blanco cuando se selecciona el botón de nuevo en el catalogo.
procedure TfrmProducts.btnNewClick(Sender: TObject);
begin
  Operation := 'A';

  edtId.Text:='';
  edtNombre.Text:= '';
  edtSku.Text:='';
  memDescrip.Text:='';
  cmbSupercategory.Text:='';
  cmbCategoria.Text:='';
  edtBarCode.Text:='';
  edtPurchacePrice.Value:=0;
  edtSellPrice.Value:=0;
  edtDiscount.Value:=0;
  chbxDiscountAvail.Checked:=False;
  cldStartDate.Date:=Date;
  cldEndDate.Date:=Date;
  edtStock.Value:=0;
  edtStockMin.Value:=0;
  edtStockMax.Value:=0;
  imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
  edtBarCodeDraw.Text:='';
  edtFilePdf.Text:='';
  chbxPedimento.Checked:=False;
end;

// Se usa para salir del formulario y se revisa si hay cambios en el
procedure TfrmProducts.btnQuitClick(Sender: TObject);
begin
    if Cambio then
      begin
        if MessageDlg('Se perderán los cambios', '¿Desea continuar?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          frmProducts.close();
      end
    else
      frmProducts.close();
end;

procedure TfrmProducts.btnReportClick(Sender: TObject);
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
procedure TfrmProducts.btnSaveClick(Sender: TObject);
var
  pname, description, SKU, bar_code, category, supercategory, information_file, image, discount_start_date, discount_end_date : String;
  category_id, discount, stock, stock_min, stock_max : Integer;
  purchase_price, sell_price : real;
  discount_available, pediment : boolean;

begin
  conInfo.HostName:=session.db.host;
  conInfo.UserName:=session.db.user;
  conInfo.Password:=session.db.pwd;

  if Cambio then
    begin
      pname := edtNombre.Text;
      SKU := edtSku.Text;
      description := memDescrip.Text;
      supercategory := cmbSupercategory.Text;
      category := cmbCategoria.Text;
      bar_code := edtBarCode.Text;
      purchase_price := edtPurchacePrice.Value;
      sell_price := edtSellPrice.Value;
      discount := edtDiscount.Value;
      discount_available := chbxDiscountAvail.Checked;
      discount_start_date := FormatDateTime('yyyy-mm-dd', cldStartDate.Date);
      discount_end_date := FormatDateTime('yyyy-mm-dd', cldEndDate.Date);
      stock := round(edtStock.Value);
      stock_min := round(edtStockMin.Value);
      stock_max := round(edtStockMax.Value);
      if opdImage.FileName <> '' then
         image := ExtractFilename(opdImage.FileName);
      information_file := edtFilePdf.Text;
      pediment := chbxPedimento.Checked;

      category_id := getCategoryID(category);

      if NOT existCode(bar_code) then
        begin
          if MessageDlg('Guardar Producto', '¿Desea guarar los datos del producto ' + edtNombre.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
                                  SQL.Text:='INSERT INTO `products` (name, description, SKU, bar_code, category_id, supercategory, information_file, discount, stock, stock_min, stock_max, purchase_price, sell_price, discount_available, discount_start_date, discount_end_date, printed, pediment) VALUES (:qryName, :qryDescription, :qrySKU, :qryBar_code, :qryCategory_id, :qrySupercategory, :qryInformation_file, :qryDiscount, :qryStock, :qryStock_min, :qryStock_max, :qryPurchase_price, :qrySell_price, :qryDiscount_available, :qryDiscount_start_date, :qryDiscount_end_date, 1, :qryPediment)';
                                  params.ParamByName('qryName').AsString:=pname;
                                  params.ParamByName('qryDescription').AsString:=description;
                                  params.ParamByName('qrySKU').AsString:=SKU;
                                  params.ParamByName('qryBar_code').AsString:=bar_code;
                                  params.ParamByName('qrySupercategory').AsString:=supercategory;
                                  params.ParamByName('qryInformation_file').AsString:=information_file;
                                  params.ParamByName('qryCategory_id').AsInteger:=category_id;
                                  params.ParamByName('qryDiscount').AsInteger:=discount;
                                  params.ParamByName('qryStock').AsInteger:=stock;
                                  params.ParamByName('qryStock_min').AsInteger:=stock_min;
                                  params.ParamByName('qryStock_max').AsInteger:=stock_max;
                                  params.ParamByName('qryPurchase_price').AsFloat:=purchase_price;
                                  params.ParamByName('qrySell_price').AsFloat:=sell_price;
                                  params.ParamByName('qryDiscount_available').AsBoolean:=discount_available;
                                  params.ParamByName('qryDiscount_start_date').AsString:=discount_start_date;
                                  params.ParamByName('qryDiscount_end_date').AsString:=discount_end_date;
                                  params.ParamByName('qryPediment').AsBoolean:=pediment;
                                end
                              else
                                begin
                                  SQL.Text:='INSERT INTO `products` (name, description, SKU, bar_code, category_id, supercategory, information_file, image, discount, stock, stock_min, stock_max, purchase_price, sell_price, discount_available, discount_start_date, discount_end_date, printed, pediment) VALUES (:qryName, :qryDescription, :qrySKU, :qryBar_code, :qryCategory_id, :qrySupercategory, :qryInformation_file, :qryImage, :qryDiscount, :qryStock, :qryStock_min, :qryStock_max, :qryPurchase_price, :qrySell_price, :qryDiscount_available, :qryDiscount_start_date, :qryDiscount_end_date, 1, :qryPediment)';
                                  params.ParamByName('qryName').AsString:=pname;
                                  params.ParamByName('qryDescription').AsString:=description;
                                  params.ParamByName('qrySKU').AsString:=SKU;
                                  params.ParamByName('qryBar_code').AsString:=bar_code;
                                  params.ParamByName('qrySupercategory').AsString:=supercategory;
                                  params.ParamByName('qryInformation_file').AsString:=information_file;
                                  params.ParamByName('qryImage').AsString:=image;
                                  params.ParamByName('qryCategory_id').AsInteger:=category_id;
                                  params.ParamByName('qryDiscount').AsInteger:=discount;
                                  params.ParamByName('qryStock').AsInteger:=stock;
                                  params.ParamByName('qryStock_min').AsInteger:=stock_min;
                                  params.ParamByName('qryStock_max').AsInteger:=stock_max;
                                  params.ParamByName('qryPurchase_price').AsFloat:=purchase_price;
                                  params.ParamByName('qrySell_price').AsFloat:=sell_price;
                                  params.ParamByName('qryDiscount_available').AsBoolean:=discount_available;
                                  params.ParamByName('qryDiscount_start_date').AsString:=discount_start_date;
                                  params.ParamByName('qryDiscount_end_date').AsString:=discount_end_date;
                                  params.ParamByName('qryPediment').AsBoolean:=pediment;
                                end;
                            end
                          else
                            begin
                              if image = '' then
                                begin
                                  SQL.Text:='UPDATE `products` SET name = :qryName, description = :qryDescription, SKU = :qrySKU, bar_code = :qryBar_code, category_id = :qryCategory_id, supercategory = :qrySupercategory, information_file = :qryInformation_file, discount = :qryDiscount, stock = :qryStock, stock_min = :qryStock_min, stock_max = :qryStock_max, purchase_price = :qryPurchase_price, sell_price = :qrySell_price, discount_available = :qryDiscount_available, discount_start_date = :qryDiscount_start_date, discount_end_date = :qryDiscount_end_date, printed = 1, pediment = :qryPediment WHERE id = :qryID';
                                  params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                                  params.ParamByName('qryName').AsString:=pname;
                                  params.ParamByName('qryDescription').AsString:=description;
                                  params.ParamByName('qrySKU').AsString:=SKU;
                                  params.ParamByName('qryBar_code').AsString:=bar_code;
                                  params.ParamByName('qrySupercategory').AsString:=supercategory;
                                  params.ParamByName('qryInformation_file').AsString:=information_file;
                                  params.ParamByName('qryCategory_id').AsInteger:=category_id;
                                  params.ParamByName('qryDiscount').AsInteger:=discount;
                                  params.ParamByName('qryStock').AsInteger:=stock;
                                  params.ParamByName('qryStock_min').AsInteger:=stock_min;
                                  params.ParamByName('qryStock_max').AsInteger:=stock_max;
                                  params.ParamByName('qryPurchase_price').AsFloat:=purchase_price;
                                  params.ParamByName('qrySell_price').AsFloat:=sell_price;
                                  params.ParamByName('qryDiscount_available').AsBoolean:=discount_available;
                                  params.ParamByName('qryDiscount_start_date').AsString:=discount_start_date;
                                  params.ParamByName('qryDiscount_end_date').AsString:=discount_end_date;
                                  params.ParamByName('qryPediment').AsBoolean:=pediment;
                                end
                              else
                                begin
                                  SQL.Text:='UPDATE `products` SET image=:qryImage, name = :qryName, description = :qryDescription, SKU = :qrySKU, bar_code = :qryBar_code, category_id = :qryCategory_id, supercategory = :qrySupercategory, information_file = :qryInformation_file, discount = :qryDiscount, stock = :qryStock, stock_min = :qryStock_min, stock_max = :qryStock_max, purchase_price = :qryPurchase_price, sell_price = :qrySell_price, discount_available = :qryDiscount_available, discount_start_date = :qryDiscount_start_date, discount_end_date = :qryDiscount_end_date, printed = 1, pediment = :qryPediment WHERE id = :qryID';
                                  params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                                  params.ParamByName('qryName').AsString:=pname;
                                  params.ParamByName('qryDescription').AsString:=description;
                                  params.ParamByName('qrySKU').AsString:=SKU;
                                  params.ParamByName('qryBar_code').AsString:=bar_code;
                                  params.ParamByName('qrySupercategory').AsString:=supercategory;
                                  params.ParamByName('qryInformation_file').AsString:=information_file;
                                  params.ParamByName('qryImage').AsString:=image;
                                  params.ParamByName('qryCategory_id').AsInteger:=category_id;
                                  params.ParamByName('qryDiscount').AsInteger:=discount;
                                  params.ParamByName('qryStock').AsInteger:=stock;
                                  params.ParamByName('qryStock_min').AsInteger:=stock_min;
                                  params.ParamByName('qryStock_max').AsInteger:=stock_max;
                                  params.ParamByName('qryPurchase_price').AsFloat:=purchase_price;
                                  params.ParamByName('qrySell_price').AsFloat:=sell_price;
                                  params.ParamByName('qryDiscount_available').AsBoolean:=discount_available;
                                  params.ParamByName('qryDiscount_start_date').AsString:=discount_start_date;
                                  params.ParamByName('qryDiscount_end_date').AsString:=discount_end_date;
                                  params.ParamByName('qryPediment').AsBoolean:=pediment;
                                end;
                            end;
                          ExecSQL;
                          SQLTransaction.Commit;

                          refreshGrid();

                          edtId.Text:='';
                          edtNombre.Text:= '';
                          edtSku.Text:='';
                          memDescrip.Text:='';
                          cmbSupercategory.Text:='';
                          cmbCategoria.Text:='';
                          edtBarCode.Text:='';
                          edtPurchacePrice.Value:=0;
                          edtSellPrice.Value:=0;
                          edtDiscount.Value:=0;
                          chbxDiscountAvail.Checked:=False;
                          cldStartDate.Date:=Date;
                          cldEndDate.Date:=Date;
                          edtStock.Value:=0;
                          edtStockMin.Value:=0;
                          edtStockMax.Value:=0;
                          imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
                          edtBarCodeDraw.Text:='';
                          edtFilePdf.Text:='';
                          chbxPedimento.Checked:=False;
                        end;
                    end;
                end;
            end;
          showmessage('El producto ha sido almacenado correctamente.');
          Operation:='E';
          Cambio := false;
          btnSave.Enabled:=false;
        end
      else
        begin
          ShowMessage('El código de barras ya existe en el sistema, no se puede guardar el producto. Verifiquelo o busque el producto.');
        end;
    end
  else
    begin
      edtId.Text:='';
      edtNombre.Text:= '';
      edtSku.Text:='';
      memDescrip.Text:='';
      cmbSupercategory.Text:='';
      cmbCategoria.Text:='';
      edtBarCode.Text:='';
      edtPurchacePrice.Value:=0;
      edtSellPrice.Value:=0;
      edtDiscount.Value:=0;
      chbxDiscountAvail.Checked:=False;
      cldStartDate.Date:=Date;
      cldEndDate.Date:=Date;
      edtStock.Value:=0;
      edtStockMin.Value:=0;
      edtStockMax.Value:=0;
      imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
      edtBarCodeDraw.Text:='';
      edtFilePdf.Text:='';
      chbxPedimento.Checked:=False;
      Operation:='E';
      Cambio := false;
      btnSave.Enabled:=false;
    end;
end;

procedure TfrmProducts.cmbSupercategoryChange(Sender: TObject);
var
  CB : Integer;
  CBar : String;
begin
  if cmbSupercategory.Text = 'PECES' then
    begin
      CBar := '';
      Randomize;
      While (existCode(CBar) = true) OR (CBar = '') do
        begin
          CB := Abs(Random(9999999999));
          if CB < 0 then
            CB := CB * -1;
          CBar := AddChar('0', IntToStr(CB), 10);
        end;
      edtBarCode.Text:=CBar;
      Cambio := true;
      btnSave.Enabled:=true;
    end;
end;

procedure TfrmProducts.conDataAfterConnect(Sender: TObject);
begin

end;

// Se usa para borrar un registro del catalogo
procedure TfrmProducts.btnDeleteClick(Sender: TObject);
begin
  if Operation = 'A' then
    begin
      edtId.Text:='';
      edtNombre.Text:= '';
      edtSku.Text:='';
      memDescrip.Text:='';
      cmbSupercategory.Text:='';
      cmbCategoria.Text:='';
      edtBarCode.Text:='';
      edtPurchacePrice.Value:=0;
      edtSellPrice.Value:=0;
      edtDiscount.Value:=0;
      chbxDiscountAvail.Checked:=False;
      cldStartDate.Date:=Date;
      cldEndDate.Date:=Date;
      edtStock.Value:=0;
      edtStockMin.Value:=0;
      edtStockMax.Value:=0;
      imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
      edtBarCodeDraw.Text:='';
      edtFilePdf.Text:='';
      chbxPedimento.Checked:=false;
      Operation:='E';
      Cambio := false;
      btnSave.Enabled:=false;
    end
  else
    begin
      if MessageDlg('Eliminar producto', '¿Desea eliminar el producto ' + edtNombre.Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
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
                      SQL.Text:='DELETE FROM `products` WHERE id = :qryID';
                      params.ParamByName('qryID').AsInteger:=StrToInt(edtId.Text);
                      ExecSQL;
                      SQLTransaction.Commit;

                      refreshGrid();

                      edtId.Text:='';
                      edtNombre.Text:= '';
                      edtSku.Text:='';
                      memDescrip.Text:='';
                      cmbSupercategory.Text:='';
                      cmbCategoria.Text:='';
                      edtBarCode.Text:='';
                      edtPurchacePrice.Value:=0;
                      edtSellPrice.Value:=0;
                      edtDiscount.Value:=0;
                      chbxDiscountAvail.Checked:=False;
                      cldStartDate.Date:=Date;
                      cldEndDate.Date:=Date;
                      edtStock.Value:=0;
                      edtStockMin.Value:=0;
                      edtStockMax.Value:=0;
                      imgProduct.Picture.LoadFromFile(GetCurrentDir + '\images\noimage.jpg');
                      edtBarCodeDraw.Text:='';
                      edtFilePdf.Text:='';
                      chbxPedimento.Checked:=False;
                      Operation:='E';
                      Cambio := false;
                      btnSave.Enabled:=false;
                    end;
                end;
            end;
        end;
    end;
end;

procedure TfrmProducts.btnLoadImageClick(Sender: TObject);
begin
  if opdImage.Execute then
    begin
      CopyFile(opdImage.FileName, GetCurrentDir + '/images/uploads/' + ExtractFilename(opdImage.FileName));
      imgProduct.Picture.LoadFromFile(opdImage.FileName);
      Cambio := true;
      btnSave.Enabled:=true;
    end;
end;

procedure TfrmProducts.btnLoadPDFClick(Sender: TObject);
begin
  if opdPdf.Execute then
    begin
      CopyFile(opdPdf.FileName, GetCurrentDir + '/fichas/' + ExtractFilename(opdPdf.FileName));
      edtFilePdf.Text:=ExtractFilename(opdPdf.FileName);
      Cambio := true;
      btnSave.Enabled:=true;
    end;
end;

// Se usa para ordenar en forma ascendente o descendente segun el numero de clicks al titulo de la columna en el grid
procedure TfrmProducts.dbgCatalogTitleClick(Column: TColumn);
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

procedure TfrmProducts.edtBarCodeChange(Sender: TObject);
begin
  Cambio := true;
  btnSave.Enabled:=true;
  edtBarCodeDraw.Text:=edtBarCode.Text;
  if existCode(edtBarCode.Text) then
    ShowMessage('El código de barras ya existe en el sistema. Verifique o busque el producto.');
end;

// Esta función se usa en todos los campos para seleccionar si ya hubo un cambio o no
procedure TfrmProducts.edtNombreChange(Sender: TObject);
begin
  Cambio := true;
  btnSave.Enabled:=true;
end;

// Esa función se utiliza para limpiar el grid y regresarlo a su estado original, realiza un refresh
procedure TfrmProducts.edtSearchButtonClick(Sender: TObject);
var
  i : Integer;
begin
  Query := 'SELECT p.`id` AS ID, p.`name` AS PRODUCTO, p.`bar_code` AS CODIGO, p.`supercategory` AS SUPERCAT, c.`name` AS CATEGORIA, p.`sell_price` AS PRECIO, p.`stock` AS STOCK FROM `products` AS p JOIN `categories` AS c ON (p.category_id = c.id)';
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
procedure TfrmProducts.edtSearchChange(Sender: TObject);
var
  number : Integer;
begin
  if (edtSearch.Text <> '') then
    begin
      number := 0;

      TryStrToInt(edtSearch.Text,number);

      qryCatalog.Close;

      if number > 0 then
        Where := ' WHERE (p.id = :qryID) OR (p.name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (p.bar_code LIKE CONCAT(''%'',:qrySearch,''%'')) OR (p.supercategory LIKE CONCAT(''%'',:qrySearch,''%'')) OR (c.name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (p.sell_price LIKE CONCAT(''%'',:qryID,''%'')) OR (p.stock LIKE CONCAT(''%'',:qryID,''%''))'
      else
        Where := 'WHERE (p.name LIKE CONCAT(''%'',:qrySearch,''%'')) OR (p.bar_code LIKE CONCAT(''%'',:qrySearch,''%'')) OR (p.supercategory LIKE CONCAT(''%'',:qrySearch,''%'')) OR (c.name LIKE CONCAT(''%'',:qrySearch,''%''))';

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
procedure TfrmProducts.dbgCatalogCellClick();
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

