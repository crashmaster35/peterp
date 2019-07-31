unit entry_inventory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, EditBtn, StdCtrls, Buttons, DbCtrls, MaskEdit, Spin;

type

  { TfrmEntryInversment }

  TfrmEntryInversment = class(TForm)
    btnDelete: TBitBtn;
    btnNew: TBitBtn;
    btnQuit: TBitBtn;
    btnReport: TBitBtn;
    btnSave: TBitBtn;
    cmbMoneda: TComboBox;
    conCatalog: TSQLConnector;
    conData: TSQLConnector;
    conInfo: TSQLConnector;
    dbgCatalog: TDBGrid;
    dbgCatalog1: TDBGrid;
    dsCatalog: TDataSource;
    dsData: TDataSource;
    dsInfo: TDataSource;
    edtCalle: TEdit;
    edtId: TEdit;
    edtNombre: TEdit;
    edtSearch: TEditButton;
    edtSearch1: TEditButton;
    edtStockMin: TFloatSpinEdit;
    grpDos: TGroupBox;
    grpTres: TGroupBox;
    grpUno: TGroupBox;
    imgProduct: TImage;
    lblApellido: TLabel;
    lblCalle: TLabel;
    lblId: TLabel;
    lblImagen: TLabel;
    lblMinimo: TLabel;
    lblNombre3: TLabel;
    navCatalog: TDBNavigator;
    pnlLeft: TPanel;
    qryCatalog: TSQLQuery;
    qryData: TSQLQuery;
    qryInfo: TSQLQuery;
    tmInfo: TSQLTransaction;
    trnCatalog: TSQLTransaction;
    trnData: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
    fondo : TBitmap;
    procedure refreshGrid();
  public
    { public declarations }
  end;

var
  frmEntryInversment: TfrmEntryInversment;
  Campo, Query, Order, Where, Direction : String;
  Operation : Char;
  Cambio : boolean;

implementation

{$R *.lfm}

{ TfrmEntryInversment }
// Refresca la gradilla con los datos del catalogo
procedure TfrmEntryInversment.refreshGrid();
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

procedure TfrmEntryInversment.FormCreate(Sender: TObject);
begin
  fondo := TBitmap.Create;
  fondo.LoadFromFile('images/back.bmp');
end;

procedure TfrmEntryInversment.FormPaint(Sender: TObject);
var
  fila, columna : integer;
  R : TRect;
begin
  R.Left:=0;
  R.Top:=0;
  R.Right:=self.Width;
  R.Bottom:=self.Height;
  canvas.CopyRect(R, Fondo.Canvas, R);
end;

end.

