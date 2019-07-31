unit reportes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  RLReport;

type

  { TfrmRpt }

  TfrmRpt = class(TForm)
    conReport: TSQLConnector;
    dsReport: TDataSource;
    qryReport: TSQLQuery;
    RLBand1: TRLBand;
    RLBand2: TRLBand;
    RLBand3: TRLBand;
    RLDBText2: TRLDBText;
    RLDBText3: TRLDBText;
    RLDBText4: TRLDBText;
    RLDBText5: TRLDBText;
    RLDBText6: TRLDBText;
    RLGroup1: TRLGroup;
    RLImage1: TRLImage;
    RLLabel1: TRLLabel;
    RLSystemInfo2: TRLSystemInfo;
    rptTitle: TRLLabel;
    RLLabel2: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLSystemInfo1: TRLSystemInfo;
    rptHeader: TRLBand;
    rptForm: TRLReport;
    trnReport: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
    campos : Integer;
  end;

var
  frmRpt: TfrmRpt;

implementation

{$R *.lfm}

{ TfrmRpt }


end.

