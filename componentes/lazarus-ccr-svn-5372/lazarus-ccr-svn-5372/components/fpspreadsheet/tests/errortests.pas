unit errortests;

{$mode objfpc}{$H+}

{ Tests for error logging by readers / writers }

interface

uses
  // Not using lazarus package as the user may be working with multiple versions
  // Instead, add ".." to unit search path
  Classes, SysUtils, fpcunit, testregistry,
  fpstypes, fpspreadsheet {and a project requirement for lclbase for utf8 handling},
  fpsutils, testsutility;

type
  { TSpreadErrorTests }

  TSpreadErrorTests= class(TTestCase)
  protected
    // Set up expected values:
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestWriteErrorMessages(AFormat: TsSpreadsheetFormat);

  published
    // Tests collection of error messages during writing
    procedure TestWriteErrorMessages_BIFF2;
    procedure TestWriteErrorMessages_BIFF5;
    procedure TestWriteErrorMessages_BIFF8;
    procedure TestWriteErrorMessages_ODS;
    procedure TestWriteErrorMessages_OOXML;
  end;

implementation

uses
  StrUtils, fpsPalette, fpsRPN, xlsbiff5;

const
  ERROR_SHEET = 'ErrorTest'; //worksheet name

procedure TSpreadErrorTests.SetUp;
begin
end;

procedure TSpreadErrorTests.TearDown;
begin
end;

procedure TSpreadErrorTests.TestWriteErrorMessages(AFormat: TsSpreadsheetFormat);
type
  TTestParam = record
    Format: TsSpreadsheetFormat;
    MaxRowCount: Cardinal;
    MaxColCount: Cardinal;
    MaxCellLen: Cardinal;
  end;
const
  TestParams: array[0..5] of TTestParam = (
    (Format: sfExcel2;       MaxRowCount:   65536; MaxColCount:   256; MaxCellLen: 255),
    (Format: sfExcel5;       MaxRowCount:   65536; MaxColCount:   256; MaxCellLen: 255),
    (Format: sfExcel8;       MaxRowCount:   65536; MaxColCount:   256; MaxCellLen: 32767),
    (Format: sfExcelXML;     MaxRowCount:   65536; MaxColCount:   256; MaxCellLen: 32767),
    (Format: sfOOXML;        MaxRowCount: 1048576; MaxColCount: 16384; MaxCellLen: $FFFFFFFF),
    (Format: sfOpenDocument; MaxRowCount: 1048576; MaxColCount:  1024; MaxCellLen: $FFFFFFFF)
  );
  (*
type
  TTestFormat = (sfExcel2, sfExcel5, sfExcel8, sfExcelXML, sfOOXML, sfOpenDocument);
const                                           // XLS2   XLS5   XLS8   XLSXML   OOXML    ODS
  MAX_ROW_COUNT: array[TTestFormat] of Cardinal = (65536, 65536, 65536, 65536,  1048576, 1048576);
  MAX_COL_COUNT: array[TTestFormat] of Cardinal = (  256,   256,   256,   256,    16384,    1024);
  MAX_CELL_LEN : array[TTestFormat] of Cardinal = (  255,   255, 32767, 32767,$FFFFFFFF,$FFFFFFFF);
  *)
var
  MyWorkbook: TsWorkbook;
  MyWorksheet: TsWorksheet;
  row, col: Cardinal;
  row1, row2: Cardinal;
  col1, col2: Cardinal;
  formula: string;
  s: String;
  TempFile: String;
  ErrList: TStringList;
  newColor: TsColor;
  expected: integer;
  palette: TsPalette;
  i: Integer;
  testIndex: Integer;
begin
  formula := '=A1';

  testIndex := -1;
  for i:=0 to High(TestParams) do
    if TestParams[i].Format = AFormat then begin
      testIndex := i;
      break;
    end;

  if testIndex = -1 then
    raise Exception.CreateFmt('[TSpreadErrorTests.TestWriteErrorMessages] File format %d not found.', [AFormat]);

  ErrList := TStringList.Create;
  try
    // Test 1: Too many rows
    MyWorkbook := TsWorkbook.Create;
    try
      MyWorkSheet:= MyWorkBook.AddWorksheet(ERROR_SHEET);
      row1 := Testparams[testIndex].MaxRowCount - 5;
      row2 := Testparams[testIndex].MaxRowCount + 5;
      for row := row1 to row2 do begin
        MyWorksheet.WriteBlank(row, 0);
        MyWorksheet.WriteNumber(row, 1, 1.0);
        MyWorksheet.WriteText(row, 2, 'A');
        MyWorksheet.WriteFormula(Row, 3, formula);
        MyWorksheet.WriteRPNFormula(row, 4, CreateRPNFormula(
          RPNCellValue('A1', nil)));
      end;
      TempFile:=NewTempFile;
      MyWorkBook.WriteToFile(TempFile, AFormat, true);
      ErrList.Text := MyWorkbook.ErrorMsg;
      CheckEquals(1, ErrList.Count, 'Error count mismatch in test 1');
    finally
      MyWorkbook.Free;
      DeleteFile(TempFile);
    end;

    // Test 2: Too many columns
    MyWorkbook := TsWorkbook.Create;
    try
      MyWorkSheet:= MyWorkBook.AddWorksheet(ERROR_SHEET);
      col1 := TestParams[testIndex].MaxColCount - 5;
      col2 := TestParams[testIndex].MaxColCount + 5;
      for col := col1 to col2 do begin
        MyWorksheet.WriteBlank(0, col);
        MyWorksheet.WriteNumber(1, col, 1.0);
        MyWorksheet.WriteText(2, col, 'A');
        MyWorksheet.WriteFormula(3, col, formula);
        MyWorksheet.WriteRPNFormula(4, col, CreateRPNFormula(
          RPNCellValue('A1', nil)));
      end;
      TempFile:=NewTempFile;
      MyWorkBook.WriteToFile(TempFile, AFormat, true);
      ErrList.Text := MyWorkbook.ErrorMsg;
      CheckEquals(1, ErrList.Count, 'Error count mismatch in test 2');
    finally
      MyWorkbook.Free;
      DeleteFile(TempFile);
    end;

    // Test 3: Too many colors
    MyWorkbook := TsWorkbook.Create;
    try
      // Prepare a full palette
      palette := TsPalette.Create;
      try
        // Create random palette of 65 unique entries - 1 too many for Excel5/8
        // and a lot too many for BIFF2
        palette.AddBuiltinColors;
        for i:=8 to 65 do
        begin
          repeat
            newColor := random(256) + random(256) shl 8 + random(256) shl 16;
          until palette.FindColor(newColor) = -1;
          palette.AddColor(newColor);
        end;

        MyWorkSheet:= MyWorkBook.AddWorksheet(ERROR_SHEET);

        // Use all colors in order to have them in the palette to be written
        // to file.
        for row := 0 to palette.Count-1 do
        begin
          MyWorksheet.WriteText(row, 0, s);
          MyWorksheet.WriteFontColor(row, 0, palette[row]);
        end;

        TempFile:=NewTempFile;
        MyWorkBook.WriteToFile(TempFile, AFormat, true);
        ErrList.Text := MyWorkbook.ErrorMsg;
        // Palette usage in biff --> expecting error due to too large palette
        if (AFormat in [sfExcel2, sfExcel5, sfExcel8]) then
          expected := 1
        else
          // no palette in xml --> no error expected
          expected := 0;
        CheckEquals(expected, ErrList.Count, 'Error count mismatch in test 3');

      finally
        palette.Free;
      end;

    finally
      MyWorkbook.Free;
      DeleteFile(TempFile);
    end;

    // Test 4: Too long cell label
    if TestParams[testIndex].MaxCellLen <> Cardinal(-1) then begin
      s := DupeString('A', TestParams[testIndex].MaxCellLen + 10);
      MyWorkbook := TsWorkbook.Create;
      try
        MyWorkSheet:= MyWorkBook.AddWorksheet(ERROR_SHEET);
        MyWorksheet.WriteText(0, 0, s);
        TempFile:=NewTempFile;
        MyWorkBook.WriteToFile(TempFile, AFormat, true);
        ErrList.Text := MyWorkbook.ErrorMsg;
        CheckEquals(1, ErrList.Count, 'Error count mismatch in test 4');
      finally
        MyWorkbook.Free;
        DeleteFile(TempFile);
      end;
    end;

    // Test 5: cell text contains forbidden XML character
    if (AFormat in [sfOOXML, sfOpenDocument]) then begin
      s := #19'Standard';
      MyWorkbook := TsWorkbook.Create;
      try
        MyWorksheet := MyWorkbook.AddWorksheet(ERROR_SHEET);
        Myworksheet.WriteText(0, 0, s);
        TempFile := NewTempFile;
        Myworkbook.WriteToFile(TempFile, AFormat, true);
        ErrList.Text := MyWorkbook.ErrorMsg;
        CheckEquals(1, ErrList.Count, 'Error count mismatch in test 5');
      finally
        MyWorkbook.Free;
        DeleteFile(TempFile);
      end;
    end else
      Ignore('Test 5 is no error condition for this format');

  finally
    ErrList.Free;
  end;
end;

procedure TSpreadErrorTests.TestWriteErrorMessages_BIFF2;
begin
  TestWriteErrorMessages(sfExcel2);
end;

procedure TSpreadErrorTests.TestWriteErrorMessages_BIFF5;
begin
  TestWriteErrorMessages(sfExcel5);
end;

procedure TSpreadErrorTests.TestWriteErrorMessages_BIFF8;
begin
  TestWriteErrorMessages(sfExcel8);
end;

procedure TSpreadErrorTests.TestWriteErrorMessages_ODS;
begin
  TestWriteErrorMessages(sfOpenDocument);
end;

procedure TSpreadErrorTests.TestWriteErrorMessages_OOXML;
begin
  TestWriteErrorMessages(sfOOXML);
end;


initialization
  // Register so these tests are included in a full run
  RegisterTest(TSpreadErrorTests);

end.

