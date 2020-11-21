unit calc_utils;

interface

uses Windows, SysUtils, Classes, graphics, Ap, mlpbase, mlptrain, Vcl.Grids, u_uidataselection;

type
  TCalcNeuroOptions = record
    lMaxIts: AlgLibInteger; // 500; // максимальное количество итераций обучения
    lMaxStep: AlgLibFloat; // 0.001; // минимальный достигаемый шаг (ошибка)
    lRestarts: AlgLibInteger; // 50; // количество рестартов обучения с рандомизацией весов
    lDecay: AlgLibFloat; // 0.001; // точность обучения. Чем меньше - тем точнее
    lPoints: AlgLibInteger; // length(lXY) количество обучающих выборок (строк в матрице)
    LayerCount: AlgLibInteger; // количество слоёв
    Layer1PtsCount: AlgLibInteger; // количество нейронов в первом слое (независимых переменных)
    Layer2PtsCount: AlgLibInteger; // количество нейронов во втором слое
    Layer3PtsCount: AlgLibInteger; // количество нейронов в третьем слое
    OutPtsCount: AlgLibInteger; // количество зависимых переменных
    IsClassificator: boolean; // тип матрицы - классификатор или обычная
  end;

procedure NewEmptyMatrix(aWidth, aHeight: Integer; out lXY: TReal2DArray);
procedure InsertDataFromClipboardToGrid(const aClipboardText: string; aGrid: TStringGrid);
procedure InsertDataFromClipboardToGridWithInfo(const aClipboardText: string; aGrid: TStringGrid);
procedure InsertDataFromClipboardToGridClassificator(const aClipboardText: string; aCurrentClassIdx: Integer;
  aGridData, aGridClass: TStringGrid);
procedure ConstructMatrixFromLine(lX: TReal1DArray; aSubLineWidth, aStep, aCount: Integer; out lXY: TReal2DArray);
procedure ConstructMatrixFromGrid(aGridSrc, aGridResult: TStringGrid; out lXY: TReal2DArray);
procedure ConstructMatrixFromOneGrid(aGridSrc: TStringGrid; aDependentCol: Integer; out lXY: TReal2DArray);
procedure CalcNeuroMatrix(aClassificator: boolean; aLayerCount: byte; lInCount, lOutCount: Integer; lXY: TReal2DArray;
  out lNetwork: MultiLayerPerceptron); overload;
procedure CalcNeuroMatrix(const aOptions: TCalcNeuroOptions; lXY: TReal2DArray;
  out lNetwork: MultiLayerPerceptron); overload;
procedure GetSubLine(aXSrc: TReal1DArray; aPosition, aSubLineWidth: Integer; var aXDest: TReal1DArray);
procedure PutImageToArrayLine(aImageIdx, aCharIdx: Integer; aCanvas: TCanvas; aWidth, aHeight: Integer;
  var lXY: TReal2DArray);
procedure Get1DArrayFromImage(aCanvas: TCanvas; aWidth, aHeight: Integer; var lX: TReal1DArray);
function NeuroRegression(var lNetwork: MultiLayerPerceptron; lX: TReal1DArray; var lY: TReal1DArray): AlgLibFloat;
function StrToFloatEx(const aStr: string): AlgLibFloat;

procedure MakeUnifySportsmenMatrix(aGridSrc, aGridDest: TStringGrid; aDataSelection: TDataSelection);
procedure OptimizeMatrixBy1Param(aGridSrc, aGridDest: TStringGrid; aDataSelection: TDataSelection;
  aDiapazonesCount: Integer);

implementation

uses Math, StrUtils;

function StrToFloatEx(const aStr: string): AlgLibFloat;
begin
  if (FormatSettings.DecimalSeparator = '.') then
    result := StrToFloatDef(ReplaceStr(aStr, ',', FormatSettings.DecimalSeparator), 0)
  else
    result := StrToFloatDef(ReplaceStr(aStr, '.', FormatSettings.DecimalSeparator), 0);
end;

procedure NewEmptyMatrix(aWidth, aHeight: Integer; out lXY: TReal2DArray);
var
  x, y: Integer;
begin
  SetLength(lXY, aHeight, aWidth);
  for y := Low(lXY) to High(lXY) do
    for x := Low(lXY[0]) to High(lXY[0]) do
      lXY[y, x] := 0;
end;

function GetMaxColTextWidth(aGrid: TStringGrid; aFirstRow, aCol: Integer): Integer;
var
  i: Integer;
begin
  result := 0;
  for i := aFirstRow to aGrid.RowCount - 1 do
    result := max(result, aGrid.Canvas.TextExtent(aGrid.Cells[aCol, i]).cx);
  result := max(5, result + 5);
end;

procedure InsertDataFromClipboardToGrid(const aClipboardText: string; aGrid: TStringGrid);
var
  i, j: Integer;
  zSL: TStringList;
  zParseSL: TStringList;
begin
  aGrid.DoubleBuffered := True;
  aGrid.Canvas.Lock;
  aGrid.ColCount := 1;
  zSL := TStringList.Create;
  zParseSL := TStringList.Create;
  zSL.BeginUpdate;
  zParseSL.BeginUpdate;
  try
    zSL.Text := aClipboardText;
    aGrid.RowCount := zSL.Count;
    zParseSL.LineBreak := #9;
    for i := 0 to zSL.Count - 1 do
    begin
      zParseSL.Text := zSL[i];
      aGrid.ColCount := max(aGrid.ColCount, zParseSL.Count);
      for j := 0 to zParseSL.Count - 1 do
        aGrid.Cells[j, i] := zParseSL[j];
    end;
  finally
    aGrid.Canvas.Unlock;
    FreeAndNil(zParseSL);
    FreeAndNil(zSL);
  end;
end;

procedure InsertDataFromClipboardToGridWithInfo(const aClipboardText: string; aGrid: TStringGrid);
var
  i, j: Integer;
  zSL: TStringList;
  zParseSL: TStringList;
begin
  aGrid.DoubleBuffered := True;
  aGrid.Canvas.Lock;
  aGrid.ColCount := 2;
  aGrid.RowCount := 2;
  zSL := TStringList.Create;
  zParseSL := TStringList.Create;
  zSL.BeginUpdate;
  zParseSL.BeginUpdate;
  try
    aGrid.FixedCols := 1;
    aGrid.FixedRows := 1;
    zSL.Text := aClipboardText;
    // удалим пустые строки
    for i := zSL.Count - 1 downto 0 do
      if (Trim(zSL[i]) = '') then
        zSL.Delete(i);

    aGrid.RowCount := zSL.Count + 1;
    zParseSL.LineBreak := #9;
    for i := 0 to zSL.Count - 1 do
    begin
      zParseSL.Text := zSL[i];
      aGrid.ColCount := max(aGrid.ColCount, zParseSL.Count);
      for j := 0 to zParseSL.Count - 1 do
        aGrid.Cells[j, i + 1] := zParseSL[j];
    end;
    // auto shrink
    for j := 0 to aGrid.ColCount - 1 do
      aGrid.ColWidths[j] := GetMaxColTextWidth(aGrid, 1, j);
  finally
    aGrid.Canvas.Unlock;
    FreeAndNil(zParseSL);
    FreeAndNil(zSL);
  end;
end;

procedure InsertDataFromClipboardToGridClassificator(const aClipboardText: string; aCurrentClassIdx: Integer;
  aGridData, aGridClass: TStringGrid);
var
  i, j: Integer;
  zSL: TStringList;
  zParseSL: TStringList;
  zCurrLine: Integer;
begin
  aGridData.DoubleBuffered := True;
  aGridClass.DoubleBuffered := True;
  aGridData.Canvas.Lock;
  aGridClass.Canvas.Lock;
  zSL := TStringList.Create;
  zParseSL := TStringList.Create;
  zSL.BeginUpdate;
  zParseSL.BeginUpdate;
  try
    if (aCurrentClassIdx > 0) then
      zCurrLine := aGridData.RowCount
    else
      zCurrLine := 0;
    zSL.Text := aClipboardText;
    aGridData.RowCount := zCurrLine + zSL.Count;
    aGridClass.RowCount := aGridData.RowCount;
    zParseSL.LineBreak := #9;
    for i := 0 to zSL.Count - 1 do
    begin
      zParseSL.Text := zSL[i];
      aGridData.ColCount := max(aGridData.ColCount, zParseSL.Count);
      for j := 0 to zParseSL.Count - 1 do
        aGridData.Cells[j, i + zCurrLine] := zParseSL[j];
      aGridClass.Cells[0, i + zCurrLine] := IntToStr(aCurrentClassIdx);
    end;
  finally
    aGridData.Canvas.Unlock;
    aGridClass.Canvas.Unlock;
    FreeAndNil(zParseSL);
    FreeAndNil(zSL);
  end;
end;

procedure ConstructMatrixFromLine(lX: TReal1DArray; aSubLineWidth, aStep, aCount: Integer; out lXY: TReal2DArray);
var
  x, y: Integer;
  zYCount: Integer;
begin
  // zYCount := Length(lX) - aSubLineWidth;
  zYCount := aCount;
  SetLength(lXY, zYCount, aSubLineWidth + 1);
  y := 0;
  while y < zYCount do
  begin
    for x := 0 to aSubLineWidth do
      lXY[y, x] := lX[y + x];
    inc(y, aStep);
  end;
end;

procedure ConstructMatrixFromGrid(aGridSrc, aGridResult: TStringGrid; out lXY: TReal2DArray);
var
  x, y: Integer;
  zXCount, zYCount: Integer;
  zGridSrcWidth, zGridResWidth: Integer;
begin
  zYCount := aGridSrc.RowCount;
  zGridSrcWidth := aGridSrc.ColCount;
  zGridResWidth := aGridResult.ColCount;
  zXCount := zGridSrcWidth + zGridResWidth;
  SetLength(lXY, zYCount, zXCount);

  for y := 0 to zYCount - 1 do
  begin
    for x := 0 to zGridSrcWidth - 1 do
      lXY[y, x] := StrToFloatEx(aGridSrc.Cells[x, y]);
    for x := 0 to zGridResWidth - 1 do
      lXY[y, x + zGridSrcWidth] := StrToFloatEx(aGridResult.Cells[x, y]);
  end;
end;

procedure ConstructMatrixFromOneGrid(aGridSrc: TStringGrid; aDependentCol: Integer; out lXY: TReal2DArray);
var
  xsrc, xdest, y: Integer;
  zYCount: Integer;
  zGridSrcWidth: Integer;
begin
  zYCount := aGridSrc.RowCount;
  zGridSrcWidth := aGridSrc.ColCount;
  SetLength(lXY, zYCount, zGridSrcWidth - 1);

  for y := 0 to zYCount - 1 do
  begin
    xsrc := 0;
    xdest := 0;
    while (xsrc < zGridSrcWidth) do
    begin
      if (xsrc <> aDependentCol) then
      begin
        lXY[y, xdest] := StrToFloatEx(aGridSrc.Cells[xsrc + 1, y]);
        inc(xdest);
      end;
      inc(xsrc);
    end;
    lXY[y, zGridSrcWidth - 2] := StrToFloatEx(aGridSrc.Cells[aDependentCol + 1, y]);
  end;
end;

procedure MakeUnifySportsmenMatrix(aGridSrc, aGridDest: TStringGrid; aDataSelection: TDataSelection);
var
  xsrc, xdest, y: Integer;
  zGridSrcHeight: Integer;
  zGridSrcWidth: Integer;
  aDependentCol: Integer;
  zValueType: TSetDataSelectionType;
  zBaseRow: Integer;
begin
  zGridSrcHeight := aGridSrc.RowCount;
  zGridSrcWidth := aGridSrc.ColCount;
  aGridDest.ColCount := aGridSrc.ColCount - 2;
  aGridDest.RowCount := aGridSrc.RowCount - 1;
  aDependentCol := aDataSelection.GetValueOfType(dstResultCol);
  zBaseRow := aDataSelection.GetValueOfType(dstBaseRow);

  for y := 0 to zGridSrcHeight - 1 do
  begin
    xsrc := 0;
    xdest := 0;
    while (xsrc < zGridSrcWidth - 1) do
    begin
      if (xsrc <> aDependentCol) then
      begin
        zValueType := aDataSelection.ValueType(xsrc, y);
        if (dstColToChange in zValueType) or (dstBaseRow in zValueType) or (dstFixedCols in zValueType) then
          aGridDest.Cells[xdest, y] := aGridSrc.Cells[xsrc + 1, y + 1]
        else
          aGridDest.Cells[xdest, y] := aGridSrc.Cells[xsrc + 1, zBaseRow + 1];
        inc(xdest);
      end;
      inc(xsrc);
    end;
  end;
end;

procedure OptimizeMatrixBy1Param(aGridSrc, aGridDest: TStringGrid; aDataSelection: TDataSelection;
  aDiapazonesCount: Integer);
var
  xsrc, xdest, y: Integer;
  zGridSrcHeight: Integer;
  zGridSrcWidth: Integer;
  aDependentCol: Integer;
  zValueType: TSetDataSelectionType;
  zBaseRow: Integer;
  zColToChange: Integer;
  zMinValue, zMaxValue, zValue: AlgLibFloat;
begin
  zGridSrcHeight := aGridSrc.RowCount;
  zGridSrcWidth := aGridSrc.ColCount;
  aGridDest.ColCount := aGridSrc.ColCount - 2;
  aGridDest.RowCount := aGridSrc.RowCount - 1;
  aDependentCol := aDataSelection.GetValueOfType(dstResultCol);
  zBaseRow := aDataSelection.GetValueOfType(dstBaseRow);

  // сначала найдём границы диапазонов в столбце dstColToChange
  zColToChange := aDataSelection.GetValueOfType(dstColToChange);
  zMinValue := StrToFloatEx(aGridSrc.Cells[zColToChange + 1, 1]);
  zMaxValue := zMinValue;
  for y := 1 to zGridSrcHeight - 1 do
  begin
    zValue := StrToFloatEx(aGridSrc.Cells[zColToChange + 1, y + 1]);
    if (zMinValue > zValue) then
      zMinValue := zValue
    else if (zMaxValue < zValue) then
      zMaxValue := zValue
  end;

  for y := 0 to zGridSrcHeight - 1 do
  begin
    xsrc := 0;
    xdest := 0;
    while (xsrc < zGridSrcWidth - 1) do
    begin
      if (xsrc <> aDependentCol) then
      begin
        zValueType := aDataSelection.ValueType(xsrc, y);
        if (dstColToChange in zValueType) or (dstBaseRow in zValueType) or (dstFixedCols in zValueType) then
          aGridDest.Cells[xdest, y] := aGridSrc.Cells[xsrc + 1, y + 1]
        else
          aGridDest.Cells[xdest, y] := aGridSrc.Cells[xsrc + 1, zBaseRow + 1];
        inc(xdest);
      end;
      inc(xsrc);
    end;
  end;
end;

procedure CalcNeuroMatrix(aClassificator: boolean; aLayerCount: byte; lInCount, lOutCount: Integer; lXY: TReal2DArray;
  out lNetwork: MultiLayerPerceptron);
var
  lMaxIts: Integer;
  lMaxStep: Double;
  lRestarts: Integer;
  lDecay: Double;
  lPoints: Integer;

  lInfo: Integer;
  lReport: MLPReport;
begin
  Randomize;
  lMaxIts := 500; // максимальное количество итераций обучения
  lMaxStep := 0.001; // минимальный достигаемый шаг (ошибка)
  lRestarts := 50; // количество рестартов обучения с рандомизацией весов
  lDecay := 0.001; // точность обучения. Чем меньше - тем точнее
  lPoints := Length(lXY); // количество обучающих выборок (в нашем случае можно и поменьше)

  // здесь можно использовать любую из функций MLPCreate
  case aClassificator of
    false:
      case aLayerCount of
        1:
          MLPCreate0(lInCount, lOutCount, { 0, 1, } lNetwork);
        2:
          MLPCreate1(lInCount, lInCount, lOutCount, { 0, 1, } lNetwork);
      else
        MLPCreate2(lInCount, lInCount, lInCount, lOutCount, { 0, 1, } lNetwork);
      end;
    True:
      case aLayerCount of
        1:
          MLPCreateC0(lInCount, lOutCount, { 0, 50, } lNetwork);
        2:
          MLPCreateC1(lInCount, lInCount, lOutCount, { 0, 50, } lNetwork);
      else
        MLPCreateC2(lInCount, lInCount, lInCount, lOutCount, { 0, 50, } lNetwork);
      end;
  end;
  // один из методов обучения. Можно использовать любой другой
  // MLPTrainLBFGS_MT_Mod(lNetwork, lXY, lPoints, lRestarts, lMaxStep, 10, lMaxIts, lInfo, lReport);
  MLPTrainLBFGS_MT(lNetwork, lXY, lPoints, lDecay, lRestarts, lMaxStep, lMaxIts, lInfo, lReport, 0);
  // MLPTrainMonteCarlo(lNetwork, lXY, lPoints, 10, lRestarts, 0, lMaxIts, lInfo, lReport);
  // MLPTrainLM(lNetwork, lXY, lPoints, lDecay, lRestarts, lInfo, lReport);
  // MLPTrainES(lNetwork, lXY, lPoints, lXY, lPoints, lDecay, lRestarts, lInfo, lReport);

end;

procedure CalcNeuroMatrix(const aOptions: TCalcNeuroOptions; lXY: TReal2DArray;
  out lNetwork: MultiLayerPerceptron); overload;
var
  lInfo: Integer;
  lReport: MLPReport;
begin
  // здесь можно использовать любую из функций MLPCreate
  case aOptions.IsClassificator of
    false:
      case aOptions.LayerCount of
        1:
          MLPCreate0(aOptions.Layer1PtsCount, aOptions.OutPtsCount, lNetwork);
        2:
          MLPCreate1(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.OutPtsCount, lNetwork);
      else
        MLPCreate2(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.Layer3PtsCount, aOptions.OutPtsCount,
          lNetwork);
      end;
    True:
      case aOptions.LayerCount of
        1:
          MLPCreateC0(aOptions.Layer1PtsCount, aOptions.OutPtsCount, lNetwork);
        2:
          MLPCreateC1(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.OutPtsCount, lNetwork);
      else
        MLPCreateC2(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.Layer3PtsCount, aOptions.OutPtsCount,
          lNetwork);
      end;
  end;

  MLPTrainLBFGS_MT(lNetwork, lXY, aOptions.lPoints, aOptions.lDecay, aOptions.lRestarts, aOptions.lMaxStep,
    aOptions.lMaxIts, lInfo, lReport, 0);
end;

// aSubLineWidth - длина строки с конечным обучающим значением
procedure GetSubLine(aXSrc: TReal1DArray; aPosition, aSubLineWidth: Integer; var aXDest: TReal1DArray); inline;
var
  i: Integer;
begin
  for i := 0 to aSubLineWidth - 1 do
    aXDest[i] := aXSrc[i + aPosition];
end;

function NeuroRegression(var lNetwork: MultiLayerPerceptron; lX: TReal1DArray; var lY: TReal1DArray): AlgLibFloat;
begin
  MLPProcess(lNetwork, lX, lY);
  result := lY[0];
end;

function RgbToGray(RGBColor: TColor): AlgLibFloat;
begin
  result := ((0.299 * GetRValue(RGBColor)) + (0.587 * GetGValue(RGBColor)) + (0.114 * GetBValue(RGBColor))) * (1 / 255);
end;

procedure PutImageToArrayLine(aImageIdx, aCharIdx: Integer; aCanvas: TCanvas; aWidth, aHeight: Integer;
  var lXY: TReal2DArray);
var
  x, y, i: Integer;
begin
  aCanvas.Lock;
  try
    i := 0;
    for y := 0 to aHeight - 1 do
      for x := 0 to aWidth - 1 do
      begin
        lXY[aImageIdx, i] := RgbToGray(aCanvas.Pixels[x, y]);
        inc(i);
      end;

    // не указываем зарезервированные параметры
    if (aCharIdx <> -1) then
      lXY[aImageIdx, aWidth * aHeight] := aCharIdx;
  finally
    aCanvas.Unlock;
  end;
end;

procedure Get1DArrayFromImage(aCanvas: TCanvas; aWidth, aHeight: Integer; var lX: TReal1DArray);
var
  x, y, i: Integer;
begin
  aCanvas.Lock;
  try
    i := 0;
    for y := 0 to aHeight - 1 do
      for x := 0 to aWidth - 1 do
      begin
        lX[i] := RgbToGray(aCanvas.Pixels[x, y]);
        inc(i);
      end;
  finally
    aCanvas.Unlock;
  end;
end;

end.
