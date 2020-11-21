unit clasterization_utils;

interface

uses Windows, SysUtils, Classes, graphics, Ap, mlpbase, mlptrain, System.Generics.Collections, calc_utils,
  U_hrv_neurontrain_MT;

type

  TClassPoint = record
    X, Y: Integer;
    ClassIdx: Integer;
  end;

  TPointsList = class(TList<TClassPoint>)
  private
    HImage: TBitmap;
    function ConstructNeuroMatrix: TReal2DArray;
    function ConstructNeuroMatrix_v2: TReal2DArray;
    function Options: TCalcNeuroOptions;
  protected
    procedure DrawNetworkResult1(aNetwork: MultiLayerPerceptron);
    procedure DrawNetworkResult2(aNetwork: MultiLayerPerceptron);
    procedure DrawNetworkResult3(aNetwork: MultiLayerPerceptron);
  public
    procedure Draw(aImage: TBitmap);
    procedure DoClusterization(aImage: TBitmap);
    procedure DoClusterization_v2(aImage: TBitmap);
    procedure DoClusterization_v3(aImage: TBitmap);
  end;

var
  CClassColors: array [0 .. 1] of TColor = (
    clRed,
    clBlue
  );

function AsClassPoint(X, Y: Integer; ClassIdx: Integer): TClassPoint;

implementation

function AsClassPoint(X, Y: Integer; ClassIdx: Integer): TClassPoint;
begin
  result.X := X;
  result.Y := Y;
  result.ClassIdx := ClassIdx;
end;

{ TPointsList }

function TPointsList.ConstructNeuroMatrix_v2: TReal2DArray;
var
  Y: Integer;
  zYCount: Integer;
begin
  zYCount := Count;
  SetLength(result, zYCount, 2 + 2);

  for Y := 0 to zYCount - 1 do
  begin
    result[Y, 0] := Items[Y].X;
    result[Y, 1] := Items[Y].Y;
    result[Y, 2 + Items[Y].ClassIdx] := 1;
  end;
end;

procedure TPointsList.DoClusterization(aImage: TBitmap);
var
  zOptions: TCalcNeuroOptions;
  zlXY: TReal2DArray;
begin
  HImage := aImage;
  zOptions := Options;
  zlXY := ConstructNeuroMatrix;
  // CalcNeuroMatrix(zOptions, zlXY, zNetwork);
  StartTrainLBFS_MT(ExtractFilePath(ParamStr(0)) + 'clasterization1.nmat', zlXY, zOptions, DrawNetworkResult1);
  SetLength(zlXY, 0);
end;

procedure TPointsList.DoClusterization_v2(aImage: TBitmap);
var
  zOptions: TCalcNeuroOptions;
  zlXY: TReal2DArray;
begin
  HImage := aImage;
  zOptions := Options;
  zOptions.IsClassificator := false;
  zlXY := ConstructNeuroMatrix_v2;
  // CalcNeuroMatrix(zOptions, zlXY, zNetwork);
  StartTrainLBFS_MT(ExtractFilePath(ParamStr(0)) + 'clasterization2.nmat', zlXY, zOptions, DrawNetworkResult2);
  SetLength(zlXY, 0);
end;

procedure TPointsList.DoClusterization_v3(aImage: TBitmap);
var
  zOptions: TCalcNeuroOptions;
  zlXY: TReal2DArray;
begin
  HImage := aImage;
  zOptions := Options;
  zOptions.OutPtsCount := 1;
  zOptions.IsClassificator := false;
  zOptions.Layer2PtsCount := 16;
  zOptions.Layer3PtsCount := 16;
  zOptions.LayerCount := 3;
  zOptions.lMaxIts := 300;
  zOptions.lRestarts := 1000;
  zlXY := ConstructNeuroMatrix;
  // CalcNeuroMatrix(zOptions, zlXY, zNetwork);
  StartTrainLBFS_MT(ExtractFilePath(ParamStr(0)) + 'clasterization3.nmat', zlXY, zOptions, DrawNetworkResult3);
  SetLength(zlXY, 0);
end;

function TPointsList.ConstructNeuroMatrix: TReal2DArray;
var
  Y: Integer;
  zYCount: Integer;
begin
  zYCount := Count;
  SetLength(result, zYCount, 3);

  for Y := 0 to zYCount - 1 do
  begin
    result[Y, 0] := Items[Y].X;
    result[Y, 1] := Items[Y].Y;
    result[Y, 2] := Items[Y].ClassIdx;
  end;
end;

procedure TPointsList.Draw(aImage: TBitmap);
var
  i: Integer;
begin
  aImage.Canvas.Brush.Color := clWhite;
  aImage.Canvas.Pen.Color := clWhite;
  aImage.Canvas.Rectangle(0, 0, aImage.Width - 1, aImage.Height - 1);

  for i := 0 to Count - 1 do
  begin
    // Image.Canvas.Pixels[Items[i].X, Items[i].Y] := CClassColors[Items[i].ClassIdx];
    aImage.Canvas.Brush.Color := CClassColors[Items[i].ClassIdx];
    aImage.Canvas.Pen.Color := CClassColors[Items[i].ClassIdx];
    aImage.Canvas.Ellipse(Items[i].X - 2, Items[i].Y - 2, Items[i].X + 2, Items[i].Y + 2);
  end;
end;

procedure TPointsList.DrawNetworkResult1(aNetwork: MultiLayerPerceptron);
var
  X, Y: Integer;
  lX: TReal1DArray;
  lY: TReal1DArray;

  function GetClassColor: TColor;
  begin
    if (lY[0] > 2 * lY[1]) then
      result := CClassColors[0]
    else if (lY[1] > 2 * lY[0]) then
      result := CClassColors[1]
    else
      result := clWhite;
  end;

begin

  SetLength(lX, 2);
  SetLength(lY, 2); // пока жёстко зададим 2 класса
  HImage.Canvas.Lock;
  for Y := 0 to HImage.Height - 1 do
    for X := 0 to HImage.Width - 1 do
    begin
      lX[0] := X;
      lX[1] := Y;
      NeuroRegression(aNetwork, lX, lY);
      HImage.Canvas.Pixels[X, Y] := GetClassColor;
    end;
  HImage.Canvas.Unlock;

  MLPFree(aNetwork);
end;

procedure TPointsList.DrawNetworkResult2(aNetwork: MultiLayerPerceptron);
var
  X, Y: Integer;
  lX: TReal1DArray;
  lY: TReal1DArray;

  function GetClassColor: TColor;
  begin
    if (lY[0] + lY[1] > 0.5) and (lY[0] + lY[1] < 2) then
    begin
      if (lY[0] > 2 * lY[1]) then
        result := CClassColors[0]
      else if (lY[1] > 2 * lY[0]) then
        result := CClassColors[1]
      else
        result := clWhite;
    end
    else
      result := clWhite;
  end;

begin

  SetLength(lX, 2);
  SetLength(lY, 2); // пока жёстко зададим 2 класса
  HImage.Canvas.Lock;
  for Y := 0 to HImage.Height - 1 do
    for X := 0 to HImage.Width - 1 do
    begin
      lX[0] := X;
      lX[1] := Y;
      NeuroRegression(aNetwork, lX, lY);
      HImage.Canvas.Pixels[X, Y] := GetClassColor;
    end;
  HImage.Canvas.Unlock;

  MLPFree(aNetwork);
end;

procedure TPointsList.DrawNetworkResult3(aNetwork: MultiLayerPerceptron);
var
  X, Y: Integer;
  lX: TReal1DArray;
  lY: TReal1DArray;

  function GetClassColor: TColor;
  begin
    if (lY[0] > -0.5) and (lY[0] < 1.5) then
    begin
      result := CClassColors[round(lY[0])];
    end
    else
      result := clWhite;
  end;

begin

  SetLength(lX, 2);
  SetLength(lY, 1); // пока жёстко зададим 2 класса
  HImage.Canvas.Lock;
  for Y := 0 to HImage.Height - 1 do
    for X := 0 to HImage.Width - 1 do
    begin
      lX[0] := X;
      lX[1] := Y;
      NeuroRegression(aNetwork, lX, lY);
      HImage.Canvas.Pixels[X, Y] := GetClassColor;
    end;
  HImage.Canvas.Unlock;

  MLPFree(aNetwork);
end;

function TPointsList.Options: TCalcNeuroOptions;
begin
  with result do
  begin
    lMaxIts := 100;
    lMaxStep := 0.001;
    lRestarts := 1000;
    lDecay := 0.001;
    lPoints := Count;
    LayerCount := 3;
    Layer1PtsCount := 2;
    Layer2PtsCount := 16;
    Layer3PtsCount := 8;
    OutPtsCount := 2;
    IsClassificator := true;
  end;
end;

end.
