unit mainform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Grids, Vcl.ValEdit,
  Vcl.ComCtrls, Vcl.ExtCtrls,
  Ap, mlpbase, calc_utils, U_hrv_neurontrain_MT, System.Actions, Vcl.ActnList,
  Vcl.StdActns, U_LoadCSV, UITypes;

type
  TMainFormExtrapolation = class(TForm)
    MainMenu: TMainMenu;
    NInsertData: TMenuItem;
    NTeaching: TMenuItem;
    NSolve: TMenuItem;
    NPreferences: TMenuItem;
    NHelp: TMenuItem;
    NInsertAtPrediction: TMenuItem;
    pcData: TPageControl;
    tsData: TTabSheet;
    sgDataTeachingSrc: TStringGrid;
    tsPrediction: TTabSheet;
    Splitter2: TSplitter;
    sgDataPredictionSrc: TStringGrid;
    sgDataPredictionResult: TStringGrid;
    tsPreferences: TTabSheet;
    vleNeuroPreferences: TValueListEditor;
    Splitter1: TSplitter;
    sgDataTeachingResult: TStringGrid;
    CSV1: TMenuItem;
    ActionList: TActionList;
    FileOpenCSV: TFileOpen;
    NInsertFromClipboard: TMenuItem;
    procedure NInsertDataClick(Sender: TObject);
    procedure NInsertAtPredictionClick(Sender: TObject);
    procedure NTeachingClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure NSolveClick(Sender: TObject);
    procedure FileOpenCSVAccept(Sender: TObject);
  private
    { Private declarations }
    HNetwork: MultiLayerPerceptron;
    HOptions: TCalcNeuroOptions;
    HClassCount: Integer;
    procedure DrawNetworkResult1(aNetwork: MultiLayerPerceptron);
  public
    { Public declarations }
    procedure UpdateNeuroOptionsFromForm;
  end;

var
  MainFormExtrapolation: TMainFormExtrapolation;

implementation

uses Math, ClipBrd;

{$R *.dfm}

procedure TMainFormExtrapolation.DrawNetworkResult1
  (aNetwork: MultiLayerPerceptron);
begin

end;

procedure TMainFormExtrapolation.FileOpenCSVAccept(Sender: TObject);
var
  i, j: Integer;
  zRow0: Integer;
  zTable: TCSV_Table;
begin
  zTable := LoadFromCSVFile(FileOpenCSV.Dialog.FileName);
  try
    // InsertDataFromClipboardToGridClassificator(Clipboard.AsText, HClassCount, sgDataTeachingSrc, sgDataTeachingResult);
    zRow0 := sgDataTeachingSrc.RowCount;
    sgDataTeachingSrc.Canvas.Lock;
    sgDataTeachingResult.Canvas.Lock;
    sgDataTeachingSrc.RowCount := sgDataTeachingSrc.RowCount + Length(zTable);
    sgDataTeachingSrc.ColCount := max(sgDataTeachingSrc.ColCount,
      Length(zTable[0]));
    sgDataTeachingResult.RowCount := sgDataTeachingSrc.RowCount;
    for i := 0 to Length(zTable) - 1 do
      for j := 0 to Length(zTable[0]) - 1 do
        sgDataTeachingSrc.Cells[j, zRow0 + i] := zTable[i, j];
    for i := 0 to Length(zTable) - 1 do
      sgDataTeachingResult.Cells[0, zRow0 + i] := IntToStr(HClassCount);
    Inc(HClassCount);

  finally
    sgDataTeachingSrc.Canvas.Unlock;
    sgDataTeachingResult.Canvas.Unlock;
    SetLength(zTable, 0);
  end;
end;

procedure TMainFormExtrapolation.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MLPFree(HNetwork);
end;

procedure TMainFormExtrapolation.FormShow(Sender: TObject);
begin
  HClassCount := 0;
  pcData.TabIndex := 0;
  sgDataTeachingSrc.FixedRows := 0;
  sgDataTeachingResult.FixedRows := 0;
  sgDataTeachingSrc.ColCount := 1;
  sgDataTeachingSrc.RowCount := 0;
  sgDataTeachingResult.ColCount := 1;
  sgDataTeachingResult.RowCount := 0;
end;

procedure TMainFormExtrapolation.NInsertAtPredictionClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    InsertDataFromClipboardToGrid(Clipboard.AsText, sgDataPredictionSrc);
  finally
    Clipboard.Close;
  end;
end;

procedure TMainFormExtrapolation.NInsertDataClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    InsertDataFromClipboardToGridClassificator(Clipboard.AsText, HClassCount,
      sgDataTeachingSrc, sgDataTeachingResult);
    Inc(HClassCount);
  finally
    Clipboard.Close;
  end;
end;

procedure TMainFormExtrapolation.NSolveClick(Sender: TObject);
var
  row, col: Integer;
  lX: TReal1DArray;
  lY: TReal1DArray;
begin
  SetLength(lX, sgDataPredictionSrc.ColCount);
  SetLength(lY, sgDataTeachingResult.ColCount);
  sgDataPredictionResult.ColCount := sgDataTeachingResult.ColCount;
  sgDataPredictionResult.RowCount := sgDataPredictionSrc.RowCount;
  try
    for row := 0 to sgDataPredictionSrc.RowCount - 1 do
    begin
      for col := 0 to sgDataPredictionSrc.ColCount - 1 do
        lX[col] := StrToFloatEx(sgDataPredictionSrc.Cells[col, row]);
      NeuroRegression(HNetwork, lX, lY);
      for col := 0 to sgDataPredictionResult.ColCount - 1 do
        sgDataPredictionResult.Cells[col, row] :=
          FloatToStr(round(lY[col] * 10000) * 0.0001);
    end;
  finally
    SetLength(lX, 0);
    SetLength(lY, 0);
  end;
end;

procedure TMainFormExtrapolation.NTeachingClick(Sender: TObject);
var
  zlXY: TReal2DArray;
begin
  if (HClassCount < 2) then
  begin
    MessageDlg('Должно быть добавлено минимум 2 класса!', mtConfirmation,
      [mbOk], 0);
    exit;
  end;
  Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    ConstructMatrixFromGrid(sgDataTeachingSrc, sgDataTeachingResult, zlXY);
    UpdateNeuroOptionsFromForm;
    HOptions.lPoints := Length(zlXY);
    HOptions.Layer1PtsCount := sgDataTeachingSrc.ColCount;
    HOptions.OutPtsCount := HClassCount;
    sgDataTeachingResult.ColCount := HClassCount;
    // CalcNeuroMatrix(HOptions, zlXY, HNetwork);
    StartTrainLBFS_MT(ExtractFilePath(ParamStr(0)) + 'classificator.nmat', zlXY,
      HOptions, DrawNetworkResult1);
    sgDataPredictionResult.ColCount := HClassCount;
  finally
    SetLength(zlXY, 0);
    Beep;
    Cursor := crDefault;
  end;
end;

procedure TMainFormExtrapolation.UpdateNeuroOptionsFromForm;
begin
  HOptions.lMaxIts := StrToIntDef(vleNeuroPreferences.Values
    ['Максимум итераций'], 30);
  HOptions.lMaxStep := StrToFloatEx(vleNeuroPreferences.Values
    ['Максимальный шаг']);
  HOptions.lRestarts := StrToIntDef(vleNeuroPreferences.Values
    ['Рестартов'], 1000);
  HOptions.lDecay := StrToFloatEx(vleNeuroPreferences.Values
    ['Максимальная ошибка']);
  HOptions.lPoints := 0;
  HOptions.LayerCount := StrToIntDef(vleNeuroPreferences.Values['Слоёв'], 3);
  HOptions.Layer1PtsCount := 0;
  HOptions.Layer2PtsCount :=
    StrToIntDef(vleNeuroPreferences.Values['Нейронов во втором слое'], 60);
  HOptions.Layer3PtsCount :=
    StrToIntDef(vleNeuroPreferences.Values['Нейронов во третьем слое'], 10);
  HOptions.OutPtsCount := 0;
  HOptions.IsClassificator := true;
end;

end.
