unit mainform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Grids, Vcl.ValEdit,
  Vcl.ComCtrls, Vcl.ExtCtrls,
  Ap, mlpbase, calc_utils, clipbrd;

type
  TMainFormExtrapolation = class(TForm)
    MainMenu: TMainMenu;
    NInsertData: TMenuItem;
    NInsertResults: TMenuItem;
    NTeaching: TMenuItem;
    NSolve: TMenuItem;
    NPreferences: TMenuItem;
    NHelp: TMenuItem;
    pcData: TPageControl;
    tsData: TTabSheet;
    tsPrediction: TTabSheet;
    NInsertAtPrediction: TMenuItem;
    tsPreferences: TTabSheet;
    vleNeuroPreferences: TValueListEditor;
    sgDataTeachingSrc: TStringGrid;
    Splitter1: TSplitter;
    sgDataTeachingResult: TStringGrid;
    sgDataPredictionSrc: TStringGrid;
    Splitter2: TSplitter;
    sgDataPredictionResult: TStringGrid;
    PopupMenuResult: TPopupMenu;
    NCopyResults: TMenuItem;
    NTeachingStart: TMenuItem;
    NTeachingSaveTo: TMenuItem;
    NTeachingLoadFrom: TMenuItem;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    procedure NInsertDataClick(Sender: TObject);
    procedure NInsertResultsClick(Sender: TObject);
    procedure NInsertAtPredictionClick(Sender: TObject);
    procedure NTeachingClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure NSolveClick(Sender: TObject);
    procedure NCopyResultsClick(Sender: TObject);
    procedure NTeachingSaveToClick(Sender: TObject);
    procedure NTeachingLoadFromClick(Sender: TObject);
  private
    { Private declarations }
    HNetwork: MultiLayerPerceptron;
    HOptions: TCalcNeuroOptions;
  public
    { Public declarations }
    procedure UpdateNeuroOptionsFromForm;
  end;

var
  MainFormExtrapolation: TMainFormExtrapolation;

implementation

uses Math;

{$R *.dfm}

procedure TMainFormExtrapolation.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MLPFree(HNetwork);
end;

procedure TMainFormExtrapolation.FormShow(Sender: TObject);
begin
  pcData.TabIndex := 0;
end;

procedure TMainFormExtrapolation.NCopyResultsClick(Sender: TObject);
var
  i, j: Integer;
  zSL: TStringList;
  ztmps: string;
begin
  zSL := TStringList.Create;
  for i := 0 to sgDataPredictionSrc.RowCount - 1 do
  begin
    ztmps := '';
    for j := 0 to sgDataPredictionSrc.ColCount - 1 do
      ztmps := ztmps + sgDataPredictionSrc.Cells[j, i] + #9;
    for j := 0 to sgDataPredictionSrc.ColCount - 2 do
      ztmps := ztmps + sgDataPredictionResult.Cells[j, i] + #9;
    ztmps := ztmps + sgDataPredictionResult.Cells
      [sgDataPredictionSrc.ColCount - 1, i];
    zSL.Add(ztmps)
  end;

  Clipboard.Open;
  Clipboard.Clear;
  Clipboard.AsText := zSL.Text;
  Clipboard.Close;

  FreeAndNil(zSL);
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
    InsertDataFromClipboardToGrid(Clipboard.AsText, sgDataTeachingSrc);
  finally
    Clipboard.Close;
  end;
end;

procedure TMainFormExtrapolation.NInsertResultsClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    InsertDataFromClipboardToGrid(Clipboard.AsText, sgDataTeachingResult);
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
        sgDataPredictionResult.Cells[col, row] := FloatToStr(lY[col]);
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
  Screen.Cursor := crHourGlass;
  try
    ConstructMatrixFromGrid(sgDataTeachingSrc, sgDataTeachingResult, zlXY);
    UpdateNeuroOptionsFromForm;
    HOptions.lPoints := Length(zlXY);
    HOptions.Layer1PtsCount := sgDataTeachingSrc.ColCount;
    HOptions.OutPtsCount := sgDataTeachingResult.ColCount;
    CalcNeuroMatrix(HOptions, zlXY, HNetwork);

  finally
    SetLength(zlXY, 0);
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainFormExtrapolation.NTeachingLoadFromClick(Sender: TObject);
begin
  if (SaveDialog.Execute(Self.Handle)) then
    MLPUnserialize(SaveDialog.FileName, HNetwork);
end;

procedure TMainFormExtrapolation.NTeachingSaveToClick(Sender: TObject);
begin
  if (SaveDialog.Execute(Self.Handle)) then
    MLPSerialize(HNetwork, SaveDialog.FileName);
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
  HOptions.IsClassificator := false;
end;

end.
