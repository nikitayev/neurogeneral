unit mainform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Grids, Vcl.ValEdit, Vcl.ComCtrls, Vcl.ExtCtrls,
  Ap, mlpbase, calc_utils, u_uidataselection, Vcl.StdActns, System.Actions,
  Vcl.ActnList, serialization_utils;

type
  TMainFormExtrapolation = class(TForm)
    MainMenu: TMainMenu;
    NInsertData: TMenuItem;
    miINS: TMenuItem;
    NPreferences: TMenuItem;
    NHelp: TMenuItem;
    pcData: TPageControl;
    tsData: TTabSheet;
    tsPrediction: TTabSheet;
    tsPreferences: TTabSheet;
    vleNeuroPreferences: TValueListEditor;
    sgDataTeachingSrc: TStringGrid;
    sgDataPredictionSrc: TStringGrid;
    Splitter2: TSplitter;
    sgDataPredictionResult: TStringGrid;
    pmMainPopupMenu: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    ActionList: TActionList;
    INSFileOpen: TFileOpen;
    INSFileSaveAs: TFileSaveAs;
    N6: TMenuItem;
    N7: TMenuItem;
    pmSecondPopupMenu: TPopupMenu;
    nInsertCalculatedData: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    procedure NInsertDataClick(Sender: TObject);
    procedure NInsertAtPredictionClick(Sender: TObject);
    procedure NTeachingClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure NSolveClick(Sender: TObject);
    procedure sgDataTeachingSrcMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sgDataTeachingSrcMouseLeave(Sender: TObject);
    procedure sgDataTeachingSrcDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgDataTeachingSrcMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure N1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure INSFileOpenAccept(Sender: TObject);
    procedure INSFileSaveAsAccept(Sender: TObject);
    procedure N11Click(Sender: TObject);
  private
    { Private declarations }
    HNetwork: MultiLayerPerceptron;
    HOptions: TCalcNeuroOptions;
    HColSelected, HRowSelected: Integer;
    HDataSelection: TDataSelection;
    HCurrentDataSelectionType: TDataSelectionType;
    HLastSelectedFixedCol: Integer;
  public
    { Public declarations }
    procedure UpdateNeuroOptionsFromForm;
  end;

var
  MainFormExtrapolation: TMainFormExtrapolation;

implementation

uses Math, ClipBrd;

{$R *.dfm}

procedure TMainFormExtrapolation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MLPFree(HNetwork);
  FreeAndNil(HDataSelection);
end;

procedure TMainFormExtrapolation.FormShow(Sender: TObject);
begin
  pcData.TabIndex := 0;
  HDataSelection := TDataSelection.Create;
  HCurrentDataSelectionType := dstNone;
  HLastSelectedFixedCol := -1;
end;

procedure TMainFormExtrapolation.INSFileOpenAccept(Sender: TObject);
begin
  MultiLayerPerceptron_Unserialize(HNetwork, INSFileOpen.Dialog.FileName);
end;

procedure TMainFormExtrapolation.INSFileSaveAsAccept(Sender: TObject);
begin
  MultiLayerPerceptron_Serialize(HNetwork, INSFileSaveAs.Dialog.FileName);
end;

procedure TMainFormExtrapolation.N11Click(Sender: TObject);
begin
  if (HDataSelection.HasAllTypes) then
  begin
    MakeUnifySportsmenMatrix(sgDataTeachingSrc, sgDataPredictionSrc, HDataSelection);
    NSolveClick(Sender);
  end;
end;

procedure TMainFormExtrapolation.N1Click(Sender: TObject);
begin
  HCurrentDataSelectionType := dstResultCol;
end;

procedure TMainFormExtrapolation.N2Click(Sender: TObject);
begin
  HCurrentDataSelectionType := dstColToChange;
end;

procedure TMainFormExtrapolation.N3Click(Sender: TObject);
begin
  HCurrentDataSelectionType := dstFixedCols;
  HDataSelection.ClearByType(dstFixedCols);
end;

procedure TMainFormExtrapolation.N4Click(Sender: TObject);
begin
  HCurrentDataSelectionType := dstBaseRow;
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
    InsertDataFromClipboardToGridWithInfo(Clipboard.AsText, sgDataTeachingSrc);
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
  if Assigned(HNetwork.StructInfo) then
  begin
    SetLength(lX, sgDataPredictionSrc.ColCount);
    SetLength(lY, 1);
    sgDataPredictionResult.ColCount := 1;
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
end;

procedure TMainFormExtrapolation.NTeachingClick(Sender: TObject);
var
  zlXY: TReal2DArray;
begin
  try
    if (HDataSelection.HasAllTypes) then
    begin
      ConstructMatrixFromOneGrid(sgDataTeachingSrc, HDataSelection.GetValueOfType(dstResultCol), zlXY);
      UpdateNeuroOptionsFromForm;
      HOptions.lPoints := Length(zlXY);
      HOptions.Layer1PtsCount := sgDataTeachingSrc.ColCount - 2;
      HOptions.OutPtsCount := 1;
      MLPFree(HNetwork);
      CalcNeuroMatrix(HOptions, zlXY, HNetwork);
    end;

  finally
    SetLength(zlXY, 0);
  end;
end;

procedure TMainFormExtrapolation.sgDataTeachingSrcDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  zGrid: TStringGrid;
begin
  zGrid := Sender as TStringGrid;
  with zGrid.Canvas do
  begin
    Lock;
    Brush.Color := GetCellColor(HDataSelection.ValueType(ACol - 1, ARow - 1));
    FillRect(Rect);
    TextOut(Rect.Left + 3, Rect.Top + 5, zGrid.Cells[ACol, ARow]);
    Unlock;
  end;
end;

procedure TMainFormExtrapolation.sgDataTeachingSrcMouseLeave(Sender: TObject);
begin
  HColSelected := -1;
  HRowSelected := -1;
end;

procedure TMainFormExtrapolation.sgDataTeachingSrcMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  zGrid: TStringGrid;
begin
  if (HCurrentDataSelectionType <> dstNone) then
  begin
    zGrid := Sender as TStringGrid;
    zGrid.MouseToCell(X, Y, HColSelected, HRowSelected);
    case HCurrentDataSelectionType of
      dstResultCol, dstColToChange:
        HDataSelection.Add(HCurrentDataSelectionType, HColSelected - 1, HRowSelected - 1);
      dstFixedCols:
        begin
          if (HLastSelectedFixedCol <> -1) then
            HDataSelection.Delete(HLastSelectedFixedCol);
          if (HDataSelection.IndexOf(dstFixedCols, HColSelected - 1, HRowSelected - 1) = -1) then
            HLastSelectedFixedCol := HDataSelection.Add(HCurrentDataSelectionType, HColSelected - 1, HRowSelected - 1)
          else
            HLastSelectedFixedCol := -1;
        end
    else
      HDataSelection.Add(HCurrentDataSelectionType, HColSelected - 1, HRowSelected - 1);
    end;

    zGrid.Repaint;
  end;
end;

procedure TMainFormExtrapolation.sgDataTeachingSrcMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  zGrid: TStringGrid;
begin
  zGrid := Sender as TStringGrid;
  if (HCurrentDataSelectionType in [dstResultCol, dstBaseRow, dstColToChange]) then
  begin
    HCurrentDataSelectionType := dstNone;
    HLastSelectedFixedCol := -1;
  end
  else if (HCurrentDataSelectionType in [dstFixedCols]) then
  begin
    HLastSelectedFixedCol := -1;
    if (not(ssCtrl in Shift)) then
      HCurrentDataSelectionType := dstNone;
  end;
end;

procedure TMainFormExtrapolation.UpdateNeuroOptionsFromForm;
begin
  HOptions.lMaxIts := StrToIntDef(vleNeuroPreferences.Values['Максимум итераций'], 300);
  HOptions.lMaxStep := StrToFloatDef(vleNeuroPreferences.Values['Максимальный шаг'], 0.001);
  HOptions.lRestarts := StrToIntDef(vleNeuroPreferences.Values['Рестартов'], 1000);
  HOptions.lDecay := StrToFloatDef(vleNeuroPreferences.Values['Максимальная ошибка'], 0.001);
  HOptions.lPoints := 0;
  HOptions.LayerCount := StrToIntDef(vleNeuroPreferences.Values['Слоёв'], 3);
  HOptions.Layer1PtsCount := 0;
  HOptions.Layer2PtsCount := StrToIntDef(vleNeuroPreferences.Values['Нейронов во втором слое'], 8);
  HOptions.Layer3PtsCount := StrToIntDef(vleNeuroPreferences.Values['Нейронов во третьем слое'], 8);
  HOptions.OutPtsCount := 0;
  HOptions.IsClassificator := false;
end;

end.
