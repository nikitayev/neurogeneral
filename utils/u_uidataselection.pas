unit u_uidataselection;

interface

uses System.Generics.Collections, Graphics;

type
  { dstResultCol - результирующий столбец с зависимыми переменными
    dstBaseRow - базовая строка, откуда будем брать изменяемые переменные и подставлять в другие после обучения
    dstColToChange - изменяемая независимая переменная для построения графика зависимой переменной
    dstFixedCols - фиксированные столбцы, значения в которых не будут изменяться
  }
  TDataSelectionType = (dstNone, dstResultCol, dstBaseRow, dstColToChange, dstFixedCols);
  TSetDataSelectionType = set of TDataSelectionType;

var
  CDataSelectionTypeColors: array [TDataSelectionType] of TColor = (
    clWindow,
    clSkyBlue,
    clRed,
    clLime,
    clMoneyGreen
  );

type

  // один элемент массива
  TDataSelectionItem = record
    DataSelectionType: TDataSelectionType;
    X, Y: Integer;
  end;

  TDataSelection = class(TList<TDataSelectionItem>)
  public
    function Add(aDataSelectionType: TDataSelectionType; X, Y: Integer): Integer;
    function IndexOf(aDataSelectionType: TDataSelectionType; X, Y: Integer): Integer;
    function HasType(aDataSelectionType: TDataSelectionType): boolean;
    function HasAllTypes: boolean;
    procedure ClearByType(aDataSelectionType: TDataSelectionType);
    function ValueType(X, Y: Integer): TSetDataSelectionType;
    function GetValueOfType(aDataSelectionType: TDataSelectionType): Integer;
  end;

function AsDataSelectionItem(aDataSelectionType: TDataSelectionType; X, Y: Integer): TDataSelectionItem;
function GetCellColor(const aValueType: TSetDataSelectionType): TColor;

implementation

function AsDataSelectionItem(aDataSelectionType: TDataSelectionType; X, Y: Integer): TDataSelectionItem;
begin
  result.DataSelectionType := aDataSelectionType;
  result.X := X;
  result.Y := Y;
end;

function CountOfSet(const aValueType: TSetDataSelectionType): Integer;
var
  M: TDataSelectionType;
begin
  result := 0;
  for M in aValueType do
    Inc(result);
end;

function GetFirstElementFromSet(const aValueType: TSetDataSelectionType): TDataSelectionType;
var
  M: TDataSelectionType;
begin
  result := dstNone;
  for M in aValueType do
  begin
    result := M;
    break;
  end;
end;

function GetCellColor(const aValueType: TSetDataSelectionType): TColor;
begin
  if (aValueType = []) then
    result := clWindow
  else if (CountOfSet(aValueType) = 1) then
    result := CDataSelectionTypeColors[GetFirstElementFromSet(aValueType)]
  else
    result := clSilver;
end;

{ TDataSelection }

function TDataSelection.Add(aDataSelectionType: TDataSelectionType; X, Y: Integer): Integer;
begin
  case aDataSelectionType of
    dstResultCol, dstBaseRow, dstColToChange:
      ClearByType(aDataSelectionType);
  end;
  result := IndexOf(aDataSelectionType, X, Y);
  if (result = -1) then
    result := inherited Add(AsDataSelectionItem(aDataSelectionType, X, Y));
end;

procedure TDataSelection.ClearByType(aDataSelectionType: TDataSelectionType);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if (Items[i].DataSelectionType = aDataSelectionType) then
      Delete(i);
end;

function TDataSelection.GetValueOfType(aDataSelectionType: TDataSelectionType): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].DataSelectionType = aDataSelectionType) then
    begin
      case aDataSelectionType of
        dstResultCol, dstColToChange, dstFixedCols:
          result := Items[i].X;
      else
        result := Items[i].Y;
      end;
      break;
    end;
end;

function TDataSelection.HasAllTypes: boolean;
var
  i: Integer;
  M: TSetDataSelectionType;
begin
  M := [];
  for i := 0 to Count - 1 do
    M := M + [Items[i].DataSelectionType];
  result := (dstResultCol in M) and (dstBaseRow in M) and (dstColToChange in M) and (dstFixedCols in M);
end;

function TDataSelection.HasType(aDataSelectionType: TDataSelectionType): boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to Count - 1 do
    if (Items[i].DataSelectionType = aDataSelectionType) then
    begin
      result := true;
      break;
    end;
end;

function TDataSelection.IndexOf(aDataSelectionType: TDataSelectionType; X, Y: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].DataSelectionType = aDataSelectionType) then
      case aDataSelectionType of
        dstResultCol, dstColToChange, dstFixedCols:
          begin
            if (Items[i].X = X) then
            begin
              result := i;
              break;
            end;
          end;
      else
        if (Items[i].Y = Y) then
        begin
          result := i;
          break;
        end;
      end;
end;

function TDataSelection.ValueType(X, Y: Integer): TSetDataSelectionType;
var
  i: Integer;
begin
  result := [];
  for i := 0 to Count - 1 do
    case Items[i].DataSelectionType of
      dstResultCol, dstColToChange, dstFixedCols:
        begin
          if (Items[i].X = X) then
          begin
            result := result + [Items[i].DataSelectionType];
          end;
        end;
    else
      if (Items[i].Y = Y) then
      begin
        result := result + [Items[i].DataSelectionType];
      end;
    end;
end;

end.
