unit U_LoadCSV;

interface

uses Classes, SysUtils;

type
  TCSV_Table = array of array of string;

function LoadFromCSVFile(const aFile: String): TCSV_Table;
function LoadFromCSVFiles(const aFiles: TStrings): TCSV_Table;

implementation

function LoadFromCSVFile(const aFile: String): TCSV_Table;
var
  zSL: TStringList;
begin
  zSL := TStringList.Create;
  try
    zSL.Add(aFile);
    Result := LoadFromCSVFiles(zSL);
  finally
    FreeAndNil(zSL);
  end;
end;

function LoadFromCSVFiles(const aFiles: TStrings): TCSV_Table;
var
  i, j, k: Integer;
  zFileIdx: Integer;
  zMatrixPos: Integer;
  flag: boolean;
  zSL, zTmpSL: TStringList;
  ztmps: string;
begin
  SetLength(Result, 0);
  zMatrixPos := 0;
  for zFileIdx := 0 to aFiles.Count - 1 do
    if (FileExists(aFiles[zFileIdx])) then
      try
        zSL := TStringList.Create;
        zTmpSL := TStringList.Create;
        zSL.LoadFromFile(aFiles[zFileIdx]);
        SetLength(Result, Length(Result) + zSL.Count);
        for i := 0 to zSL.Count - 1 do
        begin
          ztmps := zSL[i];
          j := 1;
          zTmpSL.Clear;
          while (j < Length(ztmps)) do
          begin
            k := 1;
            flag := ztmps[j] = '"';
            while (j + k < Length(ztmps)) and ((ztmps[j + k] <> ',') or flag) do
            begin
              if (flag) and (ztmps[j + k] = '"') then
                flag := false;
              inc(k);
            end;
            zTmpSL.Add(copy(ztmps, j, k));
            inc(j, k + 1);
          end;
          SetLength(Result[zMatrixPos + i], zTmpSL.Count);
          for j := 0 to zTmpSL.Count - 1 do
          begin
            Result[zMatrixPos + i, j] := zTmpSL[j];
            if (Result[zMatrixPos + i, j] <> '') and (Result[zMatrixPos + i, j][1] = '"') then
              Result[zMatrixPos + i, j] := copy(Result[zMatrixPos + i, j], 2, Length(Result[zMatrixPos + i, j]) - 2);
          end;
        end;
        inc(zMatrixPos, zSL.Count);
      finally
        FreeAndNil(zTmpSL);
        FreeAndNil(zSL);
      end;
end;

end.
