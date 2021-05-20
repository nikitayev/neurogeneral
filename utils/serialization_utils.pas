unit serialization_utils;
{

  »спользованы материалы:

  http://docwiki.embarcadero.com/CodeExamples/XE7/en/TXMLDocument_use_case_%28Delphi%29
  http://compress.ru/article.aspx?id=12504
  http://www.programmersforum.ru/showthread.php?t=106778

  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Classes_TStream_WriteComponent.html
  http://rsdn.ru/article/delphi/serialization.xml
  http://delphi-kb.blogspot.ru/2010/03/how-to-save-tcollectionitem-from.html
  http://stackoverflow.com/questions/8863019/serialization-of-a-tcollection-which-is-not-declared-in-a-tcomponent

   лассы должны быть зарегистрированы:
  RegisterClass(TQRSTClassInfo);
}

interface

uses SysUtils, Classes, Ap, mlpbase;

function SaveCollectionToFile(aCollection: TCollection; const aFileName: string): boolean;
function SaveComponentToFile(aComponent: TComponent; const aFileName: string): boolean;
// function QRSTClassInfoList_Serialize(aList: TQRSTClassInfoList; const aFileName: string): boolean;
// function QRSTClassInfoList_Unserialize(aList: TQRSTClassInfoList; const aFileName: string): boolean;
// function QRSTVectorInfoList_Serialize(aList: TQRSTVectorInfoList; const aFileName: string): boolean;
// function QRSTVectorInfoList_Unserialize(aList: TQRSTVectorInfoList; const aFileName: string;
// aClasses: TQRSTClassInfoList; const aProgramSettings: TProgramSettings): boolean;
function VectorSerialize(aVector: TReal1DArray): string;
procedure VectorUnserialize(const aValue: string; var aVector: TReal1DArray); overload;
procedure VectorUnserialize(const aValue: string; var aVector: PDouble; out aBlockSize: Integer); overload;

function MultiLayerPerceptron_Serialize(const Network: MultiLayerPerceptron; const aFileName: string): boolean;
function MultiLayerPerceptron_Unserialize(var Network: MultiLayerPerceptron; const aFileName: string): boolean;

// function Vector_Unserialize(const aFileName: string; var aNetworkVector: PDouble; out aDoubleCount: Int32): boolean;

// function UserInfo_Serialize(const aUserInfo: TUserInfo; out aUserInfoXML: unicodestring): boolean;
// function UserInfo_Unserialize(const aUserInfoXML: string; out aUserInfo: TUserInfo): boolean;

function CreateGuid: string;

// дл€ MSSQL следует использовать System.DateUtils.ISO8601ToDate и TryISO8601ToDate
function DateToStrOpeka(const aDateTme: TDate): string;
function OpekaMSecGMT2GMT(const aValue: Int64): TDateTime;

implementation

uses XMLIntf, XMLDoc, ComObj, ActiveX, DateUtils;

function CreateGuid: string;
var
  ID: TGUID;
begin
  Result := '';
  if CoCreateGuid(ID) = S_OK then
    Result := GUIDToString(ID);
end;

function DateToStrOpeka(const aDateTme: TDate): string;
var
  zDateSeparator: char;
begin
  zDateSeparator := FormatSettings.DateSeparator;
  FormatSettings.DateSeparator := '/';
  Result := FormatDateTime('yyyy/mm/dd', aDateTme);
  FormatSettings.DateSeparator := zDateSeparator;
end;

function OpekaMSecGMT2GMT(const aValue: Int64): TDateTime;
begin
  Result := EncodeDate(1970, 1, 1) + aValue * (1.0 / MSecsPerDay);
end;

function ComponentToStringProc(Component: TComponent): string;
var
  BinStream: TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result := StrStream.DataString;
    finally
      FreeAndNil(StrStream);
    end;
  finally
    FreeAndNil(BinStream);
  end;
end;

function StringToComponentProc(const Value: string): TComponent;
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(nil);
    finally
      FreeAndNil(BinStream);
    end;
  finally
    FreeAndNil(StrStream);
  end;
end;

function SaveCollectionToFile(aCollection: TCollection; const aFileName: string): boolean;
var
  fs: TFileStream;
begin
  Result := false;
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    with TWriter.Create(fs, 1024) do
    begin
      WriteCollection(aCollection);
      Free;
    end;
    Result := true;
  finally
    FreeAndNil(fs);
  end;
end;

function SaveComponentToFile(aComponent: TComponent; const aFileName: string): boolean;
var
  ss: TStringList;
begin
  Result := false;
  ss := TStringList.Create;
  try
    ss.Text := ComponentToStringProc(aComponent);
    ss.SaveToFile(aFileName);
    Result := true;
  finally
    FreeAndNil(ss);
  end;
end;

{ function QRSTClassInfoList_Serialize(aList: TQRSTClassInfoList; const aFileName: string): boolean;
  var
  i: Integer;
  LDocument: IXMLDocument;
  LNodeElement: IXMLNode;
  begin
  Result := false;
  LDocument := TXMLDocument.Create(nil);
  try
  LDocument.Active := true;
  with LDocument do
  begin
  Active := true;
  Version := '1.0';
  Encoding := 'Windows-1251';
  Options := Options + [doNodeAutoIndent];
  end;
  LDocument.DocumentElement := LDocument.CreateNode('QRSTClassInfoList', ntElement, '');
  LDocument.DocumentElement.Attributes['version'] := '1';
  LDocument.DocumentElement.Attributes['date'] := DateToStr(Now);
  LDocument.DocumentElement.Attributes['contact'] := 'testkarpov@mail.ru';

  for i := 0 to aList.Count - 1 do
  begin
  LNodeElement := LDocument.DocumentElement.AddChild('QRSTClassInfo', -1);
  LNodeElement.Attributes['classidx'] := aList[i].classidx;
  LNodeElement.Attributes['color'] := IntToHex(aList[i].color, 8);
  LNodeElement.Attributes['class_name'] := aList[i].class_name;
  LNodeElement.Attributes['description'] := aList[i].description;
  LNodeElement.Attributes['min'] := FloatToStr(aList[i].Min);
  LNodeElement.Attributes['max'] := FloatToStr(aList[i].Max);
  end;

  Result := true;
  finally
  LDocument.SaveToFile(aFileName);
  LDocument.Active := false;
  LDocument := nil;
  end;
  end; }

{
  function QRSTClassInfoList_Unserialize(aList: TQRSTClassInfoList; const aFileName: string): boolean;
  var
  i: Integer;
  LDocument: IXMLDocument;
  LNodeElement: IXMLNode;
  QRSTClassInfo: TQRSTClassInfo;
  begin
  Result := false;
  OleInitialize(nil);
  LDocument := TXMLDocument.Create(nil);
  try
  LDocument.LoadFromFile(aFileName);
  LDocument.Active := true;
  if (LDocument.DocumentElement.NodeName <> 'QRSTClassInfoList') then
  exit;

  aList.Clear;
  for i := 0 to LDocument.DocumentElement.ChildNodes.Count - 1 do
  begin
  QRSTClassInfo := TQRSTClassInfo.Create;
  LNodeElement := LDocument.DocumentElement.ChildNodes[i];
  if (LNodeElement.NodeName = 'QRSTClassInfo') then
  begin
  QRSTClassInfo.classidx := StrToInt(LNodeElement.Attributes['classidx']);
  QRSTClassInfo.color := StrToInt('$' + LNodeElement.Attributes['color']);
  QRSTClassInfo.class_name := LNodeElement.Attributes['class_name'];
  QRSTClassInfo.description := LNodeElement.Attributes['description'];
  QRSTClassInfo.Min := StrToFloat(LNodeElement.Attributes['min']);
  QRSTClassInfo.Max := StrToFloat(LNodeElement.Attributes['max']);
  end;
  aList.Add(QRSTClassInfo, false);
  end;
  aList.SortByIndex;

  Result := true;
  finally
  LDocument.Active := false;
  LDocument := nil;
  end;
  end; }

{
  function QRSTVectorInfoList_Serialize(aList: TQRSTVectorInfoList; const aFileName: string): boolean;
  var
  i: Integer;
  LDocument: IXMLDocument;
  LNodeElement: IXMLNode;
  begin
  Result := false;
  LDocument := TXMLDocument.Create(nil);
  try
  LDocument.Active := true;
  with LDocument do
  begin
  Active := true;
  Version := '1.0';
  Encoding := 'Windows-1251';
  Options := Options + [doNodeAutoIndent];
  end;
  LDocument.DocumentElement := LDocument.CreateNode('QRSTVectorInfoList', ntElement, '');
  LDocument.DocumentElement.Attributes['version'] := '1';
  LDocument.DocumentElement.Attributes['date'] := DateToStr(Now);
  LDocument.DocumentElement.Attributes['contact'] := 'testkarpov@mail.ru';

  for i := 0 to aList.Count - 1 do
  begin
  LNodeElement := LDocument.DocumentElement.AddChild('QRSTVectorInfo', -1);
  LNodeElement.Attributes['classidx'] := aList[i].ClassIdx;
  LNodeElement.Attributes['classcolor'] := aList[i].ClassColor;
  LNodeElement.Attributes['classuniqueidx'] := aList[i].ClassUniqueIdx;
  LNodeElement.Attributes['vector'] := aList[i].VectorAsText;
  LNodeElement.Attributes['vectorname'] := aList[i].VectorName;
  LNodeElement.Attributes['vectorinfo'] := aList[i].VectorInfo;
  LNodeElement.Attributes['vectoradddate'] := DateTimeToStr(aList[i].VectorAddDate);
  LNodeElement.Attributes['guid'] := aList[i].GUID;
  end;

  LDocument.SaveToFile(aFileName);
  Result := true;
  finally
  LDocument.Active := false;
  LDocument := nil;
  end;
  end; }

{
  function QRSTVectorInfoList_Unserialize(aList: TQRSTVectorInfoList; const aFileName: string;
  aClasses: TQRSTClassInfoList; const aProgramSettings: TProgramSettings): boolean;
  var
  i: Integer;
  LDocument: IXMLDocument;
  LNodeElement: IXMLNode;
  QRSTVectorInfo: TQRSTVectorInfo;
  begin
  Result := false;
  LDocument := TXMLDocument.Create(nil);
  try
  LDocument.LoadFromFile(aFileName);
  LDocument.Active := true;
  if (LDocument.DocumentElement.NodeName <> 'QRSTVectorInfoList') then
  exit;

  aList.Clear;
  for i := 0 to LDocument.DocumentElement.ChildNodes.Count - 1 do
  begin
  QRSTVectorInfo := TQRSTVectorInfo.Create(GProgramSettings.VectorLength);
  LNodeElement := LDocument.DocumentElement.ChildNodes[i];
  if (LNodeElement.NodeName = 'QRSTVectorInfo') then
  begin
  QRSTVectorInfo.ClassIdx := StrToInt(LNodeElement.Attributes['classidx']);
  QRSTVectorInfo.ClassColor := StrToInt(LNodeElement.Attributes['classcolor']);
  QRSTVectorInfo.ClassUniqueIdx := StrToInt(LNodeElement.Attributes['classuniqueidx']);
  QRSTVectorInfo.VectorAsText := LNodeElement.Attributes['vector'];
  QRSTVectorInfo.VectorName := LNodeElement.Attributes['vectorname'];
  QRSTVectorInfo.VectorInfo := LNodeElement.Attributes['vectorinfo'];
  QRSTVectorInfo.VectorAddDate := StrToDateTimeDef(LNodeElement.Attributes['vectoradddate'], Now);
  QRSTVectorInfo.GUID := LNodeElement.Attributes['guid'];
  if (QRSTVectorInfo.GUID = '') then
  QRSTVectorInfo.GUID := CreateGuid;
  end;
  aList.Add(QRSTVectorInfo, aClasses, aProgramSettings);
  end;

  Result := true;
  finally
  LDocument.Active := false;
  LDocument := nil;
  end;
  end; }

function VectorSerialize(aVector: TReal1DArray): string;
var
  i: Integer;
  tmpds: char;
begin
  tmpds := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    Result := '';
    for i := Low(aVector) to High(aVector) - 1 do
      Result := Result + FloatToStr(aVector[i]) + ';';
    Result := Result + FloatToStr(aVector[High(aVector)]);
  finally
    FormatSettings.DecimalSeparator := tmpds;
  end;
end;

procedure VectorUnserialize(const aValue: string; var aVector: TReal1DArray);
var
  i: Integer;
  SL: TStringList;
  tmpds: char;
begin
  tmpds := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    SL := TStringList.Create;
    SL.Delimiter := ';';
    SL.DelimitedText := aValue;
    SetLength(aVector, SL.Count);
    for i := 0 to SL.Count - 1 do
      aVector[i] := StrToFloat(SL[i]);
  finally
    FormatSettings.DecimalSeparator := tmpds;
    FreeAndNil(SL);
  end;
end;

procedure VectorUnserialize(const aValue: string; var aVector: PDouble; out aBlockSize: Int32); overload;
var
  i: Integer;
  SL: TStringList;
  tmpds: char;
  zValues: PDouble;
begin
  tmpds := FormatSettings.DecimalSeparator;
  try
    try
      FormatSettings.DecimalSeparator := '.';
      SL := TStringList.Create;
      ExtractStrings([';'], [], PWideChar(aValue), SL);
      GetMem(aVector, SL.Count * SizeOf(double) * 2);
      zValues := aVector;
      for i := 0 to SL.Count - 1 do
      begin
        zValues^ := StrToFloat(SL[i]);
        // SL[i] := '';
        inc(zValues);
      end;
      aBlockSize := SL.Count;
    finally
      FormatSettings.DecimalSeparator := tmpds;
      FreeAndNil(SL);
    end;
  except
    on E: Exception do;
  end;
end;

function MultiLayerPerceptron_Serialize(const Network: MultiLayerPerceptron; const aFileName: string): boolean;
var
  ss: TStringStream;
  RA: TReal1DArray;
  RLen: Integer;
begin
  Result := false;
  ss := TStringStream.Create;
  try
    MLPSerialize(Network, RA, RLen);
    ss.WriteString(VectorSerialize(RA));
    SetLength(RA, 0);
    ss.Position := 0;
    ss.SaveToFile(aFileName);
    Result := true;
  finally
    FreeAndNil(ss);
  end;
end;

function MultiLayerPerceptron_Unserialize(var Network: MultiLayerPerceptron; const aFileName: string): boolean;
var
  ss: TStringStream;
  RA: TReal1DArray;
begin
  Result := false;
  ss := TStringStream.Create;
  try
    ss.LoadFromFile(aFileName);
    ss.Position := 0;
    VectorUnserialize(ss.DataString, RA);
    MLPUnserialize(RA, Network);
    SetLength(RA, 0);
    Result := true;
  finally
    FreeAndNil(ss);
  end;
end;

{
  function Vector_Unserialize(const aFileName: string; var aNetworkVector: PDouble; out aDoubleCount: Int32)
  : boolean; overload;
  var
  ss: TStringStream;
  begin
  Result := false;
  ss := TStringStream.Create;
  try
  try
  ss.LoadFromFile(aFileName);
  ss.Position := 0;
  VectorUnserialize(ss.DataString, aNetworkVector, aDoubleCount);
  Result := true;
  finally
  FreeAndNil(ss);
  end;
  except
  on E: Exception do;
  end;
  end;
}

{
  function UserInfo_Serialize(const aUserInfo: TUserInfo; out aUserInfoXML: unicodestring): boolean;
  var
  LDocument: IXMLDocument;
  LNodeElement: IXMLNode;
  begin
  Result := false;
  LDocument := TXMLDocument.Create(nil);
  try
  LDocument.Active := true;
  with LDocument do
  begin
  Active := true;
  Version := '1.0';
  Encoding := 'Windows-1251';
  Options := Options + [doNodeAutoIndent];
  end;
  LDocument.DocumentElement := LDocument.CreateNode('UserInfo', ntElement, '');
  LDocument.DocumentElement.Attributes['version'] := '1';

  LNodeElement := LDocument.DocumentElement.AddChild('UserInfo', -1);
  LNodeElement.Attributes['family'] := aUserInfo.family;
  LNodeElement.Attributes['name'] := aUserInfo.name;
  LNodeElement.Attributes['surname'] := aUserInfo.surname;
  LNodeElement.Attributes['weight'] := aUserInfo.weight;
  LNodeElement.Attributes['height'] := aUserInfo.height;
  LNodeElement.Attributes['email1'] := aUserInfo.email1;
  LNodeElement.Attributes['email2'] := aUserInfo.email2;
  LNodeElement.Attributes['phone1'] := aUserInfo.phone1;
  LNodeElement.Attributes['phone2'] := aUserInfo.phone2;
  LNodeElement.Attributes['birthdate'] := DateToStrOpeka(aUserInfo.birthdate);
  LNodeElement.Attributes['passwordhash'] := IntToStr(aUserInfo.passwordhash);

  aUserInfoXML := LDocument.XML.Text;
  Result := true;
  finally
  LDocument.Active := false;
  LDocument := nil;
  end;
  end; }

{
  function UserInfo_Unserialize(const aUserInfoXML: string; out aUserInfo: TUserInfo): boolean;
  var
  LDocument: IXMLDocument;
  LNodeElement: IXMLNode;
  begin
  Result := false;
  OleInitialize(nil);
  LDocument := TXMLDocument.Create(nil);
  try
  LDocument.XML.Text := aUserInfoXML;
  LDocument.Active := true;
  if (LDocument.DocumentElement.NodeName <> 'UserInfo') then
  exit;

  LNodeElement := LDocument.DocumentElement.ChildNodes[0];
  if (LNodeElement.NodeName = 'UserInfo') then
  begin
  aUserInfo.family := LNodeElement.Attributes['family'];
  aUserInfo.name := LNodeElement.Attributes['name'];
  aUserInfo.surname := LNodeElement.Attributes['surname'];
  aUserInfo.weight := StrToInt(LNodeElement.Attributes['weight']);
  aUserInfo.height := StrToInt(LNodeElement.Attributes['height']);
  aUserInfo.email1 := LNodeElement.Attributes['email1'];
  aUserInfo.email2 := LNodeElement.Attributes['email2'];
  aUserInfo.phone1 := LNodeElement.Attributes['phone1'];
  aUserInfo.phone2 := LNodeElement.Attributes['phone2'];
  aUserInfo.birthdate := StrToDateOpeka(LNodeElement.Attributes['birthdate']);
  aUserInfo.passwordhash := StrToInt(LNodeElement.Attributes['passwordhash']);
  end;

  Result := true;
  finally
  LDocument.Active := false;
  LDocument := nil;
  end;
  end; }

end.
