unit U_hrv_neurontrain_MT;

interface

uses Windows, Sysutils, Math, System.Threading, Generics.Collections,
  Ap, mlpbase, mlptrain, calc_utils, serialization_utils;

type

  TNewNeuroNotifyProc = procedure(aNetwork: MultiLayerPerceptron) of object;

  TMultithreadTrainLBFS = class(TList<TMLPTrainLBFGS_Thread>);

var
  // глобальная переменная отвечающая за возможность прерывания обучения
  GTrainIsTerminated: boolean;

function StartTrainLBFS_MT(const aNeuronFileName: string; lXY: TReal2DArray; const aOptions: TCalcNeuroOptions;
  aNewNeuroNotifyProc: TNewNeuroNotifyProc): boolean;

implementation

uses Vcl.Forms, TeachingProgressUnit, JclSysInfo;

function GetProcessorsCount: Integer;
var
  SystemInfo: TSystemInfo;
begin
  GetNativeSystemInfo(SystemInfo);
  result := SystemInfo.dwNumberOfProcessors;
end;

procedure ConstructNeuroMatrix(const aOptions: TCalcNeuroOptions; var aNetwork: MultiLayerPerceptron);
begin
  case aOptions.IsClassificator of
    true:
      case aOptions.LayerCount of
        0:
          MLPCreateC0(aOptions.Layer1PtsCount, aOptions.OutPtsCount, aNetwork);
        1:
          MLPCreateC1(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.OutPtsCount, aNetwork);
      else
        MLPCreateC2(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.Layer3PtsCount, aOptions.OutPtsCount,
          aNetwork);
      end;
    false:
      case aOptions.LayerCount of
        0:
          MLPCreate0(aOptions.Layer1PtsCount, aOptions.OutPtsCount, aNetwork);
        1:
          MLPCreate1(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.OutPtsCount, aNetwork);
      else
        MLPCreate2(aOptions.Layer1PtsCount, aOptions.Layer2PtsCount, aOptions.Layer3PtsCount, aOptions.OutPtsCount,
          aNetwork);
      end;
  end;

end;

function StartTrainLBFS_MT(const aNeuronFileName: string; lXY: TReal2DArray; const aOptions: TCalcNeuroOptions;
  aNewNeuroNotifyProc: TNewNeuroNotifyProc): boolean;
var
  i, zCounter, zprocessorcount: Integer;
  lNetwork: PMultiLayerPerceptron;
  zThreadList: TMultithreadTrainLBFS;
  zEBest: AlglibFloat;
begin
  result := false;
  zThreadList := TMultithreadTrainLBFS.Create;
  try
    zCounter := 0;
    zprocessorcount := GetProcessorsCount;
    zEBest := MaxDouble;

    TeachingProgressForm.Show;
    TeachingProgressForm.BringToFront;
    TeachingProgressForm.leTotalRestarts.Text := IntToStr(aOptions.lRestarts);
    TeachingProgressForm.leRestartsFinished.Text := '0';
    TeachingProgressForm.leIsTerminated.Text := '-';
    TeachingProgressForm.leEBest.Text := '-';

    while (not Application.Terminated and (zCounter < aOptions.lRestarts)) do
    begin
      // уберём отработавшие потоки
      for i := zThreadList.Count - 1 downto 0 do
        if (zThreadList[i].Terminated) then
        begin
          if (zEBest > zThreadList[i].EBest) then
          begin
            MultiLayerPerceptron_Serialize(zThreadList[i].FNetwork^, aNeuronFileName);
            if (Assigned(aNewNeuroNotifyProc)) then
              aNewNeuroNotifyProc(zThreadList[i].FNetwork^);
            Dispose(zThreadList[i].FNetwork);
            result := true;
            zEBest := zThreadList[i].EBest;

            TeachingProgressForm.leEBest.Text := FloatToStr(zEBest);
          end;
          zThreadList[i].Free;
          zThreadList[i] := nil;
          zThreadList.Delete(i);
          Inc(zCounter);

          TeachingProgressForm.leRestartsFinished.Text := IntToStr(zCounter);
        end;

      // обучение нейросети
      for i := zThreadList.Count to zprocessorcount - 1 do
      begin
        if (zCounter < aOptions.lRestarts) then
        begin
          New(lNetwork);
          ConstructNeuroMatrix(aOptions, lNetwork^);
          zThreadList.Add(TMLPTrainLBFGS_Thread.Create(lNetwork, lXY, aOptions.lPoints, aOptions.lDecay, 1,
            aOptions.lMaxStep, aOptions.lMaxIts, @GTrainIsTerminated));
        end;
      end;
      Application.ProcessMessages;
      Sleep(1);
    end;
    GTrainIsTerminated := true;
  finally
    for i := 0 to zThreadList.Count - 1 do
      if (Assigned(zThreadList[i])) then
        zThreadList[i].Free;
    FreeAndNil(zThreadList);
  end;

end;

end.
