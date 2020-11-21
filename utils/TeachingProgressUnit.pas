unit TeachingProgressUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, mlptrain;

type
  TTeachingProgressForm = class(TForm)
    leTotalRestarts: TLabeledEdit;
    leRestartsFinished: TLabeledEdit;
    leIsTerminated: TLabeledEdit;
    leEBest: TLabeledEdit;
    btCancel: TButton;
    procedure btCancelClick(Sender: TObject);
  private
    { Private declarations }
    procedure NewMPLPTrainMessage(var Msg: TMessage); message WM_MLPTrainMTMessage;
  public
    { Public declarations }
  end;

var
  TeachingProgressForm: TTeachingProgressForm;

implementation

uses U_hrv_neurontrain_MT;

{$R *.dfm}
{ TTeachingProgressForm }

procedure TTeachingProgressForm.btCancelClick(Sender: TObject);
begin
  if not GTrainIsTerminated then
  begin
    GTrainIsTerminated := True;
    leIsTerminated.Text := 'обучение прервано';
  end;
end;

procedure TTeachingProgressForm.NewMPLPTrainMessage(var Msg: TMessage);
begin
  { FProcessData := PMLPProcessData(Msg.WParam);
    leTotalRestarts.Text := IntToStr(FProcessData.TotalRestarts);
    leRestartsFinished.Text := IntToStr(FProcessData.RestartsFinished);
    leIsTerminated.Text := BoolToStr(FProcessData.IsTerminated);
    leEBest.Text := FloatToStr(FProcessData.EBest);
    if (FProcessData.IsTerminated or (FProcessData.TotalRestarts = FProcessData.RestartsFinished)) then
    begin
    MainForm.miSaveNetwork.Enabled := True;
    MessageDlg('Процесс обучения завершён', mtWarning, [mbOk], 0, mbOk);
    Screen.Cursor := crDefault;
    end else
    begin
    TeachingProgressForm.btCancel.Enabled := True;
    end; }
end;

end.
