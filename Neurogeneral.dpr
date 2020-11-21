// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program Neurogeneral;

uses
  Vcl.Forms,
  mainform in 'mainform.pas' {MainFormExtrapolation},
  calc_utils in '..\utils\calc_utils.pas',
  u_uidataselection in '..\utils\u_uidataselection.pas',
  serialization_utils in '..\utils\serialization_utils.pas',
  ablas in 'alglib\ablas.pas',
  ablasf in 'alglib\ablasf.pas',
  ap in 'alglib\ap.pas',
  bdsvd in 'alglib\bdsvd.pas',
  blas in 'alglib\blas.pas',
  creflections in 'alglib\creflections.pas',
  densesolver in 'alglib\densesolver.pas',
  hblas in 'alglib\hblas.pas',
  hqrnd in 'alglib\hqrnd.pas',
  linmin in 'alglib\linmin.pas',
  matgen in 'alglib\matgen.pas',
  matinv in 'alglib\matinv.pas',
  minlbfgs in 'alglib\minlbfgs.pas',
  mlpbase in 'alglib\mlpbase.pas',
  mlptrain in 'alglib\mlptrain.pas',
  ortfac in 'alglib\ortfac.pas',
  rcond in 'alglib\rcond.pas',
  reflections in 'alglib\reflections.pas',
  rotations in 'alglib\rotations.pas',
  safesolve in 'alglib\safesolve.pas',
  sblas in 'alglib\sblas.pas',
  svd in 'alglib\svd.pas',
  trfac in 'alglib\trfac.pas',
  trlinsolve in 'alglib\trlinsolve.pas',
  xblas in 'alglib\xblas.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainFormExtrapolation, MainFormExtrapolation);
  Application.Run;
end.
