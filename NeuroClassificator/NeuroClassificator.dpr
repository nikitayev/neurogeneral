// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
{
  https://github.com/nikitayev/neurogeneral.git

  JCL & JVCL:
  https://github.com/project-jedi/jedi.git/
  https://github.com/project-jedi/jcl.git/trunk
  https://github.com/project-jedi/jvcl.git/trunk
}
program NeuroClassificator;

uses
  Vcl.Forms,
  mainform in 'mainform.pas' {MainFormExtrapolation} ,
  ablas in '..\alglib\ablas.pas',
  ablasf in '..\alglib\ablasf.pas',
  ap in '..\alglib\ap.pas',
  bdsvd in '..\alglib\bdsvd.pas',
  blas in '..\alglib\blas.pas',
  creflections in '..\alglib\creflections.pas',
  densesolver in '..\alglib\densesolver.pas',
  hblas in '..\alglib\hblas.pas',
  hqrnd in '..\alglib\hqrnd.pas',
  linmin in '..\alglib\linmin.pas',
  matgen in '..\alglib\matgen.pas',
  matinv in '..\alglib\matinv.pas',
  minlbfgs in '..\alglib\minlbfgs.pas',
  mlpbase in '..\alglib\mlpbase.pas',
  mlptrain in '..\alglib\mlptrain.pas',
  ortfac in '..\alglib\ortfac.pas',
  rcond in '..\alglib\rcond.pas',
  reflections in '..\alglib\reflections.pas',
  rotations in '..\alglib\rotations.pas',
  safesolve in '..\alglib\safesolve.pas',
  sblas in '..\alglib\sblas.pas',
  svd in '..\alglib\svd.pas',
  trfac in '..\alglib\trfac.pas',
  trlinsolve in '..\alglib\trlinsolve.pas',
  xblas in '..\alglib\xblas.pas',
  calc_utils in '..\utils\calc_utils.pas',
  clasterization_utils in '..\utils\clasterization_utils.pas',
  serialization_utils in '..\utils\serialization_utils.pas',
  TeachingProgressUnit
    in '..\utils\TeachingProgressUnit.pas' {TeachingProgressForm} ,
  U_hrv_neurontrain_MT in '..\utils\U_hrv_neurontrain_MT.pas',
  U_LoadCSV in '..\utils\U_LoadCSV.pas',
  u_uidataselection in '..\utils\u_uidataselection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainFormExtrapolation, MainFormExtrapolation);
  Application.CreateForm(TTeachingProgressForm, TeachingProgressForm);
  Application.Run;

end.
