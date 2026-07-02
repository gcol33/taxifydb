# Detached launcher for the all-traits BIEN build (multi-hour scrape).
$ErrorActionPreference = 'Continue'
$Repo   = 'C:\Users\GillesC\Documents\dev\taxifydb'
$Rscript = 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe'
$Script = Join-Path $Repo 'scripts\build_bien_all.R'
$RunDir = Join-Path $Repo 'output\bien_run'
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null
$proc = Start-Process -FilePath $Rscript -ArgumentList @($Script) `
  -WorkingDirectory $Repo `
  -RedirectStandardOutput (Join-Path $RunDir 'stdout.log') `
  -RedirectStandardError  (Join-Path $RunDir 'stderr.log') `
  -WindowStyle Hidden -PassThru
Set-Content -Path (Join-Path $RunDir 'pid.txt') -Value $proc.Id
$proc.WaitForExit()
Set-Content -Path (Join-Path $RunDir 'launcher_done.txt') -Value $proc.ExitCode
