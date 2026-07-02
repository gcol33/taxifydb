# Detached launcher for the enrichment rebuild. Run under a Scheduled Task so it
# survives the launching session ending/compacting. Hardcodes the real Rscript
# (no pyenv-style shim here, but be explicit) and redirects both streams.
$ErrorActionPreference = 'Continue'
$Repo   = 'C:\Users\GillesC\Documents\dev\taxifydb'
$Rscript = 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe'
$Script = Join-Path $Repo 'scripts\rebuild_all_enrichments.R'
$RunDir = Join-Path $Repo 'output\rebuild_run'
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null

$stdout = Join-Path $RunDir 'stdout.log'
$stderr = Join-Path $RunDir 'stderr.log'

$proc = Start-Process -FilePath $Rscript `
  -ArgumentList @($Script) `
  -WorkingDirectory $Repo `
  -RedirectStandardOutput $stdout `
  -RedirectStandardError  $stderr `
  -WindowStyle Hidden -PassThru
Set-Content -Path (Join-Path $RunDir 'pid.txt') -Value $proc.Id
$proc.WaitForExit()
Set-Content -Path (Join-Path $RunDir 'launcher_done.txt') -Value $proc.ExitCode
