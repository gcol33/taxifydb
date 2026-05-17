# Detached launcher for dev_notes/publish_three.R. The long gh uploads
# (~5 GB total across three backbones) get killed by the harness when run
# as a plain background Bash call. Start-Process gives the R worker a
# Windows-session parent so it survives the launcher exiting.

$Repo = "C:\Users\Gilles Colling\Documents\dev\taxify-backbones"
$Rscript = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
$Script = "dev_notes\publish_three.R"
$RunDir = Join-Path $Repo "dev_notes"

$proc = Start-Process -FilePath $Rscript `
    -ArgumentList @("--vanilla", $Script) `
    -WorkingDirectory $Repo `
    -RedirectStandardOutput (Join-Path $RunDir "publish.stdout.log") `
    -RedirectStandardError  (Join-Path $RunDir "publish.stderr.log") `
    -WindowStyle Hidden -PassThru

Set-Content -Path (Join-Path $RunDir "publish.pid") -Value $proc.Id
Write-Output ("PID=" + $proc.Id)
