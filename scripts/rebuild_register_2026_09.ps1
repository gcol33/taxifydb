$Repo = "C:\GillesC\Documents\dev\taxifydb"
$Rs = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
$RunDir = Join-Path $Repo "output\register_2026_09"
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null
& $Rs "$Repo\scripts\rebuild_register_2026_09.R" *>&1 |
  Out-File -FilePath "$RunDir\stdout.log" -Encoding utf8 -Append
"launcher exited $(Get-Date -Format s)" |
  Out-File -FilePath "$RunDir\launcher_done.txt" -Encoding utf8
