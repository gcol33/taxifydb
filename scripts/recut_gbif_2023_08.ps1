$Repo = "C:\GillesC\Documents\dev\taxifydb"
$Rs = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
$RunDir = Join-Path $Repo "output\recut_2023_08"
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null
& $Rs "$Repo\scripts\recut_gbif_2023_08.R" *>&1 |
  Out-File -FilePath "$RunDir\stdout.log" -Encoding utf8 -Append
"launcher exited $(Get-Date -Format s)" |
  Out-File -FilePath "$RunDir\launcher_done.txt" -Encoding utf8
