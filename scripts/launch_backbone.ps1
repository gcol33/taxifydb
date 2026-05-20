# Launch a single backbone build, fully detached via Task Scheduler.
# Usage:  .\scripts\launch_backbone.ps1 -Name <backend>

param(
  [Parameter(Mandatory = $true)]
  [string]$Name,
  [string]$Repo = "C:\Users\Gilles Colling\Documents\dev\taxifydb"
)

$ErrorActionPreference = "Stop"

$rs = Get-ChildItem 'C:\Program Files\R\' -Directory |
      Where-Object { $_.Name -match '^R-' } |
      Sort-Object Name | Select-Object -Last 1
$Py = Join-Path $rs.FullName 'bin\Rscript.exe'

$RunDir = Join-Path $Repo "output\$Name"
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null

$BuildScript = Join-Path $Repo "build_all.R"
$OutDir = Join-Path $Repo "output"
$StdOut = Join-Path $RunDir "build.stdout.log"
$StdErr = Join-Path $RunDir "build.stderr.log"

Remove-Item $StdOut, $StdErr, (Join-Path $RunDir "build.pid.txt") -ErrorAction SilentlyContinue

$RunCmd = Join-Path $RunDir "_build.cmd"
$cmdContent = @"
@echo off
echo %DATE% %TIME% [scheduled task starting] > "$StdOut"
"$Py" "$BuildScript" $Name "$OutDir" >> "$StdOut" 2>> "$StdErr"
echo %DATE% %TIME% [scheduled task exit %ERRORLEVEL%] >> "$StdOut"
"@
Set-Content -Path $RunCmd -Value $cmdContent -Encoding ASCII

$TaskName = "taxify_backbone_$Name`_$(Get-Random -Minimum 100000 -Maximum 999999)"

$action = New-ScheduledTaskAction -Execute "cmd.exe" -Argument "/c `"$RunCmd`"" -WorkingDirectory $Repo
$trigger = New-ScheduledTaskTrigger -Once -At (Get-Date).AddSeconds(3)
$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries `
                                          -ExecutionTimeLimit (New-TimeSpan -Hours 6) `
                                          -RestartCount 0 -MultipleInstances IgnoreNew

Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings -Force | Out-Null

Start-Sleep -Seconds 5
$rscriptProcs = Get-Process Rscript -ErrorAction SilentlyContinue |
                Where-Object { $_.StartTime -gt (Get-Date).AddSeconds(-30) } |
                Sort-Object StartTime -Descending
if ($rscriptProcs) {
  $newPid = $rscriptProcs[0].Id
  Set-Content -Path (Join-Path $RunDir "build.pid.txt") -Value $newPid
  Write-Output "Launched backbone build '$Name' as PID $newPid (via task '$TaskName')"
} else {
  Set-Content -Path (Join-Path $RunDir "build.pid.txt") -Value "unknown"
  Write-Output "Launched backbone build '$Name' via task '$TaskName' (PID not yet visible)"
}
Write-Output "Task name: $TaskName"
Write-Output "Logs: $RunDir\build.{stdout,stderr}.log"
Write-Output "To clean up: Unregister-ScheduledTask -TaskName '$TaskName' -Confirm:`$false"
