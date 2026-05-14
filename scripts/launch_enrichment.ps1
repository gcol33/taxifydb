# Launch a single enrichment build, fully detached from caller's job object.
#
# Uses Register-ScheduledTask to survive job-object teardown when the parent
# (Claude Code, PowerShell session, etc.) exits. The task runs once,
# immediately, then auto-deletes when the build finishes.
#
# Usage:  .\scripts\launch_enrichment.ps1 -Name <enrichment_name>

param(
  [Parameter(Mandatory = $true)]
  [string]$Name,
  [string]$Repo = "C:\Users\Gilles Colling\Documents\dev\taxify-backbones"
)

$ErrorActionPreference = "Stop"

$rs = Get-ChildItem 'C:\Program Files\R\' -Directory |
      Where-Object { $_.Name -match '^R-' } |
      Sort-Object Name | Select-Object -Last 1
$Py = Join-Path $rs.FullName 'bin\Rscript.exe'

$RunDir = Join-Path $Repo "output\enrichment\$Name"
New-Item -ItemType Directory -Force -Path $RunDir | Out-Null

$BuildScript = Join-Path $Repo "scripts\run_build.R"
$OutDir = Join-Path $Repo "output\enrichment"
$StdOut = Join-Path $RunDir "stdout.log"
$StdErr = Join-Path $RunDir "stderr.log"

# Wipe stale logs / pid
Remove-Item $StdOut, $StdErr, (Join-Path $RunDir "pid.txt") -ErrorAction SilentlyContinue

# Wrapper .cmd file: forward stdout+stderr to log files, write pid before running.
# Runs Rscript in the foreground of the scheduled-task host (taskeng.exe), which
# is parented by Task Scheduler service, NOT by the caller.
$RunCmd = Join-Path $RunDir "_run.cmd"
$cmdContent = @"
@echo off
echo %DATE% %TIME% [scheduled task starting] > "$StdOut"
"$Py" "$BuildScript" $Name "$OutDir" >> "$StdOut" 2>> "$StdErr"
echo %DATE% %TIME% [scheduled task exit %ERRORLEVEL%] >> "$StdOut"
"@
Set-Content -Path $RunCmd -Value $cmdContent -Encoding ASCII

# Use a unique task name so concurrent launches don't collide
$TaskName = "taxify_build_$Name`_$(Get-Random -Minimum 100000 -Maximum 999999)"

$action = New-ScheduledTaskAction -Execute "cmd.exe" -Argument "/c `"$RunCmd`"" -WorkingDirectory $Repo
$trigger = New-ScheduledTaskTrigger -Once -At (Get-Date).AddSeconds(3)
$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries `
                                          -ExecutionTimeLimit (New-TimeSpan -Hours 24) `
                                          -RestartCount 0 -MultipleInstances IgnoreNew

Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings -Force | Out-Null

# Wait briefly for the task to fire and log its PID, then capture the PID
Start-Sleep -Seconds 5
$rscriptProcs = Get-Process Rscript -ErrorAction SilentlyContinue |
                Where-Object { $_.StartTime -gt (Get-Date).AddSeconds(-30) } |
                Sort-Object StartTime -Descending
if ($rscriptProcs) {
  $newPid = $rscriptProcs[0].Id
  Set-Content -Path (Join-Path $RunDir "pid.txt") -Value $newPid
  Write-Output "Launched build '$Name' as PID $newPid (via task '$TaskName')"
} else {
  Set-Content -Path (Join-Path $RunDir "pid.txt") -Value "unknown"
  Write-Output "Launched build '$Name' via task '$TaskName' (PID not yet visible)"
}
Write-Output "Task name: $TaskName"
Write-Output "Logs: $RunDir\{stdout,stderr}.log"
Write-Output "To clean up the task after build completes: Unregister-ScheduledTask -TaskName '$TaskName' -Confirm:`$false"
