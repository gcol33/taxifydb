# Runs build-heavy.yml's steps for the given backbones on this machine, for
# when no self-hosted runner is registered. Same scripts, same order; the
# xdelta patch (optional in the workflow) is not produced.
#   powershell -File scripts/run_heavy_local.ps1 col colxr
param([Parameter(ValueFromRemainingArguments = $true)][string[]]$Backends)

$ErrorActionPreference = 'Continue'
$Repo = Split-Path $PSScriptRoot -Parent
Set-Location $Repo
$Rscript = (Get-ChildItem 'C:\Program Files\R\R-*\bin\Rscript.exe' |
  Sort-Object FullName | Select-Object -Last 1).FullName
$Log = Join-Path $Repo 'output\heavy_local.log'
New-Item -ItemType Directory -Force (Join-Path $Repo 'output') | Out-Null

function Say($m) { "$(Get-Date -Format s) $m" | Tee-Object -FilePath $Log -Append }
function RunR { & $Rscript @args 2>&1 | Tee-Object -FilePath $Log -Append
             if ($LASTEXITCODE -ne 0) { throw "Rscript failed: $args" } }

$env:GIT_SSH_COMMAND = 'ssh -i C:/Users/GillesC/.ssh/id_ed25519_gcol33'
$env:GH_TOKEN = (gh auth token)

try {
  Say "install taxifydb"
  RunR -e "devtools::install_local('.', upgrade = 'never', force = TRUE)"
  foreach ($be in $Backends) {
    $out = "output/$be"
    Say "build $be"
    RunR -e "taxifydb::build_backend('$be', output_dir = file.path('output', '$be'))"
    $ver = (& $Rscript scripts/backbone_version.R $be $out | Select-Object -Last 1).Trim()
    $chg = (& $Rscript scripts/vtr_changed.R manifest/manifest.json $be "$out/$be.vtr" |
      Select-Object -Last 1).Trim()
    Say "$be version=$ver changed=$chg"
    if ($chg -ne 'true') { continue }
    RunR scripts/publish_backbone_release.R $be $out gcol33/taxifydb
    git fetch -q origin main; git merge -q --ff-only origin/main
    RunR scripts/update_manifest_entry.R manifest/manifest.json $be $out ""
    git add manifest/manifest.json
    git diff --staged --quiet
    if ($LASTEXITCODE -ne 0) {
      git commit -q -m "Update manifest: $be v$ver"
      git push -q origin HEAD:main
      if ($LASTEXITCODE -ne 0) { throw "manifest push failed" }
    }
    Say "$be published"
  }
  Set-Content (Join-Path $Repo 'output\heavy_local.done') (Get-Date -Format s)
} catch {
  Say "FAILED: $_"
  Set-Content (Join-Path $Repo 'output\heavy_local.failed') "$_"
}
