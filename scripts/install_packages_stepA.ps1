# PowerShell script to install R packages (Step A)
# Usage: Open PowerShell in project root and run: .\Code\install_packages_stepA.ps1

$Rscript = 'Rscript'
try {
    $which = & where.exe Rscript 2>$null
} catch {
    $which = $null
}
if (-not $which) {
    Write-Host "Rscript not found on PATH. Please provide full path to Rscript.exe or add R to PATH." -ForegroundColor Yellow
    Write-Host "Example: & 'C:\\Program Files\\R\\R-4.3.1\\bin\\Rscript.exe' -e \"install.packages('languageserver', repos='https://cloud.r-project.org')\""
    exit 1
}

Write-Host "Installing languageserver..."
& Rscript -e "install.packages('languageserver', repos='https://cloud.r-project.org')"

Write-Host "Installing helper packages..."
& Rscript -e "install.packages(c('httpgd','vscDebugger','jsonlite','shiny','visNetwork','shinyjs','dplyr','DT','tibble'), repos='https://cloud.r-project.org')"

if ($LASTEXITCODE -eq 0) {
    Write-Host "Step A installation finished successfully." -ForegroundColor Green
} else {
    Write-Host "Step A installation finished with errors. Check the output above." -ForegroundColor Red
}
