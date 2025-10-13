@echo off
REM Install R packages Step A: languageserver and helper packages
REM Usage: run this file from project root in cmd.exe (or double-click)

:: Check for Rscript on PATH
where Rscript >nul 2>nul
if %ERRORLEVEL% neq 0 (
    echo Rscript not found on PATH. Please add R to PATH or run with full path to Rscript.exe.
    echo Example: "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" -e "install.packages('languageserver', repos='https://cloud.r-project.org')"
    pause
    exit /b 1
)

REM Install languageserver first
Rscript -e "install.packages('languageserver', repos='https://cloud.r-project.org')"

REM Install helper packages in one call
Rscript -e "install.packages(c('httpgd','vscDebugger','jsonlite','shiny','visNetwork','shinyjs','dplyr','DT','tibble'), repos='https://cloud.r-project.org')"

if %ERRORLEVEL% equ 0 (
    echo Step A installation finished successfully.
) else (
    echo Step A installation finished with errors. Check the output above.
)
pause
