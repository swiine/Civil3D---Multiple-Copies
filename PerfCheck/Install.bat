@echo off
setlocal
set "SRC=%~dp0PerfCheck.bundle"
set "DEST=%APPDATA%\Autodesk\ApplicationPlugins\PerfCheck.bundle"

if not exist "%SRC%\Contents\PerfCheck.lsp" (
  echo ERROR: PerfCheck.bundle was not found next to this file.
  echo Unzip the whole PerfCheck.zip first, then run Install.bat from the unzipped folder.
  pause
  exit /b 1
)

echo Installing C3D PerfCheck to:
echo   %DEST%
if not exist "%APPDATA%\Autodesk\ApplicationPlugins" mkdir "%APPDATA%\Autodesk\ApplicationPlugins"
xcopy "%SRC%" "%DEST%\" /E /I /Y /Q >nul
if errorlevel 1 (
  echo ERROR: copy failed.
  pause
  exit /b 1
)

echo.
echo Done. Restart Civil 3D.
echo If it asks about loading PerfCheck.lsp, choose "Always Load".
echo Then type PERFCHECK for the full check, or PERFQUICK for a quick one.
echo.
pause
