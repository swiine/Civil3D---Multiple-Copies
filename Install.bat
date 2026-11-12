@echo off
setlocal
set "SRC=%~dp0MultiClip.bundle"
set "DEST=%APPDATA%\Autodesk\ApplicationPlugins\MultiClip.bundle"

if not exist "%SRC%\Contents\MultiClip.lsp" (
  echo ERROR: MultiClip.bundle was not found next to this file.
  echo Unzip the whole MultiClip.zip first, then run Install.bat from the unzipped folder.
  pause
  exit /b 1
)

echo Installing MultiClip to:
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
echo If it asks about loading MultiClip.lsp, choose "Always Load".
echo Then type C1 to copy to slot 1 and P1 to paste it. Type MCLIST to see your slots.
echo.
pause
