@echo off
setlocal
set "SRC=%~dp0SweptPath.bundle"
set "DEST=%APPDATA%\Autodesk\ApplicationPlugins\SweptPath.bundle"

if not exist "%SRC%\Contents\SweptPath.lsp" (
  echo ERROR: SweptPath.bundle was not found next to this file.
  echo Unzip the whole SweptPath.zip first, then run Install.bat from the unzipped folder.
  pause
  exit /b 1
)

echo Installing SweptPath to:
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
echo If it asks about loading SweptPath.lsp, choose "Always Load".
echo Then draw a path with PLINE and type SWEPT. Type SWHELP for all commands.
echo.
pause
