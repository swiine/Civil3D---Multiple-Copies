@echo off
set "DEST=%APPDATA%\Autodesk\ApplicationPlugins\PerfCheck.bundle"
if exist "%DEST%" (
  rmdir /S /Q "%DEST%"
  echo C3D PerfCheck removed. Restart Civil 3D.
) else (
  echo C3D PerfCheck is not installed.
)
if exist "%LOCALAPPDATA%\C3D-PerfCheck" (
  choice /M "Also delete your saved reports and history"
  if not errorlevel 2 rmdir /S /Q "%LOCALAPPDATA%\C3D-PerfCheck"
)
pause
