@echo off
set "DEST=%APPDATA%\Autodesk\ApplicationPlugins\MultiClip.bundle"
if exist "%DEST%" (
  rmdir /S /Q "%DEST%"
  echo MultiClip removed. Restart Civil 3D.
) else (
  echo MultiClip is not installed.
)
if exist "%LOCALAPPDATA%\C3D-MultiClip" (
  choice /M "Also delete your saved clipboard slots"
  if not errorlevel 2 rmdir /S /Q "%LOCALAPPDATA%\C3D-MultiClip"
)
pause
