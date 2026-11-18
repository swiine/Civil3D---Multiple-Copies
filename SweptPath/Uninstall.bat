@echo off
set "DEST=%APPDATA%\Autodesk\ApplicationPlugins\SweptPath.bundle"
if exist "%DEST%" (
  rmdir /S /Q "%DEST%"
  echo SweptPath removed. Restart Civil 3D.
) else (
  echo SweptPath is not installed.
)
if exist "%APPDATA%\SweptPath" (
  choice /M "Also delete your custom vehicles"
  if not errorlevel 2 rmdir /S /Q "%APPDATA%\SweptPath"
)
pause
