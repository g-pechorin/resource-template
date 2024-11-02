@ECHO OFF

where python >nul 2>nul
if errorlevel 1 (
    echo Python is not installed or not found in PATH.
    exit /b 1
)

where java >nul 2>nul
if errorlevel 1 (
    echo java is not installed or not found in PATH.
    exit /b 1
)

where javac >nul 2>nul
if errorlevel 1 (
    echo javac is not installed or not found in PATH.
    exit /b 1
)

python lsbt %*
