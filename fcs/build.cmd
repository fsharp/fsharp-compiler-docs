@echo off

setlocal
pushd %~dp0%

if errorlevel 1 (
  endlocal
  exit /b %errorlevel%
)

REM see https://github.com/fsprojects/Paket/issues/3065#issuecomment-367829309
REM powershell -Command {new-itemproperty -path "HKLM:\SOFTWARE\Microsoft\.NETFramework\v4.0.30319" -name "SchUseStrongCrypto" -Value 1 -PropertyType "DWord";}
REM powershell -Command {new-itemproperty -path "HKLM:\SOFTWARE\Wow6432Node\Microsoft\.NETFramework\v4.0.30319" -name "SchUseStrongCrypto" -Value 1 -PropertyType "DWord"}

.paket\paket.exe restore
if errorlevel 1 (
  endlocal
  exit /b %errorlevel%
)

:: don't care if this fails
dotnet build-server shutdown >NUL 2>&1

packages\FAKE\tools\FAKE.exe build.fsx %*
if errorlevel 1 (
  endlocal
  exit /b %errorlevel%
)
endlocal
exit /b 0
