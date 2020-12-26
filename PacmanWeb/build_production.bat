rd /S /Q wwwroot\
@if EXIST wwwroot\ goto Failed

@mkdir wwwroot
@if ERRORLEVEL 1 goto Failed

copy src\assets\menutile.png wwwroot\
@if ERRORLEVEL 1 goto Failed

mkdir wwwroot\Images
@if ERRORLEVEL 1 goto Failed

mkdir wwwroot\Sounds
@if ERRORLEVEL 1 goto Failed

xcopy /s ..\PacmanEngine\Images\*.png wwwroot\Images\
@if ERRORLEVEL 1 goto Failed

xcopy /s ..\PacmanEngine\Sounds\*.* wwwroot\Sounds\
@if ERRORLEVEL 1 goto Failed

npx webpack --mode production
@if ERRORLEVEL 1 goto Failed

@echo Success!
@goto Exit

:Failed
@echo Build failed.

:Exit
