@echo off
chcp 65001 > nul
cd /d "%~dp0"
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" -e "source('clase_almacen_combustibles_v2.R', echo = TRUE, max.deparse.length = Inf, encoding = 'UTF-8')"
pause
