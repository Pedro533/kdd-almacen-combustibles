# Del almacén de datos al conocimiento

Clase práctica en R que recorre el proceso KDD completo con datos abiertos oficiales de Argentina.

## Fuentes

- **Precios en surtidor** (Res. 314/2016), de la Secretaría de Energía
- **API Georef** (apis.datos.gob.ar), la fuente externa que se integra

## Recorrido (3 clases de unas 2 horas)

**Clase 1 · El almacén**
0. Fase previa: análisis de necesidades
1. Integración y recopilación: extracción y staging
2. Reconocimiento de los datos (EDA)
3. ETL: diseño, transformación, carga y metadatos
4. Explotación OLAP

**Clase 2 · Preparación de los datos**
5. Selección, limpieza y transformación: la vista minable

**Clase 3 · Minería, evaluación y difusión**
6. Minería de datos: tareas y métodos
7. Evaluación e interpretación
8. Difusión y uso
9. Ejercicios

## Cómo ejecutarlo

- **RStudio:** abrí `clase_almacen_combustibles_v2.R` y ejecutalo sección por sección (Ctrl+Enter).
- **Windows:** hacé doble clic en `clase_v2.bat`. Supone que R 4.4.1 está instalado en `C:\Program Files\R\R-4.4.1`; si tenés otra versión, ajustá la ruta.

Los paquetes que falten (DBI, RSQLite, dplyr, tidyr, ggplot2, rpart, class) se instalan solos.

### Variables de entorno opcionales

- `SIN_DESCARGA=1`: usa las copias locales de `datos_combustibles/` en lugar de descargar los datos (útil en un aula sin internet).
- `SIN_VISOR=1`: guarda los gráficos sin abrirlos.

Al ejecutarse, el script crea las bases SQLite (staging y almacén) en `datos_combustibles/`, los gráficos en `graficos/` y los resultados en `resultados/`.
