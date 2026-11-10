# =====================================================================
#  CLASE PRÁCTICA v2: DEL ALMACÉN DE DATOS AL CONOCIMIENTO
#  El proceso KDD completo con datos abiertos oficiales de Argentina
#
#  Amplía clase_almacen_combustibles.R con la teoría de:
#   - 02 El proceso de extracción de conocimiento (fases del KDD, tareas)
#   - 03 Almacenes de datos (ROLAP, diseño, ETL, staging, metadatos,
#        datamarts, usuarios)
#   - Exploración y selección de datos (vista minable, muestreo, Laplace,
#        selección de atributos)
#   - Transformación de atributos (discretización, normalización, PCA,
#        creación de características)
#   - El problema de la extracción de patrones (tareas y métodos,
#        evaluación, sobreajuste, comprensibilidad, separabilidad)
#
#  Fuentes:
#   - Precios en surtidor (Res. 314/2016) - Secretaría de Energía
#   - API Georef (apis.datos.gob.ar) - FUENTE EXTERNA a integrar
#
#  Recorrido (pensado para 3 clases de unas 2 horas):
#   CLASE 1 · El almacén
#     0. Fase previa: análisis de necesidades
#     1. Integración y recopilación: extracción y staging
#     2. Reconocimiento de los datos (EDA)
#     3. ETL: diseño, transformación, carga y metadatos
#     4. Explotación OLAP
#   CLASE 2 · Preparación de los datos
#     5. Selección, limpieza y transformación: la VISTA MINABLE
#   CLASE 3 · Minería, evaluación y difusión
#     6. Minería de datos: tareas y métodos
#     7. Evaluación e interpretación
#     8. Difusión y uso
#     9. Ejercicios
#
#  Ejecutar sección por sección (Ctrl+Enter) y comentar cada paso.
#  Variables de entorno opcionales:
#    SIN_DESCARGA=1  usa las copias locales (aula sin internet)
#    SIN_VISOR=1     guarda los gráficos sin abrirlos
# =====================================================================


# ---------------------------------------------------------------------
# 0. PREPARACIÓN ----
# ---------------------------------------------------------------------
paquetes <- c("DBI", "RSQLite", "dplyr", "tidyr", "ggplot2", "rpart", "class")
for (p in paquetes) if (!require(p, character.only = TRUE)) install.packages(p)

library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(class)      # k vecinos más próximos (viene con R)

options(scipen = 999, dplyr.width = Inf, width = 120, dplyr.summarise.inform = FALSE)
set.seed(190)       # muestreos y particiones reproducibles
dir.create("datos_combustibles", showWarnings = FALSE)
dir.create("graficos", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

# Pausa entre secciones (funciona en RStudio, en R y en Rscript por consola)
pausa <- function(titulo) {
  cat("\n\n==================== ", titulo, " ====================\n", sep = "")
  cat("Presioná ENTER para continuar...")
  if (interactive()) invisible(readline())
  else invisible(readLines(file("stdin"), n = 1))
}

# Guarda el gráfico como PNG y lo abre con el visor de imágenes de Windows
#   (grafico = ggplot, o una función que dibuja con gráficos base de R)
mostrar <- function(grafico, nombre) {
  archivo <- file.path("graficos", paste0(nombre, ".png"))
  guardar <- function(destino) {
    if (is.function(grafico)) {
      png(destino, width = 1200, height = 750); grafico(); invisible(dev.off())
    } else {
      ggsave(destino, grafico, width = 10, height = 6, dpi = 100)
    }
  }
  # Si el archivo está abierto en el visor (o bloqueado por OneDrive),
  # se guarda con otro nombre en lugar de cortar la clase.
  ok <- tryCatch({ guardar(archivo); TRUE }, error = function(e) FALSE,
                 warning = function(w) FALSE)
  if (!ok) {
    while (dev.cur() > 1) dev.off()
    archivo <- file.path("graficos", paste0(nombre, "_", format(Sys.time(), "%H%M%S"), ".png"))
    guardar(archivo)
  }
  if (.Platform$OS.type == "windows" && Sys.getenv("SIN_VISOR") == "") shell.exec(normalizePath(archivo))
  message("Gráfico guardado en ", archivo)
}

# CSV que Excel en castellano abre bien (separador ; y tildes)
guardar_csv <- function(df, archivo) {
  con <- file(archivo, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  cat("﻿", file = con)
  write.csv2(df, con, row.names = FALSE)
  message("Guardado: ", archivo)
}

si_no     <- function(x) factor(ifelse(x, "si", "no"), levels = c("no", "si"))
precision <- function(real, pred) mean(as.character(real) == as.character(pred))

SUPER   <- "Nafta (súper) entre 92 y 95 Ron"
PREMIUM <- "Nafta (premium) de más de 95 Ron"
BANDERAS_GRANDES <- c("YPF", "SHELL C.A.P.S.A.", "AXION", "PUMA")
corto <- c("Nafta (súper) entre 92 y 95 Ron"  = "super",
           "Nafta (premium) de más de 95 Ron" = "premium",
           "Gas Oil Grado 2" = "gasoil2",
           "Gas Oil Grado 3" = "gasoil3",
           "GNC"             = "gnc")

archivo_precios    <- "datos_combustibles/precios_surtidor.csv"
archivo_provincias <- "datos_combustibles/georef_provincias.csv"
archivo_staging    <- "datos_combustibles/staging_combustibles.sqlite"
archivo_almacen    <- "datos_combustibles/almacen_combustibles_v2.sqlite"

url_precios <- paste0(
  "http://datos.energia.gob.ar/dataset/1c181390-5045-475e-94dc-410429be4b17/",
  "resource/80ac25de-a44a-4445-9215-090cf55cfda5/download/",
  "precios-en-surtidor-resolucin-3142016.csv")
url_provincias <- paste0(
  "https://apis.datos.gob.ar/georef/api/provincias?",
  "campos=id,nombre,centroide&max=30&formato=csv")


pausa("0. FASE PREVIA: ANÁLISIS DE NECESIDADES")

# ---------------------------------------------------------------------
# 0. FASE PREVIA: ANÁLISIS DE NECESIDADES ----
#    Antes de las cinco fases del KDD se define el problema. Un OBJETIVO
#    DE NEGOCIO da lugar a varios OBJETIVOS DE MINERÍA, y cada uno se
#    traduce en una TAREA (predictiva o descriptiva).
#
#    Las cinco fases del KDD y dónde aparecen en este script:
#      A. Integración y recopilación ............ secciones 1 a 4
#      B. Selección, limpieza y transformación .. sección 5
#      C. Minería de datos ...................... sección 6
#      D. Evaluación e interpretación ........... sección 7
#      E. Difusión y uso ........................ sección 8
#    El proceso es ITERATIVO (la salida de una fase puede obligar a volver
#    atrás, lo vamos a ver en 5.2) e INTERACTIVO (el experto del dominio
#    valida: "¿por qué la Patagonia es más barata?").
#
#    Objetivo de negocio: un organismo de defensa del consumidor quiere
#    entender qué determina el precio del combustible y detectar estaciones
#    que se apartan del precio de su zona.
# ---------------------------------------------------------------------
objetivos <- data.frame(
  objetivo_de_mineria = c("Predecir si una estación es CARA para su producto",
                          "Estimar el precio de la nafta súper de una estación",
                          "Encontrar provincias con perfiles de precio parecidos",
                          "Descubrir qué características aparecen juntas",
                          "Detectar estaciones con precios fuera de lo común"),
  tarea   = c("Clasificación", "Regresión", "Agrupamiento",
              "Reglas de asociación", "Detección de anomalías"),
  familia = c("Predictiva", "Predictiva", "Descriptiva", "Descriptiva", "Descriptiva"),
  seccion = c("6.1 a 6.5", "6.6", "6.7", "6.8", "6.9"))
print(objetivos, right = FALSE)


pausa("1. INTEGRACIÓN Y RECOPILACIÓN: EXTRACCIÓN")

# ---------------------------------------------------------------------
# 1. INTEGRACIÓN Y RECOPILACIÓN ----
#    La fuente es un sistema TRANSACCIONAL (OLTP): cada estación informa
#    su precio cuando lo cambia. Sirve para operar, no para analizar. Si
#    consultáramos directamente sobre él, las consultas analíticas
#    ("killer queries") perturbarían el trabajo diario: por eso separamos
#    el análisis en un ALMACÉN.
# ---------------------------------------------------------------------

## 1.1 Extracción -------------------------------------------------------
descargar <- function(url, destino) {
  if (Sys.getenv("SIN_DESCARGA") != "" && file.exists(destino)) {
    message("SIN_DESCARGA: uso la copia local ", destino)
    return(TRUE)
  }
  temporal <- tempfile()                 # si falla, la copia local no se pierde
  ok <- tryCatch({
    download.file(url, temporal, mode = "wb", quiet = TRUE)
    file.size(temporal) > 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (ok) {
    file.copy(temporal, destino, overwrite = TRUE)
    message("Descargado: ", destino)
  } else if (file.exists(destino)) {
    message("No se pudo descargar: uso la copia local ", destino)
  }
  ok || file.exists(destino)
}

if (!descargar(url_precios, archivo_precios))
  stop("No se pudo descargar ", url_precios, " y no hay copia local.")

# La API Georef limita las consultas (error 429). Si no responde y no hay
# copia local, usamos estos mismos datos oficiales guardados en el script.
if (!descargar(url_provincias, archivo_provincias)) {
  message("Uso la copia de respaldo de Georef incluida en el script")
  write.csv(data.frame(
    provincia_id = c("02","58","74","82","46","10","90","22","34","78","26","50",
                     "30","70","38","86","62","18","54","66","14","06","42","94"),
    provincia_nombre = c("Ciudad Autónoma de Buenos Aires","Neuquén","San Luis",
      "Santa Fe","La Rioja","Catamarca","Tucumán","Chaco","Formosa","Santa Cruz",
      "Chubut","Mendoza","Entre Ríos","San Juan","Jujuy","Santiago del Estero",
      "Río Negro","Corrientes","Misiones","Salta","Córdoba","Buenos Aires","La Pampa",
      "Tierra del Fuego, Antártida e Islas del Atlántico Sur"),
    provincia_centroide_lat = c(-34.6144,-38.6420,-33.7611,-30.7088,-29.6849,-27.3360,
      -26.9483,-26.3870,-24.8951,-48.8155,-43.7886,-34.6304,-32.0589,-30.8657,-23.3200,
      -27.7834,-40.4051,-28.7742,-26.8753,-24.2993,-32.1448,-36.6774,-37.1351,-82.5211),
    provincia_centroide_lon = c(-58.4459,-70.1199,-66.0253,-60.9507,-67.1818,-66.9479,
      -65.3648,-60.7651,-59.9322,-69.9558,-68.5267,-68.5829,-59.2013,-68.8882,-65.7644,
      -63.2526,-67.2297,-57.8011,-54.6516,-64.8142,-63.8020,-60.5585,-65.4476,-50.7429)),
    archivo_provincias, row.names = FALSE, fileEncoding = "UTF-8")
}

## 1.2 Almacenamiento intermedio (STAGING) ------------------------------
#  Separamos la EXTRACCIÓN de la TRANSFORMACIÓN: las fuentes se copian
#  tal cual llegan a un repositorio intermedio, con la fecha de extracción.
#  Si la transformación falla, no hace falta volver a descargar, y queda
#  constancia de qué se extrajo y cuándo.
staging <- dbConnect(SQLite(), archivo_staging)
ahora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

dbWriteTable(staging, "stg_precios_surtidor",
             read.csv(archivo_precios, encoding = "UTF-8") %>%
               mutate(stg_fecha_extraccion = ahora),
             overwrite = TRUE)
dbWriteTable(staging, "stg_georef_provincias",
             read.csv(archivo_provincias, encoding = "UTF-8",
                      colClasses = c(provincia_id = "character")) %>%
               mutate(stg_fecha_extraccion = ahora),
             overwrite = TRUE)
dbListTables(staging)

# A partir de acá el ETL lee del STAGING, no de los archivos originales
fuente <- dbReadTable(staging, "stg_precios_surtidor") %>%
  select(-geojson, -stg_fecha_extraccion)      # geojson repite latitud/longitud
georef <- dbReadTable(staging, "stg_georef_provincias") %>%
  select(-stg_fecha_extraccion)
dbDisconnect(staging)


pausa("2. RECONOCIMIENTO DE LOS DATOS (EDA)")

# ---------------------------------------------------------------------
# 2. RECONOCIMIENTO DE LOS DATOS: ANÁLISIS EXPLORATORIO (EDA) ----
#    "Datos limpios no alcanzan": antes de minar hay que conocerlos.
#    El EDA (Tukey, 1977) caracteriza los datos SIN construir modelos.
#    La fuente es PLANA y DESNORMALIZADA: cada fila repite empresa,
#    dirección, provincia, etc.
# ---------------------------------------------------------------------
dim(fuente)
str(fuente)
head(fuente, 3)

## 2.1 Datos faltantes ----------------------------------------------------
#  Antes de decidir nada hay que pensar QUÉ SIGNIFICA cada faltante.
contar_faltantes <- function(x) {
  if (is.character(x)) sum(is.na(x) | trimws(x) == "") else sum(is.na(x))
}
faltantes <- sapply(fuente, contar_faltantes)
faltantes[faltantes > 0]
# La región vacía no es "desconocida": se puede DEDUCIR de la provincia
# (conocimiento previo del dominio). Lo resolvemos en el ETL (3.1).

## 2.2 Cardinalidad: ¿qué columnas son claves o casi claves? -------------
#  Prueba rápida: un atributo con casi tantos valores como filas es una
#  clave candidata (no sirve para predecir, ver 5.4 y 6.2).
cardinalidad <- data.frame(atributo = names(fuente),
                           valores_distintos = sapply(fuente, n_distinct),
                           row.names = NULL) %>%
  mutate(pct_de_filas = round(100 * valores_distintos / nrow(fuente), 1)) %>%
  arrange(desc(valores_distintos))
cardinalidad

## 2.3 Los problemas de la fuente -----------------------------------------
table(fuente$producto)
table(fuente$region)            # PROBLEMA 1: región vacía en muchas filas
sort(table(fuente$empresabandera), decreasing = TRUE)
table(fuente$provincia)         # PROBLEMA 2: nombres sin tildes, "CAPITAL FEDERAL"

# PROBLEMA 3: precios desde $7 hasta $3000. ¿Por qué?
summary(fuente$precio)

# Cada estación informa cuando cambia su precio: hay registros de 2016 en
# adelante. Con la inflación, mezclar años no tiene sentido.
fuente$anio <- as.integer(substr(fuente$fecha_vigencia, 1, 4))
fuente %>%
  filter(producto == SUPER) %>%
  group_by(anio) %>%
  summarise(registros = n(), precio_mediano = median(precio))

# La fuente externa: nombres oficiales de provincias
georef
# PROBLEMA 4: miren el centroide de Tierra del Fuego (latitud -82: ¡la Antártida!)

## 2.4 Datos anómalos (outliers) -----------------------------------------
#  Regla del rango intercuartílico (IQR) en el año más reciente.
#  OJO: un valor anómalo puede ser un ERROR o un valor correcto pero
#  DISTINTO. Acá vamos a eliminar los errores de carga (3.1), pero en 6.9
#  vamos a BUSCAR a propósito las estaciones anómalas: en fraude o fallas,
#  los eventos raros son los más interesantes.
reciente <- fuente %>% filter(anio == max(anio))
reciente %>%
  group_by(producto) %>%
  summarise(q1 = quantile(precio, 0.25), q3 = quantile(precio, 0.75),
            lim_inf = q1 - 1.5 * (q3 - q1), lim_sup = q3 + 1.5 * (q3 - q1),
            atipicos = sum(precio < lim_inf | precio > lim_sup),
            registros = n())

## 2.5 Visualización PREVIA -----------------------------------------------
#  Aprovechar la capacidad humana de ver patrones antes de minar.
g_cajas <- ggplot(reciente, aes(producto, precio)) +
  geom_boxplot(outlier.colour = "red") +
  coord_flip() +
  labs(title = paste("Precios informados en", max(fuente$anio), "(sin limpiar)"),
       subtitle = "Los puntos rojos son atípicos según la regla del IQR",
       x = NULL, y = "$ por litro (GNC: $ por m3)")
mostrar(g_cajas, "v2_1_cajas_precio_producto")

super_reciente <- reciente %>% filter(producto == SUPER)
g_mapa <- ggplot(super_reciente, aes(longitud, latitud, color = precio)) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_viridis_c(limits = quantile(super_reciente$precio, c(0.02, 0.98)),
                        oob = scales::squish) +
  coord_quickmap() +
  labs(title = "Nafta súper: cada punto es una estación",
       subtitle = "¿Se ve algún patrón geográfico antes de minar?",
       x = "Longitud", y = "Latitud", color = "$ / litro")
mostrar(g_mapa, "v2_2_mapa_precios")


pausa("3. ETL: DISEÑO, TRANSFORMACIÓN Y CARGA")

# ---------------------------------------------------------------------
# 3. ETL: DISEÑO, TRANSFORMACIÓN Y CARGA ----
# ---------------------------------------------------------------------

## 3.0 Diseño: las cuatro decisiones (antes de escribir código) ---------
#  "La decisión clave es definir con precisión qué significa cada
#  registro antes de construir el modelo."
#  Implementación: ROLAP (tablas relacionales en SQLite, consultas SQL)
#  con esquema en ESTRELLA. En 3.4 lo comparamos con el COPO DE NIEVE.
diseno <- data.frame(
  decision  = c("1. Proceso", "2. Grano", "3. Dimensiones", "4. Medidas"),
  respuesta = c("Precios informados por las estaciones de servicio (Res. 314/2016)",
                "Un precio de UN producto, en UNA estación, en UN horario, en UN mes",
                paste("Tiempo (mes > trimestre > semestre > año), Lugar (localidad >",
                      "provincia > región), Producto (producto > tipo), Bandera, Estación"),
                paste("precio: NO aditiva (se agrega con mediana o promedio);",
                      "registros: aditiva (se puede sumar)")))
print(diseno, right = FALSE)

## 3.1 Limpieza ------------------------------------------------------------
datos <- fuente %>%
  distinct() %>%                                   # duplicados exactos
  filter(!is.na(precio), precio > 0)

# a) Completar la región vacía y hacerla CONSISTENTE: en un almacén la
#    jerarquía provincia -> región debe ser única (en la fuente hay, p. ej.,
#    estaciones de Buenos Aires marcadas como PATAGONIA). Tomamos para cada
#    provincia la región más frecuente.
region_por_provincia <- datos %>%
  filter(region != "") %>%
  count(provincia, region) %>%
  group_by(provincia) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(provincia, region_ok = region)
region_por_provincia

datos <- datos %>%
  left_join(region_por_provincia, by = "provincia") %>%
  mutate(region = region_ok) %>%
  select(-region_ok)
table(datos$region, useNA = "ifany")

# b) Valores atípicos: precios ridículamente bajos o altos dentro del mismo
#    año y producto (errores de carga). No los tiramos sin más: quedan
#    registrados para auditoría (metadatos de calidad).
datos <- datos %>%
  group_by(anio, producto) %>%
  mutate(mediana = median(precio),
         atipico = precio <= 0.3 * mediana | precio >= 3 * mediana) %>%
  ungroup()
descartados <- datos %>% filter(atipico) %>% select(-mediana, -atipico)
datos <- datos %>% filter(!atipico) %>% select(-mediana, -atipico)
nrow(descartados)
head(descartados %>% select(empresa, provincia, producto, precio, fecha_vigencia))

# c) Respetar el GRANO: un solo precio por estación, producto, horario y
#    mes. Si una estación informó dos veces en el mes, queda el último.
datos <- datos %>% mutate(mes_txt = substr(fecha_vigencia, 1, 7))
duplicados_grano <- datos %>%
  count(idempresa, producto, tipohorario, mes_txt) %>%
  filter(n > 1) %>% nrow()
duplicados_grano
datos <- datos %>%
  arrange(desc(fecha_vigencia)) %>%
  distinct(idempresa, producto, tipohorario, mes_txt, .keep_all = TRUE)

## 3.2 Integración con la fuente externa (Georef) -------------------------
normalizar <- function(x) {
  x <- toupper(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
  trimws(gsub("[^A-Z ]", "", x))
}
georef <- georef %>%
  mutate(clave = normalizar(provincia_nombre),
         clave = case_when(
           clave == "CIUDAD AUTONOMA DE BUENOS AIRES" ~ "CAPITAL FEDERAL",
           grepl("^TIERRA DEL FUEGO", clave)          ~ "TIERRA DEL FUEGO",
           TRUE ~ clave),
         # El centroide oficial de TDF incluye el sector antártico: lo
         # reemplazamos por el de la isla (Ushuaia / Río Grande).
         provincia_centroide_lat = ifelse(clave == "TIERRA DEL FUEGO", -54.3,
                                          provincia_centroide_lat),
         provincia_nombre = ifelse(clave == "TIERRA DEL FUEGO", "Tierra del Fuego",
                                   provincia_nombre))

datos$clave <- normalizar(datos$provincia)
setdiff(unique(datos$clave), georef$clave)   # debe dar vacío: todo integró

## 3.3 Dimensiones (con claves NUEVAS, propias del almacén) ----------------
#  "Creación de claves y referencias consistentes": el almacén no depende
#  de los códigos de la fuente.

# Dimensión TIEMPO: mes -> trimestre -> semestre -> año
dim_tiempo <- datos %>%
  distinct(mes_txt) %>%
  arrange(mes_txt) %>%
  mutate(id_tiempo = row_number(),
         anio      = as.integer(substr(mes_txt, 1, 4)),
         mes       = as.integer(substr(mes_txt, 6, 7)),
         trimestre = paste0("T", (mes - 1) %/% 3 + 1),
         semestre  = paste0("S", (mes - 1) %/% 6 + 1)) %>%
  select(id_tiempo, mes_txt, mes, trimestre, semestre, anio)

# Dimensión LUGAR: localidad -> provincia -> región (+ datos externos)
dim_lugar <- datos %>%
  distinct(localidad, clave, region) %>%
  left_join(georef %>% select(clave, provincia = provincia_nombre,
                              provincia_lat = provincia_centroide_lat),
            by = "clave") %>%
  mutate(id_lugar = row_number()) %>%
  select(id_lugar, localidad, provincia, region, provincia_lat, clave)

# Dimensión PRODUCTO: producto -> tipo de combustible
dim_producto <- datos %>%
  distinct(producto) %>%
  mutate(id_producto = row_number(),
         tipo = case_when(grepl("Nafta",   producto) ~ "Nafta",
                          grepl("Gas Oil", producto) ~ "Gasoil",
                          TRUE                       ~ "GNC")) %>%
  select(id_producto, producto, tipo)

# Dimensión BANDERA (marca): YPF, Shell, Axion, blancas...
dim_bandera <- datos %>%
  distinct(bandera = empresabandera) %>%
  mutate(id_bandera = row_number(),
         es_ypf     = bandera == "YPF")

# Dimensión ESTACIÓN (nueva): conserva la clave de la fuente (idempresa)
# como atributo y agrega la ubicación exacta de cada estación.
dim_estacion <- datos %>%
  group_by(idempresa) %>%
  summarise(empresa = first(empresa), direccion = first(direccion),
            est_lat = first(latitud), est_lon = first(longitud)) %>%
  mutate(id_estacion = row_number()) %>%
  select(id_estacion, idempresa, empresa, direccion, est_lat, est_lon)

## 3.4 Tabla de HECHOS ------------------------------------------------------
#  Hecho: "un precio informado por una estación". Medidas: precio y registros.
hechos_precios <- datos %>%
  left_join(dim_tiempo   %>% select(id_tiempo, mes_txt),            by = "mes_txt") %>%
  left_join(dim_lugar    %>% select(id_lugar, localidad, clave, region),
            by = c("localidad", "clave", "region")) %>%
  left_join(dim_producto %>% select(id_producto, producto),         by = "producto") %>%
  left_join(dim_bandera  %>% select(id_bandera, bandera),
            by = c("empresabandera" = "bandera")) %>%
  left_join(dim_estacion %>% select(id_estacion, idempresa),        by = "idempresa") %>%
  transmute(id_tiempo, id_lugar, id_producto, id_bandera, id_estacion,
            horario = tipohorario, precio, registros = 1L)

dim_lugar <- dim_lugar %>% select(-clave)   # la clave de integración ya no hace falta
head(hechos_precios)
colSums(is.na(hechos_precios))              # control de calidad: sin claves huérfanas

## 3.5 Estrella vs. COPO DE NIEVE --------------------------------------------
#  Estrella: la dimensión lugar repite provincia y región en cada localidad
#  (desnormalizada, consultas simples). Copo de nieve: se normaliza en
#  localidad -> provincia (más tablas, más joins, menos redundancia).
cn_dim_provincia <- dim_lugar %>%
  distinct(provincia, region, provincia_lat) %>%
  mutate(id_provincia = row_number())
cn_dim_localidad <- dim_lugar %>%
  left_join(cn_dim_provincia, by = c("provincia", "region", "provincia_lat")) %>%
  select(id_lugar, localidad, id_provincia)
nrow(dim_lugar); nrow(cn_dim_localidad); nrow(cn_dim_provincia)

## 3.6 Segundo DATAMART: oferta de cada estación -----------------------------
#  No todo cabe en una sola estrella. Otro proceso, otro grano:
#  "una estación de servicio y los productos que ofrece" (a la fecha de su
#  último informe). Comparte dimensiones con el datamart de precios: juntos
#  forman una CONSTELACIÓN. La dimensión común es, como siempre, el TIEMPO.
id_premium <- dim_producto$id_producto[dim_producto$producto == PREMIUM]
hechos_oferta <- hechos_precios %>%
  filter(horario == "Diurno") %>%
  left_join(dim_producto %>% select(id_producto, tipo), by = "id_producto") %>%
  group_by(id_estacion, id_lugar, id_bandera) %>%
  summarise(id_tiempo      = max(id_tiempo),            # último informe
            n_productos    = n_distinct(id_producto),
            ofrece_gnc     = as.integer(any(tipo == "GNC")),
            ofrece_premium = as.integer(any(id_producto == id_premium))) %>%
  ungroup()
head(hechos_oferta)

## 3.7 CARGA, índices y controles de calidad ---------------------------------
if (file.exists(archivo_almacen)) file.remove(archivo_almacen)
almacen <- dbConnect(SQLite(), archivo_almacen)

dbWriteTable(almacen, "dim_tiempo",       dim_tiempo)
dbWriteTable(almacen, "dim_lugar",        dim_lugar)
dbWriteTable(almacen, "dim_producto",     dim_producto)
dbWriteTable(almacen, "dim_bandera",      dim_bandera)
dbWriteTable(almacen, "dim_estacion",     dim_estacion)
dbWriteTable(almacen, "hechos_precios",   hechos_precios)
dbWriteTable(almacen, "hechos_oferta",    hechos_oferta)
dbWriteTable(almacen, "cn_dim_provincia", cn_dim_provincia)
dbWriteTable(almacen, "cn_dim_localidad", cn_dim_localidad)
dbWriteTable(almacen, "auditoria_atipicos", descartados)

dbExecute(almacen, "CREATE INDEX ix_tiempo   ON hechos_precios(id_tiempo)")
dbExecute(almacen, "CREATE INDEX ix_lugar    ON hechos_precios(id_lugar)")
dbExecute(almacen, "CREATE INDEX ix_producto ON hechos_precios(id_producto)")
dbExecute(almacen, "CREATE INDEX ix_bandera  ON hechos_precios(id_bandera)")
dbExecute(almacen, "CREATE INDEX ix_estacion ON hechos_precios(id_estacion)")

# Pruebas de calidad: el ETL no termina hasta que pasan
coordenada_mala <- with(dim_estacion, is.na(est_lat) | is.na(est_lon) |
                          !between(est_lat, -56, -21) | !between(est_lon, -74, -53))
controles <- data.frame(
  control = c("Hechos sin claves huérfanas",
              "Todos los precios son positivos",
              "Cada provincia pertenece a UNA sola región",
              "Estaciones con coordenadas dentro de Argentina",
              "Grano respetado (sin duplicados)"),
  ok = c(sum(is.na(hechos_precios)) == 0,
         all(hechos_precios$precio > 0),
         all(count(distinct(dim_lugar, provincia, region), provincia)$n == 1),
         !any(coordenada_mala),
         !anyDuplicated(hechos_precios[c("id_tiempo", "id_estacion", "id_producto", "horario")])))
controles
# Si un control falla, se investiga en lugar de seguir. Por ejemplo, las
# estaciones con coordenadas faltantes o fuera del país:
dim_estacion %>%
  filter(coordenada_mala) %>%
  left_join(hechos_precios %>% group_by(id_estacion) %>%
              summarise(ultimo_informe = max(id_tiempo)), by = "id_estacion") %>%
  left_join(dim_tiempo %>% select(ultimo_informe = id_tiempo, mes_txt), by = "ultimo_informe") %>%
  select(idempresa, empresa, est_lat, est_lon, ultimo_informe = mes_txt)
# Son muy pocas. Las dejamos en el almacén (es histórico) y lo
# documentamos; en la vista minable (5.1) quedan afuera, porque k vecinos
# y la detección de anomalías necesitan las coordenadas.

## 3.8 METADATOS ------------------------------------------------------------
#  "Mantenimiento de metadatos": qué tablas hay, de dónde salen, cuántas
#  filas tienen y cuándo se cargaron. Es lo primero que mira un analista.
descripciones <- c(
  dim_tiempo         = "Dimensión tiempo: mes > trimestre > semestre > año",
  dim_lugar          = "Dimensión lugar (estrella): localidad > provincia > región",
  dim_producto       = "Dimensión producto: producto > tipo",
  dim_bandera        = "Dimensión bandera (marca)",
  dim_estacion       = "Dimensión estación: empresa, dirección, coordenadas",
  hechos_precios     = "Datamart de precios. Grano: estación x producto x horario x mes",
  hechos_oferta      = "Datamart de oferta. Grano: una estación (último informe)",
  cn_dim_provincia   = "Copo de nieve: provincias",
  cn_dim_localidad   = "Copo de nieve: localidades (apunta a cn_dim_provincia)",
  auditoria_atipicos = "Registros descartados por precio atípico (control de calidad)",
  mm_predicciones    = "Minería: probabilidad de ser CARA por estación y producto",
  mm_modelos         = "Minería: comparación de modelos",
  mm_reglas_arbol    = "Minería: reglas del árbol de decisión",
  mm_reglas_asociacion = "Minería: reglas de asociación entre características",
  mm_anomalias       = "Minería: estaciones con comportamiento anómalo")
registrar_metadatos <- function() {
  tablas <- setdiff(dbListTables(almacen), "meta_tablas")
  meta <- data.frame(
    tabla       = tablas,
    filas       = sapply(tablas, function(t) dbGetQuery(almacen, paste("SELECT COUNT(*) AS n FROM", t))$n),
    descripcion = unname(ifelse(tablas %in% names(descripciones), descripciones[tablas], "")),
    fuente      = "datos.energia.gob.ar + apis.datos.gob.ar/georef",
    fecha_carga = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    row.names   = NULL)
  dbWriteTable(almacen, "meta_tablas", meta, overwrite = TRUE)
  meta
}
registrar_metadatos()
# -> Abrir almacen_combustibles_v2.sqlite con DB Browser for SQLite: dos
#    tablas de hechos que comparten dimensiones (constelación).

# Vista plana del cubo para trabajar con dplyr
cubo <- dbGetQuery(almacen, "
  SELECT t.anio, t.trimestre, t.mes_txt, t.mes,
         l.localidad, l.provincia, l.region, l.provincia_lat,
         p.producto, p.tipo,
         b.bandera, b.es_ypf,
         e.idempresa, e.empresa, e.est_lat, e.est_lon,
         h.horario, h.id_estacion, h.precio
  FROM hechos_precios h
  JOIN dim_tiempo   t ON h.id_tiempo   = t.id_tiempo
  JOIN dim_lugar    l ON h.id_lugar    = l.id_lugar
  JOIN dim_producto p ON h.id_producto = p.id_producto
  JOIN dim_bandera  b ON h.id_bandera  = b.id_bandera
  JOIN dim_estacion e ON h.id_estacion = e.id_estacion")
str(cubo)


pausa("4. EXPLOTACIÓN OLAP")

# ---------------------------------------------------------------------
# 4. OPERADORES OLAP ----
#    OLAP es DEDUCTIVO: el usuario propone una hipótesis y la verifica
#    navegando el cubo. Los operadores son refinadores de una consulta
#    ya hecha.
#    OJO: el precio es una medida NO ADITIVA. No tiene sentido "sumar
#    precios": agregamos con la MEDIANA (o el promedio).
# ---------------------------------------------------------------------

## 4.0 Consulta inicial: precio mediano por AÑO y TIPO de combustible ------
cubo %>%
  group_by(anio, tipo) %>%
  summarise(precio = median(precio), registros = n(), .groups = "drop") %>%
  pivot_wider(names_from = tipo, values_from = c(precio, registros))
# La dimensión tiempo muestra la inflación: para comparar lugares y marcas
# hay que quedarse con UN período -> SLICE sobre el año más reciente.

anio_actual <- max(cubo$anio)
actual <- cubo %>% filter(anio == anio_actual)          # SLICE
nrow(actual)

pausa("4.1 Región x Producto (vista tabular)")
## 4.1 Región x Producto (vista tabular) -----------------------------------
consulta <- actual %>%
  group_by(region, producto) %>%
  summarise(precio = median(precio), .groups = "drop")
consulta

pausa("4.2 PIVOT: la misma información como matriz")
## 4.2 PIVOT: la misma información como matriz -----------------------------
consulta %>% pivot_wider(names_from = producto, values_from = precio)

pausa("4.3 ROLL-UP: región -> todo el país")
## 4.3 ROLL-UP: región -> todo el país ---------------------------------------
#  Con una medida NO aditiva el roll-up se recalcula desde el DETALLE:
#  la mediana de las medianas no es la mediana.
actual %>%
  group_by(producto) %>%
  summarise(precio_pais = median(precio)) %>%
  left_join(consulta %>% group_by(producto) %>%
              summarise(mediana_de_medianas = median(precio)), by = "producto")

pausa("4.4 DRILL-DOWN: región -> provincia (dentro de la Patagonia)")
## 4.4 DRILL-DOWN: región -> provincia (dentro de la Patagonia) -------------
actual %>%
  filter(region == "PATAGONIA", tipo == "Nafta") %>%
  group_by(provincia, producto) %>%
  summarise(precio = median(precio), estaciones = n_distinct(id_estacion),
            .groups = "drop") %>%
  arrange(producto, precio)
# ¿Por qué la Patagonia es más barata? Hay beneficios impositivos para la
# zona sur: un dato EXTERNO que no está en la fuente. Sin él, el patrón
# se ve pero no se explica.

pausa("4.5 DRILL-ACROSS: agregar la dimensión BANDERA")
## 4.5 DRILL-ACROSS: agregar la dimensión BANDERA --------------------------
#  Drill-DOWN baja dentro de una dimensión que ya estaba en la consulta.
#  Drill-ACROSS agrega una dimensión que NO estaba. ¿YPF es más barata?
banderas <- actual %>%
  filter(producto == SUPER, bandera %in% BANDERAS_GRANDES) %>%
  group_by(region, bandera) %>%
  summarise(precio = median(precio), .groups = "drop")

banderas %>% pivot_wider(names_from = bandera, values_from = precio)

g1 <- ggplot(banderas, aes(region, precio, fill = bandera)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(min(banderas$precio) * 0.9, max(banderas$precio) * 1.02)) +
  labs(title = paste("Nafta súper por región y bandera -", anio_actual),
       y = "$ / litro (mediana)", x = NULL)
mostrar(g1, "v2_3_banderas_por_region")

pausa("4.6 ROLL-ACROSS: quitar la dimensión REGIÓN")
## 4.6 ROLL-ACROSS: quitar una dimensión de la consulta --------------------
actual %>%
  filter(producto == SUPER, bandera %in% BANDERAS_GRANDES) %>%
  group_by(bandera) %>%
  summarise(precio_pais = median(precio), estaciones = n_distinct(id_estacion)) %>%
  arrange(precio_pais)

pausa("4.7 SLICE & DICE: un trozo del cubo")
## 4.7 SLICE & DICE: un "trozo" del cubo ------------------------------------
#  Selecciona y proyecta: no agrega ni desagrega.
actual %>%
  filter(provincia == "Córdoba",                    # dice
         tipo == "Gasoil",
         horario == "Diurno") %>%
  group_by(bandera, producto) %>%
  summarise(precio = median(precio), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = producto, values_from = c(precio, n))

pausa("4.8 ROLAP: lo mismo en SQL contra el almacén")
## 4.8 ROLAP: lo mismo en SQL contra el almacén -----------------------------
#      (SQLite no tiene MEDIAN: usamos AVG)
dbGetQuery(almacen, sprintf("
  SELECT l.region, p.tipo, ROUND(AVG(h.precio), 0) AS precio_promedio,
         SUM(h.registros) AS registros
  FROM hechos_precios h
  JOIN dim_lugar    l ON h.id_lugar    = l.id_lugar
  JOIN dim_producto p ON h.id_producto = p.id_producto
  JOIN dim_tiempo   t ON h.id_tiempo   = t.id_tiempo
  WHERE t.anio = %d
  GROUP BY l.region, p.tipo
  ORDER BY p.tipo, precio_promedio DESC", anio_actual))
# "registros" sí es aditiva: SUM tiene sentido. "precio" no.

pausa("4.9 Estrella vs. copo de nieve: la misma consulta")
## 4.9 Estrella vs. copo de nieve -------------------------------------------
consulta_estrella <- "
  SELECT l.region, COUNT(*) AS registros
  FROM hechos_precios h
  JOIN dim_lugar l ON h.id_lugar = l.id_lugar
  GROUP BY l.region ORDER BY l.region"
consulta_copo <- "
  SELECT pr.region, COUNT(*) AS registros
  FROM hechos_precios h
  JOIN cn_dim_localidad lo ON h.id_lugar      = lo.id_lugar
  JOIN cn_dim_provincia pr ON lo.id_provincia = pr.id_provincia
  GROUP BY pr.region ORDER BY pr.region"
system.time(r_estrella <- dbGetQuery(almacen, consulta_estrella))
system.time(r_copo     <- dbGetQuery(almacen, consulta_copo))
all.equal(r_estrella, r_copo)   # mismo resultado, un JOIN más
# Con estos volúmenes la diferencia es mínima; con millones de filas el
# costo de los JOIN del copo de nieve se nota (inconveniente del ROLAP).

pausa("4.10 Constelación: cruzar los dos datamarts")
## 4.10 DRILL-ACROSS entre datamarts -----------------------------------------
#  ¿Las regiones con poca oferta de GNC tienen GNC más caro?
oferta_region <- dbGetQuery(almacen, "
  SELECT l.region, COUNT(*) AS estaciones,
         ROUND(100.0 * AVG(o.ofrece_gnc), 1) AS pct_con_gnc,
         ROUND(AVG(o.n_productos), 2)        AS productos_promedio
  FROM hechos_oferta o
  JOIN dim_lugar l ON o.id_lugar = l.id_lugar
  GROUP BY l.region")
oferta_region %>%
  left_join(actual %>% filter(tipo == "GNC") %>%
              group_by(region) %>% summarise(precio_gnc = median(precio)),
            by = "region") %>%
  arrange(pct_con_gnc)

pausa("4.11 El aporte de la fuente EXTERNA: precio vs. latitud")
## 4.11 El aporte de la fuente EXTERNA: precio vs. latitud -------------------
por_provincia <- actual %>%
  filter(producto == SUPER) %>%
  group_by(provincia, region, provincia_lat) %>%
  summarise(precio = median(precio), .groups = "drop")

g2 <- ggplot(por_provincia, aes(provincia_lat, precio, label = provincia, color = region)) +
  geom_point(size = 3) +
  geom_text(size = 3, vjust = -0.8, show.legend = FALSE) +
  labs(title = "Nafta súper: precio mediano por provincia según latitud",
       x = "Latitud del centroide (fuente: API Georef)", y = "$ / litro")
mostrar(g2, "v2_4_precio_vs_latitud")

pausa("4.12 Usuarios del almacén: picapedreros y exploradores")
## 4.12 Picapedreros y exploradores ------------------------------------------
#  PICAPEDREROS ("granjeros"): repiten informes periódicos con consultas
#  conocidas. Por ejemplo, el tablero mensual de precios por región:
informe_periodico <- function(tipo_combustible) {
  dbGetQuery(almacen, sprintf("
    SELECT t.mes_txt, l.region, ROUND(AVG(h.precio), 0) AS precio_promedio
    FROM hechos_precios h
    JOIN dim_tiempo   t ON h.id_tiempo   = t.id_tiempo
    JOIN dim_lugar    l ON h.id_lugar    = l.id_lugar
    JOIN dim_producto p ON h.id_producto = p.id_producto
    WHERE p.tipo = '%s' AND t.anio = %d
    GROUP BY t.mes_txt, l.region", tipo_combustible, anio_actual)) %>%
    pivot_wider(names_from = region, values_from = precio_promedio) %>%
    arrange(mes_txt)
}
informe_periodico("Nafta")
#  EXPLORADORES: buscan patrones que nadie pidió, con OLAP y minería.
#  Eso es lo que sigue: OLAP verifica hipótesis (deductivo); la minería
#  las ENCUENTRA (inductivo). Son complementarios.


pausa("5. LA VISTA MINABLE")

# ---------------------------------------------------------------------
# 5. SELECCIÓN, LIMPIEZA Y TRANSFORMACIÓN: LA VISTA MINABLE ----
#    La calidad del conocimiento no depende solo del algoritmo: depende
#    de los datos que le damos. La mayoría de los métodos trabaja sobre
#    UNA sola tabla atributo-valor: la VISTA MINABLE.
#
#    Las cuatro preguntas antes de minar, para nuestro caso:
#     1. ¿Qué datos son pertinentes?    Precios diurnos del año actual,
#        a nivel estación x producto.
#     2. ¿Qué conocimiento y cómo?       Clasificación (CARO / BARATO) con
#        modelos comprensibles (reglas), más regresión, agrupamiento,
#        asociación y anomalías.
#     3. ¿Qué es válido e interesante?   Que acierte con estaciones que
#        el modelo no vio (test) y que diga algo que no sabíamos.
#     4. ¿Qué conocimiento previo?       Jerarquías OLAP (localidad >
#        provincia > región), beneficios impositivos de la zona sur,
#        inflación mensual.
# ---------------------------------------------------------------------

## 5.1 Construcción: concatenar, seleccionar, proyectar --------------------
#  Las operaciones típicas de SQL alcanzan. El JOIN DESNORMALIZA: ciudad y
#  región quedan en la misma tabla (y reaparecen dependencias conocidas).
vista_sql <- dbGetQuery(almacen, sprintf("
  SELECT e.idempresa AS id_estacion, e.empresa,
         e.est_lat AS lat, e.est_lon AS lon,
         l.localidad, l.provincia, l.region, l.provincia_lat,
         p.producto, p.tipo, b.bandera,
         t.mes, t.mes_txt, h.precio
  FROM hechos_precios h
  JOIN dim_tiempo   t ON h.id_tiempo   = t.id_tiempo
  JOIN dim_lugar    l ON h.id_lugar    = l.id_lugar
  JOIN dim_producto p ON h.id_producto = p.id_producto
  JOIN dim_bandera  b ON h.id_bandera  = b.id_bandera
  JOIN dim_estacion e ON h.id_estacion = e.id_estacion
  WHERE t.anio = %d AND h.horario = 'Diurno'
    AND e.est_lat IS NOT NULL AND e.est_lon IS NOT NULL
  ORDER BY l.region, l.provincia, l.localidad", anio_actual))
dim(vista_sql)

pausa("5.2 La clase a predecir: una ITERACIÓN del KDD")
## 5.2 El atributo CLASE y una vuelta atrás ---------------------------------
#  Primera idea (la de la clase anterior): CARO = precio por encima de la
#  mediana de su producto en el año.
vista <- vista_sql %>%
  group_by(producto) %>%
  mutate(caro_v1 = ifelse(precio > median(precio), "CARO", "BARATO")) %>%
  ungroup()

# ¿De qué depende "caro_v1"? Miremos el mes en que se informó el precio:
round(100 * prop.table(table(mes = vista$mes, vista$caro_v1), 1), 1)
# Los precios informados en los primeros meses son casi todos "BARATOS":
# no porque la estación sea barata, sino porque su precio está viejo y
# hubo inflación. Un modelo aprendería "si mes < 7 entonces BARATO":
# VÁLIDO pero NADA NOVEDOSO.
# -> Volvemos atrás (el KDD es iterativo) y redefinimos la clase:
#    CARO = por encima de la mediana de su producto EN EL MISMO MES.
vista <- vista %>%
  group_by(producto, mes_txt) %>%
  mutate(caro = factor(ifelse(precio > median(precio), "CARO", "BARATO"),
                       levels = c("BARATO", "CARO"))) %>%
  ungroup() %>%
  select(-caro_v1)
round(100 * prop.table(table(mes = vista$mes, vista$caro), 1), 1)
table(vista$caro)

pausa("5.3 Creación de características")
## 5.3 CREACIÓN DE CARACTERÍSTICAS ---------------------------------------
#  Los atributos originales no siempre tienen poder predictivo. El
#  conocimiento del dominio sugiere atributos NUEVOS:
#   - estaciones_localidad: competencia (atributo por AGREGACIÓN)
#   - n_productos, ofrece_gnc: tamaño de la estación (del datamart de oferta)
#   - zona_sur: beneficio impositivo, aprox. al sur del paralelo 42
#   - bandera_agr: generalización de la bandera (muchos valores -> 6)
competencia <- vista %>%
  distinct(id_estacion, localidad, provincia) %>%
  count(localidad, provincia, name = "estaciones_localidad")

oferta <- dbGetQuery(almacen, "
  SELECT e.idempresa AS id_estacion, o.n_productos, o.ofrece_gnc
  FROM hechos_oferta o JOIN dim_estacion e ON o.id_estacion = e.id_estacion") %>%
  group_by(id_estacion) %>%
  summarise(n_productos = max(n_productos), ofrece_gnc = max(ofrece_gnc))

#  Además, nombres CORTOS para productos y banderas: las reglas que salgan
#  del modelo tienen que poder leerse.
vista <- vista %>%
  left_join(competencia, by = c("localidad", "provincia")) %>%
  left_join(oferta, by = "id_estacion") %>%
  mutate(bandera_agr = factor(case_when(bandera == "SHELL C.A.P.S.A." ~ "SHELL",
                                        bandera %in% BANDERAS_GRANDES ~ bandera,
                                        bandera == "BLANCA" ~ "BLANCA",
                                        TRUE ~ "OTRAS"),
                              levels = c("YPF", "SHELL", "AXION", "PUMA", "BLANCA", "OTRAS")),
         zona_sur = si_no(lat < -42),
         producto = factor(corto[producto], levels = corto),
         region   = factor(region))
table(vista$producto)
vista %>% select(estaciones_localidad, n_productos, ofrece_gnc) %>% summary()
# Qué evitar: atributos que el modelo ya construye solo (un árbol ya
# compara lat < -42 si le sirve) y combinaciones redundantes.

pausa("5.4 Selección VERTICAL: claves y dependencias")
## 5.4 SELECCIÓN VERTICAL: eliminar atributos ------------------------------
# a) Claves candidatas: ¿tiene casi tantos valores como filas?
data.frame(valores = sapply(vista, n_distinct)) %>%
  mutate(pct_de_filas = round(100 * valores / nrow(vista), 1)) %>%
  arrange(desc(valores))
anyDuplicated(vista[c("id_estacion", "producto")])   # 0: juntos son la clave
# id_estacion y empresa identifican a la estación: un modelo que los use
# "memoriza" en lugar de aprender (lo comprobamos en 6.2).

# b) Atributos dependientes: con la provincia ya se conoce la región.
vista %>% distinct(provincia, region) %>% count(provincia) %>% filter(n > 1)   # vacío
vista %>% distinct(producto, tipo) %>% count(producto) %>% filter(n > 1)       # vacío
# Conservamos UN nivel de detalle por jerarquía: región + coordenadas de la
# estación. provincia, provincia_lat, localidad y tipo quedan afuera.

# c) Fuga de información: "precio" define la clase. Si queda como
#    entrada, el modelo acierta siempre... y no sirve para nada.

# d) Fecha exacta -> generalizada al MES (jerarquía de valor).

pausa("5.5 Transformación: discretizar, numerizar, normalizar")
## 5.5 TRANSFORMACIÓN DE ATRIBUTOS ----------------------------------------
# a) DISCRETIZACIÓN: de numérico a intervalos con nombre.
#    Igual ancho vs. igual frecuencia: ¿cuántas estaciones caen en cada uno?
table(igual_ancho      = cut(vista$lat, 3))
table(igual_frecuencia = cut(vista$lat, quantile(vista$lat, c(0, 1/3, 2/3, 1)),
                             include.lowest = TRUE))
#    Mejor aún: cortes con SENTIDO en el dominio.
vista <- vista %>%
  mutate(zona_lat      = cut(lat, c(-Inf, -38, -30, Inf), labels = c("sur", "centro", "norte")),
         periodo       = cut(mes, c(0, 6, 8, 12), labels = c("ene-jun", "jul-ago", "sep-dic")),
         competencia   = cut(estaciones_localidad, c(0, 1, 5, Inf),
                             labels = c("única", "baja (2-5)", "alta (6+)")),
         n_productos_f = factor(n_productos),
         ofrece_gnc_f  = si_no(ofrece_gnc == 1))
table(vista$zona_lat, vista$region)

# b) NUMERIZACIÓN "1 a n": un nominal con n valores -> n columnas 0/1
#    (lo necesitan los métodos basados en distancias, como k vecinos).
head(model.matrix(~ region - 1, vista))

# c) NORMALIZACIÓN: llevar todo a media 0 y desvío 1 para que la latitud
#    (decenas) no pese lo mismo que la competencia (cientos).
numericas <- c("lat", "lon", "mes", "estaciones_localidad", "n_productos")
sapply(vista[numericas], function(x) round(c(media = mean(x), desvio = sd(x)), 2))
escaladas <- scale(vista[numericas])
round(rbind(media = colMeans(escaladas), desvio = apply(escaladas, 2, sd)), 2)

pausa("5.6 Análisis correlacional por provincia")
## 5.6 ANÁLISIS CORRELACIONAL --------------------------------------------
#  Perfil de cada provincia: precio mediano de cada producto + contexto.
perfil_provincia <- vista %>%
  mutate(prod = as.character(producto)) %>%
  group_by(provincia, prod) %>%
  summarise(precio = median(precio)) %>%
  pivot_wider(names_from = prod, values_from = precio) %>%
  left_join(vista %>% distinct(id_estacion, .keep_all = TRUE) %>%
              group_by(provincia) %>%
              summarise(lat_media         = mean(lat),
                        competencia_media = mean(estaciones_localidad),
                        pct_blancas       = 100 * mean(bandera_agr == "BLANCA"),
                        pct_con_gnc       = 100 * mean(ofrece_gnc)),
            by = "provincia") %>%
  ungroup()
perfil_provincia

round(cor(perfil_provincia[, -1], use = "pairwise.complete.obs"), 2)
# Correlaciones muy altas entre los precios de los distintos productos:
# hay REDUNDANCIA. Es el escenario ideal para el PCA.

pausa("5.7 Componentes principales (PCA)")
## 5.7 REDUCCIÓN DE DIMENSIONALIDAD: PCA ----------------------------------
#  Nuevos ejes independientes, ordenados por relevancia.
#  Normalizamos (scale. = TRUE): el GNC se mide en $/m3 y el resto en
#  $/litro. Si todo estuviera en la misma unidad, convendría NO escalar.
precios_prov <- perfil_provincia %>%
  select(provincia, super, premium, gasoil2, gasoil3, gnc) %>%
  filter(complete.cases(.))
setdiff(perfil_provincia$provincia, precios_prov$provincia)   # sin GNC: quedan afuera

pca <- prcomp(precios_prov[, -1], center = TRUE, scale. = TRUE)
autovalores <- pca$sdev^2
tabla_pca <- data.frame(componente = paste0("CP", seq_along(autovalores)),
                        autovalor  = round(autovalores, 2),
                        pct        = round(100 * autovalores / sum(autovalores), 1),
                        pct_acum   = round(100 * cumsum(autovalores) / sum(autovalores), 1))
tabla_pca
# ¿Con cuántas componentes nos quedamos?
which(tabla_pca$pct_acum >= 75)[1]    # criterio de la varianza acumulada (75 %)
sum(autovalores > 1)                  # criterio del autovalor mayor que la media (1)
round(pca$rotation, 2)                # cómo se arma cada componente

g_scree <- ggplot(tabla_pca, aes(componente, autovalor, group = 1)) +
  geom_line() + geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Scree diagram: cortar donde la pendiente cambia bruscamente",
       subtitle = "Línea punteada: criterio del autovalor mayor que 1",
       x = NULL, y = "Autovalor")
mostrar(g_scree, "v2_5_scree_pca")

proyeccion <- data.frame(provincia = precios_prov$provincia, pca$x[, 1:2])
g_pca <- ggplot(proyeccion, aes(PC1, PC2, label = provincia)) +
  geom_point(size = 3) +
  geom_text(size = 3, vjust = -0.8) +
  labs(title = "Provincias en el plano de las dos primeras componentes",
       subtitle = paste0("CP1 + CP2 explican el ", tabla_pca$pct_acum[2],
                         " % de la variabilidad de los 5 precios"))
mostrar(g_pca, "v2_6_pca_provincias")
# Limitación: "CP1" no es una variable que un experto reconozca. Por eso,
# para modelos comprensibles, se prefiere SELECCIONAR antes que TRANSFORMAR.

pausa("5.8 Selección HORIZONTAL: muestreo")
## 5.8 SELECCIÓN HORIZONTAL: MUESTREO ---------------------------------------
n_muestra <- 1000
k_sist    <- floor(nrow(vista) / n_muestra)
n_estrato <- round(n_muestra / n_distinct(vista$region))

muestras <- list(
  poblacion      = vista,
  aleatoria      = vista[sample(nrow(vista), n_muestra), ],
  sistematica    = vista[seq(sample(k_sist, 1), by = k_sist, length.out = n_muestra), ],
  los_n_primeros = head(vista, n_muestra),
  # Estratificada: la misma cantidad por región; si un estrato no
  # alcanza, se SOBREMUESTREA (con reemplazo).
  estratificada  = vista %>% group_by(region) %>%
    group_modify(~ .x[sample(nrow(.x), n_estrato, replace = nrow(.x) < n_estrato), ]) %>%
    ungroup())

sapply(muestras, function(m) round(100 * prop.table(table(m$region)), 1))
sapply(muestras, function(m) round(100 * mean(m$caro == "CARO"), 1))
# "Los n primeros" está sesgada: la vista viene ordenada por región.
# La estratificada equilibra los estratos (ojo: ya no es representativa
# de la población, pero da ejemplos suficientes de cada región).

pausa("5.9 Muestras pequeñas: Laplace y m-estimado")
## 5.9 ESTIMADOR DE LAPLACE Y m-ESTIMADO ----------------------------------
#  ¿Qué probabilidad de ser CARA tiene cada LOCALIDAD? Muchas tienen una
#  sola estación: la frecuencia cruda da valores extremos (0 % o 100 %).
#    Laplace:     (casos + 1) / (n + número de alternativas)
#    m-estimado:  (casos + m * priori) / (n + m)
p_priori <- mean(vista$caro == "CARO")
m <- 2
por_localidad <- vista %>%
  group_by(provincia, localidad) %>%
  summarise(n = n(), caras = sum(caro == "CARO")) %>%
  ungroup() %>%
  mutate(frecuencia = round(caras / n, 3),
         laplace    = round((caras + 1) / (n + 2), 3),
         m_estimado = round((caras + m * p_priori) / (n + m), 3))
table(n_registros = cut(por_localidad$n, c(0, 2, 5, 20, Inf)))
por_localidad %>% filter(n <= 2) %>% arrange(desc(frecuencia)) %>% head(6)
por_localidad %>% arrange(desc(n)) %>% head(4)
# Con n grande las tres coinciden; con n chico, la corrección evita
# afirmar "en esta localidad SIEMPRE es caro" con 1 o 2 casos.

pausa("5.10 Partición entrenamiento / test y selección de atributos")
## 5.10 PARTICIÓN Y SELECCIÓN DE ATRIBUTOS -------------------------------
#  Separamos un conjunto de TEST que no se toca hasta evaluar. Partimos
#  por ESTACIÓN, no por fila: si los 5 productos de una estación quedaran
#  repartidos, el modelo "reconocería" a la estación en el test.
particion <- function(datos, prop_test = 0.3) {
  est <- unique(datos$id_estacion)
  en_test <- sample(est, round(prop_test * length(est)))
  list(ent  = datos[!datos$id_estacion %in% en_test, ],
       test = datos[ datos$id_estacion %in% en_test, ])
}
p <- particion(vista)
entrenamiento <- p$ent
test          <- p$test
c(entrenamiento = nrow(entrenamiento), test = nrow(test))

# a) Método de FILTRO: medida estadística, independiente del modelo.
#    Ji-cuadrado y V de Cramér (0 = independiente, 1 = asociación perfecta)
nominales <- c("producto", "region", "bandera_agr", "zona_lat", "periodo",
               "competencia", "n_productos_f", "ofrece_gnc_f", "zona_sur")
filtro <- do.call(rbind, lapply(nominales, function(a) {
  tab <- table(factor(entrenamiento[[a]]), entrenamiento$caro)
  chi <- suppressWarnings(chisq.test(tab))
  data.frame(atributo = a, chi2 = round(unname(chi$statistic), 1),
             p_valor  = format.pval(chi$p.value, digits = 2),
             V_cramer = round(sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1))), 3))
})) %>% arrange(desc(V_cramer))
filtro
# "producto" da V casi 0: era esperable, la clase se definió DENTRO de
# cada producto (mitad caras y mitad baratas en cada uno). Pero OJO: el
# filtro mira cada atributo SOLO. Combinado con otros, el producto puede
# servir (el wrapper lo va a mostrar).

# b) Método ENVOLVENTE (wrapper), hacia adelante (forward): se entrena un
#    árbol con cada subconjunto y se mide la precisión en VALIDACIÓN
#    (una parte del entrenamiento, nunca el test).
candidatos <- c("producto", "region", "bandera_agr", "lat", "lon", "mes",
                "estaciones_localidad", "n_productos", "ofrece_gnc", "zona_sur")
pv <- particion(entrenamiento)
evaluar_subconjunto <- function(atributos) {
  modelo <- rpart(reformulate(atributos, "caro"), data = pv$ent, method = "class",
                  control = rpart.control(cp = 0.005))
  precision(pv$test$caro, predict(modelo, pv$test, type = "class"))
}
seleccion <- c(); mejor <- 0; pasos <- list()
repeat {
  restantes <- setdiff(candidatos, seleccion)
  if (length(restantes) == 0) break
  puntajes <- sapply(restantes, function(a) evaluar_subconjunto(c(seleccion, a)))
  if (max(puntajes) <= mejor + 0.002) break          # ya no mejora
  seleccion <- c(seleccion, names(which.max(puntajes)))
  mejor <- max(puntajes)
  pasos[[length(pasos) + 1]] <- data.frame(paso = length(seleccion),
                                           agrega = tail(seleccion, 1),
                                           precision_validacion = round(mejor, 3))
}
do.call(rbind, pasos)
seleccion
# El método que selecciona no tiene por qué ser el que se usa al final.


pausa("6. MINERÍA DE DATOS: TAREAS Y MÉTODOS")

# ---------------------------------------------------------------------
# 6. MINERÍA DE DATOS ----
#    Tres decisiones: la TAREA (qué queremos), el tipo de MODELO y el
#    ALGORITMO (cómo lo obtenemos). Una tarea no es un método.
# ---------------------------------------------------------------------
data.frame(
  tarea     = c("Clasificación", "Clasificación", "Clasificación", "Clasificación",
                "Regresión", "Agrupamiento", "Reglas de asociación", "Anomalías"),
  modelo    = c("Árbol de decisión", "Probabilístico", "Sin modelo (por casos)",
                "Lineal generalizado", "Lineal", "Centroides", "Reglas SI-ENTONCES",
                "Distancias"),
  algoritmo = c("CART (rpart)", "Naive Bayes", "k vecinos más próximos",
                "Regresión logística", "Mínimos cuadrados (lm)", "K medias",
                "Conteo de frecuencias (tipo Apriori)", "Distancia a los k vecinos"),
  seccion   = c("6.1", "6.4", "6.5", "6.6", "6.6", "6.7", "6.8", "6.9"))

pausa("6.1 Clasificación: árbol de decisión (CART)")
## 6.1 CLASIFICACIÓN CON ÁRBOL DE DECISIÓN (CART, "divide y vencerás") -----
#  Dejamos crecer un árbol grande y después lo PODAMOS (navaja de Occam:
#  a igual capacidad, el modelo más simple). rpart calcula el error por
#  validación cruzada (xerror) para cada tamaño de árbol.
f_arbol <- reformulate(candidatos, "caro")
arbol_grande <- rpart(f_arbol, data = entrenamiento, method = "class",
                      control = rpart.control(cp = 0.001))
printcp(arbol_grande)

# Regla de 1 error estándar: el árbol más chico cuyo xerror no supera
# al mínimo + su desvío.
tabla_cp <- arbol_grande$cptable
limite   <- min(tabla_cp[, "xerror"]) + tabla_cp[which.min(tabla_cp[, "xerror"]), "xstd"]
cp_poda  <- tabla_cp[which(tabla_cp[, "xerror"] <= limite)[1], "CP"]
arbol    <- prune(arbol_grande, cp = cp_poda)

# Y uno COMPRENSIBLE: como en el caso de atención primaria, el experto
# tiene que poder leer y validar las reglas. Lo limitamos a 10 hojas.
cp_simple    <- tabla_cp[max(which(tabla_cp[, "nsplit"] <= 9)), "CP"]
arbol_simple <- prune(arbol_grande, cp = cp_simple)

pred_arbol  <- predict(arbol, test, type = "class")
pred_simple <- predict(arbol_simple, test, type = "class")
hojas <- function(modelo) sum(modelo$frame$var == "<leaf>")
data.frame(
  arbol = c("grande (cp = 0,001)", "podado (regla 1-ES)", "comprensible (10 hojas)"),
  reglas = c(hojas(arbol_grande), hojas(arbol), hojas(arbol_simple)),
  precision_entrenamiento = round(c(
    precision(entrenamiento$caro, predict(arbol_grande, entrenamiento, type = "class")),
    precision(entrenamiento$caro, predict(arbol, entrenamiento, type = "class")),
    precision(entrenamiento$caro, predict(arbol_simple, entrenamiento, type = "class"))), 3),
  precision_test = round(c(
    precision(test$caro, predict(arbol_grande, test, type = "class")),
    precision(test$caro, pred_arbol),
    precision(test$caro, pred_simple)), 3))
# ¿Cuánta precisión estamos dispuestos a resignar a cambio de un modelo
# que se entiende? Esa decisión es del usuario, no del algoritmo.

# Matriz de confusión del árbol podado (en test)
table(real = test$caro, predicho = pred_arbol)

# Importancia de cada atributo para el árbol
round(arbol$variable.importance / sum(arbol$variable.importance) * 100, 1)

# De ÁRBOL a REGLAS: cada rama es una regla SI ... ENTONCES
reglas_arbol <- function(modelo) {
  fr <- modelo$frame
  es_hoja <- fr$var == "<leaf>"
  nodos <- as.integer(rownames(fr)[es_hoja])
  caminos <- path.rpart(modelo, nodes = nodos, print.it = FALSE)
  k <- length(attr(modelo, "ylevels"))
  data.frame(
    regla     = sapply(caminos, function(cam) paste("SI", paste(cam[-1], collapse = " Y "))),
    entonces  = attr(modelo, "ylevels")[fr$yval[es_hoja]],
    casos     = fr$n[es_hoja],
    confianza = round(apply(fr$yval2[es_hoja, (2 + k):(1 + 2 * k), drop = FALSE], 1, max), 2),
    row.names = NULL) %>%
    arrange(desc(casos))
}
reglas <- reglas_arbol(arbol_simple)
print(reglas, right = FALSE)

dibujar_arbol <- function() {
  plot(arbol_simple, margin = 0.08, uniform = TRUE)
  text(arbol_simple, use.n = TRUE, pretty = 0, cex = 0.9)
}
mostrar(dibujar_arbol, "v2_7_arbol_decision")

pausa("6.2 La trampa del identificador")
## 6.2 SOBREAJUSTE: EL IDENTIFICADOR QUE "PREDICE" TODO --------------------
#  Igual que el caso de la compañía de seguros: si dejamos el código de la
#  estación como atributo y permitimos un árbol sin límites...
entrenamiento$id_num <- as.numeric(entrenamiento$id_estacion)
test$id_num          <- as.numeric(test$id_estacion)
arbol_id <- rpart(caro ~ id_num + producto, data = entrenamiento, method = "class",
                  control = rpart.control(cp = 0, minsplit = 2, minbucket = 1, xval = 0))
pred_id <- predict(arbol_id, test, type = "class")
c(reglas         = sum(arbol_id$frame$var == "<leaf>"),
  precision_ent  = precision(entrenamiento$caro, predict(arbol_id, entrenamiento, type = "class")),
  precision_test = precision(test$caro, pred_id))
head(reglas_arbol(arbol_id), 5)
# Altísima en entrenamiento, casi una moneda al aire en test: el método fue
# engañado. Ninguna estación futura tendrá esos códigos. Por eso se quitan
# las claves candidatas en la vista minable (5.4).

pausa("6.3 Clasificación suave y estimación de probabilidad")
## 6.3 CLASIFICACIÓN SUAVE: no solo la clase, también la certeza ----------
prob_arbol <- predict(arbol, test, type = "prob")[, "CARO"]

# Ranking: las 10 estaciones con más probabilidad de ser caras
test %>%
  mutate(p_caro = round(prob_arbol, 2)) %>%
  arrange(desc(p_caro)) %>%
  select(empresa, localidad, provincia, bandera, producto, precio, caro, p_caro) %>%
  head(10)

# ¿La probabilidad estimada se corresponde con la real? (calibración)
test %>%
  mutate(tramo = cut(prob_arbol, c(0, 0.2, 0.4, 0.6, 0.8, 1), include.lowest = TRUE)) %>%
  group_by(tramo) %>%
  summarise(casos = n(), pct_caro_real = round(100 * mean(caro == "CARO"), 1))

pausa("6.4 Naive Bayes (regla de Bayes con Laplace)")
## 6.4 NAIVE BAYES -----------------------------------------------------------
#  p(clase | E) ∝ p(clase) · Π p(atributo_i | clase)
#  "Ingenuo": supone los atributos independientes dada la clase.
#  Trabaja con atributos DISCRETOS: usamos los discretizados en 5.5.
#  Lo programamos a mano para ver cada paso (con corrección de Laplace).
atributos_nb <- c("region", "bandera_agr", "zona_lat", "periodo",
                  "competencia", "n_productos_f", "ofrece_gnc_f", "producto")

nb_entrenar <- function(datos, atributos, clase = "caro") {
  y <- datos[[clase]]
  clases <- levels(y)
  priori <- (as.vector(table(y)) + 1) / (length(y) + length(clases))
  names(priori) <- clases
  condicionales <- lapply(atributos, function(a) {
    tab <- table(datos[[a]], y)                      # valores x clases
    sweep(tab + 1, 2, colSums(tab) + nrow(tab), "/")  # Laplace por clase
  })
  names(condicionales) <- atributos
  list(priori = priori, cond = condicionales, atributos = atributos, clases = clases)
}

nb_probabilidades <- function(modelo, nuevos) {
  log_p <- sapply(modelo$clases, function(k) {
    s <- rep(log(modelo$priori[[k]]), nrow(nuevos))
    for (a in modelo$atributos) {
      tabla <- modelo$cond[[a]]
      fila  <- match(as.character(nuevos[[a]]), rownames(tabla))
      s <- s + log(tabla[cbind(fila, match(k, colnames(tabla)))])
    }
    s
  })
  log_p <- matrix(log_p, ncol = length(modelo$clases), dimnames = list(NULL, modelo$clases))
  e <- exp(log_p - apply(log_p, 1, max))     # normalizar = dividir por p(E)
  e / rowSums(e)
}

nb <- nb_entrenar(entrenamiento, atributos_nb)
round(nb$priori, 3)
round(nb$cond$bandera_agr, 3)        # p(bandera | clase)

# El cálculo paso a paso para UNA estación del test
ejemplo <- test[1, ]
ejemplo[, atributos_nb]
paso_a_paso <- t(sapply(atributos_nb, function(a)
  nb$cond[[a]][as.character(ejemplo[[a]]), ]))
round(paso_a_paso, 3)
producto_clase <- nb$priori * apply(paso_a_paso, 2, prod)
producto_clase                        # p(clase) · Π p(valor | clase)
round(producto_clase / sum(producto_clase), 3)   # normalizado
ejemplo$caro                          # ¿acertó?

prob_nb <- nb_probabilidades(nb, test)[, "CARO"]
pred_nb <- factor(ifelse(prob_nb > 0.5, "CARO", "BARATO"), levels = c("BARATO", "CARO"))
precision(test$caro, pred_nb)

pausa("6.5 k vecinos más próximos (método perezoso)")
## 6.5 k VECINOS MÁS PRÓXIMOS: un método RETARDADO (lazy) ------------------
#  No construye modelo: guarda los ejemplos y decide cuando llega cada
#  pregunta. Necesita atributos NUMÉRICOS y NORMALIZADOS (5.5).
f_num <- ~ producto + region + bandera_agr + lat + lon + mes +
           estaciones_localidad + n_productos + ofrece_gnc - 1
X_ent  <- model.matrix(f_num, entrenamiento)
X_test <- model.matrix(f_num, test)
medias <- colMeans(X_ent)
desvios <- apply(X_ent, 2, sd); desvios[desvios == 0] <- 1
X_ent  <- scale(X_ent,  medias, desvios)   # el test se escala con las medias
X_test <- scale(X_test, medias, desvios)   # del ENTRENAMIENTO
dim(X_ent)

t_arbol <- system.time(rpart(f_arbol, data = entrenamiento, method = "class"))
t_knn   <- system.time(pred_knn <- knn(X_ent, X_test, entrenamiento$caro, k = 7))
precision(test$caro, pred_knn)
rbind(arbol_anticipativo = t_arbol[["elapsed"]], knn_retardado = t_knn[["elapsed"]])
# El árbol tarda al ENTRENAR y responde al instante; k vecinos no entrena
# pero tarda en cada RESPUESTA (y hay que guardar todos los ejemplos).

# SEPARABILIDAD GEOMÉTRICA (Thornton): % de ejemplos cuyo vecino más
# cercano es de la misma clase. Alta: los métodos por distancia andan bien.
# Cerca de 0,5 con dos clases: el problema no es "geométrico".
sg_todo <- mean(knn.cv(X_ent, entrenamiento$caro, k = 1) == entrenamiento$caro)
sg_lugar <- mean(knn.cv(X_ent[, c("lat", "lon")], entrenamiento$caro, k = 1) == entrenamiento$caro)
round(c(SG_todos_los_atributos = sg_todo, SG_solo_coordenadas = sg_lugar), 3)

pausa("6.6 Regresión lineal y regresión logística")
## 6.6 REGRESIÓN: predecir un valor NUMÉRICO -------------------------------
#  Tarea: estimar el precio de la nafta súper. Medida: error cuadrático.
rmse <- function(real, pred) sqrt(mean((real - pred)^2))
sup_ent  <- entrenamiento %>% filter(producto == "super")
sup_test <- test          %>% filter(producto == "super")

reg_lineal <- lm(precio ~ lat + bandera_agr + mes + estaciones_localidad, data = sup_ent)
round(coef(reg_lineal), 2)
# Cada coeficiente: cuánto cambia el precio por unidad del atributo
# (por cada grado de latitud, por cada mes, contra la bandera de referencia).

# CREACIÓN DE CARACTERÍSTICAS: si sospechamos que el efecto de la latitud
# no es lineal (el beneficio impositivo es un "escalón" al sur), creamos
# z = lat² y zona_sur. La regresión sigue siendo LINEAL en los coeficientes.
reg_construida <- lm(precio ~ lat + I(lat^2) + zona_sur + bandera_agr + mes +
                       estaciones_localidad, data = sup_ent)
data.frame(modelo = c("lineal", "con atributos construidos"),
           R2_entrenamiento = round(c(summary(reg_lineal)$r.squared,
                                      summary(reg_construida)$r.squared), 3),
           RMSE_test = round(c(rmse(sup_test$precio, predict(reg_lineal, sup_test)),
                               rmse(sup_test$precio, predict(reg_construida, sup_test))), 1))

# De REGRESIÓN a CLASIFICACIÓN: la regresión logística estima una
# probabilidad y, con un umbral, se vuelve un clasificador binario.
logistica <- glm(caro ~ producto + region + bandera_agr + lat + mes +
                   estaciones_localidad + n_productos + ofrece_gnc,
                 data = entrenamiento, family = binomial)
prob_log <- predict(logistica, test, type = "response")
pred_log <- factor(ifelse(prob_log > 0.5, "CARO", "BARATO"), levels = c("BARATO", "CARO"))
precision(test$caro, pred_log)

pausa("6.7 Agrupamiento: K medias sobre provincias")
## 6.7 AGRUPAMIENTO (K medias): la tarea descriptiva por excelencia --------
#  No hay clase: buscamos grupos de provincias con perfiles de precio
#  parecidos (máxima similitud dentro, mínima entre grupos).
X_prov <- scale(precios_prov[, -1])
inercia <- sapply(1:6, function(k) kmeans(X_prov, centers = k, nstart = 25)$tot.withinss)
g_codo <- ggplot(data.frame(k = 1:6, inercia), aes(k, inercia)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Método del codo: ¿cuántos grupos?",
       x = "Cantidad de grupos (k)", y = "Suma de distancias al centro del grupo")
mostrar(g_codo, "v2_8_codo_kmeans")

k_grupos <- 3
grupos <- kmeans(X_prov, centers = k_grupos, nstart = 25)
precios_prov$grupo <- factor(grupos$cluster)
split(precios_prov$provincia, precios_prov$grupo)
precios_prov %>%                      # cómo es cada grupo, en pesos
  group_by(grupo) %>%
  summarise(provincias = n(), across(super:gnc, ~ round(median(.x))))

g_grupos <- ggplot(cbind(proyeccion, grupo = precios_prov$grupo),
                   aes(PC1, PC2, color = grupo, label = provincia)) +
  geom_point(size = 3) +
  geom_text(size = 3, vjust = -0.8, show.legend = FALSE) +
  labs(title = "Grupos de provincias (K medias) en el plano del PCA")
mostrar(g_grupos, "v2_9_grupos_provincias")
# El grupo es un atributo NUEVO (aumento de dimensionalidad por
# aprendizaje): se podría sumar a la vista minable (ejercicio 8).

pausa("6.8 Reglas de asociación")
## 6.8 REGLAS DE ASOCIACIÓN: "si X entonces Y" entre atributos nominales ----
#  Unidad de análisis: la ESTACIÓN. Cada estación es una "transacción" con
#  sus características como "ítems".
#    soporte   = % de estaciones que cumplen X e Y
#    confianza = de las que cumplen X, % que cumple Y
#    lift      = confianza / soporte de Y (>1: X e Y se dan juntas más de
#                lo que se daría por azar)
#  No implican causa-efecto.
items <- vista %>%
  group_by(id_estacion) %>%
  summarise(bandera     = first(as.character(bandera_agr)),
            region      = first(as.character(region)),
            competencia = first(as.character(competencia)),
            gnc         = first(as.character(ofrece_gnc_f)),
            super_cara  = if (any(producto == "super"))
                            ifelse(any(producto == "super" & caro == "CARO"), "si", "no")
                          else NA_character_,
            gasoil_caro = if (any(producto == "gasoil2"))
                            ifelse(any(producto == "gasoil2" & caro == "CARO"), "si", "no")
                          else NA_character_) %>%
  pivot_longer(-id_estacion, names_to = "atributo", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(item = paste0(atributo, "=", valor))

n_est <- n_distinct(items$id_estacion)
soporte_item <- items %>% count(item, name = "n_item") %>% mutate(sop_item = n_item / n_est)

reglas_asoc <- items %>%
  select(id_estacion, antecedente = item, attr_a = atributo) %>%
  inner_join(items %>% select(id_estacion, consecuente = item, attr_c = atributo),
             by = "id_estacion", relationship = "many-to-many") %>%
  filter(attr_a != attr_c) %>%
  count(antecedente, consecuente, name = "n_ambos") %>%
  left_join(soporte_item %>% select(antecedente = item, n_ante = n_item), by = "antecedente") %>%
  left_join(soporte_item %>% select(consecuente = item, sop_cons = sop_item), by = "consecuente") %>%
  mutate(soporte   = round(n_ambos / n_est, 3),
         confianza = round(n_ambos / n_ante, 3),
         lift      = round(confianza / sop_cons, 2)) %>%
  select(antecedente, consecuente, soporte, confianza, lift)

top_reglas <- reglas_asoc %>%
  filter(soporte >= 0.03, confianza >= 0.6, lift >= 1.2) %>%
  arrange(desc(lift))
# ¿Orientada (X -> Y) o bidireccional (X <-> Y)? Vemos si la inversa también pasa.
top_reglas <- top_reglas %>%
  mutate(bidireccional = paste(consecuente, antecedente) %in%
           paste(top_reglas$antecedente, top_reglas$consecuente))
print(head(top_reglas, 15), right = FALSE)
# Para discutir con el experto:
#  - "region=NEA -> gnc=no" redescubre algo sabido (buena parte del NEA no
#    tiene red de gas natural): es válida pero NO novedosa.
#  - "super_cara <-> gasoil_caro" es BIDIRECCIONAL: una estación cara
#    tiende a serlo en todos sus productos.
#  - ¿Hay alguna regla de bandera que no hayamos visto con OLAP en 4.5?

pausa("6.9 Detección de anomalías")
## 6.9 DETECCIÓN DE ANOMALÍAS ----------------------------------------------
#  En la limpieza buscábamos atípicos para CORREGIRLOS; acá buscamos
#  comportamientos DIFERENTES para INVESTIGARLOS (fraude, errores, casos
#  especiales). Una estación es anómala por el conjunto de sus atributos:
#  se parece a muy pocas otras. Medida: distancia promedio a sus 5 vecinos
#  más cercanos en (latitud, longitud, precio relativo al mes).
anomalias <- vista %>%
  filter(producto == "super") %>%
  group_by(mes_txt) %>%
  mutate(precio_rel = precio / median(precio)) %>%
  ungroup()
X_an <- scale(anomalias[, c("lat", "lon", "precio_rel")])
X_an[, "precio_rel"] <- 2 * X_an[, "precio_rel"]   # el precio pesa el doble
distancias <- as.matrix(dist(X_an))
diag(distancias) <- Inf
anomalias$puntaje <- apply(distancias, 1, function(d) mean(sort(d)[1:5]))

top_anomalias <- anomalias %>%
  arrange(desc(puntaje)) %>%
  select(id_estacion, empresa, localidad, provincia, bandera, mes_txt,
         precio, precio_rel, puntaje) %>%
  mutate(precio_rel = round(precio_rel, 2), puntaje = round(puntaje, 2)) %>%
  head(15)
top_anomalias

g_anom <- ggplot(anomalias, aes(lon, lat)) +
  geom_point(aes(color = precio_rel), size = 1.2, alpha = 0.6) +
  geom_point(data = anomalias %>% filter(id_estacion %in% top_anomalias$id_estacion),
             shape = 21, size = 4, stroke = 1.2, color = "red") +
  scale_color_viridis_c(limits = c(0.85, 1.15), oob = scales::squish) +
  coord_quickmap() +
  labs(title = "Nafta súper: las 15 estaciones más anómalas (círculos rojos)",
       x = "Longitud", y = "Latitud", color = "Precio / mediana del mes")
mostrar(g_anom, "v2_10_anomalias")


pausa("7. EVALUACIÓN E INTERPRETACIÓN")

# ---------------------------------------------------------------------
# 7. EVALUACIÓN E INTERPRETACIÓN ----
#    Los patrones son HIPÓTESIS: pueden ser refutados por datos nuevos.
#    Siempre se comparan contra una referencia trivial (la clase
#    mayoritaria) y en datos que el modelo NO vio.
# ---------------------------------------------------------------------
mayoritaria <- names(which.max(table(entrenamiento$caro)))
pred_knn_ent <- knn(X_ent, X_ent, entrenamiento$caro, k = 7)
comparacion <- data.frame(
  modelo = c("Clase mayoritaria (referencia)", "Árbol CART comprensible", "Árbol CART podado",
             "Árbol CART sin podar", "Árbol con identificador", "Naive Bayes (Laplace)",
             "k vecinos (k = 7)", "Regresión logística"),
  tipo = c("-", "anticipativo", "anticipativo", "anticipativo", "anticipativo", "anticipativo",
           "retardado", "anticipativo"),
  comprensibilidad = c("-", paste(hojas(arbol_simple), "reglas"), paste(hojas(arbol), "reglas"),
                       paste(hojas(arbol_grande), "reglas"),
                       "memoriza, no generaliza", "tablas de probabilidad",
                       "sin modelo explícito", "coeficientes"),
  precision_entrenamiento = round(c(
    mean(entrenamiento$caro == mayoritaria),
    precision(entrenamiento$caro, predict(arbol_simple, entrenamiento, type = "class")),
    precision(entrenamiento$caro, predict(arbol, entrenamiento, type = "class")),
    precision(entrenamiento$caro, predict(arbol_grande, entrenamiento, type = "class")),
    precision(entrenamiento$caro, predict(arbol_id, entrenamiento, type = "class")),
    precision(entrenamiento$caro, ifelse(nb_probabilidades(nb, entrenamiento)[, "CARO"] > 0.5, "CARO", "BARATO")),
    precision(entrenamiento$caro, pred_knn_ent),
    precision(entrenamiento$caro, ifelse(fitted(logistica) > 0.5, "CARO", "BARATO"))), 3),
  precision_test = round(c(
    mean(test$caro == mayoritaria),
    precision(test$caro, pred_simple),
    precision(test$caro, pred_arbol),
    precision(test$caro, predict(arbol_grande, test, type = "class")),
    precision(test$caro, pred_id),
    precision(test$caro, pred_nb),
    precision(test$caro, pred_knn),
    precision(test$caro, pred_log)), 3))
print(comparacion, right = FALSE)
# Lo que hay que mirar:
#  - La distancia entre entrenamiento y test mide el SOBREAJUSTE.
#  - Ningún método gana siempre ("no hay almuerzo gratis").
#  - A igual precisión, preferimos el más COMPRENSIBLE (navaja de Occam).

g_comp <- comparacion %>%
  pivot_longer(c(precision_entrenamiento, precision_test),
               names_to = "conjunto", values_to = "precision") %>%
  mutate(conjunto = sub("precision_", "", conjunto)) %>%
  ggplot(aes(reorder(modelo, precision), precision, fill = conjunto)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_flip() +
  labs(title = "Visualización POSTERIOR: precisión de cada modelo",
       subtitle = "La brecha entre entrenamiento y test es el sobreajuste",
       x = NULL, y = "Precisión")
mostrar(g_comp, "v2_11_comparacion_modelos")

pausa("7.1 Estabilidad: ¿y si la partición hubiera sido otra?")
## 7.1 VARIABILIDAD O ESTABILIDAD -------------------------------------------
#  Una buena precisión media puede esconder altibajos. Repetimos con 10
#  particiones distintas.
estabilidad <- t(sapply(1:10, function(i) {
  pp <- particion(vista)
  a  <- rpart(f_arbol, data = pp$ent, method = "class", control = rpart.control(cp = cp_poda))
  bm <- nb_entrenar(pp$ent, atributos_nb)
  c(arbol       = precision(pp$test$caro, predict(a, pp$test, type = "class")),
    naive_bayes = precision(pp$test$caro,
                            ifelse(nb_probabilidades(bm, pp$test)[, "CARO"] > 0.5, "CARO", "BARATO")))
}))
round(rbind(media  = colMeans(estabilidad),
            desvio = apply(estabilidad, 2, sd),
            minimo = apply(estabilidad, 2, min),
            maximo = apply(estabilidad, 2, max)), 3)

pausa("7.2 ¿Cuántos datos hacen falta?")
## 7.2 CURVA DE APRENDIZAJE -------------------------------------------------
#  "Ir agrandando la muestra hasta que el modelo deje de cambiar."
tamanios <- c(100, 250, 500, 1000, 2000, nrow(entrenamiento))
curva <- do.call(rbind, lapply(tamanios, function(n) {
  prec <- replicate(5, {
    muestra <- entrenamiento[sample(nrow(entrenamiento), n), ]
    a <- rpart(f_arbol, data = muestra, method = "class", control = rpart.control(cp = cp_poda))
    precision(test$caro, predict(a, test, type = "class"))
  })
  data.frame(n = n, precision_media = mean(prec), desvio = sd(prec))
}))
curva
g_curva <- ggplot(curva, aes(n, precision_media)) +
  geom_line() + geom_point(size = 3) +
  geom_errorbar(aes(ymin = precision_media - desvio, ymax = precision_media + desvio), width = 0.03) +
  scale_x_log10() +
  labs(title = "Curva de aprendizaje del árbol (muestras aleatorias simples)",
       x = "Tamaño de la muestra de entrenamiento (escala log)", y = "Precisión en test")
mostrar(g_curva, "v2_12_curva_aprendizaje")

# ---------------------------------------------------------------------
#  INTERPRETACIÓN (con el experto del dominio): ¿qué le pedimos a un patrón?
#   - VÁLIDO:        ¿se sostiene en test y en las 10 particiones?
#   - NOVEDOSO:      "la Patagonia es más barata" ya lo sabíamos (OLAP 4.4);
#                    ¿qué reglas dicen algo nuevo?
#   - INTERESANTE:   ¿sirve para decidir? (p. ej., a qué estaciones
#                    inspeccionar primero: ranking de 6.3 y anomalías de 6.9)
#   - COMPRENSIBLE:  ¿el experto puede leer las reglas de 6.1?
#  Si algo falla, se VUELVE ATRÁS: otra clase (como en 5.2), otros
#  atributos (5.3), otro método. El KDD es iterativo.
# ---------------------------------------------------------------------


pausa("8. DIFUSIÓN Y USO")

# ---------------------------------------------------------------------
# 8. DIFUSIÓN Y USO ----
#    El conocimiento tiene que llegar a quienes lo van a usar. Lo
#    devolvemos al ALMACÉN (para que los "picapedreros" lo consulten con
#    SQL) y a archivos que se abren con Excel.
# ---------------------------------------------------------------------
predicciones <- test %>%
  mutate(prob_caro = round(prob_arbol, 3), prediccion = as.character(pred_arbol)) %>%
  select(id_estacion, empresa, localidad, provincia, bandera, producto, precio,
         caro_real = caro, prob_caro, prediccion) %>%
  mutate(across(where(is.factor), as.character))

dbWriteTable(almacen, "mm_predicciones", predicciones, overwrite = TRUE)
dbWriteTable(almacen, "mm_modelos",
             comparacion %>% mutate(fecha = format(Sys.time(), "%Y-%m-%d %H:%M")), overwrite = TRUE)
dbWriteTable(almacen, "mm_reglas_arbol", reglas, overwrite = TRUE)
dbWriteTable(almacen, "mm_reglas_asociacion", top_reglas, overwrite = TRUE)
dbWriteTable(almacen, "mm_anomalias", top_anomalias, overwrite = TRUE)
registrar_metadatos()

guardar_csv(comparacion,   "resultados/comparacion_modelos.csv")
guardar_csv(reglas,        "resultados/reglas_arbol.csv")
guardar_csv(top_reglas,    "resultados/reglas_asociacion.csv")
guardar_csv(top_anomalias, "resultados/estaciones_anomalas.csv")

# Un picapedrero ahora puede pedir, todos los meses, sin saber minería:
dbGetQuery(almacen, "
  SELECT provincia, COUNT(*) AS casos,
         ROUND(100.0 * AVG(prediccion = 'CARO'), 1) AS pct_predicho_caro,
         ROUND(100.0 * AVG(caro_real  = 'CARO'), 1) AS pct_real_caro
  FROM mm_predicciones
  GROUP BY provincia
  HAVING casos >= 20
  ORDER BY pct_predicho_caro DESC")

dbDisconnect(almacen)


cat("\n\n==================== FIN: ver los EJERCICIOS al final del script ====================\n")

# ---------------------------------------------------------------------
# 9. EJERCICIOS ----
# ---------------------------------------------------------------------
# OLAP (clase 1)
# 1) ROLL-UP: precio mediano del GNC por REGIÓN en el año actual.
# 2) DRILL-DOWN: en la región CENTRO, bajar de provincia a LOCALIDAD para
#    la nafta premium. ¿Cuáles son las 5 localidades más caras?
# 3) PIVOT: matriz provincia (filas) x tipo de combustible (columnas).
# 4) SLICE & DICE: ¿hay diferencia entre horario Diurno y Nocturno? ¿En
#    qué banderas y provincias?
# 5) TIEMPO: para YPF, armar la serie de precio mediano de la nafta súper
#    por TRIMESTRE desde 2024 y graficarla.
# 6) SQL: resolver el ejercicio 3 con una consulta sobre el almacén, en
#    estrella y en copo de nieve.
# 7) Integración: buscar en datos.gob.ar otra fuente por provincia (por
#    ejemplo, población del Censo 2022), cargarla al STAGING, incorporarla
#    a dim_lugar y calcular "estaciones de servicio cada 100.000 habitantes".
#
# PREPARACIÓN (clase 2)
# 8) Sumar a la vista minable el GRUPO de K medias (6.7) como atributo
#    nuevo. ¿Mejora el árbol en test? ¿Qué pasa con las provincias sin GNC?
# 9) Repetir la comparación de muestras (5.8) estratificando por PRODUCTO.
#    ¿Cambia el porcentaje de estaciones caras?
# 10) Aplicar Laplace y m-estimado (5.9) a la BANDERA en lugar de la
#     localidad. ¿Por qué casi no cambia nada? Probar m = 2, 5 y 10 con
#     las localidades: ¿cuáles cambian más?
# 11) Repetir el PCA (5.7) SIN normalizar. ¿Qué componente domina y por qué?
#
# MINERÍA Y EVALUACIÓN (clase 3)
# 12) Volver a la clase "caro_v1" de 5.2 y entrenar el árbol. ¿Qué atributo
#     aparece primero? ¿Es conocimiento válido, novedoso, interesante?
# 13) Regresión: predecir el precio del Gas Oil Grado 2. ¿Qué atributo
#     construido ayuda más a bajar el RMSE en test?
# 14) Reglas de asociación: bajar el soporte mínimo a 0,02. ¿Cuántas reglas
#     nuevas aparecen? ¿Cuáles son dependencias ya conocidas?
# 15) Calcular la matriz de confusión del Naive Bayes y de k vecinos. ¿Se
#     equivocan en los mismos casos?
# 16) Probar k = 1, 3, 7, 15 y 31 en k vecinos. Graficar la precisión en
#     entrenamiento y en test. ¿Dónde hay sobreajuste?
# 17) Difusión: escribir una consulta SQL sobre mm_anomalias y
#     dim_estacion que liste dirección y coordenadas de las estaciones a
#     inspeccionar.
