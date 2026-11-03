# data/

`regions.csv`: la misma tabla que `data/regions.csv` del repo, normalizada a finales de línea LF (el original usa CRLF;
al leerlo con `read_csv` el contenido es idéntico). md5 de esta copia `ae65d45f6819eff4263b1d8cbc0971d8`; del original
del repo `0fbd7a766dc93a9359ad35c59fba291e`. 110 regiones NUTS de 27 países; empieza con BOM UTF-8, que `load_regions()`
elimina. Columnas: `country`, `nuts_level`, `nuts_id`, `nameSHORT`, `countrypop`, `regionpop`, `regionpoppct` (peso
poblacional de la región dentro del país; `get_wgt_national_avg()` lo renormaliza entre las regiones con n ≥ 30; no se
recalcula aquí).

Define qué países entran en la media UE (`eu_est()`) y las ponderaciones de todas las estimaciones nacionales.

Caché de microdatos: `--stage data` escribe aquí `master.rds` (27 países, un encuestado por fila), `ctx_<slug>.rds` y
sus claves (`master.key`, `ctx_<slug>.key`), salvo que `cache_dir` (config/local.yml) o `SRC_REPORTS_CACHE` digan otra
carpeta. `data/` está en `.gitignore`: solo se versionan `regions.csv` y este README. El `.dta` se lee desde `path2EU`.
