# data/

`regions.csv`: copia literal de `data/regions.csv` del repo (md5 `ae65d45f6819eff4263b1d8cbc0971d8`; 110 regiones
NUTS de 27 países; empieza con BOM UTF-8, que `load_regions()` elimina; finales de línea LF: el original del repo usa CRLF). Columnas: `country`, `nuts_level`,
`nuts_id`, `nameSHORT`, `countrypop`, `regionpop`, `regionpoppct` (peso poblacional de la región dentro del país;
`get_wgt_national_avg()` lo renormaliza entre las regiones con n ≥ 30; no se recalcula aquí).

Define qué países entran en la media UE (`eu_est()`) y las ponderaciones de todas las estimaciones nacionales.

Aquí no se guardan microdatos: el `.dta` se lee desde `path2EU` (config/local.yml) y el caché va en `cache/`
(ignorada por git).
