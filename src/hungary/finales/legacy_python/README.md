# Scripts Python heredados (solo trazabilidad)

Copias de los scripts que produjeron varias figuras del reporte *Paths to Justice in Hungary* fuera de este repo (sep-oct 2026). Los originales siguen en `output/viz/Hungary/analisis adicionales/` (`figuras_v4/`, `figuras_v5/` y la raíz).

**No son ejecutables tal como están.** Corrieron en otro entorno: rutas `/home/claude/...` y `/mnt/user-data/uploads/Hungary/`, medios de un docx anterior descomprimido (`/home/claude/tc/u4/word/media/`), Python con `fontTools`, `uharfbuzz` y `lxml`, y un renderizador Node (`/home/claude/v4/render_svg.js`, no conservado) para los PNG. El build actual no los usa: las figuras que produjeron se entregan desde el SVG del reporte (`../svg/`), sin reimplementarlas.

| Script | Qué hizo | Figuras del reporte v9 |
|---|---|---|
| `charts_v4.py` | Primera versión (v4) en estilo del reporte: sankeys, embudos, tarjetas y opciones descartadas (`opt41`, `optF`) | Borradores de 8, 11, 16, 32, 34, 41, 42 (reemplazados por `charts5.py`) |
| `charts5.py` | Versión v5: `sankey()`, `funnel()`, `scorecards()`, `network()`, `bars()` | 5, 8, 11, 16, 32, 34, 36, 41, 42 (41 y 42 editadas después el 15-oct: «Without a non-trivial problem») |
| `fix_orig.py` | Correcciones sobre SVG originales: lienzo ensanchado a la izquierda (1, 4), barra UE de R03 (12), «Adviser» (13) | 1, 4, 12, 13 |
| `build_figs5.py` | Convierte el texto a trazos (con `outline.py`) y genera los PNG de respaldo | 3, 4, 5, 8, 11, 13, 16, 20, 32, 34, 36, 38, 39, 40, 41, 42 (1 y 12 se copian sin convertir) |
| `outline.py` | Medición de texto en Inter Tight y conversión de texto a trazos | (auxiliar de `charts5.py` y `build_figs5.py`) |
| `sankey_advice_representation_build.py` | Sankey de asistencia y representación, versión anterior | Ninguna: la figura 11 del reporte es la de `charts5.py` |

La procedencia figura por figura está en `../provenance.csv`.
