# fonts/

Inter Tight, versiones estáticas (8 pesos), copiadas el 2026-10-22 de `Data Analytics/6. Country Reports/0. Fonts/`
(las mismas con las que se produjeron las figuras publicadas). Licencia SIL Open Font License 1.1 (`OFL.txt`):
permite redistribuirlas con el código, por eso viven dentro de la carpeta y no dependen de `path2DA`.

| Archivo | Uso |
|---|---|
| InterTight-Regular / Bold / Italic / SemiBoldItalic | registro `systemfonts::register_font("inter", …)` en `R/00_setup.R` (igual que `src/hungary/setup.R:200-206`): figuras ggplot |
| InterTight-Medium / MediumItalic / SemiBold / BoldItalic | texto en trazos de las figuras svg_r (`R/04_svg.R`, `font_file()`) |

`md5.txt` (formato `md5 -r`) se comprueba al arrancar: si un archivo cambia o pesa 0 bytes (OneDrive sin
descargar) `setup_fonts()` avisa o se detiene.
