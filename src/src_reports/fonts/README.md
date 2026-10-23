# fonts/

Inter Tight, versiones estáticas (8 pesos), copiadas el 2026-10-22 de `Data Analytics/6. Country Reports/0. Fonts/`
(las mismas con las que se produjeron las figuras publicadas). Licencia SIL Open Font License 1.1 (`OFL.txt`):
permite redistribuirlas con el código, por eso viven dentro de la carpeta y no dependen de `path2DA`.

| Archivo | Uso |
|---|---|
| InterTight-Regular / Italic / SemiBoldItalic / Bold / BoldItalic | `font_file(weight, italic)` de `R/04_svg.R`: texto en trazos de las svg_r (400, 400 cursiva, 600 cursiva, 700, 700 cursiva) y contorneado de las ggplot (`outline_svg_text()`) |
| InterTight-Regular / Bold / Italic / SemiBoldItalic | registro `systemfonts::register_font("inter", plain, bold, italic, bolditalic)` en `R/00_setup.R` (igual que `src/hungary/setup.R:200-206`): las caras que pide ggplot2 |
| InterTight-Medium / MediumItalic / SemiBold | no las usa ningún camino (ni 500 ni 600 recta); se conservan con el resto del juego y `setup_fonts()` comprueba las 8 |

`md5.txt` (formato `md5 -r`) se comprueba al arrancar: si un archivo cambia o pesa 0 bytes (OneDrive sin
descargar) `setup_fonts()` avisa o se detiene.
