"""Outline text in v5 figures and render PNG fallbacks. Writes /home/claude/v5/manifest.json."""
import json, os, shutil, subprocess, sys
sys.path.insert(0, "/home/claude/v4")
from outline import outline_svg
RAW = "/home/claude/v5/raw/"; OUT = "/home/claude/v5/out/"; os.makedirs(OUT, exist_ok=True)
H = "/mnt/user-data/uploads/Hungary/"
CM = 360000
FIGS = {  # number: (source svg, outline?, width in EMU or ('scale', factor) relative to the current extent)
    1:  (RAW + "fig01_prevalence.svg", False, ("scale", 619.28 / 595.28)),
    3:  (H + "analisis adicionales/F02_prevalencia_severidad_zona.svg", True, 5120640),
    4:  (RAW + "fig04_cooccurrence.svg", True, ("scale", 619.28 / 595.28)),
    5:  (RAW + "fig05_network.svg", True, 5120640),
    8:  (RAW + "fig08_funnel_pathway.svg", True, 5120640),
    11: (RAW + "fig11_sankey_assistance.svg", True, 5120640),
    12: (RAW + "fig12_assistance.svg", False, ("scale", 1.0)),
    13: (RAW + "fig13_advisers.svg", True, 4747098),
    16: (RAW + "fig16_sankey_drm.svg", True, 5120640),
    20: (H + "Hungary_timeliness_rp.svg", True, 4070428),
    32: (RAW + "fig32_scorecards_hardship.svg", True, 4937760),
    34: (RAW + "fig34_funnel_capability.svg", True, 5120640),
    36: (RAW + "fig36_causes.svg", True, round(11.1 * CM * 595.28 / 639.95)),
    38: (H + "Hungary_trust_judges.svg", True, 3600000),
    39: (H + "Hungary_trust_prosecutors.svg", True, 3600000),
    40: (H + "Hungary_trust_pda.svg", True, 3600000),
    41: (RAW + "fig41_scorecards_exposure.svg", True, 4937760),
    42: (RAW + "fig42_scorecards_hardship.svg", True, 4937760),
}
man, args = {}, []
for k, (src, outl, w) in FIGS.items():
    dst = OUT + f"fig{k:02d}.svg"
    n = outline_svg(src, dst) if outl else (shutil.copy(src, dst), 0)[1]
    png = OUT + f"fig{k:02d}.png"
    args += [dst, png]
    man[k] = {"svg": dst, "png": png, "width": w, "texts_outlined": n}
subprocess.run(["node", "/home/claude/v4/render_svg.js"] + args, check=True)
json.dump(man, open("/home/claude/v5/manifest.json", "w"), indent=1)
for k, v in man.items(): print(k, v["texts_outlined"], os.path.getsize(v["svg"]) // 1024, "KB")
