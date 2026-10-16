"""Corrections to original figures (v5): Figure 1 and 4 cut labels, Figure 12 EU average, Figure 13 'Adviser'."""
import re, sys, csv
sys.path.insert(0, "/home/claude/v4")
from outline import path_d

M = "/home/claude/tc/u4/word/media/"
H = "/mnt/user-data/uploads/Hungary/"
OUT = "/home/claude/v5/raw/"
D = 24.0  # extra room on the left for the "National Average" label


def widen_left(s, d, full_w, full_h):
    """Move the left edge of the canvas by d units (same scale) and widen full-canvas clip rectangles."""
    nw = full_w + d
    m = re.search(r"<svg\b[^>]*>", s); tag = m.group(0)
    tag = re.sub(r"viewBox=(['\"])[^'\"]*\1", lambda k: f"viewBox={k.group(1)}{-d:.2f} 0 {nw:.2f} {full_h}{k.group(1)}", tag, count=1)
    tag = re.sub(r"\swidth=(['\"])([\d.]+)(pt|px|)\1", lambda k: f" width={k.group(1)}{float(k.group(2)) * nw / full_w:.3f}{k.group(3)}{k.group(1)}", tag, count=1)
    bg = f"<rect x='{-d:.2f}' y='0' width='{nw:.2f}' height='{full_h}' style='fill:#FFFFFF;stroke:none;'/>"
    s = s[:m.start()] + tag + bg + s[m.end():]
    pat = r"(<clipPath[^>]*>\s*<rect\s+)x=(['\"])0(?:\.00)?\2(\s+)y=(['\"])0(?:\.00)?\4(\s+)width=(['\"])" + re.escape(f"{full_w}") + r"\6"
    s, n = re.subn(pat, lambda k: f"{k.group(1)}x={k.group(2)}{-d:.2f}{k.group(2)}{k.group(3)}y={k.group(4)}0{k.group(4)}{k.group(5)}width={k.group(6)}{nw:.2f}{k.group(6)}", s)
    assert n >= 1, "no full-canvas clip found"
    return s


# ---------------- Figure 1: "National Average" cut at the left edge
s = open(M + "image2.svg").read()
s = widen_left(s, D, 595.28, "563.39")
open(OUT + "fig01_prevalence.svg", "w").write(s)

# ---------------- Figure 4: same label; restore the two pipeline words and widen the canvas
s = open(H + "Hungary_cooccurence_nproblems.svg").read()
assert ">National<" in s and ">Average<" in s
s = widen_left(s, D, 595.28, "478.35")
open(OUT + "fig04_cooccurrence.svg", "w").write(s)

# ---------------- Figure 12: EU average with the same definition as the national value (R03, variant c)
r03 = {r["variable"]: float(r["media_ue"]) for r in csv.DictReader(open(H + "analisis adicionales/R03.csv"))}
eu = r03["rep_c_published"]
s = open(M + "image15.svg").read()
# EU row: the purple bar and the grey track whose label sits at y = 40.32
rects = re.findall(r'<rect x="([\d.e-]+)" y="([\d.]+)" width="([\d.]+)" height="([\d.]+)"[^>]*fill="(#575796|#E5E8E8)"/>', s)
nat = [r for r in rects if abs(float(r[1]) + float(r[3]) / 2 - 18.4) < 4]
eur = [r for r in rects if abs(float(r[1]) + float(r[3]) / 2 - 37.0) < 4]
assert len(eur) == 2 and len(nat) == 2, (nat, eur)
x0 = min(float(r[0]) for r in eur); full = sum(float(r[2]) for r in eur)
wbar = full * eu / 100
for r in eur:
    old = f'<rect x="{r[0]}" y="{r[1]}" width="{r[2]}" height="{r[3]}"'
    assert s.count(old) == 1, old
    if r[4] == "#575796":
        new = f'<rect x="{r[0]}" y="{r[1]}" width="{wbar:.2f}" height="{r[3]}"'
    else:
        new = f'<rect x="{x0 + wbar:.2f}" y="{r[1]}" width="{full - wbar:.2f}" height="{r[3]}"'
    s = s.replace(old, new)
assert s.count('y="40.32">54%</text>') == 1
s = s.replace('y="40.32">54%</text>', f'y="40.32">{round(eu)}%</text>')
open(OUT + "fig12_assistance.svg", "w").write(s)
print("fig12 EU", round(eu, 1), "bar", round(wbar, 2), "of", round(full, 2))

# ---------------- Figure 13: "Advisor" -> "Adviser" (glyph outlines; the label's last two glyphs are "or")
s = open(H + "Hungary_contacted_advisers.svg").read()
paths = list(re.finditer(r"<path d='([^']*)'([^>]*)/>", s))
def bbox(d):
    n = [float(x) for x in re.findall(r"-?[\d.]+", d)]
    return min(n[0::2]), max(n[0::2]), min(n[1::2]), max(n[1::2])
row = [m for m in paths if bbox(m.group(1))[1] < 205 and bbox(m.group(1))[2] < 50]
assert len(row) == 27, len(row)  # L-a-w-y-e-r o-r P-r-o-f-e-s-s-i-o-n-a-l A-d-v-i-s-o-r
o, r = row[-2], row[-1]
bo = bbox(o.group(1)); baseline = 38.03; size = 14.0
d_o, _ = path_d("o", 0, baseline, size)
lsb_o = bbox(d_o)[0]
x_origin = bo[0] - lsb_o
d_er, _ = path_d("er", x_origin, baseline, size)
style = o.group(2)
s = s[:o.start()] + f"<path d='{d_er}'{style}/>" + s[o.end():r.start()] + s[r.end():]
open(OUT + "fig13_advisers.svg", "w").write(s)
print("fig13 ok; 'o' origin", round(x_origin, 2))
