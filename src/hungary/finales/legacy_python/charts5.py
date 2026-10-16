"""Report-style figures, v5. Layout uses real Inter Tight metrics; text is outlined afterwards."""
import re, csv, sys, os
sys.path.insert(0, "/home/claude/v4")
from outline import measure, wrap

FONT = '"Inter Tight"'
INK = "#1A1A1A"; PURPLE = "#575796"; PURPLE_MID = "#7D7DA8"; PURPLE_LIGHT = "#A8A8C8"
MAGENTA = "#B200AA"; GREY_NODE = "#8E9393"; GREY_EXCL = "#C4C8C8"; TRACK = "#E5E8E8"
BOX_FILL = "#F2F3F3"; BOX_STROKE = "#1A1A1A"
OUT_COL = {"access": PURPLE, "no_access": PURPLE_LIGHT, "excluded": GREY_EXCL}
A = "/mnt/user-data/uploads/Hungary/analisis adicionales/"
OUT = "/home/claude/v5/raw/"


def head(w, h, x0=0):
    return [f"<?xml version='1.0' encoding='UTF-8' ?>",
            f"<svg xmlns='http://www.w3.org/2000/svg' width='{w:.2f}pt' height='{h:.2f}pt' viewBox='{x0:.2f} 0 {w:.2f} {h:.2f}'>",
            f"<rect x='{x0:.2f}' y='0' width='{w:.2f}' height='{h:.2f}' style='fill:#FFFFFF;stroke:none;'/>"]


def esc(s): return s.replace("&", "&amp;").replace("<", "&lt;")


def text(x, y, s, size=14, anchor="start", weight=400, italic=False, fill=INK):
    st = f"font-size:{size:.2f}px;fill:{fill};font-family:{FONT};"
    if weight != 400: st += f"font-weight:{weight};"
    if italic: st += "font-style:italic;"
    return f"<text x='{x:.2f}' y='{y:.2f}' text-anchor='{anchor}' style='{st}'>{esc(s)}</text>"


def rect(x, y, w, h, fill, rx=0, stroke=None, sw=0.6):
    st = f"fill:{fill};" + (f"stroke:{stroke};stroke-width:{sw};" if stroke else "stroke:none;")
    return f"<rect x='{x:.2f}' y='{y:.2f}' width='{w:.2f}' height='{h:.2f}' rx='{rx}' style='{st}'/>"


def save(name, out):
    out.append("</svg>")
    os.makedirs(OUT, exist_ok=True)
    open(OUT + name, "w").write("\n".join(out))


def pct(v): return f"{round(v):.0f}%"


def lines_block(x, yc, lines, size, anchor="end", weight=400, italic=False, fill=INK, lh=1.22):
    """Text lines vertically centred on yc (cap-height aware)."""
    step = size * lh
    y = yc - step * (len(lines) - 1) / 2 + size * 0.36
    return [text(x, y + i * step, ln, size, anchor, weight, italic, fill) for i, ln in enumerate(lines)]


# ----------------------------------------------------------------- sankey
def box(out, xc, yc, lines, val, size=12.0, note=None, val_fill=PURPLE):
    lh = size * 1.22
    rows = [(l, "lab") for l in lines] + ([(val, "val")] if val else []) + ([(n, "note") for n in note] if note else [])
    def wd(t, k):
        if k == "note": return measure(t, 10.5, 400, True)
        if k == "val": return measure(t, size, 600, True)
        return measure(t, size)
    w = max(wd(t, k) for t, k in rows) + 16
    h = lh * len(rows) + 7
    x0, y0 = xc - w / 2, yc - h / 2
    out.append(rect(x0, y0, w, h, BOX_FILL, 2.5, BOX_STROKE))
    y = y0 + 3.5 + lh * 0.8
    for t, k in rows:
        if k == "lab": out.append(text(xc, y, t, size, "middle"))
        elif k == "val": out.append(text(xc, y, t, size, "middle", 600, True, val_fill))
        else: out.append(text(xc, y, t, 10.5, "middle", 400, True, PURPLE_MID))
        y += lh


def band(xa, ya0, ya1, xb, yb0, yb1, fill, op):
    xm = (xa + xb) / 2
    return (f"<path d='M{xa:.2f},{ya0:.2f} C{xm:.2f},{ya0:.2f} {xm:.2f},{yb0:.2f} {xb:.2f},{yb0:.2f} "
            f"L{xb:.2f},{yb1:.2f} C{xm:.2f},{yb1:.2f} {xm:.2f},{ya1:.2f} {xa:.2f},{ya1:.2f} Z' "
            f"style='fill:{fill};fill-opacity:{op};stroke:none;'/>")


def sankey(name, C1, LAB1, C2, LAB2, order2, M, order3, LAB3, VAL3, NOTE3, headers):
    W, H = 595.28, 408.0; TOP, BOT = 76.0, 396.0; GAP = 10.0; BAR = 12.0
    X1, X2, X3 = 82.0, 296.0, 512.0
    order1 = list(C1)
    val2 = {k: C2[k][0] for k in order2}
    val3 = {o: sum(val2[k] for k in order2 if M[k] == o) for o in order3}
    total_h = BOT - TOP
    scale = (total_h - GAP * (len(order2) - 1)) / 100.0

    def layout(values, order):
        used = sum(values[k] for k in order) * scale + GAP * (len(order) - 1)
        y = TOP + (total_h - used) / 2; pos = {}
        for k in order:
            pos[k] = [y, y + values[k] * scale]; y += values[k] * scale + GAP
        return pos
    p1, p2, p3 = layout(C1, order1), layout(val2, order2), layout(val3, order3)
    out = head(W, H)
    cur1 = {k: p1[k][0] for k in order1}
    for k in order2:
        h = val2[k] * scale; par = C2[k][1]
        out.append(band(X1 + BAR, cur1[par], cur1[par] + h, X2, p2[k][0], p2[k][1], OUT_COL[M[k]], 0.30)); cur1[par] += h
    cur3 = {o: p3[o][0] for o in order3}
    for k in order2:
        o = M[k]; h = val2[k] * scale
        out.append(band(X2 + BAR, p2[k][0], p2[k][1], X3, cur3[o], cur3[o] + h, OUT_COL[o], 0.45)); cur3[o] += h
    for k in order1: out.append(rect(X1, p1[k][0], BAR, p1[k][1] - p1[k][0], GREY_NODE))
    for k in order2: out.append(rect(X2, p2[k][0], BAR, p2[k][1] - p2[k][0], OUT_COL[M[k]]))
    for o in order3: out.append(rect(X3, p3[o][0], BAR, p3[o][1] - p3[o][0], OUT_COL[o]))
    for xc, lines in zip([X1 + BAR / 2, X2 + BAR / 2, X3 + BAR / 2], headers):
        for i, ln in enumerate(lines):
            out.append(text(xc, 22 + 16 * i, ln, 13.0, "middle", 700, True, PURPLE))
    for k in order1: box(out, X1 + BAR / 2, sum(p1[k]) / 2, [LAB1[k]], pct(C1[k]))
    for k in order2: box(out, X2 + BAR / 2, sum(p2[k]) / 2, LAB2[k], pct(val2[k]))
    for o in order3:
        box(out, X3 + BAR / 2, sum(p3[o]) / 2, LAB3[o], VAL3[o], note=NOTE3.get(o),
            val_fill=PURPLE if o != "excluded" else PURPLE_MID)
    save(name, out)


# ----------------------------------------------------------------- funnel
def funnel(name, rows, label_w=222, size=12.5, vsize=14.5):
    """rows: [(label, value)]; centred bars, light connectors, values inside the bars, wrapped labels."""
    W = 595.28; bh = 40; gap = 16; top = 6
    x0 = label_w + 20; x1 = W - 6; cx = (x0 + x1) / 2; full = x1 - x0
    H = top * 2 + len(rows) * bh + (len(rows) - 1) * gap
    out = head(W, H)
    geo = []
    for i, (lab, v) in enumerate(rows):
        y = top + i * (bh + gap); w = full * v / 100
        geo.append((y, cx - w / 2, cx + w / 2))
    for i in range(len(geo) - 1):
        (ya, la, ra), (yb, lb, rb) = geo[i], geo[i + 1]
        out.append(f"<path d='M{la:.2f},{ya+bh:.2f} L{ra:.2f},{ya+bh:.2f} L{rb:.2f},{yb:.2f} L{lb:.2f},{yb:.2f} Z' "
                   f"style='fill:{PURPLE_LIGHT};fill-opacity:0.45;stroke:none;'/>")
    for (lab, v), (y, l, r) in zip(rows, geo):
        out.append(rect(l, y, r - l, bh, PURPLE))
        lines = lab.split("\n") if "\n" in lab else wrap(lab, size, label_w - 2)
        assert len(lines) <= 2 and all(measure(l, size) <= label_w for l in lines), lines
        out += lines_block(label_w, y + bh / 2, lines, size, "end")
        assert measure(pct(v), vsize, 600, True) + 12 < r - l, (lab, v)
        out.append(text(cx, y + bh / 2 + vsize * 0.36, pct(v), vsize, "middle", 600, True, "#FFFFFF"))
    save(name, out)
    return W, H


# ----------------------------------------------------------------- scorecards
def mix(c1, c2, t):
    a = [int(c1[i:i + 2], 16) for i in (1, 3, 5)]; b = [int(c2[i:i + 2], 16) for i in (1, 3, 5)]
    return "#" + "".join(f"{round(a[i] + (b[i] - a[i]) * t):02X}" for i in range(3))


def scorecards(name, row_labels, col_labels, vals, ref="row", label_w=188, lsize=13, hsize=13, vsize=19, dsize=11):
    """vals[r][c] in %. Deltas (percentage points) vs. first row (ref='row') or first column (ref='col')."""
    W = 595.28; ch = 50; cg = 6
    nc = len(col_labels); nr = len(row_labels)
    cw = (W - label_w - 14 - cg * (nc - 1)) / nc
    heads = [wrap(c, hsize, cw - 10, 700, True) for c in col_labels]
    nl = max(len(h) for h in heads); hstep = hsize * 1.2
    top = 8 + nl * hstep + 6
    H = top + nr * ch + (nr - 1) * cg + 4
    flat = [round(v) for row in vals for v in row]; lo, hi = min(flat), max(flat)
    out = head(W, H)
    for j, hl in enumerate(heads):
        x = label_w + 14 + j * (cw + cg) + cw / 2
        for k, ln in enumerate(hl):  # bottom-aligned headers
            y = top - 8 - hstep * (len(hl) - 1 - k)
            out.append(text(x, y, ln, hsize, "middle", 700, True, PURPLE))
    for i, rl in enumerate(row_labels):
        y = top + i * (ch + cg)
        lines = wrap(rl, lsize, label_w - 4)
        assert len(lines) <= 2, lines
        out += lines_block(label_w, y + ch / 2, lines, lsize, "end")
        for j in range(nc):
            v = round(vals[i][j]); t = 0.12 + 0.88 * (v - lo) / (hi - lo if hi > lo else 1)
            fill = mix("#ECECF4", PURPLE, t); dark = t > 0.55
            x = label_w + 14 + j * (cw + cg)
            out.append(rect(x, y, cw, ch, fill, 3))
            is_ref = (ref == "row" and i == 0) or (ref == "col" and j == 0)
            fc = "#FFFFFF" if dark else INK
            if is_ref:
                out.append(text(x + cw / 2, y + ch / 2 + vsize * 0.36, f"{v}%", vsize, "middle", 700, False, fc))
            else:
                base = round(vals[0][j]) if ref == "row" else round(vals[i][0])
                d = v - base; ds = ("+" if d > 0 else "−" if d < 0 else "±") + str(abs(d))
                out.append(text(x + cw / 2, y + ch / 2 + 1, f"{v}%", vsize, "middle", 700, False, fc))
                out.append(text(x + cw / 2, y + ch / 2 + 16, f"{ds} points", dsize, "middle", 400, True,
                                "#FFFFFF" if dark else "#4B4B7A"))
    save(name, out)
    return W, H


# ----------------------------------------------------------------- network
def lum(c):
    r, g, b = (int(c[i:i + 2], 16) / 255 for i in (1, 3, 5))
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def network(name, src, r_new=35.0, size=10.5, wmin=0.8, wmax=4.2, gamma=0.8, margin=6):
    s = open(src).read()
    edges = [(float(a), float(b), float(c), float(d), float(w), float(o)) for a, b, c, d, w, o in re.findall(
        r"<line x1='([\d.]+)' y1='([\d.]+)' x2='([\d.]+)' y2='([\d.]+)' style='stroke-width: ([\d.]+); stroke: #5D6D7E; stroke-opacity: ([\d.]+);' />", s)]
    nodes = [(float(a), float(b), f) for a, b, f in re.findall(
        r"<circle cx='([\d.]+)' cy='([\d.]+)' r='[\d.]+' style='[^']*fill: (#[0-9A-F]+);' />", s)]
    labels = re.findall(r"<text x='([\d.]+)' y='([\d.]+)' text-anchor='middle' style='font-size: 15.65px;[^']*'[^>]*>([^<]*)</text>", s)
    lab = {}
    for x, y, t in labels:
        k = min(range(len(nodes)), key=lambda i: (nodes[i][0] - float(x)) ** 2 + (nodes[i][1] - float(y)) ** 2)
        lab.setdefault(k, []).append((float(y), t.replace("&amp;", "&")))
    ws = [e[4] for e in edges]; lo, hi = min(ws), max(ws)
    xs = [n[0] for n in nodes]; ys = [n[1] for n in nodes]
    X0, Y0 = min(xs) - r_new - margin, min(ys) - r_new - margin
    Wc, Hc = max(xs) - min(xs) + 2 * (r_new + margin), max(ys) - min(ys) + 2 * (r_new + margin)
    out = [f"<?xml version='1.0' encoding='UTF-8' ?>",
           f"<svg xmlns='http://www.w3.org/2000/svg' width='{Wc:.2f}pt' height='{Hc:.2f}pt' viewBox='{X0:.2f} {Y0:.2f} {Wc:.2f} {Hc:.2f}'>",
           f"<rect x='{X0:.2f}' y='{Y0:.2f}' width='{Wc:.2f}' height='{Hc:.2f}' style='fill:#FFFFFF;stroke:none;'/>"]
    for x1, y1, x2, y2, w, o in sorted(edges, key=lambda e: e[4]):
        nw = wmin + (wmax - wmin) * ((w - lo) / (hi - lo)) ** gamma
        out.append(f"<line x1='{x1}' y1='{y1}' x2='{x2}' y2='{y2}' style='stroke:#5D6D7E;stroke-opacity:{o};stroke-width:{nw:.2f};stroke-linecap:round;'/>")
    for i, (cx, cy, f) in enumerate(nodes):
        out.append(f"<circle cx='{cx}' cy='{cy}' r='{r_new}' style='fill:{f};stroke:#2C3E50;stroke-width:1.2;'/>")
        lines = [t for _, t in sorted(lab[i])]
        fc = "#FFFFFF" if lum(f) < 0.45 else INK
        for ln in lines:
            assert measure(ln, size, 700) < 2 * r_new - 4, ln
        out += lines_block(cx, cy, lines, size, "middle", 700, False, fc, lh=1.15)
    save(name, out)
    return Wc, Hc


# ----------------------------------------------------------------- single-series bars (Figure 36 without the EU average)
def bars(name, labels, vals, disp=None, label_w=170, size=14, vsize=12.52, pitch=25.0, bh=15.0):
    W = 595.28; x0 = label_w + 6; x1 = W - 56; full = x1 - x0; top = 8
    H = top * 2 + len(labels) * pitch - (pitch - bh)
    out = head(W, H)
    for i, (lab, v) in enumerate(zip(labels, vals)):
        y = top + i * pitch
        out.append(rect(x0, y, full, bh, TRACK)); out.append(rect(x0, y, full * v / 100, bh, PURPLE))
        out.append(text(label_w, y + bh / 2 + size * 0.36, lab, size, "end"))
        out.append(text(x1 + 7, y + bh / 2 + vsize * 0.36, disp[i] if disp else pct(v), vsize, "start", 600, True, PURPLE))
    save(name, out)
    return W, H


if __name__ == "__main__":
    # ---- Figure 11 and 16: sankeys (unchanged content)
    sankey("fig11_sankey_assistance.svg",
           {"contacted": 29.2, "not_contacted": 70.8},
           {"contacted": "Contacted an adviser:", "not_contacted": "Did not contact an adviser:"},
           {"appropriate": (21.4, "contacted"), "non_appropriate": (7.8, "contacted"),
            "needed_no": (37.8, "not_contacted"), "no_need": (25.0, "not_contacted"), "unknown": (8.0, "not_contacted")},
           {"appropriate": ["Appropriate adviser:"], "non_appropriate": ["Non-appropriate adviser:"],
            "needed_no": ["Needed but did not have", "access to an adviser:"], "no_need": ["Did not need an adviser:"],
            "unknown": ["Unknown reason:"]},
           ["appropriate", "non_appropriate", "needed_no", "no_need", "unknown"],
           {"appropriate": "access", "non_appropriate": "no_access", "needed_no": "no_access",
            "no_need": "excluded", "unknown": "excluded"},
           ["access", "no_access", "excluded"],
           {"access": ["Had access:"], "no_access": ["Had no access:"], "excluded": ["Did not need or", "unknown reason:"]},
           {"access": "32%", "no_access": "68%", "excluded": "33%"},
           {"excluded": ["of all respondents"]},
           [["Contacted", "an adviser"], ["Type of adviser or", "reason for no contact"],
            ["Access among those", "who needed assistance", "(SDG 16.3.3)"]])
    sankey("fig16_sankey_drm.svg",
           {"contacted": 12.6, "not_contacted": 87.4},
           {"contacted": "Contacted a DRM:", "not_contacted": "Did not contact a DRM:"},
           {"turned": (12.6, "contacted"), "needed_no": (31.9, "not_contacted"),
            "no_need": (47.1, "not_contacted"), "unknown": (8.4, "not_contacted")},
           {"turned": ["Turned to a DRM:"], "needed_no": ["Needed but did not have", "access to a DRM:"],
            "no_need": ["Did not need to", "turn to a DRM:"], "unknown": ["Unknown reason:"]},
           ["turned", "needed_no", "no_need", "unknown"],
           {"turned": "access", "needed_no": "no_access", "no_need": "excluded", "unknown": "excluded"},
           ["access", "no_access", "excluded"],
           {"access": ["Had access:"], "no_access": ["Had no access:"], "excluded": ["Did not need or", "unknown reason:"]},
           {"access": "28%", "no_access": "72%", "excluded": "56%"},
           {"excluded": ["of all respondents"]},
           [["Contacted", "a DRM"], ["Reason for", "no contact"], ["Access among those", "who needed a DRM", "(SDG 16.3.3)"]])
    # ---- Figure 8: funnel; assistance and DRMs among those who needed them (R03 variant c)
    r03 = {r["variable"]: float(r["hungria"]) for r in csv.DictReader(open(A + "R03.csv"))}
    print("fig08", funnel("fig08_funnel_pathway.svg", [
        ("Had access to information and advice", 61.46),
        ("Obtained appropriate assistance\nwhen they needed it", r03["rep_c_published"]),
        ("Turned to a dispute resolution\nmechanism when they needed it", r03["drm_c_published"])]))
    # ---- Figure 34: funnel, labels as in the subtitle
    print("fig34", funnel("fig34_funnel_capability.svg", [
        ("Reported knowledge of their legal rights", 85.49),
        ("Had access to information and advice", 61.46),
        ("Believe people know where\nto obtain advice", 27.19)]))
    # ---- Figure 32: scorecards (reference = fully resolved)
    a3 = {(r["desenlace"], r["grupo"]): float(r["pct"]) for r in csv.DictReader(open(A + "A03.csv")) if r["corte"] == "Resolución"}
    hs = ["At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol/Drug problems"]
    print("fig32", scorecards("fig32_scorecards_hardship.svg", ["At least one hardship", "Health issues", "Economic issues",
          "Relationship breakdown", "Alcohol or drug problems"], ["Fully resolved", "Not resolved"],
          [[a3[(h, "Fully resolved")], a3[(h, "Not resolved")]] for h in hs], ref="col", label_w=196))
    # ---- Figure 41 and 42: scorecards (reference = no non-trivial problem)
    a4 = {(r["actor"], r["grupo"]): float(r["pct"]) for r in csv.DictReader(open(A + "A04.csv"))}
    g41 = ["No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, had access", "Needed a DRM, no access"]
    actors = ["Judges", "Prosecutors", "Public defense attorneys"]
    print("fig41", scorecards("fig41_scorecards_exposure.svg", g41, actors,
          [[a4[(a, g)] for a in actors] for g in g41], ref="row"))
    a5 = {(r["actor"], r["grupo"]): float(r["pct"]) for r in csv.DictReader(open(A + "A05.csv")) if r["corte"] == "Escalera con hardship"}
    g42 = ["No non-trivial problem", "Problem, no hardship", "Problem, with hardship"]
    print("fig42", scorecards("fig42_scorecards_hardship.svg", g42, actors,
          [[a5[(a, g)] for a in actors] for g in g42], ref="row"))
    # ---- Figure 5: network
    print("fig05", network("fig05_network.svg", A + "cooccurrence_network.svg"))
    # ---- Figure 36: Hungary only
    s36 = open("/home/claude/tc/u4/word/media/image79.svg").read()
    labs = [t for t in re.findall(r"text-anchor='end' style='font-size: 14.00px;[^']*'[^>]*>([^<]*)</text>", s36)]
    hu = [float(w) / 363.71 * 100 for w in re.findall(r"<rect x='175.66' y='[\d.]+' width='([\d.]+)' height='[\d.]+' style='[^']*fill: #575796;' />", s36)]
    print("fig36", list(zip(labs, [round(v, 1) for v in hu])))
    disp = re.findall(r"<text x='546.65'[^>]*fill: #575796;[^>]*>([^<]*)</text>", s36)
    assert len(disp) == len(hu) == len(labs) and all(pct(v) == d for v, d in zip(hu, disp)), disp
    print("fig36", bars("fig36_causes.svg", [l.replace("&#39;", "'") for l in labs], hu, disp))
