"""Report-style figures for the SEP30 v4 revision (Inter Tight, WJP purple palette)."""
import re, csv

FONT = '"Inter Tight"'
INK = "#1A1A1A"; PURPLE = "#575796"; PURPLE_MID = "#7D7DA8"; PURPLE_LIGHT = "#A8A8C8"
MAGENTA = "#B200AA"; GREY_NODE = "#8E9393"; GREY_EXCL = "#C4C8C8"; TRACK = "#E5E8E8"
BOX_FILL = "#F2F3F3"; BOX_STROKE = "#1A1A1A"
OUT_COL = {"access": PURPLE, "no_access": PURPLE_LIGHT, "excluded": GREY_EXCL}
A = "/mnt/user-data/uploads/Hungary/analisis adicionales/"
OUT = "/home/claude/v4/fig/"


def head(w, h):
    return [f"<?xml version='1.0' encoding='UTF-8' ?>",
            f"<svg xmlns='http://www.w3.org/2000/svg' width='{w}pt' height='{h}pt' viewBox='0 0 {w} {h}'>",
            f"<rect width='100%' height='100%' style='fill:#FFFFFF;stroke:none;'/>"]


def text(x, y, s, size=14, anchor="start", weight=400, italic=False, fill=INK):
    st = f"font-size:{size:.2f}px;fill:{fill};font-family:{FONT};"
    if weight != 400: st += f"font-weight:{weight};"
    if italic: st += "font-style:italic;"
    return f"<text x='{x:.2f}' y='{y:.2f}' text-anchor='{anchor}' style='{st}'>{s}</text>"


def rect(x, y, w, h, fill, rx=0, stroke=None, sw=0.6):
    st = f"fill:{fill};" + (f"stroke:{stroke};stroke-width:{sw};" if stroke else "stroke:none;")
    return f"<rect x='{x:.2f}' y='{y:.2f}' width='{w:.2f}' height='{h:.2f}' rx='{rx}' style='{st}'/>"


def save(name, out):
    out.append("</svg>")
    open(OUT + name, "w").write("\n".join(out))


def pct(v): return f"{round(v):.0f}%"


# ----------------------------------------------------------------- sankey
def tw(s, size, bold=False): return len(s) * size * (0.52 if bold else 0.49)


def box(out, xc, yc, lines, val, size=12.0, note=None, val_fill=PURPLE):
    lh = size * 1.22
    rows = [(l, "lab") for l in lines] + ([(val, "val")] if val else []) + ([(n, "note") for n in note] if note else [])
    w = max(tw(t, 10.5 if k == "note" else size, k == "val") for t, k in rows) + 14
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
def funnel(name, rows, label_w=230):
    """rows: [(label, value)] drawn as a centered funnel, no EU average."""
    W = 595.28; bh = 34; gap = 16; top = 8
    H = top * 2 + len(rows) * bh + (len(rows) - 1) * gap
    x0 = label_w + 16; x1 = W - 12; cx = (x0 + x1) / 2; full = x1 - x0
    out = head(W, H)
    geo = []
    for i, (lab, v) in enumerate(rows):
        y = top + i * (bh + gap); w = full * v / 100
        geo.append((y, cx - w / 2, cx + w / 2))
    for i in range(len(geo) - 1):  # connectors
        (ya, la, ra), (yb, lb, rb) = geo[i], geo[i + 1]
        out.append(f"<path d='M{la:.2f},{ya+bh:.2f} L{ra:.2f},{ya+bh:.2f} L{rb:.2f},{yb:.2f} L{lb:.2f},{yb:.2f} Z' "
                   f"style='fill:{PURPLE_LIGHT};fill-opacity:0.45;stroke:none;'/>")
    for (lab, v), (y, l, r) in zip(rows, geo):
        out.append(rect(l, y, r - l, bh, PURPLE))
        out.append(text(label_w, y + bh / 2 + 5, lab, 14, "end"))
        if r - l > 44:
            out.append(text(cx, y + bh / 2 + 5, pct(v), 14, "middle", 600, True, "#FFFFFF"))
        else:
            out.append(text(r + 8, y + bh / 2 + 5, pct(v), 14, "start", 600, True, PURPLE))
    save(name, out)


# ----------------------------------------------------------------- scorecards
def mix(c1, c2, t):
    a = [int(c1[i:i + 2], 16) for i in (1, 3, 5)]; b = [int(c2[i:i + 2], 16) for i in (1, 3, 5)]
    return "#" + "".join(f"{round(a[i] + (b[i] - a[i]) * t):02X}" for i in range(3))


def scorecards(name, row_labels, col_labels, vals, ref="row", label_w=190, note_ref=None):
    """vals[r][c] in %. Deltas (percentage points) vs. first row (ref='row') or first column (ref='col')."""
    W = 595.28; ch = 52; cg = 6
    nl = max(len(c) if isinstance(c, list) else 1 for c in col_labels); top = 18 + 16 * nl
    nc = len(col_labels); nr = len(row_labels)
    cw = (W - label_w - 12 - cg * (nc - 1)) / nc
    H = top + nr * ch + (nr - 1) * cg + 8
    flat = [round(v) for row in vals for v in row]; lo, hi = min(flat), max(flat)
    out = head(W, H)
    for j, cl in enumerate(col_labels):
        x = label_w + 12 + j * (cw + cg) + cw / 2
        ls = cl if isinstance(cl, list) else [cl]
        for k, ln in enumerate(ls):
            out.append(text(x, top - 10 - 16 * (len(ls) - 1 - k), ln, 13, "middle", 700, True, PURPLE))
    for i, rl in enumerate(row_labels):
        y = top + i * (ch + cg)
        lines = rl if isinstance(rl, list) else [rl]
        for k, ln in enumerate(lines):
            out.append(text(label_w, y + ch / 2 + 5 - 8 * (len(lines) - 1) + 16 * k, ln, 14, "end"))
        for j in range(nc):
            v = round(vals[i][j]); t = 0.12 + 0.88 * (v - lo) / (hi - lo if hi > lo else 1)
            fill = mix("#ECECF4", PURPLE, t); dark = t > 0.55
            x = label_w + 12 + j * (cw + cg)
            out.append(rect(x, y, cw, ch, fill, 3))
            is_ref = (ref == "row" and i == 0) or (ref == "col" and j == 0)
            fc = "#FFFFFF" if dark else INK
            if is_ref:
                out.append(text(x + cw / 2, y + ch / 2 + 7, f"{v}%", 20, "middle", 700, False, fc))
            else:
                base = round(vals[0][j]) if ref == "row" else round(vals[i][0])
                d = v - base; ds = ("+" if d > 0 else "−" if d < 0 else "±") + str(abs(d))
                out.append(text(x + cw / 2, y + ch / 2 + 1, f"{v}%", 20, "middle", 700, False, fc))
                out.append(text(x + cw / 2, y + ch / 2 + 17, f"{ds} points", 11, "middle", 400, True,
                                "#FFFFFF" if dark else PURPLE_MID))
    save(name, out)


# ----------------------------------------------------------------- dot plot (option B)
def dotplot(name, actors, groups, vals, colors, xmax=60, label_w=170):
    W = 595.28; rh = 44; top = 62
    H = top + len(actors) * rh + 24
    x0 = label_w + 16; x1 = W - 24
    X = lambda v: x0 + (x1 - x0) * v / xmax
    out = head(W, H)
    for k, (g, c) in enumerate(zip(groups, colors)):
        lx = x0 + (k % 2) * 200; ly = 12 + (k // 2) * 16
        out.append(f"<circle cx='{lx+5:.2f}' cy='{ly}' r='5' style='fill:{c};stroke:none;'/>")
        out.append(text(lx + 14, ly + 4.5, g, 11.5))
    for t in range(0, xmax + 1, 10):
        out.append(f"<line x1='{X(t):.2f}' y1='{top-8}' x2='{X(t):.2f}' y2='{H-24}' style='stroke:#E5E8E8;stroke-width:0.8;'/>")
        out.append(text(X(t), H - 8, f"{t}%", 11, "middle"))
    for i, a in enumerate(actors):
        y = top + i * rh + rh / 2
        out.append(text(label_w, y + 5, a, 14, "end"))
        vs = [round(v) for v in vals[i]]
        out.append(f"<line x1='{X(min(vs)):.2f}' y1='{y}' x2='{X(max(vs)):.2f}' y2='{y}' style='stroke:#C4C8C8;stroke-width:2;'/>")
        for v, c in zip(vs, colors):
            out.append(f"<circle cx='{X(v):.2f}' cy='{y}' r='6.5' style='fill:{c};stroke:#FFFFFF;stroke-width:1.2;'/>")
    save(name, out)


if __name__ == "__main__":
    import os; os.makedirs(OUT, exist_ok=True)
    # ---- Figure 11: assistance and representation
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
    # ---- Figure 16: DRMs (same layout and colors)
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
    # ---- Figure 8 and 34: funnels
    funnel("fig08_funnel_pathway.svg", [("Had access to information and advice", 61.46),
                                         ("Obtained appropriate assistance", 21.39),
                                         ("Turned to a dispute resolution mechanism", 12.60)], label_w=235)
    funnel("fig34_funnel_capability.svg", [("Understand their legal rights", 85.49),
                                            ("Had access to information and advice", 61.46),
                                            ("Believe people know where to get advice", 27.19)], label_w=235)
    # ---- Figure 32: scorecards (reference = fully resolved)
    a3 = {(r["desenlace"], r["grupo"]): float(r["pct"]) for r in csv.DictReader(open(A + "A03.csv")) if r["corte"] == "Resolución"}
    hs = ["At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol/Drug problems"]
    scorecards("fig32_scorecards_hardship.svg", ["At least one hardship", "Health issues", "Economic issues",
               "Relationship breakdown", "Alcohol or drug problems"], ["Fully resolved", "Not resolved"],
               [[a3[(h, "Fully resolved")], a3[(h, "Not resolved")]] for h in hs], ref="col", label_w=200)
    # ---- Figure 41 and 42: scorecards (reference = no non-trivial problem)
    a4 = {(r["actor"], r["grupo"]): float(r["pct"]) for r in csv.DictReader(open(A + "A04.csv"))}
    g41 = ["No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, had access", "Needed a DRM, no access"]
    actors = ["Judges", "Prosecutors", "Public defense attorneys"]
    scorecards("fig41_scorecards_exposure.svg", g41, ["Judges", "Prosecutors", ["Public defense", "attorneys"]],
               [[a4[(a, g)] for a in actors] for g in g41], ref="row", label_w=185)
    a5 = {(r["actor"], r["grupo"]): float(r["pct"]) for r in csv.DictReader(open(A + "A05.csv")) if r["corte"] == "Escalera con hardship"}
    g42 = ["No non-trivial problem", "Problem, no hardship", "Problem, with hardship"]
    scorecards("fig42_scorecards_hardship.svg", g42, ["Judges", "Prosecutors", ["Public defense", "attorneys"]],
               [[a5[(a, g)] for a in actors] for g in g42], ref="row", label_w=185)
    # ---- options for 41 (preview only): dot plot
    dotplot("opt41_dotplot.svg", actors, g41, [[a4[(a, g)] for g in g41] for a in actors],
            [PURPLE, PURPLE_LIGHT, MAGENTA, "#E0A3DD"])
    print("ok")


# ----------------------------------------------------------------- funnel alternatives (preview)
def funnel_trap(name, rows, label_w=235):
    """B: continuous funnel; each stage is a trapezoid from its value to the next one."""
    W = 595.28; sh = 46; top = 8; H = top * 2 + len(rows) * sh
    x0 = label_w + 16; x1 = W - 12; cx = (x0 + x1) / 2; full = x1 - x0
    out = head(W, H)
    shades = [PURPLE, "#6B6BA4", PURPLE_MID]
    for i, (lab, v) in enumerate(rows):
        y = top + i * sh
        wt = full * v / 100; nxt = rows[i + 1][1] if i + 1 < len(rows) else v * 0.75
        wb = full * nxt / 100
        out.append(f"<path d='M{cx-wt/2:.2f},{y:.2f} L{cx+wt/2:.2f},{y:.2f} L{cx+wb/2:.2f},{y+sh-3:.2f} L{cx-wb/2:.2f},{y+sh-3:.2f} Z' style='fill:{shades[i%3]};stroke:none;'/>")
        out.append(text(cx, y + sh / 2 + 5, pct(v), 14, "middle", 600, True, "#FFFFFF"))
        out.append(text(label_w, y + sh / 2 + 5, lab, 14, "end"))
    save(name, out)


def funnel_steps(name, rows, label_w=235):
    """C: left-aligned bars on a common 0-100 track, with the drop between stages annotated."""
    W = 595.28; bh = 28; gap = 26; top = 8
    H = top * 2 + len(rows) * bh + (len(rows) - 1) * gap
    x0 = label_w + 16; x1 = W - 50; full = x1 - x0
    out = head(W, H)
    for i, (lab, v) in enumerate(rows):
        y = top + i * (bh + gap)
        out.append(rect(x0, y, full, bh, TRACK)); out.append(rect(x0, y, full * v / 100, bh, PURPLE))
        out.append(text(label_w, y + bh / 2 + 5, lab, 14, "end"))
        out.append(text(x1 + 8, y + bh / 2 + 5, pct(v), 14, "start", 600, True, PURPLE))
        if i + 1 < len(rows):
            nv = rows[i + 1][1]; d = round(v) - round(nv)
            xa = x0 + full * nv / 100; xb = x0 + full * v / 100; ym = y + bh + gap / 2
            out.append(f"<line x1='{xa:.2f}' y1='{ym:.2f}' x2='{xb:.2f}' y2='{ym:.2f}' style='stroke:{MAGENTA};stroke-width:1.2;'/>")
            out.append(text((xa + xb) / 2, ym + 4, f"−{d} points", 11, "middle", 600, True, MAGENTA).replace("<text", "<text paint-order='stroke' ", 1).replace("font-style:italic;", "font-style:italic;stroke:#FFFFFF;stroke-width:4px;paint-order:stroke;"))
    save(name, out)


if __name__ == "__main__":
    R8 = [("Had access to information and advice", 61.46), ("Obtained appropriate assistance", 21.39), ("Turned to a dispute resolution mechanism", 12.60)]
    funnel_trap("optF_trapezoid.svg", R8, label_w=262)
    funnel_steps("optF_steps.svg", R8, label_w=262)
