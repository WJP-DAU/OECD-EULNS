"""Sankey 'Pathways to accessing appropriate assistance and representation' (Hungary)
rebuilt in the report style (Inter Tight, WJP purple palette, svglite-like canvas).
Values: R03.csv (weighted, base = respondents with a non-trivial problem, n = 1166).
"""
import sys

# ---- data (R03) -----------------------------------------------------------
C1 = {"contacted": 29.2, "not_contacted": 70.8}
C2 = {  # node: (value, parent in column 1)
    "appropriate": (21.4, "contacted"),
    "non_appropriate": (7.8, "contacted"),
    "needed_no": (37.8, "not_contacted"),
    "no_need": (25.0, "not_contacted"),
    "unknown": (8.0, "not_contacted"),
}

VARIANTS = {
    # A: faithful to the published figure ('did not need' counted as access)
    "A": {
        "order3": ["access", "no_access"],
        "map": {"appropriate": "access", "no_need": "access",
                "non_appropriate": "no_access", "needed_no": "no_access", "unknown": "no_access"},
        "labels3": {"access": ["Had access"], "no_access": ["Had no access"]},
        "labels3b": {"access": ["Had access:"], "no_access": ["Had no access:"]},
        "notes3": {},
    },
    # B: aligned with the indicator in the text (SDG 16.3.3: excludes 'did not need' and 'unknown')
    "B": {
        "order3": ["access", "no_access", "excluded"],
        "map": {"appropriate": "access", "non_appropriate": "no_access", "needed_no": "no_access",
                "no_need": "excluded", "unknown": "excluded"},
        "labels3": {"access": ["Had access"], "no_access": ["Had no access"],
                    "excluded": ["Outside the", "indicator"]},
        "labels3b": {"access": ["Had access:"], "no_access": ["Had no access:"], "excluded": ["Outside the indicator:"]},
        "notes3": {"access": ("32% of those", "who needed it"), "no_access": ("68% of those", "who needed it")},
    },
}

# ---- style (taken from the report SVGs) -----------------------------------
FONT = '"Inter Tight"'
INK = "#1A1A1A"
PURPLE = "#575796"
PURPLE_MID = "#7D7DA8"
PURPLE_LIGHT = "#A8A8C8"
GREY_NODE = "#8E9393"
GREY_EXCL = "#C4C8C8"
OUT_COL = {"access": PURPLE, "no_access": PURPLE_LIGHT, "excluded": GREY_EXCL}

LAB2 = {
    "appropriate": "Appropriate adviser",
    "non_appropriate": "Non-appropriate adviser",
    "needed_no": "Needed, but had no access",
    "no_need": "Did not need an adviser",
    "unknown": "Unknown reason",
}
LAB1 = {"contacted": ["Contacted", "an adviser"], "not_contacted": ["Did not contact", "an adviser"]}

W, H = 595.28, 408.0
TOP, BOT = 76.0, 396.0
GAP = 10.0
BAR = 12.0
X1, X2, X3 = 82.0, 296.0, 512.0


def pct(v):
    return f"{round(v):.0f}%"


def layout(values, order, total_h, gap, scale):
    n = len(order)
    used = sum(values[k] for k in order) * scale + gap * (n - 1)
    y = TOP + (total_h - used) / 2
    pos = {}
    for k in order:
        h = values[k] * scale
        pos[k] = [y, y + h]
        y += h + gap
    return pos


def band(xa, ya0, ya1, xb, yb0, yb1, fill, op):
    xm = (xa + xb) / 2
    return (f"<path d='M{xa:.2f},{ya0:.2f} C{xm:.2f},{ya0:.2f} {xm:.2f},{yb0:.2f} {xb:.2f},{yb0:.2f} "
            f"L{xb:.2f},{yb1:.2f} C{xm:.2f},{yb1:.2f} {xm:.2f},{ya1:.2f} {xa:.2f},{ya1:.2f} Z' "
            f"style='fill:{fill};fill-opacity:{op};stroke:none;'/>")


def text(x, y, s, size=14, anchor="start", weight=400, italic=False, fill=INK, halo=False):
    st = f"font-size:{size:.2f}px;fill:{fill};font-family:{FONT};"
    if weight != 400:
        st += f"font-weight:{weight};"
    if italic:
        st += "font-style:italic;"
    if halo:
        st += "stroke:#FFFFFF;stroke-width:3.2px;stroke-linejoin:round;paint-order:stroke;"
    return f"<text x='{x:.2f}' y='{y:.2f}' text-anchor='{anchor}' style='{st}'>{s}</text>"


def label_val(x, y, lab, val, size=14, anchor="start", halo=False):
    """'Label 21%' with the value in the report's bold-italic purple."""
    st = f"font-size:{size:.2f}px;fill:{INK};font-family:{FONT};"
    if halo:
        st += "stroke:#FFFFFF;stroke-width:3.2px;stroke-linejoin:round;paint-order:stroke;"
    return (f"<text x='{x:.2f}' y='{y:.2f}' text-anchor='{anchor}' style='{st}'>{lab} "
            f"<tspan style='font-weight:600;font-style:italic;fill:{PURPLE};'>{val}</tspan></text>")


BOX_FILL = "#F2F3F3"
BOX_STROKE = "#1A1A1A"
LAB1B = {"contacted": "Contacted an adviser", "not_contacted": "Did not contact an adviser"}
LAB2B = {
    "appropriate": ["Appropriate adviser:"],
    "non_appropriate": ["Non-appropriate adviser:"],
    "needed_no": ["Needed but did not have", "access to an adviser:"],
    "no_need": ["Did not need an adviser:"],
    "unknown": ["Unknown reason:"],
}


def tw(s, size, bold=False):
    return len(s) * size * (0.52 if bold else 0.49)


def box(out, xc, yc, lines, val, size=12.0, note=None):
    lh = size * 1.22
    rows = [(ln, "lab") for ln in lines] + [(val, "val")] + ([(n, "note") for n in note] if note else [])
    widths = [tw(t, 10.5 if kind == "note" else size, kind == "val") for t, kind in rows]
    w = max(widths) + 14
    h = lh * len(rows) + 7
    x0, y0 = xc - w / 2, yc - h / 2
    out.append(f"<rect x='{x0:.2f}' y='{y0:.2f}' width='{w:.2f}' height='{h:.2f}' rx='2.5' "
               f"style='fill:{BOX_FILL};stroke:{BOX_STROKE};stroke-width:0.6;'/>")
    y = y0 + 3.5 + lh * 0.8
    for t, kind in rows:
        if kind == "lab":
            out.append(text(xc, y, t, size, "middle"))
        elif kind == "val":
            out.append(text(xc, y, t, size, "middle", 600, True, PURPLE))
        else:
            out.append(text(xc, y, t, 10.5, "middle", 400, True, PURPLE_MID))
        y += lh


def build(vkey):
    v = VARIANTS[vkey]
    m = v["map"]
    order1 = ["contacted", "not_contacted"]
    order2 = ["appropriate", "non_appropriate", "needed_no", "no_need", "unknown"]
    order3 = v["order3"]
    val2 = {k: C2[k][0] for k in order2}
    val3 = {o: sum(val2[k] for k in order2 if m[k] == o) for o in order3}
    total_h = BOT - TOP
    scale = (total_h - GAP * (len(order2) - 1)) / 100.0
    p1 = layout(C1, order1, total_h, GAP, scale)
    p2 = layout(val2, order2, total_h, GAP, scale)
    p3 = layout(val3, order3, total_h, GAP, scale)

    out = [f"<?xml version='1.0' encoding='UTF-8' ?>",
           f"<svg xmlns='http://www.w3.org/2000/svg' width='{W}pt' height='{H}pt' viewBox='0 0 {W} {H}'>",
           f"<rect width='100%' height='100%' style='fill:#FFFFFF;stroke:none;'/>"]

    # flows col1 -> col2 (colored by final outcome)
    cur1 = {k: p1[k][0] for k in order1}
    for k in order2:
        h = val2[k] * scale
        par = C2[k][1]
        out.append(band(X1 + BAR, cur1[par], cur1[par] + h, X2, p2[k][0], p2[k][1], OUT_COL[m[k]], 0.30))
        cur1[par] += h
    # flows col2 -> col3
    cur3 = {o: p3[o][0] for o in order3}
    for k in order2:
        o = m[k]
        h = val2[k] * scale
        out.append(band(X2 + BAR, p2[k][0], p2[k][1], X3, cur3[o], cur3[o] + h, OUT_COL[o], 0.45))
        cur3[o] += h

    # nodes
    for k in order1:
        out.append(f"<rect x='{X1}' y='{p1[k][0]:.2f}' width='{BAR}' height='{p1[k][1]-p1[k][0]:.2f}' style='fill:{GREY_NODE};stroke:none;'/>")
    for k in order2:
        out.append(f"<rect x='{X2}' y='{p2[k][0]:.2f}' width='{BAR}' height='{p2[k][1]-p2[k][0]:.2f}' style='fill:{OUT_COL[m[k]]};stroke:none;'/>")
    for o in order3:
        out.append(f"<rect x='{X3}' y='{p3[o][0]:.2f}' width='{BAR}' height='{p3[o][1]-p3[o][0]:.2f}' style='fill:{OUT_COL[o]};stroke:none;'/>")

    # column headers (report facet-title style: bold italic purple), centered on columns
    hs = 13.0
    for xc, lines in [(X1 + BAR / 2, ["Contacted", "an adviser"]),
                      (X2 + BAR / 2, ["Type of adviser or", "reason for no contact"]),
                      (X3 + BAR / 2, ["Access to appropriate", "assistance and", "representation"])]:
        for i, ln in enumerate(lines):
            out.append(text(xc, 22 + 16 * i, ln, hs, "middle", 700, True, PURPLE))

    # labels: boxes centered on the nodes (original layout), report typography
    for k in order1:
        box(out, X1 + BAR / 2, sum(p1[k]) / 2, [LAB1B[k] + ":"], pct(C1[k]))
    for k in order2:
        box(out, X2 + BAR / 2, sum(p2[k]) / 2, LAB2B[k], pct(val2[k]))
    for o in order3:
        note = v["notes3"].get(o)
        box(out, X3 + BAR / 2, sum(p3[o]) / 2, v["labels3b"][o], pct(val3[o]),
            note=note)
    out.append("</svg>")
    return "\n".join(out)


if __name__ == "__main__":
    for k in VARIANTS:
        with open(f"/home/claude/sankey/Hungary_sankey_advice_representation_{k}.svg", "w") as f:
            f.write(build(k))
    print("ok")
