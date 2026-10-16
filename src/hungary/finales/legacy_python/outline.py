"""Inter Tight text measurement and SVG text-to-path conversion.

Word renders SVG <text> with whatever font it finds on the machine; when Inter Tight is
missing, a wider fallback font clips labels. Converting text to outlines removes the
dependency: the figure looks the same everywhere.
"""
import io, re, functools, copy
from fontTools.ttLib import TTFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.transformPen import TransformPen
import uharfbuzz as hb
from lxml import etree

FD = "/home/claude/node_modules/@fontsource/inter-tight/files/inter-tight-{sub}-{w}-{s}.woff2"
SVG = "http://www.w3.org/2000/svg"


def _nearest(weight):
    return min([400, 600, 700], key=lambda x: abs(x - weight))


@functools.lru_cache(None)
def _font(weight, italic, sub="latin"):
    tt = TTFont(FD.format(sub=sub, w=_nearest(weight), s="italic" if italic else "normal"))
    tt.flavor = None
    buf = io.BytesIO(); tt.save(buf); data = buf.getvalue()
    tt = TTFont(io.BytesIO(data))
    face = hb.Face(data)
    f = hb.Font(face)
    return tt, f, face.upem, set(tt.getBestCmap())


def _shape(s, weight, italic):
    """Return [(font, glyph_name, x_advance_units, x_offset, y_offset, upem)] using latin, falling back to latin-ext."""
    out = []
    # split into runs by subset coverage
    runs, cur, cur_sub = [], "", None
    for ch in s:
        sub = "latin" if ord(ch) in _font(weight, italic)[3] or ch in "  " else "latin-ext"
        if cur_sub is None or sub == cur_sub:
            cur += ch; cur_sub = sub
        else:
            runs.append((cur, cur_sub)); cur, cur_sub = ch, sub
    if cur: runs.append((cur, cur_sub))
    for txt, sub in runs:
        tt, f, upem, _ = _font(weight, italic, sub)
        buf = hb.Buffer(); buf.add_str(txt.replace(" ", " ")); buf.guess_segment_properties()
        hb.shape(f, buf, {"kern": True, "liga": True})
        order = tt.getGlyphOrder()
        for info, pos in zip(buf.glyph_infos, buf.glyph_positions):
            out.append((tt, order[info.codepoint], pos.x_advance, pos.x_offset, pos.y_offset, upem))
    return out


def measure(s, size, weight=400, italic=False):
    return sum(adv * size / upem for _, _, adv, _, _, upem in _shape(s, weight, italic))


def wrap(s, size, maxw, weight=400, italic=False):
    """Greedy word wrap with real metrics."""
    words, lines, cur = s.split(" "), [], ""
    for w in words:
        t = (cur + " " + w).strip()
        if cur and measure(t, size, weight, italic) > maxw:
            lines.append(cur); cur = w
        else:
            cur = t
    if cur: lines.append(cur)
    return lines


def _num(v):
    s = f"{v:.2f}"
    s = s.rstrip("0").rstrip(".") if "." in s else s
    return "0" if s in ("-0", "") else s


def path_d(s, x, y, size, weight=400, italic=False, anchor="start", sx=1.0, length=None):
    """Outline of s with its baseline at (x, y). length forces the rendered width (textLength)."""
    glyphs = _shape(s, weight, italic)
    natural = sum(adv * size / upem for _, _, adv, _, _, upem in glyphs)
    if length is not None and natural > 0:
        sx = length / natural
    width = natural * sx
    x0 = x - {"start": 0, "middle": width / 2, "end": width}[anchor]
    parts, pen_x = [], 0.0
    for tt, gname, adv, xo, yo, upem in glyphs:
        k = size / upem
        gs = tt.getGlyphSet()
        pen = SVGPathPen(gs, ntos=_num)
        tp = TransformPen(pen, (k * sx, 0, 0, -k, x0 + (pen_x + xo * k) * sx, y - yo * k))
        gs[gname].draw(tp)
        d = pen.getCommands()
        if d: parts.append(d)
        pen_x += adv * k
    return "".join(parts), width


# ------------------------------------------------------------------ SVG post-processing
def _style(st):
    out = {}
    for decl in (st or "").split(";"):
        if ":" in decl:
            k, v = decl.split(":", 1); out[k.strip().lower()] = v.strip()
    return out


def _weight(v):
    if not v: return 400
    v = v.strip().lower()
    if v == "bold": return 700
    if v == "normal": return 400
    try: return int(float(v))
    except ValueError: return 400


def _px(v, default=12.0):
    if v is None: return default
    m = re.match(r"\s*([\d.]+)", str(v))
    return float(m.group(1)) if m else default


def outline_svg(src, dst):
    """Replace every <text> in src with an equivalent <path>. Returns the number of texts converted."""
    parser = etree.XMLParser(remove_blank_text=False, huge_tree=True)
    tree = etree.parse(src, parser); root = tree.getroot()
    n = 0
    for t in list(root.iter(f"{{{SVG}}}text")):
        txt = "".join(t.itertext())
        if not txt.strip():
            t.getparent().remove(t); continue
        st = _style(t.get("style"))
        size = _px(st.get("font-size") or t.get("font-size"), 12.0)
        weight = _weight(st.get("font-weight") or t.get("font-weight"))
        italic = (st.get("font-style") or t.get("font-style") or "") == "italic"
        anchor = st.get("text-anchor") or t.get("text-anchor") or "start"
        fill = st.get("fill") or t.get("fill") or "#000000"
        x = float((t.get("x") or "0").split()[0]); y = float((t.get("y") or "0").split()[0])
        length = t.get("textLength")
        length = _px(length) if length and (t.get("lengthAdjust") == "spacingAndGlyphs") else None
        if "white-space" not in st and "\n" not in txt:
            pass
        d, _ = path_d(txt, x, y, size, weight, italic, anchor, length=length)
        p = etree.Element(f"{{{SVG}}}path")
        p.set("d", d)
        pst = f"fill:{fill};stroke:none;"
        if st.get("fill-opacity"): pst += f"fill-opacity:{st['fill-opacity']};"
        if st.get("opacity"): pst += f"opacity:{st['opacity']};"
        # white halo (paint-order: stroke) used for labels drawn over lines
        if st.get("paint-order", "").startswith("stroke") and st.get("stroke"):
            halo = copy.deepcopy(p)
            halo.set("style", f"fill:{st['stroke']};stroke:{st['stroke']};stroke-width:{st.get('stroke-width','3px')};stroke-linejoin:round;")
            for a in ("transform", "clip-path"):
                if t.get(a): halo.set(a, t.get(a))
            t.addprevious(halo)
        p.set("style", pst)
        for a in ("transform", "clip-path", "class", "id"):
            if t.get(a): p.set(a, t.get(a))
        t.addprevious(p); t.getparent().remove(t); n += 1
    tree.write(dst, xml_declaration=True, encoding="UTF-8")
    return n


if __name__ == "__main__":
    import sys
    for a, b in zip(sys.argv[1::2], sys.argv[2::2]):
        print(a, "->", b, outline_svg(a, b))
