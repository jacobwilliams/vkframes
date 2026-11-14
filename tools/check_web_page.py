"""Check web pages written by vkframes (WindowProxy%export_web).

    python tools/check_web_page.py page.html ...

For each page: the scene's JSON parses, its arrays decode (base64) and every
array reference ([byte offset, count]) lies inside them, the frames' children
are frames, the views' frames exist, and the viewer script is there. Prints
a summary per page; exits 1 if a page fails.
"""

import base64
import json
import re
import sys


def refs(node, path=""):
    """The array references in the JSON: (path, [offset, count]), for keys
    that hold them."""
    keys = {"p", "t", "c", "q", "i", "n", "d", "s", "a", "b"}
    if isinstance(node, dict):
        for k, v in node.items():
            if k in keys and isinstance(v, list) and len(v) == 2 and all(isinstance(x, int) for x in v):
                yield f"{path}.{k}", v
            else:
                yield from refs(v, f"{path}.{k}")
    elif isinstance(node, list):
        for k, v in enumerate(node):
            yield from refs(v, f"{path}[{k}]")


def check(file):
    """Check one page: print its summary and return None if it's good, else the problems found."""
    text = open(file, encoding="utf-8").read()
    scene = re.search(r'<script type="application/json" id="vkf-scene">(.*?)</script>', text, re.S)
    data = re.search(r'<script type="application/octet-stream" id="vkf-data">(.*?)</script>', text, re.S)
    if not scene or not data:
        return f"{file}: not a vkframes page"
    s = json.loads(scene.group(1))
    nbytes = len(base64.b64decode(data.group(1).strip() or b""))
    problems = []
    for where, (offset, count) in refs(s):
        if offset % 4 or offset < 0 or offset + 4 * count > nbytes:
            problems.append(f"{where}: [{offset}, {count}] outside the {nbytes} bytes")
    frames = s["frames"]
    for k, f in enumerate(frames):
        for c in f["children"]:
            if not 0 <= c < len(frames):
                problems.append(f"frame {k}: child {c} is not a frame")
    for v in s.get("views", []):
        if not 0 <= v["frame"] < len(frames):
            problems.append(f"view {v['name']}: frame {v['frame']} is not a frame")
    if "<script type=\"module\">" not in text or "vkframes' web viewer" not in text:
        problems.append("no viewer script")
    if problems:
        return f"{file}: " + "; ".join(problems[:10])
    items = sum(len(f["items"]) for f in frames)
    tracks = sum(1 for f in frames if "track" in f)
    t = s.get("time", {"animate": False, "now": 0})
    kind = f"animated {t['start']:g} to {t['end']:g} s" if t["animate"] else f"a snapshot at {t['now']:g} s"
    print(f"{file}: {len(frames)} frames, {items} items, {tracks} tracks, {len(s.get('views', []))} views, "
          f"{nbytes // 1024} KiB of arrays, {kind}: ok")
    return None


def main():
    """Check the pages named on the command line; exit 1 if any fails (or none is named)."""
    failed = [r for r in (check(f) for f in sys.argv[1:]) if r]
    for r in failed:
        print(r)
    sys.exit(1 if failed or len(sys.argv) < 2 else 0)


if __name__ == "__main__":
    main()
