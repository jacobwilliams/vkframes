"""Download NASA's Apollo Lunar Module model (NASA 3D Resources,
https://github.com/nasa/NASA-3D-Resources) and convert it into
data/Models/apollo_lunar_module.glb, for the lunar demo's lander (`pixi run
-e rocky lander-model`).

Options: --force to make the file again.

The model is a glTF binary whose meshes are Draco-compressed and whose
textures are WebP, neither of which vsgXchange's glTF reader supports. The
conversion decodes the meshes (DracoPy) into plain accessors (floats;
indices as 32-bit integers) and the textures into PNG (Pillow), and writes a
glTF binary without those extensions; everything else (nodes, materials)
is kept. The model is in meters (about 6.4 m across the footpads, 5 m tall),
y up, its footpads' bottoms near y = 0.

NASA's 3D resources are free and without copyright; see NASA's usage
guidelines (https://www.nasa.gov/nasa-brand-center/images-and-media): no
endorsement by NASA is implied.
"""

import argparse
import io
import json
import os
import struct
import sys
import tempfile
import urllib.request

import DracoPy
import numpy as np
from PIL import Image

URL = ("https://raw.githubusercontent.com/nasa/NASA-3D-Resources/master/"
       "3D%20Models/Apollo%20Lunar%20Module/Apollo%20Lunar%20Module.glb")

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUTPUT = os.path.join(ROOT, "data", "Models", "apollo_lunar_module.glb")

GLB_MAGIC, CHUNK_JSON, CHUNK_BIN = 0x46546C67, 0x4E4F534A, 0x004E4942
FLOAT, UNSIGNED_INT = 5126, 5125
COMPONENTS = {"SCALAR": 1, "VEC2": 2, "VEC3": 3, "VEC4": 4}
DRACO, WEBP = "KHR_draco_mesh_compression", "EXT_texture_webp"


def read_glb(data):
    """The JSON and binary chunks of a glTF binary."""
    magic, version, _ = struct.unpack_from("<III", data, 0)
    if magic != GLB_MAGIC or version != 2:
        raise ValueError("not a glTF 2 binary")
    gltf, binary, offset = None, b"", 12
    while offset < len(data):
        length, kind = struct.unpack_from("<II", data, offset)
        chunk = data[offset + 8:offset + 8 + length]
        if kind == CHUNK_JSON:
            gltf = json.loads(chunk)
        elif kind == CHUNK_BIN:
            binary = chunk
        offset += 8 + length
    return gltf, binary


def write_glb(gltf, binary):
    """A glTF binary of the JSON and binary chunks (each padded to 4 bytes)."""
    text = json.dumps(gltf, separators=(",", ":")).encode()
    text += b" " * (-len(text) % 4)
    binary += b"\0" * (-len(binary) % 4)
    total = 12 + 8 + len(text) + 8 + len(binary)
    return (struct.pack("<III", GLB_MAGIC, 2, total) + struct.pack("<II", len(text), CHUNK_JSON) + text +
            struct.pack("<II", len(binary), CHUNK_BIN) + binary)


class Buffer:
    """The new binary chunk: buffer views appended, each 4-byte aligned."""

    def __init__(self):
        self.data = bytearray()
        self.views = []

    def add(self, payload, target=None):
        self.data += b"\0" * (-len(self.data) % 4)
        view = {"buffer": 0, "byteOffset": len(self.data), "byteLength": len(payload)}
        if target is not None:
            view["target"] = target
        self.data += payload
        self.views.append(view)
        return len(self.views) - 1


def draco_attribute(mesh, unique_id):
    """The decoded values of a Draco attribute, by its unique id."""
    for attribute in mesh.attributes:
        if attribute["unique_id"] == unique_id:
            return np.asarray(attribute["data"])
    raise ValueError(f"no Draco attribute {unique_id}")


def convert(gltf, binary):
    """The model without Draco meshes or WebP textures (gltf, binary)."""
    views = gltf.get("bufferViews", [])
    accessors = gltf.get("accessors", [])
    out = Buffer()
    copied = {}

    def copy_view(k):                                   # an existing buffer view, into the new buffer
        if k not in copied:
            v = views[k]
            start = v.get("byteOffset", 0)
            new = out.add(binary[start:start + v["byteLength"]], v.get("target"))
            if "byteStride" in v:
                out.views[new]["byteStride"] = v["byteStride"]
            copied[k] = new
        return copied[k]

    # the Draco meshes: decoded into plain accessors
    decoded = set()
    for mesh in gltf.get("meshes", []):
        for primitive in mesh["primitives"]:
            draco = primitive.get("extensions", {}).pop(DRACO, None)
            if draco is None:
                continue
            if not primitive["extensions"]:
                del primitive["extensions"]
            v = views[draco["bufferView"]]
            start = v.get("byteOffset", 0)
            decoded_mesh = DracoPy.decode(binary[start:start + v["byteLength"]])
            for semantic, unique_id in draco["attributes"].items():
                k = primitive["attributes"][semantic]
                a = accessors[k]
                values = draco_attribute(decoded_mesh, unique_id).astype("<f4")
                values = values.reshape(a["count"], COMPONENTS[a["type"]])
                a["bufferView"] = out.add(values.tobytes(), 34962)
                a["byteOffset"] = 0
                a["componentType"] = FLOAT
                a.pop("normalized", None)
                if semantic == "POSITION":
                    a["min"] = values.min(axis=0).tolist()
                    a["max"] = values.max(axis=0).tolist()
                decoded.add(k)
            if "indices" in primitive:
                k = primitive["indices"]
                faces = np.asarray(decoded_mesh.faces, dtype="<u4").reshape(-1)
                a = accessors[k]
                a.update(bufferView=out.add(faces.tobytes(), 34963), byteOffset=0, componentType=UNSIGNED_INT,
                         count=int(faces.size))
                decoded.add(k)

    # the other accessors' data, copied
    for k, a in enumerate(accessors):
        if k not in decoded and "bufferView" in a:
            a["bufferView"] = copy_view(a["bufferView"])

    # the WebP images: as PNG; textures pointed at them
    for image in gltf.get("images", []):
        if "bufferView" not in image:
            raise ValueError("an image outside the file: " + image.get("uri", "?"))
        v = views[image["bufferView"]]
        start = v.get("byteOffset", 0)
        payload = binary[start:start + v["byteLength"]]
        if image.get("mimeType") == "image/webp":
            png = io.BytesIO()
            Image.open(io.BytesIO(payload)).save(png, format="PNG", optimize=True)
            image["mimeType"] = "image/png"
            image["bufferView"] = out.add(png.getvalue())
        else:
            image["bufferView"] = copy_view(image["bufferView"])
    for texture in gltf.get("textures", []):
        webp = texture.get("extensions", {}).pop(WEBP, None)
        if webp is not None:
            texture["source"] = webp["source"]
            if not texture["extensions"]:
                del texture["extensions"]

    gltf["bufferViews"] = out.views
    out.data += b"\0" * (-len(out.data) % 4)            # (the binary chunk's length, as readers check)
    gltf["buffers"] = [{"byteLength": len(out.data)}]
    for key in ("extensionsUsed", "extensionsRequired"):
        if key in gltf:
            gltf[key] = [e for e in gltf[key] if e not in (DRACO, WEBP)]
            if not gltf[key]:
                del gltf[key]
    return gltf, bytes(out.data)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--force", action="store_true")
    args = parser.parse_args()
    if os.path.exists(OUTPUT) and not args.force:
        print(f"{os.path.relpath(OUTPUT, ROOT)} exists (--force to make it again)")
        return 0
    print(f"downloading {URL}")
    with urllib.request.urlopen(URL) as response:
        data = response.read()
    gltf, binary = convert(*read_glb(data))
    os.makedirs(os.path.dirname(OUTPUT), exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=os.path.dirname(OUTPUT), delete=False) as f:
        f.write(write_glb(gltf, binary))
    os.replace(f.name, OUTPUT)
    print(f"wrote {os.path.relpath(OUTPUT, ROOT)} ({os.path.getsize(OUTPUT) / 1e6:.1f} MB)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
