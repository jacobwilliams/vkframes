"""Download the Moon's global elevation (LRO LOLA's gridded DEM from NASA's
PDS) and convert it into data/moon_lola_ldem<N>.tif, for Rocky maps of the
Moon (`pixi run -e rocky moon-data`).

Options: --resolution N, pixels per degree: 4 (7.6 km, 2 MB), 16 (1.9 km,
33 MB; the default) or 64 (474 m, 530 MB to download); --force to make the
file again.

LDEM_<N> is a raw grid of 16-bit heights in half meters above a sphere of
radius 1737.4 km, longitudes 0 to 360 degrees east (centered on 180). The
GeoTIFF holds 32-bit float heights in meters, longitudes -180 to 180, on that
sphere (Rocky's "moon" coordinate system), tiled and compressed. GDAL
(gdal_translate, in the rocky environment) does the conversion, through two
VRTs: the raw grid, and the heights scaled with the hemispheres swapped.
"""

import argparse
import os
import subprocess
import sys
import tempfile
import urllib.request

BASE = "https://pds-geosciences.wustl.edu/lro/lro-l-lola-3-rdr-v1/lrolol_1xxx/data/lola_gdr/cylindrical/img"
SIZES = {4: "2 MB", 16: "33 MB", 64: "530 MB"}
SCALE = 0.5                                # meters per DN
SRS = "+proj=longlat +R=1737400 +no_defs"

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def output_file(resolution):
    return os.path.join(ROOT, "data", f"moon_lola_ldem{resolution}.tif")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--resolution", type=int, choices=sorted(SIZES), default=16)
    parser.add_argument("--force", action="store_true")
    args = parser.parse_args()
    res = args.resolution
    out = output_file(res)
    if os.path.exists(out) and not args.force:
        print(f"{os.path.relpath(out, ROOT)} exists (--force to make it again)")
        return 0
    width, height = 360 * res, 180 * res
    half = width // 2
    with tempfile.TemporaryDirectory() as tmp:
        img = os.path.join(tmp, f"ldem_{res}.img")
        url = f"{BASE}/ldem_{res}.img"
        print(f"downloading {url} ({SIZES[res]})", flush=True)
        urllib.request.urlretrieve(url, img)

        # the raw grid (LSB 16-bit integers, rows from 90 degrees north, columns from 0 east)
        raw = os.path.join(tmp, "raw.vrt")
        with open(raw, "w") as f:
            f.write(f"""<VRTDataset rasterXSize="{width}" rasterYSize="{height}">
  <VRTRasterBand dataType="Int16" band="1" subClass="VRTRawRasterBand">
    <SourceFilename relativeToVRT="1">{os.path.basename(img)}</SourceFilename>
    <ByteOrder>LSB</ByteOrder>
    <ImageOffset>0</ImageOffset><PixelOffset>2</PixelOffset><LineOffset>{2 * width}</LineOffset>
  </VRTRasterBand>
</VRTDataset>
""")
        # heights in meters, columns from -180 degrees: the eastern half (180-360) first
        def source(src_x, dst_x):
            return f"""    <ComplexSource>
      <SourceFilename relativeToVRT="1">raw.vrt</SourceFilename><SourceBand>1</SourceBand>
      <SrcRect xOff="{src_x}" yOff="0" xSize="{half}" ySize="{height}"/>
      <DstRect xOff="{dst_x}" yOff="0" xSize="{half}" ySize="{height}"/>
      <ScaleOffset>0</ScaleOffset><ScaleRatio>{SCALE}</ScaleRatio>
    </ComplexSource>
"""
        heights = os.path.join(tmp, "heights.vrt")
        with open(heights, "w") as f:
            f.write(f"""<VRTDataset rasterXSize="{width}" rasterYSize="{height}">
  <VRTRasterBand dataType="Float32" band="1">
{source(half, 0)}{source(0, half)}  </VRTRasterBand>
</VRTDataset>
""")
        cmd = ["gdal_translate", "-of", "GTiff", "-co", "TILED=YES", "-co", "COMPRESS=DEFLATE", "-co", "PREDICTOR=3",
               "-co", "BIGTIFF=IF_SAFER", "-a_srs", SRS, "-a_ullr", "-180", "90", "180", "-90", heights, out]
        print("+", " ".join(cmd), flush=True)
        subprocess.check_call(cmd)
    print(f"wrote {os.path.relpath(out, ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
