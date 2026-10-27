# Greyscale JPEG/PNG imagery renders red: the GDAL reader takes `.jpg`/`.png` ahead of vsgXchange

## Summary

`VSGContextImpl::ctor` registers Rocky's `GDAL_VSG_ReaderWriter` **before** `vsgXchange::all`. Image streams are read by the first reader that matches the extension (`findReaderWriter`), and the GDAL reader claims `.jpg` and `.png` (as well as `.tif` and `.webp`). So every JPEG and PNG tile is decoded by GDAL, not by vsgXchange's stb reader. The class's own comment says it is meant for formats vsgXchange *doesn't* support:

```cpp
/**
* VSG reader-writer that uses GDAL to read some image formats that are
* not supported by vsgXchange
*/
```

For greyscale images this is visibly wrong. `GDAL_detail::readImage` maps a `GCI_GrayIndex` band to a single-channel `Image::R32_SFLOAT` holding the raw 0–255 values; it isn't normalized to 0–1, and it isn't expanded to RGB. The terrain shader samples `u_colorTex` as RGBA, so a one-channel texture reads as `(v, 0, 0, 1)`, and with values far above 1 it is clamped. The terrain renders in saturated red.

## Environment

- **Rocky:** `main` at a792a5f (2026-10-21, version 1.1.0)
- **Platform:** macOS 26, arm64 (Vulkan through Mesa KosmicKrisp)
- **VSG:** VulkanSceneGraph 1.1.16
- **vsgXchange:** 1.1.14
- **GDAL:** libgdal-core 3.13.3
- **All dependencies:** conda-forge

## Steps to reproduce

1. Use the `moon` profile.
2. Add a `TMSImageLayer` with greyscale JPEG tiles, for example NASA Moon Trek's LRO WAC mosaic:

   ```
   https://trek.nasa.gov/tiles/Moon/EQ/LRO_WAC_Mosaic_Global_303ppd_v02/1.0.0/default/default028mm/{z}/{y}/{x}.jpg
   ```

   with a geodetic profile of 2 × 1 tiles at level 0.
3. View the Moon.

## Actual result

The Moon is red, with almost no surface detail (see `rocky-moon-gdal-first.png`).

## Expected result

A grey Moon, as the tiles are (see `rocky-moon-vsgxchange-first.png`).

## Fix

Register the GDAL reader **after** vsgXchange's readers. Then JPEG and PNG are decoded by vsgXchange (stb, which always returns `R8G8B8A8_SRGB`, so greyscale comes out as grey RGBA). GDAL remains the fallback for what vsgXchange can't read, which is what the comment above says it's for.

```diff
--- a/src/rocky/vsg/VSGContext.cpp
+++ b/src/rocky/vsg/VSGContext.cpp
@@ -356,15 +356,15 @@ VSGContextImpl::ctor(int& argc, char** argv)
         else if (log_level == "off") Log()->set_level(spdlog::level::off);
     }
 
-#ifdef ROCKY_HAS_GDAL
-    readerWriterOptions->add(GDAL_VSG_ReaderWriter::create());
-#endif
-
 #ifdef ROCKY_HAS_VSGXCHANGE
     // Adds all the readerwriters in vsgxchange to the options data.
     readerWriterOptions->add(vsgXchange::all::create());
 #endif
 
+#ifdef ROCKY_HAS_GDAL
+    readerWriterOptions->add(GDAL_VSG_ReaderWriter::create());
+#endif
+
     // For system fonts
     readerWriterOptions->paths.push_back("C:/Windows/Fonts");
     readerWriterOptions->paths.push_back("/usr/share/fonts/truetype");
```

With this change the Moon renders correctly. Both screenshots were made with the same build, differing only in this change.

## Related problems in `GDAL_detail::readImage`

The GDAL path stays reachable without vsgXchange, and for `.tif` and `.webp`. Two problems in it are worth fixing too. I haven't tested fixes for either.

- **Greyscale band:** a `GCI_GrayIndex` band always becomes `R32_SFLOAT` with raw values. That suits elevation data, but an 8-bit (`GDT_Byte`) grey band is imagery. It could be read as `R8G8B8A8_SRGB`, with the grey value copied to R, G and B, and alpha from a `GCI_AlphaBand` if there is one (a grey and alpha PNG currently loses its alpha).
- **Two-band format:** the case `R && G && !B && !A` selects `Image::R8G8B8_SRGB`, a three-component format, but only two bands are read into it. This looks like it should be `Image::R8G8_SRGB`.

```cpp
else if (R && !G && !B && !A)
    format = Image::R8_SRGB;
else if (R && G && !B && !A)
    format = Image::R8G8B8_SRGB;      // <- two bands into a three-component format
```

The broader point is that a one- or two-channel image layer reaches the terrain shader as an R or RG texture, and that shows up as red, or red and green. Expanding such images to RGBA when the tile is created, for example in `ImageLayer::invokeCreateTileImplementation`, would make any reader safe. So would giving the texture a component swizzle of (r, r, r, 1), or (r, r, r, g) for grey with alpha.
