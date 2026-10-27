# SRS.cpp doesn't compile as C++20: `contains()` resolves to `std::unordered_map::contains`

## Summary

`src/rocky/SRS.cpp` fails to compile when the `rocky` target is built as C++20. The member functions of `SRSFactory` call the file's helper `contains(const std::string&, const char*)` unqualified. But `SRSFactory` derives from `std::unordered_map<std::string, SRSEntry>`, and in C++20 that class has a member `contains(const key_type&)`. Class-scope lookup finds the member first and stops, so the namespace-scope helper is never considered. The call then fails with a "no matching member function" error.

## Environment

- **Rocky:** `main` at a792a5f (2026-10-21, version 1.1.0)
- **Platform:** macOS 26, arm64
- **Compiler:** clang 21.1.8 with libc++ (conda-forge)
- **EnTT:** 4.0.0 (conda-forge)

The target becomes C++20 even though `CMakeLists.txt` sets `CMAKE_CXX_STANDARD 17` and `src/rocky/CMakeLists.txt` sets `CXX_STANDARD 17` on `rocky`. EnTT 4.0's exported target carries `INTERFACE_COMPILE_FEATURES "cxx_std_20"`, which raises the consumer to `-std=c++20`.

This isn't specific to clang or libc++. Any C++20 standard library has `unordered_map::contains`, so GCC with libstdc++ and MSVC will hit the same error whenever the target is compiled as C++20.

## Steps to reproduce

1. Configure Rocky against EnTT 4.0, or add `-DCMAKE_CXX_STANDARD=20`.
2. Build the `rocky` target.

## Actual result

```
src/rocky/SRS.cpp:223:25: error: no matching member function for call to 'contains'
<libc++>/unordered_map:1323:30: note: candidate function not viable: requires single argument '__k', but 2 arguments were provided
<libc++>/unordered_map:1326:30: note: candidate function template not viable: requires single argument '__k', but 2 arguments were provided
src/rocky/SRS.cpp:225:30: error: no matching member function for call to 'contains'
src/rocky/SRS.cpp:225:66: error: no matching member function for call to 'contains'
src/rocky/SRS.cpp:354:34: error: no matching member function for call to 'contains'
...
```

There are seven calls in all: lines 223, 225 (twice), 354, 356, 362 and 367. All are inside `SRSFactory::get_or_create`.

## Expected result

`SRS.cpp` compiles as both C++17 and C++20.

## Fix

Qualify the calls so they find the helper at namespace scope:

```diff
--- a/src/rocky/SRS.cpp
+++ b/src/rocky/SRS.cpp
@@ -220,9 +220,9 @@ namespace
                         to_try = "+proj=longlat +R=1737400 +no_defs +type=crs";
 
                     // if it's a PROJ string, be sure to add the +type=crs
-                    if (contains(ndef, "+proj"))
+                    if (::contains(ndef, "+proj"))
                     {
-                        if (!contains(ndef, "proj=pipeline") && !contains(ndef, "type=crs"))
+                        if (!::contains(ndef, "proj=pipeline") && !::contains(ndef, "type=crs"))
                         {
                             to_try += " +type=crs";
                         }
@@ -351,20 +351,20 @@ namespace
                             new_entry.bounds = Box(-180, -90, 180, 90);
                         }
 
-                        else if (contains(new_entry.proj, "proj=utm"))
+                        else if (::contains(new_entry.proj, "proj=utm"))
                         {
-                            if (contains(new_entry.proj, "+south"))
+                            if (::contains(new_entry.proj, "+south"))
                                 new_entry.bounds = Box(166000, 1116915, 834000, 10000000);
                             else
                                 new_entry.bounds = Box(166000, 0, 834000, 9330000);
                         }
 
-                        else if (contains(new_entry.proj, "proj=merc"))
+                        else if (::contains(new_entry.proj, "proj=merc"))
                         {
                             new_entry.bounds = Box(-20037508.34278925, -20037508.34278925, 20037508.34278925, 20037508.34278925);
                         }
 
-                        else if (contains(new_entry.proj, "proj=qsc"))
+                        else if (::contains(new_entry.proj, "proj=qsc"))
                         {
                             new_entry.bounds = Box(-6378137, -6378137, 6378137, 6378137);
                         }
```

`::contains` works because the helper lives in an unnamed namespace at file scope, whose members are found by qualified lookup in the global namespace.

An alternative that is less fragile is to rename the helper, for example to `has_substring`, so it can never collide with a member of a standard container.

With this change, `SRS.cpp` compiles and Rocky runs correctly as C++20 (macOS, clang 21, libc++).
