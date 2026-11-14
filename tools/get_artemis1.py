"""Download NASA's Artemis I trajectory (Orion, from JPL Horizons) with the
Moon's and the Sun's positions, into data/artemis1/, for the Artemis I demo
(demos/artemis): `pixi run artemis-data`.

Files written (text; times in seconds from the epoch in each file's header,
TDB; positions km and velocities km/s, Earth-centered, ICRF (J2000 equator)):

* orion.txt: Orion every minute (Horizons' -1023, "Artemis 1 (spacecraft)"),
  2022-11-16 08:47 to 2022-12-11 17:21 TDB (after the trans-lunar injection,
  until just before entry);
* moon.txt: the Moon every 10 minutes;
* sun.txt: the Sun every 6 hours (the scene's light);
* events.txt: events found in the data: the burns (where Orion's
  acceleration isn't gravity's: the Earth with J2, the Moon and the Sun),
  with their delta-v and names for the main ones; the closest approaches to
  the Moon and the farthest point from the Earth.

Options: --force to download again.
"""

import argparse
import math
import os
import sys
import urllib.parse
import urllib.request

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "data", "artemis1")
API = "https://ssd.jpl.nasa.gov/api/horizons.api"
START, STOP = "2022-11-16 08:47", "2022-12-11 17:21"

MU_EARTH, MU_MOON, MU_SUN = 398600.4418, 4902.800066, 1.32712440018e11   # km^3/s^2
RE, J2 = 6378.137, 1.08262668e-3
MOON_RADIUS = 1737.4

# The main burns, by NASA's mission blog (UTC): named when a burn found is within an hour
NAMED_BURNS = [
    ("2022-11-21 12:44", "Outbound powered flyby burn"),
    ("2022-11-25 21:52", "Distant retrograde orbit insertion"),
    ("2022-12-01 21:53", "Distant retrograde orbit departure"),
    ("2022-12-05 16:43", "Return powered flyby burn"),
]


def horizons(target, step):
    """Earth-centered state vectors of `target` from START to STOP every
    `step`: (julian dates TDB, states [x, y, z, vx, vy, vz])."""
    params = {
        "format": "text", "COMMAND": f"'{target}'", "EPHEM_TYPE": "'VECTORS'", "CENTER": "'500@399'",
        "START_TIME": f"'{START}'", "STOP_TIME": f"'{STOP}'", "STEP_SIZE": f"'{step}'",
        "OBJ_DATA": "'NO'", "CSV_FORMAT": "'YES'", "VEC_TABLE": "'2'", "REF_PLANE": "'FRAME'",
        "REF_SYSTEM": "'ICRF'", "OUT_UNITS": "'KM-S'", "VEC_LABELS": "'NO'",
    }
    url = API + "?" + urllib.parse.urlencode(params, quote_via=urllib.parse.quote, safe="'@")
    with urllib.request.urlopen(url, timeout=120) as r:
        text = r.read().decode()
    if "$$SOE" not in text:
        sys.exit(f"get_artemis1: Horizons gave no vectors for {target}:\n{text[:2000]}")
    body = text.split("$$SOE")[1].split("$$EOE")[0]
    jd, states = [], []
    for line in body.strip().splitlines():
        f = [x.strip() for x in line.split(",")]
        jd.append(float(f[0]))
        states.append([float(x) for x in f[2:8]])
    return jd, states


def calendar(jd):
    """'YYYY-MM-DD hh:mm:ss' of a julian date (in its own time scale)."""
    z = int(jd + 0.5)
    f = jd + 0.5 - z
    a = z if z < 2299161 else z + 1 + int((z - 1867216.25) / 36524.25) - int(int((z - 1867216.25) / 36524.25) / 4)
    b = a + 1524
    c = int((b - 122.1) / 365.25)
    d = int(365.25 * c)
    e = int((b - d) / 30.6001)
    day = b - d - int(30.6001 * e)
    month = e - 1 if e < 14 else e - 13
    year = c - 4716 if month > 2 else c - 4715
    s = round(f * 86400.0)
    if s >= 86400:          # (rounded up to midnight: the next day, roughly; not at these dates' ends)
        s = 86399
    return f"{year:04d}-{month:02d}-{day:02d} {s // 3600:02d}:{s % 3600 // 60:02d}:{s % 60:02d}"


def julian(date):
    """The julian date of 'YYYY-MM-DD hh:mm'."""
    y, m, d = (int(x) for x in date[:10].split("-"))
    hh, mm = (int(x) for x in date[11:16].split(":"))
    if m <= 2:
        y, m = y - 1, m + 12
    a = y // 100
    b = 2 - a + a // 4
    return int(365.25 * (y + 4716)) + int(30.6001 * (m + 1)) + d + b - 1524.5 + (hh + mm / 60) / 24


def read(name):
    """A file written by write: (epoch, times (s), states)."""
    epoch, ts, states = None, [], []
    with open(os.path.join(OUT, name)) as f:
        for line in f:
            if line.startswith("# epoch JD TDB"):
                epoch = float(line.split()[4])
            elif not line.startswith("#"):
                v = [float(x) for x in line.split()]
                ts.append(v[0])
                states.append(v[1:7])
    return epoch, ts, states


def write(name, epoch, jd, states, comment):
    """Write states to data/artemis1/<name> for the demo: a header (`comment`, the source, the epoch),
    then each time (s from `epoch`, a julian date TDB; `jd` are julian dates) and state (km, km/s)."""
    with open(os.path.join(OUT, name), "w") as f:
        f.write(f"# {comment}\n")
        f.write("# JPL Horizons (https://ssd.jpl.nasa.gov/horizons/), Earth-centered, ICRF; km, km/s\n")
        f.write(f"# epoch JD TDB {epoch:.9f} = {calendar(epoch)} TDB\n")
        f.write("# t (s from the epoch), x, y, z, vx, vy, vz\n")
        for t, s in zip(jd, states):
            f.write(f"{(t - epoch) * 86400.0:.3f} " + " ".join(f"{v:.6f}" for v in s[:3]) + " "
                    + " ".join(f"{v:.9f}" for v in s[3:]) + "\n")


def hermite(ts, states, t):
    """The position at time t from states at times ts (cubic Hermite)."""
    lo, hi = 0, len(ts) - 1
    while hi - lo > 1:
        mid = (lo + hi) // 2
        if ts[mid] <= t:
            lo = mid
        else:
            hi = mid
    h = ts[hi] - ts[lo]
    s = (t - ts[lo]) / h
    h00, h10, h01, h11 = 2*s**3 - 3*s**2 + 1, s**3 - 2*s**2 + s, -2*s**3 + 3*s**2, s**3 - s**2
    a, b = states[lo], states[hi]
    return [h00 * a[k] + h10 * h * a[k + 3] + h01 * b[k] + h11 * h * b[k + 3] for k in range(3)]


def gravity(r, moon, sun):
    """The acceleration of gravity at r (Earth with J2, the Moon and the Sun as third bodies)."""
    x, y, z = r
    d = math.sqrt(x * x + y * y + z * z)
    a = [-MU_EARTH * c / d**3 for c in r]
    f = 1.5 * J2 * MU_EARTH * RE**2 / d**5
    zz = 5 * z * z / (d * d)
    a[0] += f * x * (zz - 1)
    a[1] += f * y * (zz - 1)
    a[2] += f * z * (zz - 3)
    for p, mu in ((moon, MU_MOON), (sun, MU_SUN)):
        rel = [r[k] - p[k] for k in range(3)]
        dr = math.sqrt(sum(c * c for c in rel))
        dp = math.sqrt(sum(c * c for c in p))
        for k in range(3):
            a[k] += -mu * rel[k] / dr**3 - mu * p[k] / dp**3
    return a


def earth_altitude(state):
    """The height (km) above the Earth's equatorial radius of a state's position (km)."""
    return math.sqrt(sum(c * c for c in state[:3])) - RE


def find_events(t, orion, tm, moon, tsun, sun, epoch):
    """Burns, the closest approaches to the Moon, the farthest point from the Earth."""
    events = []
    n = len(t)
    resid = [0.0] * n
    moon_at = [hermite(tm, moon, x) for x in t]
    sun_at = [hermite(tsun, sun, x) for x in t]
    for i in range(1, n - 1):
        acc = [(orion[i + 1][k + 3] - orion[i - 1][k + 3]) / (t[i + 1] - t[i - 1]) for k in range(3)]
        g = gravity(orion[i][:3], moon_at[i], sun_at[i])
        resid[i] = math.sqrt(sum((acc[k] - g[k]) ** 2 for k in range(3)))
    # burns: runs of acceleration beyond gravity's (runs within 15 minutes
    # merged), each with its net delta-v (the velocity change beyond
    # gravity's across it): a glitch where the ephemeris' segments meet (a
    # jump and its return) nets to nothing
    threshold = 2.0e-5                                          # km/s^2
    runs = []
    i = 2                                                       # (not the first points' differences)
    while i < n - 1:
        if resid[i] <= threshold:
            i += 1
            continue
        j = i
        while j + 1 < n - 1 and resid[j + 1] > threshold:
            j += 1
        if runs and t[i] - t[runs[-1][1]] < 900.0:
            runs[-1][1] = j
        else:
            runs.append([i, j])
        i = j + 1
    burns = []
    for i, j in runs:
        a, b = max(i - 2, 0), min(j + 2, n - 1)
        g = [0.0, 0.0, 0.0]
        for k in range(a, b):                                   # (gravity's velocity change, trapezoids)
            g0 = gravity(orion[k][:3], moon_at[k], sun_at[k])
            g1 = gravity(orion[k + 1][:3], moon_at[k + 1], sun_at[k + 1])
            g = [g[c] + 0.5 * (g0[c] + g1[c]) * (t[k + 1] - t[k]) for c in range(3)]
        dv = 1000.0 * math.sqrt(sum((orion[b][c + 3] - orion[a][c + 3] - g[c]) ** 2 for c in range(3)))
        if dv < 1.0 or earth_altitude(orion[i]) < 1000.0:       # (small; near the Earth, gravity's model isn't enough)
            continue
        burns.append((t[i], dv))
    for k, (tb, dv) in enumerate(burns):        # (each named burn: the largest within an hour of NASA's time)
        name = "Trajectory correction burn"
        for date, label in NAMED_BURNS:
            near = [b for b in burns if abs(epoch + b[0] / 86400.0 - (julian(date) + 69.184 / 86400.0)) < 1.0 / 24.0]
            if near and max(near, key=lambda b: b[1]) == (tb, dv):
                name = label
        events.append((tb, f"{name}, {dv:.0f} m/s"))
    # the closest approaches to the Moon (minima within 20000 km), the farthest from the Earth
    dist = [math.dist(orion[k][:3], moon_at[k]) for k in range(n)]
    for k in range(1, n - 1):
        if dist[k] < dist[k - 1] and dist[k] <= dist[k + 1] and dist[k] < 20000.0:
            up = dist[k] - MOON_RADIUS
            near = [e for e in events if "powered flyby" in e[1] and abs(e[0] - t[k]) < 3600.0]
            if near:                                            # (one event: the flyby's burn and its closest approach)
                events.remove(near[0])
                name, dv = near[0][1].split(" burn, ")
                events.append((near[0][0], f"{name}: {dv}, {up:.0f} km up"))
            else:
                events.append((t[k], f"Closest to the Moon, {up:.0f} km up"))
    earth = [math.sqrt(sum(c * c for c in s[:3])) for s in orion]
    k = max(range(n), key=lambda q: earth[q])
    events.append((t[k], f"Farthest from the Earth, {earth[k]:,.0f} km from its center"))
    events.append((t[0], "After trans-lunar injection"))
    events.append((t[-1], "Entry"))
    return sorted(events)


def main():
    """Download the data (unless there, or --force), find the events, and write events.txt."""
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--force", action="store_true", help="download again")
    args = parser.parse_args()
    files = [os.path.join(OUT, f) for f in ("orion.txt", "moon.txt", "sun.txt")]
    if args.force or not all(os.path.exists(f) for f in files):
        os.makedirs(OUT, exist_ok=True)
        print("get_artemis1: Orion (every minute), the Moon (10 minutes), the Sun (6 hours), from JPL Horizons")
        jd, orion = horizons("-1023", "1m")
        jdm, moon = horizons("301", "10m")
        jds, sun = horizons("10", "6h")
        epoch = jd[0]
        write("orion.txt", epoch, jd, orion, "Artemis I: Orion (JPL Horizons -1023)")
        write("moon.txt", epoch, jdm, moon, "The Moon (JPL Horizons 301)")
        write("sun.txt", epoch, jds, sun, "The Sun (JPL Horizons 10)")
    epoch, t, orion = read("orion.txt")
    _, tm, moon = read("moon.txt")
    _, tsun, sun = read("sun.txt")
    events = find_events(t, orion, tm, moon, tsun, sun, epoch)
    with open(os.path.join(OUT, "events.txt"), "w") as f:
        f.write("# Artemis I events found in the data (tools/get_artemis1.py): t (s from orion.txt's epoch), text\n")
        for te, text in events:
            f.write(f"{te:.1f} {text}\n")
    for te, text in events:
        print(f"  {calendar(epoch + te / 86400.0 - 69.184 / 86400.0)} UTC  {text}")
    print(f"get_artemis1: {len(t)} points of Orion's trajectory in {OUT}")


if __name__ == "__main__":
    main()
