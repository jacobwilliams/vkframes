"""Run the vkframes / OpenFrames benchmark (bench/README.md): each scenario at
each size with both apps, then print a comparison. From the vkframes root:

    pixi run bench                                   everything (about 10 minutes)
    pixi run bench -- --points 1000000 --scenarios grow
    pixi run bench -- --report bench/results/results.jsonl     only the tables

Options:
    --points N [N ...]        trajectory sizes (default 100000 1000000 5000000)
    --scenarios S [S ...]     static, grow, trace (default all)
    --libraries L [L ...]     vkframes, openframes (default both)
    --seconds S, --warmup S   measured and warmup time per run (default 10, 2)
    --rate R                  points appended per second in grow (default 60000)
    --out FILE                results, one JSON line per run
                              (default bench/results/results-<date>.jsonl)
    --report FILE             print the tables of an earlier results file
    --no-build                don't build the apps first

Each run is a separate process: `pixi run -e qt bench-vkframes` or
`pixi run -e openframes bench-openframes` (OpenFrames built once with
`pixi run -e openframes openframes-build`). vkframes also runs the static
scenario adding the points one at a time (--per-point), to compare the
insertion with OpenFrames', which has only a per-point interface.
"""

import argparse
import datetime
import json
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PIXI = os.environ.get("PIXI_EXE") or shutil.which("pixi") or "pixi"
APPS = {
    "vkframes": ("qt", "bench-vkframes"),
    "openframes": ("openframes", "bench-openframes"),
}


def pixi(env, task, args, check=True):
    cmd = [PIXI, "run", "--manifest-path", os.path.join(ROOT, "pixi.toml"), "-e", env, task, *args]
    print("+", " ".join(cmd[1:]), flush=True)
    result = subprocess.run(cmd, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    lines = [l for l in result.stdout.splitlines() if l.startswith("RESULT ")]
    if check and result.returncode != 0:
        print(result.stdout[-4000:])
        sys.exit(f"run.py: {task} failed")
    if args and not lines and task.startswith("bench-") and not task.endswith("-build"):
        print(result.stdout[-4000:])
        print(f"run.py: {task} gave no result", flush=True)
    return [json.loads(l[len("RESULT "):]) for l in lines]


def run_all(opt):
    os.makedirs(os.path.dirname(opt.out), exist_ok=True)
    if not opt.no_build:
        for lib in opt.libraries:
            env, task = APPS[lib]
            pixi(env, task + "-build", [])
    common = ["--warmup", str(opt.warmup), "--seconds", str(opt.seconds), "--rate", str(opt.rate), "--out", opt.out]
    for n in opt.points:
        for scenario in opt.scenarios:
            for lib in opt.libraries:
                env, task = APPS[lib]
                args = ["--scenario", scenario, "--points", str(n), *common]
                for r in pixi(env, task, args, check=False):
                    print(summary_line(r), flush=True)
                if lib == "vkframes" and scenario == "static":
                    for r in pixi(env, task, args + ["--per-point"], check=False):
                        print(summary_line(r), flush=True)
    report(opt.out)


def summary_line(r):
    return (f"  {r['library']:10s} {r['scenario']:6s} {r['points']:>9d} points: {r['fps']:8.1f} fps, "
            f"median {r['frame_ms_median']:6.2f} ms, p99 {r['frame_ms_p99']:6.2f} ms, insert {r['insert_s']:.3f} s")


def fmt(x, digits=2):
    if x is None:
        return "-"
    if isinstance(x, int):
        return f"{x:,}"
    return f"{x:,.{digits}f}"


def report(path):
    """Tables: one per scenario, a row per size and library (the last run of each)."""
    rows = {}
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            r = json.loads(line)
            key = (r["scenario"], r["points"], r["library"], r["insert_method"])
            rows[key] = r
    print(f"\nResults: {os.path.relpath(path, ROOT)}\n")
    for scenario in ("static", "grow", "trace"):
        sel = sorted((k for k in rows if k[0] == scenario), key=lambda k: (k[1], k[2], k[3]))
        if not sel:
            continue
        print(f"### {scenario}\n")
        cols = ["Points", "Library", "Insert", "Insert (s)", "First frame (s)", "FPS", "Median (ms)", "p95 (ms)",
                "p99 (ms)", "Max (ms)"]
        if scenario == "grow":
            cols += ["Appended", "Append mean (ms)", "Append max (ms)"]
        cols += ["CPU / wall", "Memory, loaded (MB)", "Memory, peak (MB)"]
        print("| " + " | ".join(cols) + " |")
        print("|" + "---|" * len(cols))
        for k in sel:
            r = rows[k]
            cells = [fmt(r["points"]), r["library"], r["insert_method"], fmt(r["insert_s"], 3), fmt(r["first_frame_s"], 3),
                     fmt(r["fps"], 1), fmt(r["frame_ms_median"]), fmt(r["frame_ms_p95"]), fmt(r["frame_ms_p99"]),
                     fmt(r["frame_ms_max"])]
            if scenario == "grow":
                cells += [fmt(r["appended"]), fmt(r["append_ms_mean"], 3), fmt(r["append_ms_max"], 3)]
            cells += [fmt(r["cpu_per_wall"]), fmt(r["rss_mb_load"], 0), fmt(r["rss_mb_peak"], 0)]
            print("| " + " | ".join(cells) + " |")
        print()


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    p.add_argument("--points", type=int, nargs="+", default=[100000, 1000000, 5000000])
    p.add_argument("--scenarios", nargs="+", default=["static", "grow", "trace"], choices=["static", "grow", "trace"])
    p.add_argument("--libraries", nargs="+", default=["vkframes", "openframes"], choices=list(APPS))
    p.add_argument("--seconds", type=float, default=10.0)
    p.add_argument("--warmup", type=float, default=2.0)
    p.add_argument("--rate", type=float, default=60000.0)
    p.add_argument("--out", default=None)
    p.add_argument("--report", default=None)
    p.add_argument("--no-build", action="store_true")
    opt = p.parse_args()
    if opt.report:
        report(os.path.abspath(opt.report))
        return
    if opt.out is None:
        stamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
        opt.out = os.path.join(HERE, "results", f"results-{stamp}.jsonl")
    opt.out = os.path.abspath(opt.out)
    run_all(opt)


if __name__ == "__main__":
    main()
