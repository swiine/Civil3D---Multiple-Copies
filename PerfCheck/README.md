# C3D PerfCheck for Civil 3D 2026

A benchmark and lag finder that runs inside Civil 3D. It measures how the PC and the drawing are performing **right now**, finds what is causing lag or stutter, and gives you a report that lists the problems most important first, each with its fix.

It is a single AutoLISP file. Nothing has to be compiled, and it also works in plain AutoCAD 2021 and later.

## Commands

| Type        | What it does                                                                 |
|-------------|------------------------------------------------------------------------------|
| `PERFCHECK` | Full check + benchmarks (about 1–2 min). Opens the report in Notepad.        |
| `PERFQUICK` | Quick "how is it right now" check (about 5–10 s), without the heavy tests.   |
| `PERFWATCH` | Times every command while you work and warns when one is slow (stays on).    |
| `PERFLOG`   | Shows the slowest commands that PERFWATCH recorded.                          |
| `PERFFIX`   | Applies the recommended settings. It backs up the old values first and offers to clean REGAPP / scale-list bloat. |
| `PERFUNDO`  | Puts back the settings PERFFIX changed.                                      |
| `PERFHELP`  | Lists the commands.                                                          |

## What it checks

**The PC**
- RAM: total, free, and whether Windows is about to start paging to disk.
- CPU: model, cores, clock, and its current speed as a % of rated speed (shows throttling).
- Graphics card(s): driver version and **driver age**, "Microsoft Basic Display Adapter" (no driver), laptops with two GPUs (Civil 3D running on the Intel chip), Remote Desktop / virtual displays, and 4K on integrated graphics.
- Power plan (Power saver / Balanced) and **running on battery**.
- Free disk space, and spinning HDD vs SSD / NVMe.
- How long Windows has been running since its last restart.

**Background programs**
- What else is using CPU and RAM right now, for example antivirus, OneDrive / Dropbox / Desktop Connector, Windows Update, Search indexing, browsers, Teams. Each one comes with a specific tip.

**Network**
- Connected adapters: **Wi-Fi vs cable**, link speed (e.g. a 100 Mbps link from a bad cable or port), and **VPN**.
- Which **file server** your drawing is on (including mapped drives) and the **ping time** to that server.

**More Windows checks**
- Page file: missing, or holding a lot of swapped-out memory.
- Display scaling and the installed antivirus products (more than one scanner is flagged).
- Whether Windows is **waiting for a restart** to finish updates, and how many programs start with Windows.

**Add-ins / plugins / customization**
- Third-party **ARX** modules, **plugin bundles** in all three ApplicationPlugins folders, the **Startup Suite**, and `acad.lsp` / `acaddoc.lsp` files.
- **All code loaded inside acad.exe** that is not from Windows or Autodesk. This includes **.NET plugins loaded with NETLOAD**, as well as known troublemakers that inject themselves into programs: screen overlays (Discord, Steam, NVIDIA overlay, RivaTuner, OBS), security software, cloud-sync shell extensions, and mouse / window-management tools.
- The number of LISP commands, LISP reactors and menu groups.
- **Support file search path**: folders on the network or folders that no longer exist. Both slow down every file lookup.
- **Command overhead benchmark**: runs a do-nothing command 200 times. If each one takes more than a few ms, an add-in or reactor is running code on every command.

**The Civil 3D process**
- Memory used by acad.exe, handle count, how long it has been running, and more than one copy running.

**The drawing**
- Object counts by type: COGO points, surfaces (with point count and out-of-date status), corridors, labels, pipes, hatches, images, PDF underlays, point clouds and **proxy objects**.
- Bloat: **REGAPPs**, **annotation scale list**, **DGN linestyle junk**, **layer filters**, layer states, groups, viewports, layers, blocks, layouts.
- Xrefs, including any that are not found or not loaded.
- File size, and objects placed absurdly far away.

**File locations**
- Drawing, autosave and temp folders on a **network share** or a **cloud-synced folder** (OneDrive, Dropbox, Autodesk Docs, …).

**Settings known to cause lag**
- `ROLLOVERTIPS`, `SELECTIONPREVIEW`, `QPMODE`, `VTENABLE`, `LAYOUTREGENCTL`, `XLOADCTL`, `SAVETIME` (autosave freezes), `AUTOMATICPUB`, `COMMANDPREVIEW`, `HPQUICKPREVIEW`, `LINESMOOTHING`, `GRIPOBJLIMIT`, `LOGFILEMODE`, and the Nearest object snap left on over surfaces.

**Benchmarks**
- REGEN time (ms per 1000 objects) and zoom time.
- Single-core CPU score. It is compared against **your own best run on this PC**, so it can tell you "the PC is 40% slower right now than usual".
- Database create, modify and erase speed. The test objects are deleted again afterwards.
- Disk latency and copy speed in the temp, autosave and **drawing** folders.
- **Stutter test**: runs the same small job 60 times. On a quiet PC every run takes about the same time. Runs that take much longer mean Civil 3D got paused by something else, which is exactly what stutter is.

## The report

The report ends up looking like this:

```
HEALTH SCORE: 58 / 100   POOR - several problems; fix the HIGH items first
Problems found: 2 high, 3 medium, 4 low

=== PROBLEMS FOUND (most important first) ===
  1. [HIGH] This drawing is in a cloud-synced folder (OneDrive / Dropbox / Docs ...)
             The sync app reads, uploads and sometimes locks the DWG, .bak and .dwl ...
  2. [HIGH] Civil 3D was interrupted in 17 of 60 test runs
             Something keeps taking the CPU away from Civil 3D ...
  3. [MED ] ROLLOVERTIPS is 1
             Rollover tooltips make Civil 3D query every object the cursor passes ...
```

After the list of problems come the full details: hardware, background programs, drawing statistics, settings and benchmark numbers.

Everything is saved in `%LOCALAPPDATA%\C3D-PerfCheck`:
- `PerfReport_<date>.txt`: one report per run.
- `History.csv`: one line per run. Open it in Excel to compare runs, for example a run while it stutters against a run after a restart.
- `SlowCommands.log`: every slow command that PERFWATCH caught.
- `PerfFix_Backup.txt`: your original settings, which PERFUNDO restores.

## Install

**Option A: installer (easiest)**
1. Unzip `PerfCheck.zip`.
2. Double-click `Install.bat`.
3. Restart Civil 3D. If a security prompt about loading PerfCheck.lsp appears, choose **Always Load**.

To remove it, double-click `Uninstall.bat`.

**Option B: by hand**
- Copy the `PerfCheck.bundle` folder to `%APPDATA%\Autodesk\ApplicationPlugins\`, **or**
- Type `APPLOAD` and load `PerfCheck.bundle\Contents\PerfCheck.lsp`.

When it loads, the command line shows `C3D PerfCheck 1.1 loaded: ...`.

## How to track down stutter

0. **Plugins?** Add ` /safemode` to the end of the Target box of your Civil 3D desktop shortcut (Properties). Start Civil 3D with it, which loads no add-ins, and run `PERFCHECK`. A big improvement in the score or the command overhead means a plugin is the cause. (If 2026 doesn't recognise `/safemode`, a fresh profile in OPTIONS > Profiles does the same job.)

1. Run `PERFCHECK` **while it is stuttering**. Fix the HIGH items first.
2. Restart Civil 3D (and Windows if the report says so), then run `PERFCHECK` again. If the score and stutter test are much better, the cause was the long session, memory, or background programs.
3. Turn on `PERFWATCH` and work normally for an hour. `PERFLOG` then shows exactly which commands are slow. A slow `<file write: save/autosave>` line means saving is the problem (SAVETIME, network or cloud folder, file size). A slow `REGEN` means the drawing is too heavy to display.
4. Run `PERFFIX` to apply the safe setting changes. `PERFUNDO` reverts them.

## Notes

- PERFCHECK regenerates and zooms the drawing during the benchmarks, then zooms back. The database test makes 2000 temporary lines on layer 0 and deletes them again. It is skipped if layer 0 is locked or frozen. Use `U` afterwards if you want to remove the undo entries.
- System information comes from Windows WMI. If your IT department blocks WMI, the drawing, settings and benchmark parts still work.
- Benchmark numbers are timed from inside Civil 3D with a timer that is accurate to about 16 ms, so treat small differences as noise. Comparing runs on the same PC (History.csv) is what matters.
