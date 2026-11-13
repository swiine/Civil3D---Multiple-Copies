# Series Scripts for Civil 3D

Runs the right script on every drawing for its **series**. The series comes from the drawing number, which is read from the title block and the file name:

| Drawing number | Series | Script it gets |
|---|---|---|
| `01000`, `1000`, `01250` | 01 | `Series Scripts\01 Siteworks.csx` |
| `02000`, `2000`, `02310` | 02 | `Series Scripts\02 Erosion and Sediment Control.csx` |

It's built on the TTW eTransmit tool and works the same way. Drawings are run in the background by headless `accoreconsole` workers, 5 at a time, while you keep working. A spinning TTW logo in the status bar shows progress, and an HTML report opens at the end. **Each drawing is copied to `_superseded` before its script runs and it's saved.**

## Install

1. Copy `ttw_stdCountry_Series_Scripts.csx` into the Advanced Tools folder, next to `bin\TTW.CsxConsole.dll`. That's the console loader Xref Bind and eTransmit already use, and it isn't included here.
2. Create a `Series Scripts` folder next to it, and put one script in it per series (see below).
3. Run **Series Scripts** from Advanced Tools.

```
Advanced Tools\
  bin\TTW.CsxConsole.dll
  ttw_stdCountry_Series_Scripts.csx
  Series Scripts\
    01 Siteworks.csx
    02 Erosion and Sediment Control.csx
```

## Series scripts

Name each script with its series number first, followed by the series name: `01 Siteworks.csx`, `2 - Erosion and Sediment Control.lsp` and `03_Roads.scr` all work. Adding a series means dropping in another file. Files that don't start with a number are ignored, and so are subfolders such as `_templates`. If two scripts have the same series number, nothing runs until you remove one.

| Type | How it runs | What it's told about the drawing |
|---|---|---|
| `.csx` | As an Advanced Tools script. `Document` / `Database` / `Editor` are the drawing. | `Environment.GetEnvironmentVariable("TTW_SS_NUMBER")` → `"01000"`, plus `TTW_SS_SERIES`, `TTW_SS_SERIES_NAME` and `TTW_SS_SUPERSEDED` |
| `.lsp` | Loaded. The work goes at the top level of the file. | LISP variables `ttw-dwg-number`, `ttw-series`, `ttw-series-name` and `ttw-superseded` |
| `.scr` | Its lines are fed to the command line. | The same LISP variables |

The drawing is saved (`QSAVE`) after its script, so scripts shouldn't save, close or quit. There are starting points in `Series Scripts\_templates\`.

## Running it

```
Series Scripts: series scripts: 01 Siteworks (.csx), 02 Erosion and Sediment Control (.csx).
Run [All 29 in this folder/This drawing only/Pick] <All>:
Series Scripts:    14  01 Siteworks
Series Scripts:    13  02 Erosion and Sediment Control
Series Scripts:     2  series 05 - no script
Run the series scripts on 29 drawings and save them? Each is copied to _superseded first [Yes/No] <Yes>:
```

- Nothing is changed until you answer **Yes**.
- The per-series counts come from the file names. Each drawing's title block is checked again when it opens.
- A drawing that's open in Civil 3D can't be saved by a background console. If any of the chosen drawings are open here, it offers to close them when the batch starts. It never closes a drawing with unsaved changes; it stops and asks you to save first. Drawings open on someone else's machine are reported as **OPEN ELSEWHERE** and left alone.
- Run **Series Scripts** again while a batch is going for `[Status/Cancel/New]`.

## Finding the drawing number

- **Title block:** any attribute tagged `DWGNO`, `DWG_NO`, `DRAWING_NO`, `DRAWINGNUMBER`, `DRG_NO` and similar, on any layout. Change `NumberTags` at the top of the `.csx` if your title block uses another tag.
- **File name:** the 4- or 5-digit number in it. A six-digit job number such as `231606` is never taken for it.
- **The series** is what comes before the last 3 digits (`SheetDigits`).

The title block wins when it has a number, and the file name is used when it doesn't. If the two give **different series**, the drawing is not changed. It's reported as **NUMBER MISMATCH**, so the wrong script never runs on a sheet. Different numbers in the same series do run, and are listed for checking.

## What you get

```
<drawing folder>\_superseded\<date time>\
  <drawing>.dwg                 each changed drawing as it was, before its script
  Series Scripts Report.html    one report for the batch
```

To undo a drawing, copy its superseded copy back over it. A drawing that wasn't changed (no script, a mismatch, open elsewhere, or the script failed before saving) gets no superseded copy.

| Status | Meaning |
|---|---|
| **DONE** | Its script ran and the drawing was saved. |
| **DONE - CHECK** | Saved, but something's listed to look at, such as errors in the console log or a file-name/title-block number difference. |
| **NO SCRIPT / NO NUMBER / NUMBER MISMATCH / MIXED SERIES / READ ONLY / OPEN ELSEWHERE** | Not changed, for the reason given. |
| **SCRIPT FAILED / SCRIPT STOPPED / NOT SAVED / TIMED OUT / NO RESULT / COPY FAILED** | Something went wrong; the logs say what. If the drawing changed, its superseded copy is kept. |

Working files (console logs, per-drawing logs and results) go to `%LOCALAPPDATA%\TTW\BatchSeriesScripts\<date-time>\` and are cleared out after 14 days.

## How it works

1. **The launcher, in Civil 3D.** It asks All/This/Pick, shows what each drawing will get, asks before changing anything, and hands the batch to a background PowerShell runner.
2. **The runner,** for each drawing:
   - waits until the drawing isn't open anywhere
   - copies it to `_superseded`
   - starts `accoreconsole` on it with a script that loads this tool (`TTWRUNCSX`)
3. **The tool, inside the console.** It reads the drawing number from the title block and the file name and picks the series script. Then it writes the console's next steps: run the script, `QSAVE`, and leave a "done" marker.
4. **The runner again.** It checks that the marker exists and that the file changed, scans the console log for errors, deletes the superseded copy of anything left unchanged, and writes the report.

Run by hand in `accoreconsole` (`TTWRUNCSX` on this file), the tool only says which script it would run, and nothing is changed.
