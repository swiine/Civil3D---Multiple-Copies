// @name:        Series Scripts
// @description: Runs the right script on every drawing for its series - the series comes from the drawing number in the title block and the file name (01xxx Siteworks, 02xxx Erosion and Sediment Control...). Headless in the background, 5 at a time, while you keep working; a spinning TTW logo in the status bar shows progress.
// @description: Each drawing is copied to a generated _superseded directory before its script runs and it's saved. One report for the batch.
// @tags:        series, script, batch, drawing number, title block, superseded, accoreconsole, headless
// @mode:        read
//
// ttw_stdCountry_Series_Scripts.csx  (v1.0 - built on eTransmit's launcher, runner and report)
// -----------------------------------------------------------------------------
// HOW TO USE
//   Open a drawing and run Series Scripts. At the command line:
//     Run [All 29 in this folder/This drawing only/Pick] <All>:
//       All        every drawing in this drawing's folder (Enter)
//       This       just this drawing
//       Pick       a file picker opens in the folder - choose the sheets
//   It shows which script each drawing gets by its file name and asks before
//   anything is changed. The drawings are then run in the background, 5 at a
//   time (accoreconsole - no windows), while you keep working. A TTW logo spins
//   in the status bar (bottom right) - hover for progress. When it's done the
//   logo turns into a tick and a notification links to the report.
//   Run Series Scripts again while one is going: [Status/Cancel/New] <Status>.
//
//   A drawing that's open in Civil 3D can't be saved by a background console.
//   Ones open here are closed for you when the batch starts (you're asked first,
//   and ones with unsaved changes are never closed); ones open on someone else's
//   machine are reported as OPEN ELSEWHERE and left alone.
//
// THE SERIES SCRIPTS
//   One script per series, in the "Series Scripts" folder next to this tool,
//   named for the series it's for - the number first:
//     Series Scripts\01 Siteworks.csx
//     Series Scripts\02 Erosion and Sediment Control.csx
//   A script can be:
//     .csx   an Advanced Tools script - runs exactly as it would from Advanced
//            Tools, on the drawing (Document / Database / Editor are that drawing)
//     .scr   an AutoCAD script - its lines are fed to the command line
//     .lsp   AutoLISP - loaded; put the work at the top level of the file (or call
//            your command at the end of it)
//   Files that don't start with a number (notes, _templates\...) are ignored.
//   Each script gets the drawing's details:
//     .csx   Environment.GetEnvironmentVariable("TTW_SS_NUMBER") - "01000"
//            TTW_SS_SERIES "1", TTW_SS_SERIES_NAME "Siteworks", TTW_SS_SUPERSEDED (the copy)
//     .lsp / .scr   the LISP variables ttw-dwg-number, ttw-series, ttw-series-name, ttw-superseded
//   The drawing is saved (QSAVE) after its script - the script doesn't need to.
//
// THE DRAWING NUMBER AND ITS SERIES
//   Read from the title block - an attribute tagged DWGNO, DWG_NO, DRAWING_NO...
//   (NumberTags below) on any layout - and from the file name. The drawing number
//   is the 4- or 5-digit number in it (a six-digit job number isn't one); its
//   series is what's before the last 3 digits:
//     01000, 1000, 01250  ->  series 1   ->  01 Siteworks.csx
//     02000, 2000, 02310  ->  series 2   ->  02 Erosion and Sediment Control.csx
//   The title block is used when it has a number, the file name when it doesn't.
//   If the two give different series the drawing is NOT changed - it's reported
//   as NUMBER MISMATCH, so the wrong script never runs on a sheet. Different
//   numbers in the same series run, and are listed to check.
//
// WHAT YOU GET
//   <drawing folder>\_superseded\<date time>\
//     <drawing>.dwg                      each drawing as it was, before its script
//     Series Scripts Report.html         one report for the batch
//   Your drawings, saved with their series script applied. A drawing that wasn't
//   changed (no script for it, a mismatch, open elsewhere) has no superseded copy.
//   To undo one, copy its superseded copy back over it.
//   Working material (console logs, per-drawing logs and results) is kept in
//   %LOCALAPPDATA%\TTW\BatchSeriesScripts\<date-time>\ and cleared out after 14 days.
//
// ONE FILE, TWO ROLES
//   In Civil 3D (Advanced Tools) it's the launcher above. Inside accoreconsole
//   (how the background workers run it, via TTWRUNCSX) it reads that console's
//   drawing number and picks its script; the console then runs the script and
//   saves the drawing.
//
// NEEDS, next to this tool:
//   bin\TTW.CsxConsole.dll   the console loader - adds TTWRUNCSX so a console can
//                            run a .csx (Advanced Tools itself needs the full UI)
//   Series Scripts\          the series scripts
//
// BY HAND
//   Run in accoreconsole on a drawing without the batch runner (TTWRUNCSX this
//   file) it only says which script it would run - nothing is changed.
// -----------------------------------------------------------------------------

using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

// --- settings ----------------------------------------------------------------
const string SeriesFolder  = "Series Scripts";   // next to this tool
const string NumberTags    = "DWGNO,DWG_NO,DWG-NO,DWGNUM,DWG_NUM,DWG_NUMBER,DRAWINGNO,DRAWING_NO,DRAWING-NO,DRAWINGNUMBER,DRAWING_NUMBER,DRGNO,DRG_NO,DRG-NO";   // title block attribute tags that hold the drawing number
const string NumberPattern = @"(?<!\d)(\d{4,5})(?!\d)";   // the drawing number: 4 or 5 digits on their own (not part of a six-digit job number)
const int    SheetDigits   = 3;                  // the last 3 digits are the sheet; the series is what's before them (01250 -> 1)
const string SupersededSubfolder = "_superseded";   // created next to the drawings
// -----------------------------------------------------------------------------

bool Headless = Process.GetCurrentProcess().ProcessName.Equals("accoreconsole", StringComparison.OrdinalIgnoreCase);
if (!Headless) return RunLauncher();   // in Civil 3D: ask at the command line and hand the work to background consoles

// =============================================================================
// ENGINE - inside accoreconsole: which script does this drawing get?
// =============================================================================
var ed = Editor;
var sw = Stopwatch.StartNew();
string dwg = Database.Filename ?? "";
if (string.IsNullOrEmpty(dwg) || !File.Exists(dwg))
{
    ed.WriteMessage("\nSeries Scripts: this drawing isn't saved - nothing to do.");
    return null;
}
dwg = Path.GetFullPath(dwg);
string dwgName = Path.GetFileNameWithoutExtension(dwg);
string nextScr    = Env("TTW_SS_NEXT");         // where the console's next steps go; not set = run by hand, change nothing
string doneFile   = Env("TTW_SS_DONE");         // written by the console once the script has run and the drawing is saved
string supCopy    = Env("TTW_SS_SUPERSEDED");   // the runner's copy of the drawing as it was
string toolDir    = !string.IsNullOrEmpty(ScriptFolder) ? ScriptFolder : (!string.IsNullOrEmpty(ScriptPath) ? Path.GetDirectoryName(ScriptPath) : "");
string seriesDir  = Env("TTW_SS_SCRIPTS") ?? Path.Combine(toolDir, SeriesFolder);
string resultFile = Env("TTW_SS_RESULT") ?? Path.Combine(Path.GetTempPath(), "TTW Series Scripts - " + dwgName + ".json");
string logFile    = Env("TTW_SS_LOG") ?? Path.Combine(Path.GetTempPath(), "TTW Series Scripts - " + dwgName + ".log");
try { Directory.CreateDirectory(Path.GetDirectoryName(logFile)); File.WriteAllText(logFile, ""); } catch { }

string Env(string n) { var v = Environment.GetEnvironmentVariable(n); return string.IsNullOrWhiteSpace(v) ? null : v; }
void Say(string s) { ed.WriteMessage("\nSeries Scripts: " + s); Log(s); }
void Log(string s) { try { File.AppendAllText(logFile, DateTime.Now.ToString("HH:mm:ss  ") + s + Environment.NewLine); } catch { } }

var issues = new List<string>();      // things the user should check
var titles = new List<TitleNumber>(); // every drawing-number attribute in the drawing
string verdict = null, number = null, numberFrom = null, fileNumber = null;
int series = -1;
SeriesScript script = null;

try
{
    Say("reading the drawing number of " + Path.GetFileName(dwg));

    // ---- the title block: drawing-number attributes on every layout --------------------------
    var tags = new HashSet<string>(NumberTags.Split(',').Select(t => t.Trim()).Where(t => t.Length > 0), StringComparer.OrdinalIgnoreCase);
    var brClass = Autodesk.AutoCAD.Runtime.RXObject.GetClass(typeof(BlockReference));
    try
    {
        using (var tr = Database.TransactionManager.StartOpenCloseTransaction())
        {
            var bt = (BlockTable)tr.GetObject(Database.BlockTableId, OpenMode.ForRead);
            foreach (ObjectId btrId in bt)
            {
                var btr = (BlockTableRecord)tr.GetObject(btrId, OpenMode.ForRead);
                if (!btr.IsLayout) continue;
                string layout = btr.Name;
                try { if (tr.GetObject(btr.LayoutId, OpenMode.ForRead) is Layout lo) layout = lo.LayoutName; } catch { }
                foreach (ObjectId id in btr)
                {
                    if (!id.ObjectClass.IsDerivedFrom(brClass)) continue;
                    if (!(tr.GetObject(id, OpenMode.ForRead) is BlockReference br) || br.AttributeCollection.Count == 0) continue;
                    string block = br.Name;
                    try { if (br.IsDynamicBlock) block = ((BlockTableRecord)tr.GetObject(br.DynamicBlockTableRecord, OpenMode.ForRead)).Name; } catch { }
                    foreach (ObjectId aid in br.AttributeCollection)
                    {
                        if (!(tr.GetObject(aid, OpenMode.ForRead) is AttributeReference a) || a.IsErased || !tags.Contains(a.Tag)) continue;
                        var value = (a.TextString ?? "").Trim();
                        titles.Add(new TitleNumber { Layout = layout, Block = block, Tag = a.Tag, Value = value, Number = NumberIn(value) });
                    }
                }
            }
            tr.Commit();
        }
    }
    catch (Exception ex) { issues.Add("The title block couldn't be read (" + ex.Message + ") - the file name was used."); Log(ex.ToString()); }
    foreach (var t in titles) Log("title block: " + t.Layout + " / " + t.Block + " / " + t.Tag + " = \"" + t.Value + "\"" + (t.Number == null ? " (no drawing number in it)" : ""));

    // ---- and the file name --------------------------------------------------------------------
    fileNumber = NumberIn(dwgName);
    Log("file name: " + (fileNumber ?? "no drawing number"));

    // ---- which number, which series --------------------------------------------------------------
    var titleNumbers = titles.Where(t => t.Number != null).ToList();
    var titleSeries = titleNumbers.Select(t => SeriesOf(t.Number)).Distinct().ToList();
    if (titleSeries.Count > 1)
    {
        verdict = "MIXED SERIES";
        issues.Add("The title blocks are in different series: " + string.Join(", ", titleNumbers.Select(t => t.Value + " (" + t.Layout + ")")) + " - not changed.");
    }
    else if (titleSeries.Count == 1)
    {
        number = titleNumbers[0].Number; numberFrom = "title block"; series = titleSeries[0];
        var otherNumbers = titleNumbers.Select(t => t.Number).Distinct().Where(n => n != number).ToList();
        if (otherNumbers.Count > 0) issues.Add("The title blocks have different drawing numbers in the same series: " + string.Join(", ", titleNumbers.Select(t => t.Value + " (" + t.Layout + ")")) + ".");
        if (fileNumber != null && SeriesOf(fileNumber) != series)
        {
            verdict = "NUMBER MISMATCH";
            issues.Add("The file name says " + fileNumber + " (series " + SeriesOf(fileNumber).ToString("00") + ") but the title block says " + titleNumbers[0].Value + " (series " + series.ToString("00") + ") - not changed. Fix whichever is wrong and run it again.");
        }
        else if (fileNumber != null && fileNumber != number)
            issues.Add("The file name says " + fileNumber + " but the title block says " + titleNumbers[0].Value + " - same series, so its script ran; check which is right.");
    }
    else if (fileNumber != null)
    {
        number = fileNumber; series = SeriesOf(fileNumber);
        numberFrom = titles.Count == 0 ? "file name (no drawing-number attribute found)" : "file name (the title block's drawing number is blank)";
        if (titles.Count > 0) issues.Add("The title block's drawing number (" + string.Join(", ", titles.Select(t => t.Tag + " on " + t.Layout)) + ") has no number in it - the file name's " + fileNumber + " was used.");
    }
    else
    {
        verdict = "NO NUMBER";
        issues.Add("No drawing number in the title block (" + (titles.Count == 0 ? "no attribute tagged " + NumberTags.Replace(",", ", ") : string.Join(", ", titles.Select(t => t.Tag + " = \"" + t.Value + "\""))) + ") or the file name - not changed.");
    }

    // ---- its script -------------------------------------------------------------------------------------
    if (verdict == null)
    {
        var scripts = ScanSeries(seriesDir);
        var mine = scripts.Where(s => s.Series == series).ToList();
        Say("drawing number " + number + " (from the " + numberFrom + ") - series " + series.ToString("00"));
        if (mine.Count == 0)
        {
            verdict = "NO SCRIPT";
            issues.Add("No script for series " + series.ToString("00") + " in " + seriesDir + " - not changed. Add one named \"" + series.ToString("00") + " <series name>.csx\".");
        }
        else if (mine.Count > 1)
        {
            verdict = "NO SCRIPT";
            issues.Add("More than one script for series " + series.ToString("00") + ": " + string.Join(", ", mine.Select(s => Path.GetFileName(s.File))) + " - not changed. Keep one.");
        }
        else script = mine[0];
    }

    // ---- read-only: open on someone else's machine, or the file is read-only -----------------------------
    if (verdict == null && nextScr != null)
    {
        bool ro = false;
        try { ro = Document != null && Document.IsReadOnly; } catch { }
        try { if ((File.GetAttributes(dwg) & FileAttributes.ReadOnly) != 0) ro = true; } catch { }
        if (ro) { verdict = "READ ONLY"; issues.Add("The drawing opened read-only (open on someone else's machine, or the file is read-only) - not changed."); }
        else if (string.IsNullOrEmpty(supCopy) || !File.Exists(supCopy)) { verdict = "NO SUPERSEDED COPY"; issues.Add("The superseded copy wasn't made, so the script wasn't run - not changed."); }
    }

    // ---- hand the console its next steps: the script, then save ----------------------------------------
    if (verdict == null)
    {
        if (nextScr == null)
        {
            verdict = "WOULD RUN";
            Say("would run " + Path.GetFileName(script.File) + " - run from Civil 3D (Series Scripts) to apply it. Nothing was changed.");
        }
        else
        {
            Environment.SetEnvironmentVariable("TTW_SS_NUMBER", number);
            Environment.SetEnvironmentVariable("TTW_SS_SERIES", series.ToString());
            Environment.SetEnvironmentVariable("TTW_SS_SERIES_NAME", script.Name);
            Environment.SetEnvironmentVariable("TTW_SS_DRAWING", dwg);
            var s = new StringBuilder();
            s.Append("(setq ttw-dwg-number " + Lisp(number) + " ttw-series " + series + " ttw-series-name " + Lisp(script.Name) + " ttw-superseded " + Lisp(supCopy) + ")\n");
            s.Append("(princ \"\\nTTW-SERIES-START " + Path.GetFileName(script.File).Replace("\"", "'") + "\\n\")\n");
            if (script.Kind == "csx") s.Append("TTWRUNCSX\n" + script.File + "\n");
            else if (script.Kind == "lsp") s.Append("(load " + Lisp(script.File) + ")\n");
            else
            {
                var body = File.ReadAllText(script.File).Replace("\r\n", "\n").Replace("\r", "\n");
                s.Append(body.EndsWith("\n") ? body : body + "\n");
            }
            s.Append("(princ \"\\nTTW-SERIES-END\\n\")\n");
            s.Append("_.QSAVE\n");
            s.Append("(progn (setq ttw-done (open " + Lisp(doneFile) + " \"w\")) (if ttw-done (progn (write-line \"done\" ttw-done) (close ttw-done))) (princ))\n");
            File.WriteAllText(nextScr, s.ToString());
            verdict = "RUN";
            Say("running " + Path.GetFileName(script.File) + ", then saving.");
        }
    }
    else Say(verdict + " - " + (issues.LastOrDefault() ?? ""));
}
catch (Exception ex)
{
    verdict = "FAILED";
    issues.Add(ex.Message);
    Say("[ERROR] " + ex.Message + " - details in " + logFile);
    Log(ex.ToString());
}

var resultObj = new { verdict, drawing = dwg, number, numberFrom, fileNumber, titles, series, seriesName = script?.Name, script = script?.File, scriptKind = script?.Kind,
                      issues = issues.Distinct().ToList(), seconds = (int)sw.Elapsed.TotalSeconds, log = logFile };
try { File.WriteAllText(resultFile, System.Text.Json.JsonSerializer.Serialize(resultObj)); } catch { }
return resultObj;


// =============================================================================
// Drawing numbers and series - shared by the launcher and the engine
// =============================================================================
string NumberIn(string s)
{
    if (string.IsNullOrWhiteSpace(s)) return null;
    var m = Regex.Match(s, NumberPattern);
    return m.Success ? m.Groups[1].Value : null;
}

int SeriesOf(string drawingNumber)
{
    long n = long.Parse(drawingNumber);
    for (int i = 0; i < SheetDigits; i++) n /= 10;
    return (int)n;
}

// "01 Siteworks.csx" -> series 1, "Siteworks". Only .csx / .scr / .lsp files that start with a number.
List<SeriesScript> ScanSeries(string dir)
{
    var list = new List<SeriesScript>();
    if (string.IsNullOrEmpty(dir) || !Directory.Exists(dir)) return list;
    foreach (var f in Directory.GetFiles(dir).OrderBy(x => x, StringComparer.OrdinalIgnoreCase))
    {
        var ext = Path.GetExtension(f).ToLowerInvariant();
        if (ext != ".csx" && ext != ".scr" && ext != ".lsp") continue;
        var m = Regex.Match(Path.GetFileNameWithoutExtension(f), @"^\s*(\d{1,3})(?!\d)[\s._-]*(.*)$");
        if (!m.Success) continue;
        int n = int.Parse(m.Groups[1].Value);
        var name = m.Groups[2].Value.Trim();
        list.Add(new SeriesScript { Series = n, Name = name.Length > 0 ? name : "Series " + n.ToString("00"), File = f, Kind = ext.Substring(1) });
    }
    return list;
}

// A LISP string: forward slashes for paths, no quotes inside.
string Lisp(string s) { return "\"" + (s ?? "").Replace("\\", "/").Replace("\"", "'") + "\""; }


// =============================================================================
// LAUNCHER - runs when the tool is started from Advanced Tools. Command line only.
// =============================================================================
object RunLauncher()
{
    const string LoaderFile = @"bin\TTW.CsxConsole.dll";
    const int    Workers    = 5;         // drawings run at once
    const int    TimeoutMin = 30;        // a drawing still running after this is stopped and reported
    var lci = StringComparer.OrdinalIgnoreCase;
    var led = Editor;
    void Note(string s) { led.WriteMessage("\nSeries Scripts: " + s); }
    var jobsRoot = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "TTW", "BatchSeriesScripts");

    // ---- one already running? (old working folders are cleared out on the way) ------------
    string runningJob = null; int runningPid = 0;
    if (Directory.Exists(jobsRoot))
        foreach (var d in Directory.GetDirectories(jobsRoot))
            if (Directory.GetLastWriteTime(d) < DateTime.Now.AddDays(-14)) { try { Directory.Delete(d, true); } catch { } }
    if (Directory.Exists(jobsRoot))
        foreach (var d in Directory.GetDirectories(jobsRoot).OrderByDescending(x => x))
        {
            var pf = Path.Combine(d, "runner.pid");
            if (!File.Exists(pf) || !int.TryParse(File.ReadAllText(pf).Trim(), out var pid)) continue;
            try { var rp = Process.GetProcessById(pid); if (!rp.HasExited && rp.ProcessName.StartsWith("powershell", StringComparison.OrdinalIgnoreCase)) { runningJob = d; runningPid = pid; break; } } catch { }
        }
    if (runningJob != null)
    {
        string Progress()
        {
            int total = 0, done = 0, ok = 0, check = 0, failed = 0;
            try
            {
                var st = System.Text.Json.JsonDocument.Parse(File.ReadAllText(Path.Combine(runningJob, "status.json"))).RootElement;
                total = st.GetProperty("total").GetInt32(); done = st.GetProperty("done").GetInt32();
                ok = st.GetProperty("ok").GetInt32(); check = st.GetProperty("check").GetInt32(); failed = st.GetProperty("failed").GetInt32();
            }
            catch { }
            return done + " of " + total + " done" + (check + failed > 0 ? " (" + ok + " done, " + check + " to check, " + failed + " failed)" : "");
        }
        Note("a batch is running - " + Progress() + ".");
        if (AppDomain.CurrentDomain.GetData(TrayKey) == null) StartTray(runningJob, 0);   // Civil 3D was restarted: bring the status-bar logo back
        var pk = new PromptKeywordOptions("\nSeries Scripts") { AllowNone = true };
        pk.Keywords.Add("Status"); pk.Keywords.Add("Cancel"); pk.Keywords.Add("New");
        pk.Keywords.Default = "Status";
        var kr = led.GetKeywords(pk);
        if (kr.Status != PromptStatus.OK && kr.Status != PromptStatus.None) return null;
        var pick = kr.Status == PromptStatus.None ? "Status" : kr.StringResult;
        if (pick == "Status")
        {
            Note(Progress() + ". The report opens when it's finished. Job folder: " + runningJob);
            return null;
        }
        if (pick == "Cancel")
        {
            try { Process.Start(new ProcessStartInfo("taskkill", "/PID " + runningPid + " /T /F") { UseShellExecute = false, CreateNoWindow = true })?.WaitForExit(15000); } catch { }
            try { File.Delete(Path.Combine(runningJob, "runner.pid")); } catch { }
            StopTray();
            Note("cancelled - " + Progress() + ". Drawings already done are saved; their copies as they were are in _superseded. A drawing stopped part-way may need its superseded copy put back - check its log in " + runningJob + ".");
            return null;
        }
        // New: carry on and start another alongside it
    }

    // ---- this tool, its loader and the series scripts -------------------------------------
    string folder = ScriptFolder ?? "";
    // This tool's own file (the host passes it in) - so renaming the tool's @name doesn't break it.
    string self = !string.IsNullOrEmpty(ScriptPath) && File.Exists(ScriptPath) ? ScriptPath : Path.Combine(folder, "ttw_stdCountry_Series_Scripts.csx");
    string loader = Path.Combine(folder, LoaderFile);
    string acadDir = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName);
    string consoleExe = Path.Combine(acadDir, "accoreconsole.exe");
    foreach (var need in new[] { self, loader, consoleExe })
        if (string.IsNullOrEmpty(need) || !File.Exists(need)) { Note("can't find " + (need ?? "this tool's .csx in " + folder) + " - nothing was started."); return null; }

    string seriesDir = Path.Combine(folder, SeriesFolder);
    var scripts = ScanSeries(seriesDir);
    if (scripts.Count == 0)
    {
        Note("no series scripts in " + seriesDir + " - add one per series, named for it: \"01 Siteworks.csx\", \"02 Erosion and Sediment Control.csx\" (.csx, .scr or .lsp). Nothing was started.");
        return null;
    }
    var twice = scripts.GroupBy(s => s.Series).Where(g => g.Count() > 1).ToList();
    if (twice.Count > 0)
    {
        Note("more than one script for series " + string.Join("; ", twice.Select(g => g.Key.ToString("00") + " (" + string.Join(", ", g.Select(s => Path.GetFileName(s.File))) + ")")) + " in " + seriesDir + " - keep one per series. Nothing was started.");
        return null;
    }
    Note("series scripts: " + string.Join(", ", scripts.Select(s => s.Series.ToString("00") + " " + s.Name + " (." + s.Kind + ")")) + ".");

    // ---- what to run ----------------------------------------------------------------------
    var current = Database.Filename;
    if (string.IsNullOrEmpty(current) || !File.Exists(current)) { Note("save this drawing first - it's used to find the drawings' folder."); return null; }
    current = Path.GetFullPath(current);

    // every drawing in this folder, except ones this drawing uses as xrefs (frames etc.)
    var dwgDir = Path.GetDirectoryName(current);
    var xrefFiles = new HashSet<string>(lci);
    try
    {
        var g = Database.GetHostDwgXrefGraph(true);
        for (int i = 1; i < g.NumNodes; i++) { var n = (XrefGraphNode)g.GetXrefNode(i); if (n.Database != null && !string.IsNullOrEmpty(n.Database.Filename)) xrefFiles.Add(Path.GetFullPath(n.Database.Filename)); }
    }
    catch { }
    var inFolder = Directory.GetFiles(dwgDir, "*.dwg").Select(Path.GetFullPath).Where(f => !xrefFiles.Contains(f)).OrderBy(f => f, lci).ToList();
    if (!inFolder.Contains(current, lci)) inFolder.Insert(0, current);

    var drawings = new List<string> { current };
    if (inFolder.Count > 1)
    {
        var pk = new PromptKeywordOptions("\nRun") { AllowNone = true };
        pk.Keywords.Add("All", "All", "All " + inFolder.Count + " in this folder");
        pk.Keywords.Add("This", "This", "This drawing only");
        pk.Keywords.Add("Pick", "Pick", "Pick");
        pk.Keywords.Default = "All";
        var kr = led.GetKeywords(pk);
        if (kr.Status != PromptStatus.OK && kr.Status != PromptStatus.None) { Note("cancelled."); return null; }
        var pick = kr.Status == PromptStatus.None ? "All" : kr.StringResult;
        if (pick == "All") drawings = inFolder;
        else if (pick == "Pick")
        {
            using (var dlg = new System.Windows.Forms.OpenFileDialog { Filter = "Drawings (*.dwg)|*.dwg", Multiselect = true, InitialDirectory = dwgDir, Title = "Series Scripts - pick the drawings to run" })
            {
                if (dlg.ShowDialog() != System.Windows.Forms.DialogResult.OK || dlg.FileNames.Length == 0) { Note("cancelled."); return null; }
                drawings = dlg.FileNames.Select(Path.GetFullPath).OrderBy(f => f, lci).ToList();
            }
        }
    }

    // ---- what each one gets, going by its file name (the title block is checked as each opens) ----
    string Expect(string d)
    {
        var n = NumberIn(Path.GetFileNameWithoutExtension(d));
        if (n == null) return "no number in the file name";
        var s = scripts.FirstOrDefault(x => x.Series == SeriesOf(n));
        return s != null ? s.Series.ToString("00") + " " + s.Name : "series " + SeriesOf(n).ToString("00") + " - no script";
    }
    foreach (var grp in drawings.GroupBy(Expect).OrderBy(x => x.Key, lci))
        Note("  " + grp.Count().ToString().PadLeft(3) + "  " + grp.Key);

    // ---- drawings open here have to be closed so a console can save them ------------------------
    var dm = Autodesk.AutoCAD.ApplicationServices.Core.Application.DocumentManager;
    var openHere = new List<Document>();
    var unsaved = new List<string>();
    foreach (Document d in dm)
    {
        string fn = null;
        try { fn = d.Database?.Filename; } catch { }
        if (string.IsNullOrEmpty(fn) || !drawings.Contains(Path.GetFullPath(fn), lci)) continue;
        bool saved = false;
        try
        {
            var acad = DocumentExtension.GetAcadDocument(d);
            saved = Convert.ToBoolean(acad.GetType().InvokeMember("Saved", System.Reflection.BindingFlags.GetProperty, null, acad, null));
        }
        catch { }
        if (saved) openHere.Add(d); else unsaved.Add(Path.GetFileName(fn));
    }
    if (unsaved.Count > 0)
    {
        Note("open here with unsaved changes: " + string.Join(", ", unsaved) + " - save or close " + (unsaved.Count == 1 ? "it" : "them") + " first. Nothing was started.");
        return null;
    }
    if (openHere.Count > 0)
    {
        Note("open here: " + string.Join(", ", openHere.Select(d => Path.GetFileName(d.Name))) + ". A drawing has to be closed for its script to save it.");
        var pk = new PromptKeywordOptions("\nClose " + (openHere.Count == 1 ? "it" : "them") + " when the batch starts? (Reopen once it's done.)") { AllowNone = true };
        pk.Keywords.Add("Yes"); pk.Keywords.Add("No");
        pk.Keywords.Default = "Yes";
        var kr = led.GetKeywords(pk);
        if (kr.Status != PromptStatus.OK && kr.Status != PromptStatus.None) { Note("cancelled."); return null; }
        if (kr.Status == PromptStatus.OK && kr.StringResult == "No")
        {
            var skip = new HashSet<string>(openHere.Select(d => Path.GetFullPath(d.Database.Filename)), lci);
            drawings = drawings.Where(d => !skip.Contains(d)).ToList();
            openHere.Clear();
            Note("left out - they stay open.");
            if (drawings.Count == 0) { Note("nothing left to run."); return null; }
        }
    }

    // ---- last check: this saves the drawings --------------------------------------------------
    var stamp = DateTime.Now.ToString("yyyy-MM-dd HHmm");
    var firstDir = Path.GetDirectoryName(drawings[0]);
    var supDir = Path.Combine(firstDir, SupersededSubfolder, stamp);
    for (int n = 2; Directory.Exists(supDir); n++) supDir = Path.Combine(firstDir, SupersededSubfolder, stamp + " (" + n + ")");
    {
        var pk = new PromptKeywordOptions("\nRun the series scripts on " + drawings.Count + " drawing" + (drawings.Count == 1 ? "" : "s") + " and save " + (drawings.Count == 1 ? "it" : "them") + "? Each is copied to " + SupersededSubfolder + " first") { AllowNone = true };
        pk.Keywords.Add("Yes"); pk.Keywords.Add("No");
        pk.Keywords.Default = "Yes";
        var kr = led.GetKeywords(pk);
        if ((kr.Status != PromptStatus.OK && kr.Status != PromptStatus.None) || (kr.Status == PromptStatus.OK && kr.StringResult == "No")) { Note("cancelled - nothing was changed."); return null; }
    }

    // ---- hand it to the background --------------------------------------------------
    var mp = Regex.Match(Environment.CommandLine, @"/product\s+""?(\w+)", RegexOptions.IgnoreCase);
    var jobDir = Path.Combine(jobsRoot, DateTime.Now.ToString("yyyyMMdd-HHmmss"));
    Directory.CreateDirectory(jobDir);
    int workers = Math.Min(Workers, drawings.Count);
    var job = new
    {
        Drawings = drawings,
        Workers = workers,
        Superseded = supDir,
        Folder = firstDir,
        SeriesDir = seriesDir,
        Scripts = scripts.Select(s => new { s.Series, s.Name, s.Kind, s.File }).ToList(),
        Loader = loader,
        Self = self,
        Console = consoleExe,
        AecBase = Path.Combine(acadDir, "AecBase.dbx"),
        Product = mp.Success ? mp.Groups[1].Value : "C3D",
        TimeoutMin,
        OpenWaitSec = openHere.Count > 0 ? 120 : 0,
        Started = DateTime.Now.ToString("d MMM yyyy h:mm tt"),
        By = Environment.UserName
    };
    File.WriteAllText(Path.Combine(jobDir, "job.json"), System.Text.Json.JsonSerializer.Serialize(job, new System.Text.Json.JsonSerializerOptions { WriteIndented = true }));
    File.WriteAllText(Path.Combine(jobDir, "report-template.html"), ReportTemplate.Replace("{{LOGO}}", LogoB64), new UTF8Encoding(false));
    var runner = Path.Combine(jobDir, "runner.ps1");
    File.WriteAllText(runner, RunnerScript, new UTF8Encoding(true));   // BOM so Windows PowerShell reads it as UTF-8
    Process.Start(new ProcessStartInfo("powershell.exe", "-NoProfile -WindowStyle Hidden -ExecutionPolicy Bypass -File \"" + runner + "\" -JobDir \"" + jobDir + "\"")
                  { UseShellExecute = false, CreateNoWindow = true, WorkingDirectory = jobDir });
    bool tray = StartTray(jobDir, drawings.Count);
    Note(drawings.Count + " drawing" + (drawings.Count == 1 ? "" : "s") + " queued" + (drawings.Count > 1 ? ", " + workers + " at a time" : "") + ", in the background - carry on working.");
    Note("copies as they were, and the report, go to " + supDir + ". " + (tray ? "The TTW logo in the status bar shows progress." : "Run Series Scripts again for progress or to cancel."));

    // close the ones open here once this command has finished (the runner waits for them)
    if (openHere.Count > 0)
    {
        var toClose = openHere.ToList();
        try
        {
            dm.ExecuteInApplicationContext(_ =>
            {
                foreach (var d in toClose) { try { DocumentExtension.CloseAndDiscard(d); } catch { } }
            }, null);
        }
        catch (Exception ex) { Note("couldn't close " + string.Join(", ", toClose.Select(d => Path.GetFileName(d.Name))) + " (" + ex.Message + ") - close " + (toClose.Count == 1 ? "it" : "them") + " in the next 2 minutes, or " + (toClose.Count == 1 ? "it's" : "they're") + " reported OPEN ELSEWHERE."); }
    }
    return new { queued = drawings.Count, workers, jobDir, superseded = supDir, tray };
}

// =============================================================================
// Status bar - a spinning TTW logo while the batch runs, a tick and a notification when it's done
// =============================================================================
const string TrayKey = "TTW.SeriesScripts.Tray";

void StopTray()
{
    try
    {
        if (AppDomain.CurrentDomain.GetData(TrayKey) is object[] old)
        {
            (old[1] as System.Windows.Forms.Timer)?.Stop();
            (old[1] as System.Windows.Forms.Timer)?.Dispose();
            var sb = Autodesk.AutoCAD.ApplicationServices.Application.StatusBar;
            if (old[0] is Autodesk.AutoCAD.Windows.TrayItem ti) { try { ti.CloseBubbleWindows(); } catch { } sb.TrayItems.Remove(ti); }
            sb.Update();
        }
    }
    catch { }
    AppDomain.CurrentDomain.SetData(TrayKey, null);
}

bool StartTray(string jobDir, int total)
{
    try
    {
        StopTray();
        var frames = TrayFrames();
        var item = new Autodesk.AutoCAD.Windows.TrayItem { ToolTipText = "Series Scripts: starting " + total + " drawing" + (total == 1 ? "" : "s"), Icon = frames[0], Visible = true };
        var sbar = Autodesk.AutoCAD.ApplicationServices.Application.StatusBar;
        sbar.TrayItems.Add(item);
        sbar.Update();
        var timer = new System.Windows.Forms.Timer { Interval = 150 };
        int tick = 0, done = 0, check = 0, failed = 0;
        bool finished = false;
        string report = null, statusFile = Path.Combine(jobDir, "status.json"), alive = Path.Combine(jobDir, "tray.alive");
        int Num(System.Text.Json.JsonElement e, string p) { return e.TryGetProperty(p, out var v) && v.ValueKind == System.Text.Json.JsonValueKind.Number ? v.GetInt32() : 0; }
        void OpenReport() { if (report != null && File.Exists(report)) try { Process.Start(new ProcessStartInfo(report) { UseShellExecute = true }); } catch { } }
        timer.Tick += (s, e) =>
        {
            try
            {
                if (!finished) { item.Icon = frames[++tick % frames.Length]; sbar.Update(); }
                if (tick % 14 != 1 && !finished) return;   // read the runner's status every ~2 s
                try { File.WriteAllText(alive, DateTime.Now.ToString("s")); } catch { }
                if (finished || !File.Exists(statusFile)) return;
                var st = System.Text.Json.JsonDocument.Parse(File.ReadAllText(statusFile)).RootElement;
                total = Num(st, "total"); done = Num(st, "done"); check = Num(st, "check"); failed = Num(st, "failed");
                int running = Num(st, "running");
                bool fin = st.TryGetProperty("finished", out var fv) && fv.ValueKind == System.Text.Json.JsonValueKind.True;
                bool runnerGone = !File.Exists(Path.Combine(jobDir, "runner.pid"));
                if (!fin && !runnerGone)
                {
                    item.ToolTipText = "Series Scripts: " + done + " of " + total + " done" + (running > 0 ? ", " + running + " running now" : "") + (check + failed > 0 ? " (" + (check + failed) + " to check)" : "") + " - click for details";
                    return;
                }
                finished = true;
                timer.Interval = 2000;
                if (st.TryGetProperty("report", out var rv) && rv.ValueKind == System.Text.Json.JsonValueKind.String) report = rv.GetString();
                bool clean = fin && check + failed == 0 && done == total;
                item.Icon = clean ? TrayDone(true) : TrayDone(false);
                var summary = !fin ? "Stopped - " + done + " of " + total + " done."
                            : clean ? "All " + total + " drawing" + (total == 1 ? "" : "s") + " done."
                            : done + " of " + total + " run: " + (done - check - failed) + " done, " + check + " to check, " + failed + " failed.";
                item.ToolTipText = "Series Scripts finished - " + summary + (report != null ? " Click to open the report." : "");
                sbar.Update();
                var bw = new Autodesk.AutoCAD.Windows.TrayItemBubbleWindow
                {
                    Title = "Series Scripts finished",
                    Text = summary,
                    IconType = clean ? Autodesk.AutoCAD.Windows.IconType.Information : Autodesk.AutoCAD.Windows.IconType.Warning
                };
                if (report != null) { bw.HyperText = "Open the report"; bw.HyperLink = new Uri(report).AbsoluteUri; }
                bw.Closed += (o, a) => { if (a.CloseReason == Autodesk.AutoCAD.Windows.TrayItemBubbleWindowCloseReason.HyperlinkClicked) StopTray(); };
                item.ShowBubbleWindow(bw);
            }
            catch { }
        };
        item.MouseDown += (s, e) =>
        {
            if (finished) { OpenReport(); StopTray(); return; }
            var bw = new Autodesk.AutoCAD.Windows.TrayItemBubbleWindow
            {
                Title = "Series Scripts",
                Text = done + " of " + total + " done" + (check + failed > 0 ? " (" + (check + failed) + " to check)" : "") + ". The report opens from here when it's finished. Run Series Scripts again to cancel.",
                IconType = Autodesk.AutoCAD.Windows.IconType.Information
            };
            item.ShowBubbleWindow(bw);
        };
        timer.Start();
        AppDomain.CurrentDomain.SetData(TrayKey, new object[] { item, timer });
        return true;
    }
    catch (Exception ex) { Editor.WriteMessage("\nSeries Scripts: no status-bar progress (" + ex.Message + ")."); return false; }
}

// The five dots of the TTW mark on a Water disc, turned a little each frame (it repeats every 90 degrees).
System.Drawing.Icon[] TrayFrames()
{
    using (var ms = new MemoryStream(Convert.FromBase64String(LogoB64)))
    using (var logo = new System.Drawing.Bitmap(ms))
    using (var mark = new System.Drawing.Bitmap(48, 48))
    {
        using (var g = System.Drawing.Graphics.FromImage(mark))
            g.DrawImage(logo, new System.Drawing.Rectangle(2, 3, 44, 44), new System.Drawing.Rectangle(196, 4, 44, 44), System.Drawing.GraphicsUnit.Pixel);
        var list = new List<System.Drawing.Icon>();
        for (int a = 0; a < 90; a += 10)
        {
            var bmp = new System.Drawing.Bitmap(16, 16);
            using (var g = System.Drawing.Graphics.FromImage(bmp))
            {
                g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
                g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.HighQualityBicubic;
                using (var water = new System.Drawing.SolidBrush(System.Drawing.Color.FromArgb(0x02, 0x43, 0x60))) g.FillEllipse(water, 0, 0, 15.5f, 15.5f);
                g.TranslateTransform(8, 8); g.RotateTransform(a);
                g.DrawImage(mark, new System.Drawing.RectangleF(-5.5f, -5.5f, 11, 11));
            }
            list.Add(System.Drawing.Icon.FromHandle(bmp.GetHicon()));
        }
        return list.ToArray();
    }
}

// Finished: a tick on Land green, or an exclamation mark on Clay when something needs checking.
System.Drawing.Icon TrayDone(bool clean)
{
    var bmp = new System.Drawing.Bitmap(16, 16);
    using (var g = System.Drawing.Graphics.FromImage(bmp))
    {
        g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;
        var c = clean ? System.Drawing.Color.FromArgb(0x32, 0x54, 0x42) : System.Drawing.Color.FromArgb(0xA8, 0x49, 0x33);
        using (var b = new System.Drawing.SolidBrush(c)) g.FillEllipse(b, 0, 0, 15.5f, 15.5f);
        using (var p = new System.Drawing.Pen(System.Drawing.Color.FromArgb(0xF7, 0xF4, 0xE7), 2.2f) { StartCap = System.Drawing.Drawing2D.LineCap.Round, EndCap = System.Drawing.Drawing2D.LineCap.Round, LineJoin = System.Drawing.Drawing2D.LineJoin.Round })
        {
            if (clean) g.DrawLines(p, new[] { new System.Drawing.PointF(4.2f, 8.2f), new System.Drawing.PointF(6.8f, 10.8f), new System.Drawing.PointF(11.8f, 5.2f) });
            else { g.DrawLine(p, 7.8f, 3.6f, 7.8f, 8.6f); g.DrawLine(p, 7.8f, 11.6f, 7.8f, 11.8f); }
        }
    }
    return System.Drawing.Icon.FromHandle(bmp.GetHicon());
}

// =============================================================================
// Background runner (Windows PowerShell 5.1) - written to the job folder and started by the launcher
// =============================================================================
const string RunnerScript = """"
param([Parameter(Mandatory = $true)][string]$JobDir)
$ErrorActionPreference = 'Continue'
$job = Get-Content -Raw -LiteralPath (Join-Path $JobDir 'job.json') | ConvertFrom-Json
$pidFile = Join-Path $JobDir 'runner.pid'
Set-Content -LiteralPath $pidFile -Value $PID   # lets Series Scripts find this batch for Status / Cancel
foreach ($d in 'results', 'logs', 'scripts', 'done') { New-Item -ItemType Directory -Force -Path (Join-Path $JobDir $d) | Out-Null }
$env:TTW_SS_SCRIPTS = $job.SeriesDir

$total = @($job.Drawings).Count
$queue = New-Object System.Collections.Queue
$n = 0
foreach ($d in $job.Drawings) { $n++; $queue.Enqueue([pscustomobject]@{ N = $n; Dwg = $d }) }
$running  = New-Object System.Collections.ArrayList
$finished = New-Object System.Collections.ArrayList
$t0 = Get-Date
$waitOpenUntil = $t0.AddSeconds([int]$job.OpenWaitSec)   # drawings Civil 3D is closing for this batch
$script:report = $null
$script:allDone = $false
$Good = 'DONE'
$NotChanged = @('NO SCRIPT', 'NO NUMBER', 'NUMBER MISMATCH', 'MIXED SERIES', 'READ ONLY', 'OPEN ELSEWHERE')
$Check = @('DONE - CHECK') + $NotChanged

function Write-Status {
    $f = @($finished)
    $st = [ordered]@{ total = $total; done = $f.Count; running = $running.Count
        ok = @($f | Where-Object Verdict -eq $Good).Count
        check = @($f | Where-Object { $_.Verdict -in $Check }).Count
        failed = @($f | Where-Object { $_.Verdict -ne $Good -and $_.Verdict -notin $Check }).Count
        finished = $script:allDone; report = $script:report
        updated = (Get-Date).ToString('s') }
    try { $st | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $JobDir 'status.json') } catch { }
}

# can this drawing be written? (not open in Civil 3D here or on anyone else's machine)
function Test-Free($path) {
    try { $s = [IO.File]::Open($path, 'Open', 'ReadWrite', 'None'); $s.Close(); return $true } catch { return $false }
}
function Get-Stamp($path) {
    try { $i = Get-Item -LiteralPath $path -ErrorAction Stop; return '{0}|{1}' -f $i.Length, $i.LastWriteTimeUtc.Ticks } catch { return '' }
}
# accoreconsole writes its output as UTF-16; anything else is read as UTF-8
function Read-ConsoleLog($path) {
    try {
        $b = [IO.File]::ReadAllBytes($path)
        $zeros = @($b | Select-Object -First 4000 | Where-Object { $_ -eq 0 }).Count
        if ($zeros -gt 200) { return [Text.Encoding]::Unicode.GetString($b) } else { return [Text.Encoding]::UTF8.GetString($b) }
    } catch { return '' }
}
function Add-Finished($item, $verdict, $data, $issues, $sup, $minutes, $log, $ssLog) {
    [void]$finished.Add([pscustomobject]@{ N = $item.N; Dwg = $item.Dwg; Name = [IO.Path]::GetFileNameWithoutExtension($item.Dwg); Verdict = $verdict
        Data = $data; Issues = @($issues); Sup = $sup; Minutes = $minutes; Log = $log; SsLog = $ssLog })
    Write-Status
}
Write-Status

try {
    while ($queue.Count -gt 0 -or $running.Count -gt 0) {
        $tries = $queue.Count
        while ($running.Count -lt $job.Workers -and $queue.Count -gt 0 -and $tries -gt 0) {
            $tries--
            $item = $queue.Dequeue()
            $dwg  = $item.Dwg
            $name = [IO.Path]::GetFileNameWithoutExtension($dwg)
            if (-not (Test-Path -LiteralPath $dwg)) { Add-Finished $item 'NOT FOUND' $null @('The drawing is no longer there.') $null 0 $null $null; continue }

            # open somewhere: wait a little for drawings Civil 3D is closing for us, otherwise leave it alone
            if (-not (Test-Free $dwg)) {
                if ((Get-Date) -lt $waitOpenUntil) { $queue.Enqueue($item); continue }
                Add-Finished $item 'OPEN ELSEWHERE' $null @('The drawing is open (in Civil 3D here or on someone else''s machine), so it couldn''t be saved - not changed. Close it and run it again.') $null 0 $null $null
                continue
            }

            # the drawing as it was, before its script - kept only if the drawing changes
            $sup = Join-Path $job.Superseded ([IO.Path]::GetFileName($dwg))
            try {
                New-Item -ItemType Directory -Force -Path $job.Superseded -ErrorAction Stop | Out-Null
                Copy-Item -LiteralPath $dwg -Destination $sup -Force -ErrorAction Stop
                if ((Get-Item -LiteralPath $sup).Length -ne (Get-Item -LiteralPath $dwg).Length) { throw 'the copy is a different size' }
            }
            catch {
                Remove-Item -LiteralPath $sup -Force -ErrorAction SilentlyContinue
                Add-Finished $item 'COPY FAILED' $null @("The superseded copy couldn't be made ($($_.Exception.Message)) - the script wasn't run, not changed.") $null 0 $null $null
                continue
            }
            $before = Get-Stamp $dwg

            $tag  = '{0:D4} {1}' -f $item.N, $name
            $res  = Join-Path $JobDir "results\$tag.json"
            $log  = Join-Path $JobDir "logs\$tag - console.txt"
            $ssLog = Join-Path $JobDir "logs\$tag - series.log"
            $next = Join-Path $JobDir "scripts\$tag - next.scr"
            $done = Join-Path $JobDir "done\$tag.done"
            $scr  = Join-Path $JobDir "scripts\$tag.scr"
            # the engine (this tool) picks the script and writes it into $next; the console then runs $next: the script, QSAVE, and $done
            [IO.File]::WriteAllText($next, "(princ)`n")
            [IO.File]::WriteAllText($scr, "_.NETLOAD`n$($job.Loader)`nTTWRUNCSX`n$($job.Self)`n_.SCRIPT`n`"$next`"`n")
            $env:TTW_SS_RESULT = $res
            $env:TTW_SS_LOG = $ssLog
            $env:TTW_SS_NEXT = $next
            $env:TTW_SS_DONE = $done
            $env:TTW_SS_SUPERSEDED = $sup
            $argLine = '/i "{0}" /s "{1}" /ld "{2}" /product {3} /l en-US' -f $dwg, $scr, $job.AecBase, $job.Product
            $p = Start-Process -FilePath $job.Console -ArgumentList $argLine -RedirectStandardOutput $log -NoNewWindow -PassThru
            [void]$running.Add([pscustomobject]@{ Item = $item; P = $p; Res = $res; Log = $log; SsLog = $ssLog; Done = $done; Sup = $sup; Before = $before; Start = Get-Date })
            Write-Status
        }
        Start-Sleep -Seconds 3
        foreach ($r in @($running)) {
            $elapsed = (Get-Date) - $r.Start
            $timedOut = (-not $r.P.HasExited) -and ($elapsed.TotalMinutes -gt $job.TimeoutMin)
            if (-not ($r.P.HasExited -or $timedOut)) { continue }
            if ($timedOut) { & taskkill /PID $r.P.Id /T /F 2>&1 | Out-Null; Start-Sleep -Seconds 2 }

            $data = $null
            if (Test-Path -LiteralPath $r.Res) { try { $data = Get-Content -Raw -LiteralPath $r.Res | ConvertFrom-Json } catch { } }
            $issues = New-Object System.Collections.ArrayList
            if ($data) { foreach ($i in @($data.issues)) { if ($i) { [void]$issues.Add([string]$i) } } }
            $changed = (Get-Stamp $r.Item.Dwg) -ne $r.Before
            $saved = Test-Path -LiteralPath $r.Done

            if ($timedOut) { $verdict = 'TIMED OUT'; [void]$issues.Add("Still running after $($job.TimeoutMin) minutes, so it was stopped.") }
            elseif (-not $data) { $verdict = 'NO RESULT' }
            elseif ($data.verdict -ne 'RUN') { $verdict = [string]$data.verdict }
            elseif ($saved -and $changed) { $verdict = 'DONE' }
            elseif ($saved) { $verdict = 'NOT SAVED'; [void]$issues.Add("The script ran but the drawing wasn't saved - see the console log.") }
            elseif ($changed) { $verdict = 'SCRIPT STOPPED' }
            else { $verdict = 'SCRIPT FAILED'; [void]$issues.Add("The script stopped before the drawing was saved, so it wasn't changed - see the console log.") }
            if ($changed -and $verdict -ne 'DONE') { [void]$issues.Add("The drawing was saved but didn't finish cleanly - check it against its superseded copy, or copy that back over it.") }

            # errors the console printed while the script ran
            if ($data -and $data.verdict -eq 'RUN') {
                $txt = Read-ConsoleLog $r.Log
                $at = $txt.IndexOf('TTW-SERIES-START')
                if ($at -ge 0) {
                    $hits = @([regex]::Matches($txt.Substring($at), '(?im)^.*(; error:|\[ERROR\]|exception|unknown command|\*invalid|error CS\d+).*$') | Select-Object -First 3 | ForEach-Object { $_.Value.Trim() })
                    if ($hits.Count) { [void]$issues.Add('The console log shows errors while the script ran: ' + ($hits -join ' / ')) }
                }
            }
            if ($verdict -eq 'DONE' -and $issues.Count) { $verdict = 'DONE - CHECK' }

            # not changed: its superseded copy isn't needed
            $sup = $r.Sup
            if (-not $changed) { Remove-Item -LiteralPath $sup -Force -ErrorAction SilentlyContinue; $sup = $null }
            $running.Remove($r)
            Add-Finished $r.Item $verdict $data $issues $sup ([math]::Round($elapsed.TotalMinutes, 1)) $r.Log $r.SsLog
        }
    }
}
finally {
    foreach ($r in @($running)) { if (-not $r.P.HasExited) { & taskkill /PID $r.P.Id /T /F 2>&1 | Out-Null } }
}

# ---- the batch report, in the superseded folder ----------------------------------------------
# (not 'H' - that's PowerShell's built-in alias for Get-History)
function HtmlEsc($s) { [System.Net.WebUtility]::HtmlEncode([string]$s) }
function Link($path, $text) { if ($path -and (Test-Path -LiteralPath $path)) { "<a href='" + (HtmlEsc (([Uri]$path).AbsoluteUri)) + "'>" + (HtmlEsc $text) + "</a>" } else { '' } }
function RowClass($v) { if ($v -eq $Good) { 'ok' } elseif ($v -in $Check) { 'warn' } else { 'fail' } }
function SeriesLabel($s) { if ($null -eq $s -or [int]$s -lt 0) { '' } else { ([int]$s).ToString('00') } }
$rows  = @($finished | Sort-Object N)
$ok    = @($rows | Where-Object Verdict -eq $Good).Count
$okChk = @($rows | Where-Object Verdict -eq 'DONE - CHECK').Count
$same  = @($rows | Where-Object { $_.Verdict -in $NotChanged }).Count
$bad   = $rows.Count - $ok - $okChk - $same
$skipped = $total - $rows.Count
$supCount = @($rows | Where-Object Sup).Count
$mins  = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
$attention = @($rows | Where-Object { $_.Verdict -ne $Good -or @($_.Issues).Count })

$m = New-Object System.Text.StringBuilder
function AddLine($s) { [void]$m.AppendLine($s) }
AddLine "<div class='meta'>Folder: $(HtmlEsc $job.Folder) &nbsp;|&nbsp; Started: $(HtmlEsc $job.Started) by $(HtmlEsc $job.By) &nbsp;|&nbsp; $total drawing(s), $($job.Workers) at a time &nbsp;|&nbsp; $mins min</div>"
if ($skipped -gt 0) { AddLine "<div class='note warnnote'><strong>The batch was stopped</strong> - $skipped drawing(s) weren't run and are unchanged.</div>" }
$clean = ($ok -eq $total)
AddLine ("<div class='gradebanner " + $(if ($clean) { 'ok' } else { 'fail' }) + "'><div class='gradescore'>$ok / $total</div><div class='graderesult'>" + $(if ($clean) { '&#10003; ALL DONE' } else { '&#10007; ' + ($total - $ok) + ' TO CHECK' }) + "</div></div>")
AddLine "<div class='note'><strong>Superseded copies:</strong> $(Link $job.Superseded $job.Superseded) &nbsp;|&nbsp; $supCount drawing(s) as they were before their script. To undo one, copy it back over the drawing.</div>"
AddLine "<div class='legend'><strong>DONE</strong> - its series script ran and the drawing was saved. <strong>DONE - CHECK</strong> - saved, but something's listed to look at. <strong>NO SCRIPT / NO NUMBER / NUMBER MISMATCH / MIXED SERIES / READ ONLY / OPEN ELSEWHERE</strong> - not changed, for the reason given. <strong>SCRIPT FAILED / SCRIPT STOPPED / NOT SAVED / TIMED OUT / NO RESULT / COPY FAILED</strong> - something went wrong; the logs say what, and a drawing that changed has its superseded copy to go back to.</div>"
AddLine "<h2 id='sec-summary'>Summary</h2><div class='summary'>"
foreach ($c in @(@($total, 'Drawings'), @($ok, 'Done'), @($okChk, 'Done - to check'), @($same, 'Not changed'), @($bad, 'Failed'), @($supCount, 'Superseded copies'), @($mins, 'Minutes'))) {
    AddLine "<div class='card'><div class='num'>$($c[0])</div><div class='lbl'>$($c[1])</div></div>"
}
AddLine "</div>"

AddLine "<h2 id='sec-attention'>Needs Attention</h2>"
if ($attention.Count -eq 0) { AddLine "<div class='meta'>Nothing - every drawing had its series script run and was saved.</div>" }
else {
    AddLine "<table><tr><th>Drawing</th><th>Status</th><th>What to check</th></tr>"
    foreach ($r in $attention) {
        $iss = if (@($r.Issues).Count) { "<ul class='layerlist'>" + ((@($r.Issues) | ForEach-Object { '<li>' + (HtmlEsc $_) + '</li>' }) -join '') + '</ul>' } elseif (-not $r.Data) { 'No result was written - see ' + (Link $r.SsLog 'the log') + ' ' + (Link $r.Log 'the console log') } else { '' }
        if ($r.Sup) { $iss += "<div class='path'>As it was: $(Link $r.Sup $r.Sup)</div>" }
        AddLine "<tr class='$(RowClass $r.Verdict)'><td><a href='#d$($r.N)'>$(HtmlEsc $r.Name)</a></td><td class='status'>$(HtmlEsc $r.Verdict)</td><td>$iss</td></tr>"
    }
    AddLine "</table>"
}

AddLine "<h2 id='sec-series'>Series</h2><table><tr><th>Series</th><th>Script</th><th>Drawings</th><th>Done</th></tr>"
foreach ($s in @($job.Scripts | Sort-Object Series)) {
    $mine = @($rows | Where-Object { $_.Data -and $null -ne $_.Data.series -and [int]$_.Data.series -eq [int]$s.Series })
    $d = @($mine | Where-Object { $_.Verdict -in @('DONE', 'DONE - CHECK') }).Count
    AddLine "<tr><td>$(SeriesLabel $s.Series) $(HtmlEsc $s.Name)</td><td class='path'>$(Link $s.File ([IO.Path]::GetFileName($s.File)))</td><td>$($mine.Count)</td><td>$d</td></tr>"
}
$none = @($rows | Where-Object { -not $_.Data -or $null -eq $_.Data.script }).Count
AddLine "<tr><td colspan='2'><i>No script run (not changed, or failed before choosing one)</i></td><td>$none</td><td></td></tr></table>"

AddLine "<h2 id='sec-drawings'>Drawings</h2><table><tr><th>#</th><th>Drawing</th><th>Number</th><th>Series</th><th>Status</th><th>Superseded copy</th><th>Min</th><th>Logs</th></tr>"
foreach ($r in $rows) {
    $d = $r.Data
    $num = if ($d -and $d.number) { (HtmlEsc $d.number) + "<div class='path'>" + (HtmlEsc $d.numberFrom) + "</div>" } elseif ($d -and $d.fileNumber) { (HtmlEsc $d.fileNumber) + "<div class='path'>file name</div>" } else { '' }
    $ser = if ($d -and $d.script) { (SeriesLabel $d.series) + ' ' + (HtmlEsc $d.seriesName) + "<div class='path'>" + (HtmlEsc ([IO.Path]::GetFileName($d.script))) + "</div>" } elseif ($d) { SeriesLabel $d.series } else { '' }
    $iss = if (@($r.Issues).Count) { "<ul class='layerlist'>" + ((@($r.Issues) | ForEach-Object { '<li>' + (HtmlEsc $_) + '</li>' }) -join '') + '</ul>' } else { '' }
    AddLine "<tr id='d$($r.N)' class='$(RowClass $r.Verdict)'><td>$($r.N)</td><td>$(Link $r.Dwg $r.Name)<div class='path'>$(HtmlEsc $r.Dwg)</div>$iss</td><td>$num</td><td>$ser</td><td class='status'>$(HtmlEsc $r.Verdict)</td><td>$(Link $r.Sup 'as it was')</td><td>$($r.Minutes)</td><td>$(Link $r.SsLog 'log') $(Link $r.Log 'console')</td></tr>"
}
AddLine "</table>"

$nav = New-Object System.Text.StringBuilder
[void]$nav.Append("<div class='navlbl'>Sections</div><a href='#sec-summary'>Summary</a>")
[void]$nav.Append("<a href='#sec-attention'>Needs Attention<span class='count" + $(if ($attention.Count) { ' bad' } else { '' }) + "'>$($attention.Count)</span></a>")
[void]$nav.Append("<a href='#sec-series'>Series<span class='count'>$(@($job.Scripts).Count)</span></a>")
[void]$nav.Append("<a href='#sec-drawings'>Drawings<span class='count'>$($rows.Count)</span></a><div class='navlbl gap'>Drawings</div>")
foreach ($r in $rows) { [void]$nav.Append("<a class='small' href='#d$($r.N)'>$(HtmlEsc $r.Name)" + $(if ($r.Verdict -ne $Good) { "<span class='count bad'>!</span>" } else { '' }) + "</a>") }

$html = [IO.File]::ReadAllText((Join-Path $JobDir 'report-template.html'))
$html = $html.Replace('{{TITLE}}', 'Series Scripts Report').Replace('{{SUB}}', (HtmlEsc ($job.Folder + '  ·  ' + $job.Started))).Replace('{{NAV}}', $nav.ToString()).Replace('{{MAIN}}', $m.ToString())
$html = $html.Replace('{{FOOT}}', 'Generated by Advanced Tools &ndash; Series Scripts &nbsp;|&nbsp; TTW CAD standards &nbsp;|&nbsp; Working files: ' + (HtmlEsc $JobDir))
$report = Join-Path $job.Superseded 'Series Scripts Report.html'
try { New-Item -ItemType Directory -Force -Path $job.Superseded | Out-Null; [IO.File]::WriteAllText($report, $html, (New-Object System.Text.UTF8Encoding($false))) }
catch { $report = Join-Path $JobDir 'Series Scripts Report.html'; [IO.File]::WriteAllText($report, $html, (New-Object System.Text.UTF8Encoding($false))) }

$script:report = $report
$script:allDone = ($skipped -eq 0)
Write-Status
Remove-Item -LiteralPath $pidFile -ErrorAction SilentlyContinue
# Civil 3D's status-bar logo offers the report; open it here only if nothing is watching (Civil 3D closed)
$alive = Join-Path $JobDir 'tray.alive'
if (-not (Test-Path -LiteralPath $alive) -or ((Get-Date) - (Get-Item -LiteralPath $alive).LastWriteTime).TotalSeconds -gt 20) { Start-Process $report }
"""";

// =============================================================================
// Batch report - the same look as eTransmit, Xref Bind and the Drawing Conformance report. The runner fills in the {{...}} parts.
// =============================================================================
const string ReportTemplate = """
<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>
<title>{{TITLE}}</title>
<style>
:root{--water:#024360;--water-hover:#03587E;--alabaster:#F7F4E7;--steel:#1E1711;--land:#325442;--clay:#A84933;--skyline:#8ECAEF;
--surface:#FFFDF8;--hover:#EFEADB;--selection:#DDE8EC;--border:#DED7C4;--muted:#6F6A5D;
--serif:Konfus,Georgia,'Times New Roman',serif;--sans:'Arial Nova',Arial,'Helvetica Neue',sans-serif;}
*{box-sizing:border-box;}
html{scroll-behavior:smooth;}
body{font-family:var(--sans);margin:0;color:var(--steel);background:var(--surface);}
.band{background:var(--water);color:var(--alabaster);padding:26px 40px;display:flex;justify-content:space-between;align-items:center;gap:24px;}
.band h1{font-family:var(--serif);font-weight:400;font-size:34px;line-height:1.15;margin:0;letter-spacing:-0.01em;}
.band .sub{font-size:15px;color:var(--skyline);margin-top:6px;word-break:break-word;}
.brandmark{flex:0 0 auto;display:block;width:132px;height:auto;}
.shell{display:flex;align-items:flex-start;}
.side{flex:0 0 240px;position:sticky;top:0;height:100vh;overflow:auto;background:var(--alabaster);border-right:1px solid var(--border);padding:18px 0;}
.side .navlbl{font-size:10.5px;letter-spacing:0.08em;text-transform:uppercase;color:var(--muted);padding:0 20px 8px;}
.side .navlbl.gap{margin-top:18px;}
.side a{display:block;padding:10px 20px 10px 17px;border-left:3px solid transparent;color:var(--steel);text-decoration:none;font-size:13px;}
.side a.small{padding:6px 20px 6px 17px;font-size:12px;}
.side a:hover{background:var(--hover);}
.side a.on{background:var(--selection);border-left-color:var(--water);color:var(--water);font-weight:700;}
.side .count{float:right;color:var(--muted);font-weight:400;}
.side .count.bad{color:var(--clay);font-weight:700;}
.main{flex:1;min-width:0;padding:28px 40px 40px;}
.meta{color:var(--muted);margin-bottom:18px;font-size:12.5px;line-height:1.5;}
.note{background:#fff;border:1px solid var(--border);border-left:4px solid var(--skyline);border-radius:6px;padding:10px 16px;margin:0 0 16px;font-size:12.5px;line-height:1.5;}
.note.warnnote{border-left-color:var(--clay);}
h2{font-family:var(--serif);font-weight:400;font-size:24px;color:var(--water);margin:34px 0 8px;padding-bottom:6px;border-bottom:1px solid var(--border);scroll-margin-top:12px;}
h2:first-of-type{margin-top:8px;}
h3{font-family:var(--serif);font-weight:400;font-size:16px;color:var(--water);margin:22px 0 6px;scroll-margin-top:12px;}
.gradebanner{display:flex;align-items:center;gap:20px;background:#fff;border:1px solid var(--border);border-radius:6px;padding:16px 22px;margin-bottom:16px;border-left:8px solid #ccc;}
.gradebanner.ok{border-left-color:var(--land);} .gradebanner.fail{border-left-color:var(--clay);}
.gradescore{font-family:var(--serif);font-size:34px;color:var(--water);}
.graderesult{font-size:14px;font-weight:700;letter-spacing:0.02em;}
.gradebanner.ok .graderesult{color:var(--land);} .gradebanner.fail .graderesult{color:var(--clay);}
.legend{background:#fff;border:1px solid var(--border);border-radius:6px;padding:12px 18px;margin-bottom:16px;font-size:12.5px;line-height:1.5;color:#544d3f;}
.legend strong{color:var(--water);}
.summary{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:8px;}
.card{background:#fff;border:1px solid var(--border);border-top:3px solid var(--water);border-radius:6px;padding:12px 18px;min-width:130px;}
.card .num{font-family:var(--serif);font-size:28px;color:var(--water);} .card .lbl{font-size:11px;color:var(--muted);text-transform:uppercase;letter-spacing:0.03em;max-width:150px;}
table{border-collapse:collapse;width:100%;background:#fff;border:1px solid var(--border);margin-bottom:4px;}
th,td{border:1px solid #ece7d8;padding:8px 10px;font-size:12.5px;text-align:left;vertical-align:top;}
th{background:var(--water);color:var(--alabaster);font-weight:700;text-transform:uppercase;font-size:10.5px;letter-spacing:0.04em;position:sticky;top:0;}
tr.ok td.status{color:var(--land);font-weight:700;}
tr.fail td.status{color:var(--clay);font-weight:700;} tr.fail{background:rgba(168,73,51,.06);}
tr.warn td.status{color:var(--steel);font-weight:700;} tr.warn{background:rgba(30,23,17,.04);}
tr.arch td.status{color:var(--water);font-weight:700;} tr.arch{background:rgba(142,202,239,.16);}
td.status{white-space:nowrap;}
td a{color:var(--water);margin-right:10px;white-space:nowrap;}
.path{color:var(--muted);font-size:11.5px;word-break:break-all;}
ul.layerlist{margin:0;padding-left:16px;}
.foot{background:var(--alabaster);border-top:1px solid var(--border);color:var(--muted);font-size:12px;padding:12px 40px;}
@media (max-width:900px){.side{display:none;}.main{padding:20px;}.band{padding:20px;}}
@media print{.side,.foot{display:none;}.band{-webkit-print-color-adjust:exact;print-color-adjust:exact;}.main{padding:16px 0;}}
</style></head><body>
<div class='band'><div><h1>{{TITLE}}</h1><div class='sub'>{{SUB}}</div></div><img class='brandmark' src='data:image/png;base64,{{LOGO}}' alt='TTW'></div>
<div class='shell'><nav class='side'>{{NAV}}</nav><div class='main'>
{{MAIN}}
</div></div>
<div class='foot'>{{FOOT}}</div>
<script>var L=document.querySelectorAll('.side a'),S=[];L.forEach(function(a){var e=document.getElementById(a.getAttribute('href').slice(1));if(e)S.push([a,e]);});function spy(){var c=null;S.forEach(function(p){if(p[1].getBoundingClientRect().top<=140)c=p[0];});L.forEach(function(a){a.classList.toggle('on',a===c);});}window.addEventListener('scroll',spy);spy();</script>
</body></html>
""";

const string LogoB64 = "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";   // the TTW wordmark - the same image as Drawing Conformance

// =============================================================================
public class SeriesScript
{
    public int Series { get; set; }
    public string Name { get; set; }    // "Siteworks"
    public string File { get; set; }
    public string Kind { get; set; }    // csx, scr or lsp
}

public class TitleNumber
{
    public string Layout { get; set; }
    public string Block { get; set; }
    public string Tag { get; set; }
    public string Value { get; set; }   // as it reads in the title block
    public string Number { get; set; }  // the drawing number in it, or null
}
