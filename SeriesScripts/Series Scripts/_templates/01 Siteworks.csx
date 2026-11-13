// 01 Siteworks.csx - an example series script for 01xxx drawings (Siteworks).
// To use it, copy it up one folder (into "Series Scripts") and put your changes in below.
//
// Series Scripts runs it on each drawing in the series, in a background console, and then
// saves the drawing - so don't save here. Document / Database / Editor are that drawing,
// the same as a tool run from Advanced Tools.

using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;
using System;

string number = Environment.GetEnvironmentVariable("TTW_SS_NUMBER");        // "01000"
string series = Environment.GetEnvironmentVariable("TTW_SS_SERIES_NAME");   // "Siteworks"
Editor.WriteMessage("\n" + series + " script on " + number);

using (var tr = Database.TransactionManager.StartTransaction())
{
    // ---- your changes here, for example:
    // var lt = (LayerTable)tr.GetObject(Database.LayerTableId, OpenMode.ForRead);
    // if (!lt.Has("C-SITE")) { lt.UpgradeOpen(); var l = new LayerTableRecord { Name = "C-SITE" }; lt.Add(l); tr.AddNewlyCreatedDBObject(l, true); }
    tr.Commit();
}
return null;
