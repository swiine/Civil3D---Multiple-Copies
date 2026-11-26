# MultiClip 2.1 for AutoCAD / Civil 3D

Nine numbered clipboards in place of one. Copy several things in one drawing, switch to the other drawing once, and paste them all.

```
Old way:  Ctrl+C → switch → Ctrl+V → switch back → Ctrl+C → switch → Ctrl+V → ...
MultiClip: C1, C2, C3 → switch once → P1, P2, P3
```

It is a single standalone AutoLISP file, `MultiClip.lsp`. Nothing has to be compiled. It works in Civil 3D and plain AutoCAD 2021 and later.

## Commands

| Type        | What it does                                                         |
|-------------|----------------------------------------------------------------------|
| `C1` … `C9` | Copy the selection into slot 1–9 (works with objects already selected) |
| `P1` … `P9` | Paste slot 1–9. Asks `[Original/Select]`: original coordinates, or pick a point. Enter repeats your last choice |
| `PO1` … `PO9` | Paste slot 1–9 at its **original coordinates**                     |
| `MC`        | Copy, then **press a number key** to choose the slot (Enter = normal Ctrl+C) |
| `MCB`       | Same as `MC`, but you pick the base point                            |
| `MV`        | Paste, then **press a number key** (Enter = normal Ctrl+V)           |
| `MVO`       | Paste to original coordinates, then press a number key               |
| `MCLIST`    | Show what is in each slot                                            |
| `MCCLEAR`   | Empty all slots                                                      |
| `MCDIAG`    | Run a self-test and save a report (troubleshooting)                  |

For example, type `C2` and press Enter to copy into slot 2. Later, type `P2` and press Enter to paste it.

Each slot is saved as a small DWG in `%LOCALAPPDATA%\C3D-MultiClip`. Slots work in any open drawing and are still there after you restart Civil 3D. Copying into a slot that is already full replaces what was in it.

## Install

1. Unzip `MultiClip.zip` (or download `MultiClip.lsp`) anywhere you like.
2. Type `APPLOAD`, browse to `MultiClip.lsp` and click **Load**. If a security prompt appears, choose **Always Load**.
3. To load it every session, click **Contents…** under *Startup Suite* and add the file there.

When it loads, the command line shows `MultiClip 2.1 loaded: ...`.

Upgrading from 1.x: version 1.x was installed as `MultiClip.bundle`. Delete `%APPDATA%\Autodesk\ApplicationPlugins\MultiClip.bundle` so only one copy loads. Slots you already filled keep working.

## Making it "Ctrl+C then 1"

AutoCAD shortcut keys can't be two-step chords. You can get the same feel by assigning a key to `MC` / `MV`, because those commands wait for one number key press:

1. Type `CUI` and press Enter.
2. In the **Command List** pane (bottom-left), click **Create a new command**.
   - Name: `MultiClip Copy`, Macro: `^C^C_MC`
3. Create another one: Name `MultiClip Paste`, Macro: `^C^C_MV`
4. Drag each new command into **Keyboard Shortcuts → Shortcut Keys** in the tree at the top-left.
5. Select each one, click the **Key(s)** box on the right, and press the key you want, for example:
   - `Ctrl+Shift+1` style keys, or a spare key you like, **or**
   - **Ctrl+C / Ctrl+V themselves.** First remove those keys from the built-in *Copy Clip* and *Paste* shortcuts.
6. Click **OK**.

If you take over Ctrl+C / Ctrl+V:
- **Ctrl+C, 3** copies to slot 3, and **Ctrl+V, 3** pastes slot 3.
- **Ctrl+C, Enter** (or Space, or right-click) is still the normal Windows copy, and **Ctrl+V, Enter** is the normal paste.

## Notes

- **Elevations are kept.** The drag point is the lower-left corner of what you copied, at Z = 0, so a paste moves objects only in X/Y. Surfaces, feature lines and 3D polylines keep their Z values.
- The paste is a single undo step. Press Esc while dragging and the paste is cancelled.
- Civil 3D objects are copied the same way the built-in clipboard copies them, by writing them out to a DWG. Styles come with them. If the target drawing already has a style with the same name, the target drawing's version is used, just like a normal paste.
- If the source and target drawings use different units (`INSUNITS`, e.g. feet vs. meters), AutoCAD scales the paste, the same as an insert does.

## Troubleshooting

**"Slot 1 is empty"**: the paste could not find a saved slot 1.
- Copy with `C1`, not Ctrl+C. Ctrl+C only fills the normal Windows clipboard.
- After `C1` the command line should say `Copied N object(s) to slot 1.` If it says something else, the copy failed.
- Type `MCLIST` to see every slot and the folder they're saved in.

**`MCDIAG`** checks each step MultiClip relies on:
1. Whether the slot folder exists and can be written to.
2. What is saved in each slot.
3. A test copy, using both export methods.
4. A test paste, which is undone automatically afterwards.

It prints the results, saves them to `MultiClip_Diag.txt` in the slot folder, and opens that file in Notepad so you can share it.
