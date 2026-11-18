# SweptPath for Civil 3D 2026

Vehicle swept-path and turning analysis, in the same spirit as AutoTURN. You draw the path the front axle should follow. SweptPath drives a design vehicle along it and draws what the vehicle sweeps:

- **Tyre paths**: the outer edge of every tyre (layer `SWEPT-WHEELS`)
- **Body envelope**: the area the body covers, as one REGION (layer `SWEPT-ENVELOPE`)
- **Vehicle outlines** at the start, at the end and along the way (layer `SWEPT-VEHICLE`)
- **Problem spots**, circled in red where the path asks for more steering or articulation than the vehicle has (layer `SWEPT-WARN`)
- Optional **body corner traces** (layer `SWEPT-BODY`)

Everything one run creates goes into a group (`SWEPT_1`, `SWEPT_2`, …), so you can select or erase it as one.

It is a single AutoLISP file. Nothing has to be compiled, and it also works in plain AutoCAD 2021 and later.

## Quick start

1. Draw the path of the **centre of the front axle** with `PLINE`. Use arcs for the turns, or draw straight lines and `FILLET` them with a radius.
2. Type `SWEPT`. Pick the path **near the end the vehicle starts from**.
3. Read the report on the command line (example below; your numbers depend on the path). Then move the cursor along the path to "drive" the vehicle. Click to leave an outline wherever you want one, and press Enter to finish.

```
WB-67: path 312.4 FT
  Max steering angle:  26.9 deg  (limit 28.4)  OK
  Max articulation:    61.2 deg  (limit 70.0)  OK
  Max off-tracking:    18.7 FT (last axle from path)
```

## Commands

| Type      | What it does |
|-----------|--------------|
| `SWEPT`   | Pick a path and run the swept path. Options: `Vehicle` (pick another vehicle) and `Settings` |
| `SWDRIVE` | Drive the last swept path again with the cursor and leave more outlines |
| `SWVEH`   | Vehicle library: `List`, `Select`, `Add` (your own vehicle), `Delete` |
| `SWSET`   | Turn the envelope, tyre paths, corner traces and drive mode on or off. Also sets the outline spacing and drawing units |
| `SWHELP`  | Show the command list |

A path can be a line, arc, polyline, 3D polyline, spline, or Civil 3D alignment.

## Vehicles

| Name     | Vehicle                         | Units |
|----------|---------------------------------|-------|
| P        | Passenger car, 19 ft            | ft |
| SU-30    | Single-unit truck, 30 ft        | ft |
| SU-40    | Single-unit truck, 39.5 ft      | ft |
| BUS-40   | Intercity bus, 40 ft            | ft |
| WB-40    | Intermediate semitrailer, 45.5 ft | ft |
| WB-62    | Interstate semitrailer, 69 ft   | ft |
| WB-67    | Interstate semitrailer, 73.5 ft | ft |
| CAR-M    | Generic car, 4.6 m              | m |
| TRUCK-M  | Generic rigid truck, 8.8 m      | m |
| SEMI-M   | Generic semitrailer, 16.5 m     | m |

The AASHTO vehicles use the dimensions as commonly published. The `-M` vehicles are generic metric examples. **Check every vehicle against the design standard your project uses** before you rely on the results. To add your own, type `SWVEH` → `Add`.

Vehicle sizes are converted to the drawing's units (`INSUNITS`) automatically, so you can use a metric vehicle in a feet drawing. If the drawing has no units set, SweptPath asks you once, or you can set the units in `SWSET`.

When you add a vehicle, SweptPath asks for:
- **Wheelbase** of each unit. For a truck this is front axle to rear axle. For a trailer it is hitch to trailer axle. Use the centre of a tandem group.
- **Front / rear overhang**, **body width** and **width over tyres** (outside edge to outside edge).
- **Minimum turning radius at the centre of the front axle**. This sets the steering limit, where sin(limit) = wheelbase / radius.
- For trailers, **where the hitch is** relative to the axle in front of it (0 = fifth wheel over the axle, negative = drawbar behind it), and the **maximum articulation angle**.

Up to three trailers are supported. Custom vehicles are saved to `%APPDATA%\SweptPath\vehicles.lsp`.

## How it works

The front axle follows the path exactly. Each rear axle and trailer is pulled along by the point in front of it, using the exact tractrix solution for each small step. Off-tracking is therefore right for any path shape, not just circular turns. Tested against the closed-form steady-turn result, the error is under 0.001 ft for a WB-67.

The steering angle at each point is the angle between the front axle's direction of travel and the truck's heading. Where it goes over the vehicle's limit, the vehicle cannot follow the path. The usual cause is a sharp polyline corner or an arc that is too tight.

Limits to keep in mind:
- **Forward driving only.** Reversing manoeuvres are not modelled.
- **Steering is instant.** The front wheels turn as fast as the path asks, with no lock-to-lock time or speed. Keep transitions realistic, or treat sudden curvature changes with care.
- The body is modelled as a rectangle per unit, and tandem groups as a single axle. Tyre scrub is not modelled.
- The envelope REGION can be `EXPLODE`d into lines, or used with `HATCH` → *Select objects* and `AREA`.

## Install

**Option A: installer (easiest)**
1. Unzip `SweptPath.zip`.
2. Double-click `Install.bat`.
3. Restart Civil 3D. If a security prompt about loading SweptPath.lsp appears, choose **Always Load**.

To remove it, double-click `Uninstall.bat`.

**Option B: by hand**: copy the `SweptPath.bundle` folder to `%APPDATA%\Autodesk\ApplicationPlugins\` and restart Civil 3D. Or type `APPLOAD` and load `SweptPath.bundle\Contents\SweptPath.lsp`.

When it loads, the command line shows `SweptPath 1.0 loaded: ...`.
