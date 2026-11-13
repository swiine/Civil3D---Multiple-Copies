;;; ==========================================================================
;;;  PerfCheck.lsp  -  Performance check / benchmark for Civil 3D 2026
;;; --------------------------------------------------------------------------
;;;  Finds out why Civil 3D is slow or stuttering. It checks:
;;;    - the PC: RAM, CPU, graphics card + driver age, power plan / battery,
;;;      disk space, HDD vs SSD, Windows uptime
;;;    - what else is using the CPU / RAM right now (antivirus, OneDrive...)
;;;    - the Civil 3D process: memory, handles, how long it has been running
;;;    - the drawing: object counts, COGO points, surfaces, labels, proxies,
;;;      REGAPP / scale-list / DGN bloat, xrefs, file size, far-away objects
;;;    - file locations: drawing / autosave / temp on network or cloud sync
;;;    - settings that are known to cause lag (ROLLOVERTIPS, SAVETIME, ...)
;;;    - benchmarks: CPU, database, REGEN, zoom, disk latency, and a
;;;      "stutter test" that detects background interruptions
;;;  and writes a report with the problems found, most important first.
;;;
;;;  COMMANDS
;;;    PERFCHECK   Full check + benchmarks (about 30-60 s). Opens a report.
;;;    PERFQUICK   Quick "right now" check (about 5-10 s), no heavy tests.
;;;    PERFWATCH   Time every command while you work; warns about slow ones.
;;;    PERFLOG     Show the slowest commands recorded by PERFWATCH.
;;;    PERFFIX     Apply the recommended settings (backs up the old values).
;;;    PERFUNDO    Put back the settings PERFFIX changed.
;;;    PERFHELP    List the commands.
;;;
;;;  Reports and history are saved in %LOCALAPPDATA%\C3D-PerfCheck
;;; ==========================================================================

(vl-load-com)

(setq *pc-version* "1.1")
(setq *pc-dir* nil)

;;; ---------------------------------------------------------------- basics

(defun pc:ms () (getvar "MILLISECS"))
(defun pc:since (t0 / d) (setq d (- (getvar "MILLISECS") t0)) (if (< d 0) 0 d))
(defun pc:try (f args / r)
  (setq r (vl-catch-all-apply f args))
  (if (vl-catch-all-error-p r) nil r)
)
(defun pc:sv (name) (pc:try 'getvar (list name)))
(defun pc:acad () (vlax-get-acad-object))
(defun pc:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))
(defun pc:n (x) (if (numberp x) x 0))
(defun pc:now (fmt) (menucmd (strcat "M=$(edtime,$(getvar,date)," fmt ")")))

(defun pc:dir (/ dir)
  (if (not *pc-dir*)
    (foreach root (list (getenv "LOCALAPPDATA") (getenv "TEMP") (getvar "TEMPPREFIX")
                        (getvar "MYDOCUMENTSPREFIX"))
      (if (and (not *pc-dir*) (= (type root) 'STR) (/= root ""))
        (progn
          (setq dir (strcat (vl-string-right-trim "\\/" root) "\\C3D-PerfCheck"))
          (if (not (vl-file-directory-p dir)) (vl-mkdir dir))
          (if (vl-file-directory-p dir) (setq *pc-dir* dir))
        )
      )
    )
  )
  (if *pc-dir* *pc-dir* (vl-string-right-trim "\\/" (getvar "TEMPPREFIX")))
)

;; Date/time stamp safe for file names, e.g. 20261113-142530 (no DIESEL)
(defun pc:stamp (/ s)
  (setq s (rtos (getvar "CDATE") 2 6))
  (while (< (strlen s) 15) (setq s (strcat s "0")))
  (strcat (substr s 1 8) "-" (substr s 10 6))
)

;; Any value (number, string, variant) -> real. Unknown -> 0.0
(defun pc:num (v)
  (cond
    ((numberp v) (float v))
    ((= (type v) 'STR) (atof v))
    ((= (type v) 'VARIANT) (pc:num (vlax-variant-value v)))
    ((= v :vlax-true) 1.0)
    (T 0.0)
  )
)

;; Any value -> string
(defun pc:str (v)
  (cond
    ((null v) "")
    ((= (type v) 'STR) (vl-string-trim " \t" v))
    ((= (type v) 'VARIANT) (pc:str (vlax-variant-value v)))
    ((= (type v) 'REAL) (rtos v 2 2))
    (T (vl-princ-to-string v))
  )
)

(defun pc:r (x d) (rtos (pc:num x) 2 d))
(defun pc:gb (bytes) (strcat (rtos (/ (pc:num bytes) 1073741824.0) 2 1) " GB"))
(defun pc:sec (ms) (strcat (rtos (/ (pc:num ms) 1000.0) 2 1) " s"))
(defun pc:pad (s n)
  (if (null s) (setq s ""))
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s
)
(defun pc:lpad (s n)
  (while (< (strlen s) n) (setq s (strcat " " s)))
  s
)

(defun pc:split (s d / p r)
  (while (setq p (vl-string-search d s))
    (setq r (cons (substr s 1 p) r)
          s (substr s (+ p 1 (strlen d))))
  )
  (reverse (cons s r))
)

(defun pc:wrap (s w / lines cur)
  (setq cur "")
  (foreach wd (pc:split s " ")
    (cond
      ((= cur "") (setq cur wd))
      ((> (+ (strlen cur) 1 (strlen wd)) w) (setq lines (cons cur lines) cur wd))
      (T (setq cur (strcat cur " " wd)))
    )
  )
  (reverse (cons cur lines))
)

;; Julian day number for a calendar date
(defun pc:jday (y m d / a yy mm)
  (setq a  (/ (- 14 m) 12)
        yy (- (+ y 4800) a)
        mm (- (+ m (* 12 a)) 3))
  (+ d (/ (+ (* 153 mm) 2) 5) (* 365 yy) (/ yy 4) (- (/ yy 100)) (/ yy 400) -32045)
)

;; WMI date "YYYYMMDDhhmmss.ffffff+zzz" -> Julian date (like the DATE sysvar)
(defun pc:wmidate (s)
  (setq s (pc:str s))
  (if (and (>= (strlen s) 14) (wcmatch (substr s 1 8) "########"))
    (+ (pc:jday (atoi (substr s 1 4)) (atoi (substr s 5 2)) (atoi (substr s 7 2)))
       (/ (+ (* 3600 (atoi (substr s 9 2))) (* 60 (atoi (substr s 11 2))) (atoi (substr s 13 2)))
          86400.0))
  )
)

;;; ---------------------------------------------------------------- report

(defun pc:out (s) (setq *pc-lines* (cons s *pc-lines*)))
(defun pc:kv (k v) (pc:out (strcat "  " (pc:pad k 28) v)))
(defun pc:progress (s) (princ (strcat "\n  " s "...")) (princ))
(defun pc:section (s)
  (pc:out "")
  (pc:out (strcat "=== " s " "
                  (substr "=========================================================================="
                          1 (max 3 (- 70 (strlen s))))))
)

;; sev: 3 = HIGH, 2 = MEDIUM, 1 = LOW
(defun pc:issue (sev title fix)
  (setq *pc-issues* (cons (list sev title fix) *pc-issues*))
)
(defun pc:sevname (s) (cond ((>= s 3) "HIGH") ((= s 2) "MED ") (T "LOW ")))

(defun pc:setres (k v) (setq *pc-res* (cons (cons k v) *pc-res*)))
(defun pc:res (k / v)
  (setq v (cdr (assoc k *pc-res*)))
  (if (numberp v) (rtos v 2 0) "")
)

;;; ------------------------------------------------------------ error / env

(defun pc:err (msg)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
    (princ (strcat "\nPerfCheck error: " msg))
  )
  (if *pc-temp*
    (progn
      (foreach e *pc-temp* (if (entget e) (entdel e)))
      (setq *pc-temp* nil)
    )
  )
  (if *pc-vt* (progn (setvar "VTENABLE" *pc-vt*) (setq *pc-vt* nil)))
  (if *pc-clayer* (progn (setvar "CLAYER" *pc-clayer*) (setq *pc-clayer* nil)))
  (pc:finish)
)

(defun pc:begin ()
  (setq *pc-olderr*  *error*
        *error*      pc:err
        *pc-cmdecho* (getvar "CMDECHO")
        *pc-temp*    nil
        *pc-vt*      nil
        *pc-clayer*  nil)
  (setvar "CMDECHO" 0)
)

(defun pc:finish ()
  (if *pc-cmdecho* (setvar "CMDECHO" *pc-cmdecho*))
  (setq *error* *pc-olderr* *pc-olderr* nil *pc-cmdecho* nil)
  (princ)
)

;;; ------------------------------------------------------------------- WMI

(defun pc:wmi (ns / loc svc)
  (if (setq loc (pc:try 'vlax-create-object '("WbemScripting.SWbemLocator")))
    (progn
      (setq svc (pc:try 'vlax-invoke-method (list loc 'ConnectServer "." ns)))
      (pc:try 'vlax-release-object (list loc))
      svc
    )
  )
)

(defun pc:get (obj prop / sym)
  (setq sym (if (= (type prop) 'STR) (read prop) prop))
  (or (pc:try 'vlax-get (list obj sym))
      (pc:try 'vlax-get-property (list obj sym)))
)

;; Run a WQL query. Returns a list of rows; each row is (("Prop" . value) ...)
(defun pc:query (svc wql props / rows coll)
  (setq rows nil)
  (if (and svc (setq coll (pc:try 'vlax-invoke-method (list svc 'ExecQuery wql))))
    (progn
      (vl-catch-all-apply
        '(lambda ()
           (vlax-for item coll
             (setq rows (cons (mapcar '(lambda (p) (cons p (pc:get item p))) props) rows))
           )
         )
      )
      (pc:try 'vlax-release-object (list coll))
    )
  )
  (reverse rows)
)

(defun pc:f (row key) (cdr (assoc key row)))

;;; ============================================================ SYSTEM CHECKS

(defun pc:sys-os (svc / r total free pct boot up)
  (pc:section "WINDOWS / HARDWARE")
  (setq r (car (pc:query svc
                 "SELECT Caption,Version,TotalVisibleMemorySize,FreePhysicalMemory,LastBootUpTime FROM Win32_OperatingSystem"
                 '("Caption" "Version" "TotalVisibleMemorySize" "FreePhysicalMemory" "LastBootUpTime"))))
  (if r
    (progn
      (pc:kv "Windows" (strcat (pc:str (pc:f r "Caption")) "  (" (pc:str (pc:f r "Version")) ")"))
      (setq total (/ (pc:num (pc:f r "TotalVisibleMemorySize")) 1048576.0)
            free  (/ (pc:num (pc:f r "FreePhysicalMemory")) 1048576.0)
            *pc-ram-gb* total)
      (if (> total 0)
        (progn
          (setq pct (* 100.0 (/ free total)))
          (pc:kv "RAM" (strcat (rtos total 2 1) " GB total, " (rtos free 2 1) " GB free ("
                               (rtos pct 2 0) "%)"))
          (cond
            ((< total 9)
             (pc:issue 3 (strcat "Only " (rtos total 2 0) " GB of RAM")
               "Civil 3D needs 16 GB minimum and 32 GB is recommended. With this little RAM Windows swaps to disk constantly, which feels exactly like stuttering. Close other programs and keep only one or two drawings open; a RAM upgrade is the real fix."))
            ((< total 15)
             (pc:issue 2 (strcat "Only " (rtos total 2 0) " GB of RAM")
               "Civil 3D works with 16 GB but 32 GB is recommended for surfaces, corridors and xrefs. Keep fewer drawings open and close browsers while you work."))
            ((< total 30)
             (pc:issue 1 (strcat (rtos total 2 0) " GB of RAM")
               "OK for most work, but large surfaces / corridors / many xrefs will want 32 GB or more."))
          )
          (cond
            ((< pct 10)
             (pc:issue 3 (strcat "Almost out of free RAM (" (rtos free 2 1) " GB free)")
               "Windows is paging memory to disk, which causes freezes of several seconds. Close other programs and drawings you don't need, or restart Civil 3D. See the BACKGROUND section for what is using the memory."))
            ((< pct 20)
             (pc:issue 2 (strcat "Free RAM is low (" (rtos free 2 1) " GB free)")
               "Close programs and drawings you don't need. When free RAM runs out Windows pages to disk and Civil 3D freezes."))
          )
        )
      )
      (if (setq boot (pc:wmidate (pc:f r "LastBootUpTime")))
        (progn
          (setq up (- (getvar "DATE") boot))
          (pc:kv "Windows running for" (strcat (rtos up 2 1) " days since last restart"))
          (if (> up 14)
            (pc:issue 1 (strcat "Windows has not been restarted for " (rtos up 2 0) " days")
              "Restart Windows (a real Restart, not Shut down - Fast Startup keeps the old session). Long uptimes build up memory leaks in drivers and leave updates half-installed."))
        )
      )
    )
    (pc:kv "Windows" "(could not read)")
  )
)

(defun pc:sys-cpu (svc / r name mhz cores logical load p perf)
  (setq r (car (pc:query svc
                 "SELECT Name,NumberOfCores,NumberOfLogicalProcessors,MaxClockSpeed,LoadPercentage FROM Win32_Processor"
                 '("Name" "NumberOfCores" "NumberOfLogicalProcessors" "MaxClockSpeed" "LoadPercentage"))))
  (if r
    (progn
      (setq name    (pc:str (pc:f r "Name"))
            cores   (fix (pc:num (pc:f r "NumberOfCores")))
            logical (fix (pc:num (pc:f r "NumberOfLogicalProcessors")))
            mhz     (pc:num (pc:f r "MaxClockSpeed"))
            *pc-logical* (max 1 logical))
      (pc:kv "CPU" name)
      (pc:kv "  Cores / threads / base" (strcat (itoa cores) " cores, " (itoa logical) " threads, "
                                            (rtos (/ mhz 1000.0) 2 2) " GHz base clock"))
      (pc:out "  (Civil 3D does most work on ONE core, so clock speed matters more than core count.)")
    )
  )
  ;; Actual speed vs. rated speed (over 100% = turbo, well under = throttled)
  (setq p (car (pc:query svc
                 "SELECT Name,PercentProcessorPerformance FROM Win32_PerfFormattedData_Counters_ProcessorInformation WHERE Name = '_Total'"
                 '("Name" "PercentProcessorPerformance"))))
  (if (and p (> (setq perf (pc:num (pc:f p "PercentProcessorPerformance"))) 0))
    (progn
      (pc:kv "  Running at" (strcat (rtos perf 2 0) "% of base clock right now"))
      (setq *pc-cpuperf* perf)
    )
  )
)

(defun pc:sys-gpu (svc / rows name un ram drvdate age hres vres integ discrete basic remote)
  (setq rows (pc:query svc
               "SELECT Name,AdapterRAM,DriverVersion,DriverDate,CurrentHorizontalResolution,CurrentVerticalResolution,CurrentRefreshRate FROM Win32_VideoController"
               '("Name" "AdapterRAM" "DriverVersion" "DriverDate" "CurrentHorizontalResolution"
                 "CurrentVerticalResolution" "CurrentRefreshRate")))
  (setq hres 0)
  (foreach r rows
    (setq name (pc:str (pc:f r "Name"))
          un   (strcase name)
          ram  (pc:num (pc:f r "AdapterRAM")))
    (if (< ram 0) (setq ram (+ ram 4294967296.0)))
    (pc:kv "Graphics card" name)
    (pc:kv "  Video memory / driver"
           (strcat (if (> ram 0)
                     (strcat (rtos (/ ram 1073741824.0) 2 1) " GB" (if (>= ram 4.29e9) "+ (Windows reports max 4)" ""))
                     "?")
                   ", driver " (pc:str (pc:f r "DriverVersion"))))
    (if (> (pc:num (pc:f r "CurrentHorizontalResolution")) 0)
      (progn
        (pc:kv "  Screen"
               (strcat (pc:r (pc:f r "CurrentHorizontalResolution") 0) " x "
                       (pc:r (pc:f r "CurrentVerticalResolution") 0) " @ "
                       (pc:r (pc:f r "CurrentRefreshRate") 0) " Hz"))
        (setq hres (max hres (pc:num (pc:f r "CurrentHorizontalResolution"))))
      )
    )
    (cond
      ((wcmatch un "*BASIC DISPLAY*,*BASIC RENDER*") (setq basic name))
      ((wcmatch un "*REMOTE*,*VMWARE*,*VIRTUAL*,*CITRIX*,*HYPER-V*,*PARSEC*,*DISPLAYLINK*,*SPACEDESK*")
       (setq remote name))
      ((wcmatch un "*NVIDIA*,*QUADRO*,*GEFORCE*,*RTX*,*GTX*,*RADEON RX*,*RADEON PRO*,*FIREPRO*,*ARC(TM) A#*,*ARC A#*")
       (setq discrete name))
      ((wcmatch un "*INTEL*,*RADEON(TM) GRAPHICS*,*RADEON GRAPHICS*,*VEGA*,*ADRENO*")
       (setq integ name))
    )
    ;; Driver age
    (if (setq drvdate (pc:wmidate (pc:f r "DriverDate")))
      (progn
        (setq age (/ (- (getvar "DATE") drvdate) 365.25))
        (pc:kv "  Driver date" (strcat (substr (pc:str (pc:f r "DriverDate")) 1 4) "-"
                                       (substr (pc:str (pc:f r "DriverDate")) 5 2) "  ("
                                       (rtos age 2 1) " years old)"))
        (if (and (> age 1.0) (not (wcmatch un "*BASIC*,*REMOTE*,*VIRTUAL*")))
          (pc:issue (if (> age 2.0) 2 1)
            (strcat "Graphics driver is " (rtos age 2 1) " years old (" name ")")
            "Old graphics drivers are a top cause of display stutter in AutoCAD-based products. Install the current driver from NVIDIA (Studio or RTX Enterprise driver), AMD (Pro driver) or Intel - not just Windows Update. Then run GRAPHICSCONFIG and make sure Hardware Acceleration is ON."))
      )
    )
  )
  (cond
    (basic
     (pc:issue 3 "No real graphics driver installed (Microsoft Basic Display Adapter)"
       "Windows is drawing the screen in software. Civil 3D will stutter on everything. Install the driver for your graphics card from the manufacturer's website."))
    ((and integ discrete)
     (pc:issue (if *pc-laptop* 2 1) "This PC has two graphics chips (integrated + dedicated)"
       (strcat "Laptops often run Civil 3D on the weak integrated chip (" integ ") instead of " discrete
               ". Fix: Windows Settings > System > Display > Graphics > add C:\\Program Files\\Autodesk\\AutoCAD 2026\\acad.exe > Options > High performance. In NVIDIA Control Panel > Manage 3D settings > Program settings, pick acad.exe and set 'High-performance NVIDIA processor' and Power management 'Prefer maximum performance'. Restart Civil 3D.")))
    ((and integ (not discrete))
     (pc:issue (if (>= hres 3800) 2 1) (strcat "Only an integrated graphics chip (" integ ")")
       (strcat "Integrated graphics share system RAM and are slow with big Civil 3D drawings"
               (if (>= hres 3800) " - especially at 4K resolution" "")
               ". Use lighter surface/point styles, freeze layers you don't need, and turn off LINESMOOTHING. A dedicated (NVIDIA/AMD) card is the real fix.")))
  )
  (if remote
    (pc:issue 1 (strcat "Virtual / remote display adapter found (" remote ")")
      "If you are working through Remote Desktop, a VM, Citrix or a DisplayLink USB dock, the screen is compressed and sent over the connection, so pans and zooms stutter. Compare by working directly on the machine / plugging the monitor into the graphics card."))
  (if (wcmatch (strcase (pc:str (getenv "SESSIONNAME"))) "RDP*")
    (pc:issue 2 "You are running Civil 3D over Remote Desktop"
      "Remote Desktop renders graphics with limited or no GPU acceleration, so stutter while panning/zooming is expected. Use the PC locally, or a remote tool with GPU support (e.g. Parsec, HP Anyware, NICE DCV, Citrix HDX 3D)."))
)

(defun pc:sys-power (svc / psvc r plan bat st chg)
  (setq psvc (pc:wmi "root\\cimv2\\power"))
  (if (setq r (car (pc:query psvc "SELECT ElementName FROM Win32_PowerPlan WHERE IsActive = TRUE" '("ElementName"))))
    (progn
      (setq plan (pc:str (pc:f r "ElementName")))
      (pc:kv "Power plan" plan)
      (cond
        ((wcmatch (strcase plan) "*SAVER*,*ECO*,*QUIET*,*SILENT*")
         (pc:issue 3 (strcat "Windows power plan is '" plan "'")
           "Power saving slows the CPU and GPU a lot. Control Panel > Power Options > choose High performance (or Balanced + Windows Settings > System > Power > Power mode = Best performance)."))
        ((wcmatch (strcase plan) "*BALANCED*")
         (pc:issue 1 "Windows power plan is 'Balanced'"
           "Balanced lets the CPU slow down between clicks, which can make Civil 3D feel sluggish. Set Windows Settings > System > Power > Power mode = Best performance (on laptops, while plugged in)."))
      )
    )
  )
  (if psvc (pc:try 'vlax-release-object (list psvc)))
  ;; Battery
  (if (setq r (car (pc:query svc "SELECT BatteryStatus,EstimatedChargeRemaining FROM Win32_Battery"
                             '("BatteryStatus" "EstimatedChargeRemaining"))))
    (progn
      (setq st  (fix (pc:num (pc:f r "BatteryStatus")))
            chg (pc:r (pc:f r "EstimatedChargeRemaining") 0)
            *pc-laptop* T)
      (pc:kv "Battery" (strcat chg "%, " (if (= st 1) "RUNNING ON BATTERY" "plugged in")))
      (if (= st 1)
        (pc:issue 3 "Laptop is running on battery"
          "On battery Windows and the graphics driver throttle the CPU and GPU heavily. Plug in the charger (and use the full-power charger that came with the laptop - a weak USB-C charger also causes throttling)."))
    )
  )
)

(defun pc:sys-disk (svc / want rows id free size pct ssvc mt bt nm)
  (setq want (list (strcase (pc:str (getenv "SystemDrive")))
                   (strcase (substr (getvar "TEMPPREFIX") 1 2))
                   (strcase (substr (getvar "DWGPREFIX") 1 2))))
  (setq *pc-netdrives* nil)
  (setq rows (pc:query svc
               "SELECT DeviceID,FreeSpace,Size,DriveType,ProviderName FROM Win32_LogicalDisk WHERE DriveType = 3 OR DriveType = 4"
               '("DeviceID" "FreeSpace" "Size" "DriveType" "ProviderName")))
  (setq *pc-netmap* nil)
  (foreach r rows
    (setq id   (strcase (pc:str (pc:f r "DeviceID")))
          free (/ (pc:num (pc:f r "FreeSpace")) 1073741824.0)
          size (/ (pc:num (pc:f r "Size")) 1073741824.0))
    (if (= 4 (fix (pc:num (pc:f r "DriveType"))))
      (setq *pc-netdrives* (cons id *pc-netdrives*)
            *pc-netmap* (cons (cons id (pc:str (pc:f r "ProviderName"))) *pc-netmap*))
      (progn
        (setq pct (if (> size 0) (* 100.0 (/ free size)) 100.0))
        (pc:kv (strcat "Drive " id) (strcat (rtos free 2 0) " GB free of " (rtos size 2 0) " GB ("
                                         (rtos pct 2 0) "%)"))
        (if (member id want)
          (cond
            ((< free 10)
             (pc:issue 3 (strcat "Drive " id " is almost full (" (rtos free 2 1) " GB free)")
               "Civil 3D writes temp files, autosaves and .bak files here; Windows also needs space for its page file. Free up at least 20-30 GB (Disk Cleanup, empty %TEMP%, move old projects)."))
            ((< pct 10)
             (pc:issue 2 (strcat "Drive " id " is over 90% full")
               "Nearly-full drives get slower and the page file can't grow. Free up some space."))
          )
        )
      )
    )
  )
  (if *pc-netdrives*
    (pc:kv "Network drives" (apply 'strcat (mapcar '(lambda (x) (strcat x " ")) *pc-netdrives*))))
  ;; SSD or spinning disk?
  (setq ssvc (pc:wmi "root\\Microsoft\\Windows\\Storage"))
  (foreach r (pc:query ssvc "SELECT FriendlyName,MediaType,BusType FROM MSFT_PhysicalDisk"
                       '("FriendlyName" "MediaType" "BusType"))
    (setq mt (fix (pc:num (pc:f r "MediaType")))
          bt (fix (pc:num (pc:f r "BusType")))
          nm (pc:str (pc:f r "FriendlyName")))
    (pc:kv "Physical disk"
           (strcat nm "  (" (cond ((= mt 3) "HDD - spinning disk")
                                  ((= mt 4) (if (= bt 17) "NVMe SSD" "SSD"))
                                  ((= mt 5) "SCM")
                                  (T "type not reported"))
                   (if (= bt 7) ", USB" "") ")"))
    (if (= mt 3)
      (pc:issue 1 (strcat "A spinning hard disk is installed (" nm ")")
        "If Windows, TEMP, Civil 3D or your project files are on this disk, opening, saving and xref loading will be slow. Keep them on the SSD."))
  )
  (if ssvc (pc:try 'vlax-release-object (list ssvc)))
)

(defun pc:proc-tip (nm / u)
  (setq u (strcase nm))
  (cond
    ((wcmatch u "MSMPENG*,MSSENSE*,NISSRV*,*DEFENDER*,CSFALCON*,SENTINEL*,CYLANCE*,MCSHIELD*,AVP*,EKRN*,BDSERVICEHOST*,CCSVCHST*,SAVSERVICE*")
     "Antivirus is scanning. It scans every DWG, BAK, SV$ and xref Civil 3D opens or saves. Ask IT to exclude your project folders (or *.dwg, *.sv$, *.dwl and acad.exe) from real-time scanning.")
    ((wcmatch u "ONEDRIVE*,DROPBOX*,GOOGLEDRIVEFS*,*DESKTOPCONNECTOR*,BOX,BOXSYNC*,SYNCTHING*,EGNYTE*")
     "A cloud-sync app is busy. It re-reads every file Civil 3D saves (DWG, BAK, autosave). Pause syncing while you work or keep active drawings outside the synced folder.")
    ((wcmatch u "SEARCHINDEXER*,SEARCHPROTOCOLHOST*,SEARCHFILTERHOST*")
     "Windows Search is indexing files. Exclude your project folders: Settings > Privacy & security > Searching Windows > Excluded folders.")
    ((wcmatch u "CHROME*,MSEDGE*,FIREFOX*,OPERA*,BRAVE*")
     "Web browser. Close heavy tabs (video, web apps, maps) while working in Civil 3D.")
    ((wcmatch u "TEAMS*,MS-TEAMS*,ZOOM*,SLACK*,WEBEX*")
     "Chat / meeting app. Video calls and screen sharing use a lot of CPU and GPU.")
    ((wcmatch u "TIWORKER*,TRUSTEDINSTALLER*,MOUSOCOREWORKER*,WUAUCLT*,USOCLIENT*,MSIEXEC*")
     "Windows Update / an installer is running in the background. Let it finish, then restart Windows.")
    ((wcmatch u "ADSK*,AUTODESK*,ADP*,ADSKACCESS*,GENUINESERVICE*,ADODIS*")
     "Autodesk background service (licensing, Autodesk Access updates, analytics). If it stays busy, restart Windows.")
    ((wcmatch u "SYSTEM,INTERRUPTS,DPC*,REGISTRY")
     "Windows kernel / drivers. Constant high usage usually means a driver problem: update chipset, graphics, network and storage drivers.")
    ((wcmatch u "DWM")
     "Desktop Window Manager (draws the screen). High use points at the graphics driver, display scaling or many monitors.")
    ((wcmatch u "VMMEM*")
     "A virtual machine or WSL is running. Shut it down when you don't need it (wsl --shutdown).")
    ((wcmatch u "ACCORECONSOLE*,ACAD*")
     "Another AutoCAD / Civil 3D process.")
    (T "Close it if you don't need it while working in Civil 3D.")
  )
)

(defun pc:sys-procs (svc / rows n ws hrs jd q agg nm base p cpu mem a total lst k)
  ;; ---- acad.exe itself
  (pc:section "CIVIL 3D PROCESS (acad.exe)")
  (setq rows (pc:query svc
               "SELECT ProcessId,WorkingSetSize,HandleCount,ThreadCount,CreationDate FROM Win32_Process WHERE Name = 'acad.exe'"
               '("ProcessId" "WorkingSetSize" "HandleCount" "ThreadCount" "CreationDate")))
  (setq n 0)
  (foreach r rows
    (if (= n 0) (setq *pc-acadpid* (pc:r (pc:f r "ProcessId") 0)))
    (setq n   (1+ n)
          ws  (/ (pc:num (pc:f r "WorkingSetSize")) 1073741824.0)
          jd  (pc:wmidate (pc:f r "CreationDate"))
          hrs (if jd (* 24.0 (- (getvar "DATE") jd)) 0.0))
    (pc:kv (strcat "acad.exe (PID " (pc:r (pc:f r "ProcessId") 0) ")")
           (strcat (rtos ws 2 1) " GB RAM, " (pc:r (pc:f r "HandleCount") 0) " handles, "
                   (pc:r (pc:f r "ThreadCount") 0) " threads, running " (rtos hrs 2 1) " h"))
    (if (and *pc-ram-gb* (> *pc-ram-gb* 0) (> (/ ws *pc-ram-gb*) 0.6))
      (pc:issue 2 (strcat "Civil 3D is using " (rtos ws 2 1) " GB = "
                          (rtos (* 100 (/ ws *pc-ram-gb*)) 2 0) "% of all RAM")
        "Close drawings you are not using, unload xrefs you don't need, PURGE, and restart Civil 3D. Memory used by closed drawings is not always given back until you restart."))
    (if (> hrs 12)
      (pc:issue 1 (strcat "Civil 3D has been running for " (rtos hrs 2 0) " hours")
        "Long sessions slowly get slower (memory fragmentation, undo history, cached xrefs). Save and restart Civil 3D at least once a day."))
    (if (> (pc:num (pc:f r "HandleCount")) 40000)
      (pc:issue 1 (strcat "Civil 3D has " (pc:r (pc:f r "HandleCount") 0) " open handles")
        "A very high handle count usually means a leak in a plugin or long session. Restart Civil 3D; if it climbs again quickly, try without third-party add-ins."))
  )
  (if (> n 1)
    (pc:issue 1 (strcat (itoa n) " copies of AutoCAD / Civil 3D are running")
      "Each copy uses its own RAM and CPU. Close the ones you don't need (check Task Manager for hidden ones left over after a crash)."))
  (if (= n 0) (pc:out "  (could not read acad.exe process info)"))

  ;; ---- everything else
  (pc:section "BACKGROUND - WHAT ELSE IS USING THE PC RIGHT NOW")
  (setq q "SELECT Name,PercentProcessorTime,WorkingSetPrivate FROM Win32_PerfFormattedData_PerfProc_Process")
  (pc:query svc q '("Name"))          ; first read only primes the counters
  (command "_.DELAY" 1500)
  (setq rows (pc:query svc q '("Name" "PercentProcessorTime" "WorkingSetPrivate")))
  (setq agg nil total 0.0)
  (foreach r rows
    (setq nm (pc:str (pc:f r "Name")))
    (if (not (member (strcase nm) '("_TOTAL" "IDLE")))
      (progn
        (setq base (if (setq p (vl-string-search "#" nm)) (substr nm 1 p) nm)
              cpu  (/ (pc:num (pc:f r "PercentProcessorTime")) (float (max 1 (pc:n *pc-logical*))))
              mem  (pc:num (pc:f r "WorkingSetPrivate"))
              total (+ total cpu))
        (if (setq a (assoc base agg))
          (setq agg (subst (list base (+ (cadr a) cpu) (+ (caddr a) mem)) a agg))
          (setq agg (cons (list base cpu mem) agg))
        )
      )
    )
  )
  (setq *pc-procnames* (mapcar '(lambda (x) (strcase (car x))) agg))
  (if (null agg)
    (pc:out "  (could not read process counters)")
    (progn
      (pc:kv "Total CPU in use" (strcat (rtos total 2 0) "% (measured over 1.5 s while Civil 3D was idle)"))
      (pc:out (strcat "  " (pc:pad "Top CPU users" 34) (pc:lpad "CPU %" 7) (pc:lpad "RAM" 10)))
      (setq lst (vl-sort agg '(lambda (a b) (> (cadr a) (cadr b)))) k 0)
      (foreach x lst
        (if (< k 8)
          (pc:out (strcat "    " (pc:pad (car x) 32) (pc:lpad (rtos (cadr x) 2 1) 7)
                          (pc:lpad (pc:gb (caddr x)) 10))))
        (setq k (1+ k))
      )
      (pc:out (strcat "  " (pc:pad "Top RAM users" 34)))
      (setq k 0)
      (foreach x (vl-sort agg '(lambda (a b) (> (caddr a) (caddr b))))
        (if (< k 6)
          (pc:out (strcat "    " (pc:pad (car x) 32) (pc:lpad (rtos (cadr x) 2 1) 7)
                          (pc:lpad (pc:gb (caddr x)) 10))))
        (setq k (1+ k))
      )
      (cond
        ((>= total 70)
         (pc:issue 3 (strcat "The PC is " (rtos total 2 0) "% busy before Civil 3D does anything")
           "Something else is using most of the CPU, so Civil 3D has to wait. See the BACKGROUND section of the report for which programs."))
        ((>= total 35)
         (pc:issue 2 (strcat "The PC is " (rtos total 2 0) "% busy while Civil 3D is idle")
           "Background programs are competing with Civil 3D. See the BACKGROUND section of the report."))
      )
      (foreach x lst
        (if (and (>= (cadr x) 12) (not (wcmatch (strcase (car x)) "ACAD,WMIPRVSE")))
          (pc:issue (if (>= (cadr x) 35) 3 2)
                    (strcat (car x) " is using " (rtos (cadr x) 2 0) "% of the CPU")
                    (pc:proc-tip (car x)))))
      (foreach x agg
        (if (and (>= (caddr x) 3.2e9) (not (wcmatch (strcase (car x)) "ACAD,MEMORY COMPRESSION")))
          (pc:issue 1 (strcat (car x) " is using " (pc:gb (caddr x)) " of RAM")
                    (pc:proc-tip (car x)))))
    )
  )
)

;;; =========================================================== DRAWING CHECKS

(defun pc:count (flt / ss)
  (if (setq ss (if flt (ssget "_X" flt) (ssget "_X"))) (sslength ss) 0)
)
(defun pc:cnt (k) (pc:n (cdr (assoc k *pc-cnt*))))

(defun pc:tblcount (tbl / n r)
  (setq n 0 r (tblnext tbl T))
  (while r (setq n (1+ n) r (tblnext tbl)))
  n
)

;; Number of entries in a named-object dictionary
(defun pc:dictcount (name / d)
  (if (setq d (dictsearch (namedobjdict) name))
    (length (vl-remove-if-not '(lambda (x) (= (car x) 3)) d))
    0
  )
)

(setq *pc-surface-types* "AECC_TIN_SURFACE,AECC_GRID_SURFACE,AECC_TIN_VOLUME_SURFACE,AECC_GRID_VOLUME_SURFACE")

(defun pc:civil-counts ()
  (setq *pc-cnt*
    (list
      (cons "civil"        (pc:count '((0 . "AECC_*"))))
      (cons "points"       (pc:count '((0 . "AECC_COGO_POINT"))))
      (cons "surfaces"     (pc:count (list (cons 0 *pc-surface-types*))))
      (cons "corridors"    (pc:count '((0 . "AECC_CORRIDOR"))))
      (cons "alignments"   (pc:count '((0 . "AECC_ALIGNMENT"))))
      (cons "featurelines" (pc:count '((0 . "AECC_FEATURE_LINE"))))
      (cons "pipes"        (pc:count '((0 . "AECC_PIPE,AECC_STRUCTURE,AECC_PRESSURE*"))))
      (cons "profileviews" (pc:count '((0 . "AECC_PROFILE_VIEW"))))
      (cons "sectionviews" (pc:count '((0 . "AECC_SECTION_VIEW*"))))
      (cons "labels"       (pc:count '((0 . "AECC_*LABEL*"))))
      (cons "proxy"        (pc:count '((0 . "ACAD_PROXY_ENTITY"))))
      (cons "images"       (pc:count '((0 . "IMAGE,OLE2FRAME"))))
      (cons "underlays"    (pc:count '((0 . "PDFUNDERLAY,DWFUNDERLAY,DGNUNDERLAY"))))
      (cons "pointclouds"  (pc:count '((0 . "ACDBPOINTCLOUD*,POINTCLOUD*"))))
      (cons "hatches"      (pc:count '((0 . "HATCH"))))
      (cons "blockrefs"    (pc:count '((0 . "INSERT"))))
      (cons "text"         (pc:count '((0 . "TEXT,MTEXT"))))
    )
  )
)

(defun pc:check-drawing (/ fn sz ss n lim i tp a tally k o nm st pts ood regapps layers r f
                           blocks xrefs xunres scales dgn layouts mn mx span ndocs)
  (pc:section "CIVIL 3D SESSION")
  (pc:kv "Product" (strcat (pc:str (getvar "PRODUCT")) "  " (pc:str (getvar "_VERNUM"))
                           "  (ACADVER " (getvar "ACADVER") ")"))
  (setq ndocs (pc:n (pc:try 'vla-get-Count (list (vla-get-Documents (pc:acad))))))
  (pc:kv "Open drawings" (itoa ndocs))
  (pc:kv "Profile" (pc:str (getvar "CPROFILE")))
  (if (> ndocs 5)
    (pc:issue 1 (strcat (itoa ndocs) " drawings are open")
      "Every open drawing uses memory (and Civil 3D keeps their data references live). Close the ones you are not working in."))

  (pc:section "DRAWING")
  (setq fn (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (pc:kv "Drawing" fn)
  (setq *pc-dwgmb* nil)
  (if (and (= (getvar "DWGTITLED") 1) (setq sz (vl-file-size fn)))
    (progn
      (setq *pc-dwgmb* (/ sz 1048576.0))
      (pc:kv "File size (last save)" (strcat (rtos *pc-dwgmb* 2 1) " MB"))
      (cond
        ((> *pc-dwgmb* 300)
         (pc:issue 3 (strcat "Drawing file is very large (" (rtos *pc-dwgmb* 2 0) " MB)")
           "Every save/autosave writes all of it, and every REGEN redraws it. Split the model: keep the surface, corridor and pipe networks in their own drawings and use data shortcuts / xrefs in the sheet drawings. Also check the bloat items below (REGAPPs, DGN data, scale list) and run PURGE + AUDIT."))
        ((> *pc-dwgmb* 120)
         (pc:issue 2 (strcat "Drawing file is large (" (rtos *pc-dwgmb* 2 0) " MB)")
           "Large files mean slow saves/autosaves (a freeze every SAVETIME minutes) and slow regens. Consider data shortcuts to split surfaces/corridors into their own drawings; PURGE and AUDIT."))
      )
    )
    (pc:kv "File size" "(drawing not saved yet)")
  )

  ;; ---- objects
  (pc:progress "Counting objects")
  (setq ss (ssget "_X")
        n  (if ss (sslength ss) 0)
        *pc-ents* n)
  (pc:kv "Objects (all layouts)" (itoa n))
  (setq i 0 tally nil lim (min n 150000))
  (while (< i lim)
    (setq tp (cdr (assoc 0 (entget (ssname ss i)))))
    (if (setq a (assoc tp tally))
      (setq tally (subst (cons tp (1+ (cdr a))) a tally))
      (setq tally (cons (cons tp 1) tally))
    )
    (setq i (1+ i))
  )
  (setq ss nil)
  (if tally
    (progn
      (pc:out (strcat "  Most common object types" (if (> n lim) (strcat " (first " (itoa lim) " counted)") "") ":"))
      (setq k 0)
      (foreach x (vl-sort tally '(lambda (a b) (> (cdr a) (cdr b))))
        (if (< k 12) (pc:out (strcat "    " (pc:pad (car x) 34) (pc:lpad (itoa (cdr x)) 9))))
        (setq k (1+ k))
      )
    )
  )
  (pc:civil-counts)
  (pc:out (strcat "  Civil 3D objects: " (itoa (pc:cnt "civil"))
                  "  | COGO points " (itoa (pc:cnt "points"))
                  "  | surfaces " (itoa (pc:cnt "surfaces"))
                  "  | corridors " (itoa (pc:cnt "corridors"))
                  "  | alignments " (itoa (pc:cnt "alignments"))))
  (pc:out (strcat "                    feature lines " (itoa (pc:cnt "featurelines"))
                  "  | pipes/structures " (itoa (pc:cnt "pipes"))
                  "  | profile views " (itoa (pc:cnt "profileviews"))
                  "  | section views " (itoa (pc:cnt "sectionviews"))
                  "  | labels " (itoa (pc:cnt "labels"))))
  (pc:out (strcat "  Other: blocks " (itoa (pc:cnt "blockrefs")) "  | text " (itoa (pc:cnt "text"))
                  "  | hatches " (itoa (pc:cnt "hatches")) "  | images/OLE " (itoa (pc:cnt "images"))
                  "  | PDF/DWF/DGN underlays " (itoa (pc:cnt "underlays"))
                  "  | point clouds " (itoa (pc:cnt "pointclouds"))
                  "  | proxy objects " (itoa (pc:cnt "proxy"))))

  (cond
    ((> n 500000)
     (pc:issue 2 (strcat "Very many objects in this drawing (" (itoa n) ")")
       "Everything has to be drawn and checked on every regen. Split the work into several drawings with xrefs / data shortcuts, and freeze layers you don't need."))
    ((> n 200000)
     (pc:issue 1 (strcat "Many objects in this drawing (" (itoa n) ")")
       "Freeze layers you don't need while working, and consider moving parts of the design into xrefs / data shortcuts."))
  )
  (if (> (pc:cnt "points") 20000)
    (pc:issue 2 (strcat (itoa (pc:cnt "points")) " COGO points in the drawing")
      "Lots of points (and their labels) are slow to draw and to select. Use a point group with a 'none' point style / no label style for points you don't need to see, or keep survey points in a separate drawing and data-reference the surface."))
  (if (> (pc:cnt "labels") 10000)
    (pc:issue 2 (strcat (itoa (pc:cnt "labels")) " Civil 3D labels in the drawing")
      "Civil 3D labels are recalculated when the objects they label change and redrawn on every regen. Delete labels you don't need, or move annotation to sheet drawings."))
  (if (> (pc:cnt "corridors") 0)
    (pc:issue 1 (strcat (itoa (pc:cnt "corridors")) " corridor(s) in the drawing")
      "Corridors rebuild whenever an alignment, profile, assembly, target or surface they use changes. While editing, turn OFF 'Rebuild - Automatic' (right-click the corridor in Prospector) and rebuild by hand. Large frequency/region counts and corridor surfaces with many points also slow everything down."))
  (if (> (pc:cnt "proxy") 0)
    (pc:issue 2 (strcat (itoa (pc:cnt "proxy")) " proxy objects (from apps that are not installed)")
      "Proxy objects come from add-ins / verticals that are not loaded (e.g. Plant 3D, MEP, third-party). They can't be displayed efficiently. Install the matching Object Enabler, or explode/remove them (e.g. -EXPORTTOAUTOCAD on a copy)."))
  (if (> (pc:cnt "pointclouds") 0)
    (pc:issue 2 (strcat (itoa (pc:cnt "pointclouds")) " point cloud(s) attached")
      "Point clouds are very heavy for the graphics card. Lower the density (POINTCLOUDDENSITY / POINTCLOUD2DVSDISPLAY), crop them, or unload them when not needed."))
  (if (> (+ (pc:cnt "images") (pc:cnt "underlays")) 0)
    (pc:issue 1 (strcat (itoa (+ (pc:cnt "images") (pc:cnt "underlays"))) " raster image(s) / OLE / PDF underlays")
      "Large aerial images and PDF underlays slow down pans and zooms. Unload them when you don't need them, clip them, and use compressed/tiled images (ECW, MrSID) instead of huge TIFFs. PDF underlays: turn off PDFOSNAP / set PDFSHX 0 to keep them light."))
  (if (> (pc:cnt "hatches") 10000)
    (pc:issue 1 (strcat (itoa (pc:cnt "hatches")) " hatches")
      "Lots of hatches (especially dense patterns) are slow to draw. FILLMODE 0 hides them while you work."))

  ;; ---- surfaces in detail
  (if (setq ss (ssget "_X" (list (cons 0 *pc-surface-types*))))
    (progn
      (setq i 0)
      (repeat (min 30 (sslength ss))
        (setq o   (vlax-ename->vla-object (ssname ss i))
              i   (1+ i)
              nm  (pc:str (pc:try 'vlax-get-property (list o 'Name)))
              st  (pc:try 'vlax-get-property (list o 'Statistics)))
        (if (= (type st) 'VARIANT) (setq st (vlax-variant-value st)))
        (setq pts (if (= (type st) 'VLA-OBJECT)
                    (pc:num (pc:try 'vlax-get-property (list st 'NumberOfPoints)))
                    0.0)
              ood (pc:try 'vlax-get-property (list o 'OutOfDate)))
        (pc:kv (strcat "  Surface " (itoa i))
               (strcat (if (= nm "") "(name not readable)" nm)
                       (if (> pts 0) (strcat ", " (rtos pts 2 0) " points") "")
                       (if (= ood :vlax-true) "  [OUT OF DATE]" "")))
        (if (> pts 1000000)
          (pc:issue 2 (strcat "Surface '" nm "' has " (rtos (/ pts 1e6) 2 1) " million points")
            "Huge surfaces are slow to display, rebuild and snap to. Use a surface style that shows only the border (or contours at a large interval) while you work, simplify it (Edit Surface > Simplify Surface), or keep it in its own drawing and data-reference it. Turn off 'Rebuild - Automatic'."))
        (if (= ood :vlax-true)
          (pc:issue 1 (strcat "Surface '" nm "' is out of date")
            "Rebuild it once (right-click > Rebuild). If it keeps going out of date while you work, something it depends on keeps changing."))
      )
      (setq ss nil)
    )
  )

  ;; ---- tables / dictionaries (drawing bloat)
  (pc:progress "Checking drawing bloat")
  (setq regapps (pc:tblcount "APPID")
        layers  (pc:tblcount "LAYER")
        r (tblnext "BLOCK" T) blocks 0 xrefs 0 xunres 0 *pc-xpaths* nil)
  (while r
    (setq blocks (1+ blocks) f (cdr (assoc 70 r)))
    (if (= 4 (logand f 4))
      (progn
        (setq xrefs (1+ xrefs) *pc-xpaths* (cons (pc:str (cdr (assoc 1 r))) *pc-xpaths*))
        (if (= 0 (logand f 32)) (setq xunres (1+ xunres)))
      )
    )
    (setq r (tblnext "BLOCK"))
  )
  (setq scales  (pc:dictcount "ACAD_SCALELIST")
        dgn     (pc:dictcount "ACAD_DGNLINESTYLECOMP")
        layouts (max 0 (1- (pc:dictcount "ACAD_LAYOUT")))
        *pc-regapps* regapps
        *pc-scales* scales
        *pc-xrefs* xrefs)
  (pc:kv "Layers" (itoa layers))
  (pc:kv "Block definitions" (itoa blocks))
  (pc:kv "Xrefs" (strcat (itoa xrefs) (if (> xunres 0) (strcat "  (" (itoa xunres) " not loaded / not found)") "")))
  (pc:kv "Layouts" (itoa layouts))
  (pc:kv "Registered apps (REGAPP)" (itoa regapps))
  (pc:kv "Annotation scales" (itoa scales))
  (pc:kv "DGN linestyle data" (itoa dgn))
  (pc:check-drawing-more)

  (cond
    ((> regapps 2000)
     (pc:issue 3 (strcat (itoa regapps) " registered applications (REGAPP bloat)")
       "This junk (usually inherited from old xrefs/blocks) slows opening, saving, copy/paste and xref loading. Fix: -PURGE > Regapps > * (PERFFIX can do it). Purge it from your xrefs and blocks too or it comes back."))
    ((> regapps 500)
     (pc:issue 2 (strcat (itoa regapps) " registered applications (REGAPP bloat)")
       "Fix: -PURGE > Regapps > * (PERFFIX can do it). Civil 3D needs a few hundred; thousands is bloat."))
  )
  (if (> scales 100)
    (pc:issue 2 (strcat (itoa scales) " annotation scales (scale list bloat)")
      "Scale lists copied in from other drawings slow down copy/paste, xrefs and the scale menus. Fix: SCALELISTEDIT > Reset (PERFFIX can do it)."))
  (if (> dgn 0)
    (pc:issue (if (> dgn 1000) 3 2) (strcat "DGN linestyle data found (" (itoa dgn) " entries)")
      "Leftover data from imported MicroStation (DGN) files - a classic cause of huge, slow drawings and slow copy/paste. Fix: PURGE > tick 'Purge orphaned data' > Purge All, then save. Clean xrefs/blocks that came from DGN too."))
  (if (> xunres 0)
    (pc:issue 2 (strcat (itoa xunres) " xref(s) not loaded or not found")
      "Missing xrefs make Civil 3D search for the files on open and on reload. Fix the paths or detach the ones you don't need (XREF palette)."))
  (if (> xrefs 25)
    (pc:issue 1 (strcat (itoa xrefs) " xrefs attached")
      "Every xref is loaded and drawn. Unload or detach the ones you don't need, use XCLIP, and keep XLOADCTL = 2."))
  (if (> layers 3000)
    (pc:issue 1 (strcat (itoa layers) " layers")
      "Thousands of layers slow down the layer palette and layer changes. PURGE unused layers; consider xref layer filters."))
  (if (> blocks 5000)
    (pc:issue 1 (strcat (itoa blocks) " block definitions")
      "PURGE unused blocks."))
  (if (> layouts 40)
    (pc:issue 1 (strcat (itoa layouts) " layouts")
      "Many layouts (each with viewports) slow down opening, saving and layout switching. Use Sheet Set Manager / separate sheet drawings."))

  ;; ---- extents
  (setq mn (getvar "EXTMIN") mx (getvar "EXTMAX"))
  (if (and (< (abs (car mn)) 1e19) (< (abs (car mx)) 1e19))
    (progn
      (setq span (distance (list (car mn) (cadr mn)) (list (car mx) (cadr mx))))
      (pc:kv "Drawing extents span" (rtos span 2 0))
      (if (> span 1e7)
        (pc:issue 1 (strcat "Objects are spread over a huge area (" (rtos span 2 0) " units)")
          "Usually a stray object far from the project (or something at 0,0 in a state-plane drawing). It makes ZOOM EXTENTS and regens slower. ZOOM > Extents, find the far-away object and delete or move it."))
    )
  )
)

(defun pc:pathkind (p / u)
  (setq u (strcase (pc:str p)))
  (cond
    ((= u "") nil)
    ((wcmatch u "*ONEDRIVE*,*DROPBOX*,*GOOGLE DRIVE*,*GOOGLEDRIVE*,*\\BOX\\*,*ACCDOCS*,*AUTODESK DOCS*,*SHAREPOINT*,*ICLOUD*,*EGNYTE*")
     "cloud-synced folder")
    ((wcmatch u "\\\\*") "network share")
    ((member (substr u 1 2) *pc-netdrives*) "mapped network drive")
  )
)

(defun pc:check-paths (/ p k n)
  (pc:section "FILE LOCATIONS")
  (foreach pair (list (list "Drawing folder" (if (= 1 (getvar "DWGTITLED")) (getvar "DWGPREFIX") ""))
                      (list "Autosave folder" (getvar "SAVEFILEPATH"))
                      (list "Temp folder" (getvar "TEMPPREFIX")))
    (setq p (pc:str (cadr pair))
          k (pc:pathkind p))
    (pc:kv (car pair) (strcat (if (= p "") "(drawing not saved yet)" p) (if k (strcat "   <-- " k) "")))
    (if k
      (if (= (car pair) "Drawing folder")
        (if (wcmatch k "cloud*")
          (pc:issue 3 "This drawing is in a cloud-synced folder (OneDrive / Dropbox / Docs ...)"
            "The sync app reads, uploads and sometimes locks the DWG, .bak and .dwl files every time Civil 3D saves or autosaves. That causes freezes after every save and 'file in use' errors. Pause syncing while you work, or work on a local copy. With Autodesk Docs, keep Desktop Connector updated and avoid opening drawings while it is still syncing.")
          (pc:issue 2 (strcat "This drawing is opened from a " k)
            "Every save, autosave and xref / data-shortcut load waits on the network. Compare the disk test numbers for the drawing folder vs. the temp folder below; if the network is slow (Wi-Fi and VPN are the usual culprits), use a wired connection or work on a local copy.")
        )
        (pc:issue 2 (strcat (car pair) " is on a " k ": " p)
          "Point it to a local SSD folder: OPTIONS > Files > 'Automatic Save File Location' / 'Temporary Drawing File Location'. Restart Civil 3D afterwards.")
      )
    )
  )
  (setq n 0)
  (foreach x *pc-xpaths* (if (pc:pathkind x) (setq n (1+ n))))
  (if (> n 0)
    (progn
      (pc:kv "Xrefs on network / cloud" (itoa n))
      (pc:issue 1 (strcat (itoa n) " xref(s) are on a network or cloud location")
        "Xrefs are read on open and on every reload. Keep XLOADCTL = 2 (works from a local copy) and unload xrefs you don't need.")
    )
  )
)

;;; ============================================================ NETWORK

(defun pc:speed (bps)
  (if (>= bps 1e9)
    (strcat (rtos (/ bps 1e9) 2 1) " Gbps")
    (strcat (rtos (/ bps 1e6) 2 0) " Mbps"))
)

;; UNC path (\\server\share\...) behind a drawing path, or nil if local
(defun pc:unc-of (path / p)
  (setq path (pc:str path))
  (cond
    ((wcmatch path "\\\\*") path)
    ((setq p (assoc (strcase (substr path 1 2)) *pc-netmap*)) (cdr p))
  )
)

(defun pc:host (unc / s p)
  (setq s (substr unc 3))
  (if (setq p (vl-string-search "\\" s)) (substr s 1 p) s)
)

(defun pc:ping (svc host count / r times)
  (setq host (vl-string-trim " " (vl-string-translate "'\"" "  " host)))
  (repeat count
    (setq r (car (pc:query svc (strcat "SELECT StatusCode,ResponseTime FROM Win32_PingStatus WHERE Address='" host "'")
                           '("StatusCode" "ResponseTime"))))
    (if (and r (pc:f r "StatusCode") (= 0 (fix (pc:num (pc:f r "StatusCode")))))
      (setq times (cons (pc:num (pc:f r "ResponseTime")) times)))
  )
  times
)

(defun pc:sys-network (svc full / rows nm u spd wifi wired vpn unc host times avg mx)
  (pc:section "NETWORK")
  (setq rows (pc:query svc
               "SELECT Name,NetConnectionID,Speed FROM Win32_NetworkAdapter WHERE NetConnectionStatus = 2"
               '("Name" "NetConnectionID" "Speed")))
  (foreach r rows
    (setq nm  (strcat (pc:str (pc:f r "NetConnectionID")) " - " (pc:str (pc:f r "Name")))
          u   (strcase nm)
          spd (pc:num (pc:f r "Speed")))
    (cond
      ((wcmatch u "*HYPER-V*,*VETHERNET*,*VMWARE*,*VIRTUALBOX*,*BLUETOOTH*,*LOOPBACK*,*WSL*") nil)
      ((wcmatch u "*VPN*,*ANYCONNECT*,*FORTI*,*GLOBALPROTECT*,*PANGP*,*WIREGUARD*,*TAP-WINDOWS*,*ZSCALER*,*SONICWALL*,*PULSE SECURE*,*IVANTI*,*OPENVPN*,*NORDLYNX*,*TAILSCALE*,*ZEROTIER*")
       (setq vpn (cons nm vpn)))
      ((wcmatch u "*WI-FI*,*WIFI*,*WIRELESS*,*WLAN*,*802?11*")
       (setq wifi (cons (list nm spd) wifi)))
      (T (setq wired (cons (list nm spd) wired)))
    )
    (pc:kv "Connected adapter" (strcat nm (if (> spd 0) (strcat "  (" (pc:speed spd) ")") "")))
  )
  (if (null rows) (pc:out "  (no connected network adapters reported)"))
  (foreach w wired
    (if (and (> (cadr w) 0) (< (cadr w) 9e8))
      (pc:issue (if *pc-dwg-unc* 2 1) (strcat "Wired network link is only " (pc:speed (cadr w)) " (" (car w) ")")
        "A normal office link is 1 Gbps or more. 100 Mbps usually means a damaged/old cable, an old switch or wall port, or network-adapter power saving. Try another cable/port; in Device Manager > network adapter > Power Management, untick 'Allow the computer to turn off this device'.")))
  ;; drawing on a file server?
  (if (setq unc *pc-dwg-unc*)
    (progn
      (setq host (pc:host unc))
      (pc:kv "Drawing file server" (strcat host "   (" unc ")"))
      (setq times (pc:ping svc host (if full 4 2)))
      (if times
        (progn
          (setq avg (/ (apply '+ times) (float (length times)))
                mx  (apply 'max times))
          (pc:kv "Ping to file server" (strcat (rtos avg 2 0) " ms average, " (rtos mx 2 0) " ms worst  (good office LAN: under 2 ms)"))
          (cond
            ((> avg 40)
             (pc:issue 3 (strcat "The file server is far away: " (rtos avg 2 0) " ms ping")
               "That is internet / VPN / other-office distance. Every save, autosave, xref and data-shortcut read takes many round trips, so it crawls. Work on a local copy of the project and copy it back, or ask IT about a WAN-friendly setup (Autodesk Docs, Panzura, Egnyte, a local server)."))
            ((> avg 8)
             (pc:issue 2 (strcat "Network delay to the file server is " (rtos avg 2 0) " ms")
               "A healthy office network is under 2 ms. Higher values point to Wi-Fi, VPN, a remote office or an overloaded network. Use a cable, avoid VPN, or work on a local copy."))
          )
        )
        (pc:kv "Ping to file server" "no answer (ping may be blocked - that's OK)")
      )
      (if (and wifi (null wired))
        (pc:issue 3 "The drawing is on the network and this PC is on Wi-Fi"
          "Wi-Fi adds delay and drops packets, and every save / autosave / xref load waits on it. Plug in an ethernet cable (and turn Wi-Fi off so Windows really uses the cable), or work on a local copy."))
      (if vpn
        (pc:issue 2 (strcat "VPN is connected (" (car vpn) ") and the drawing is on the network")
          "Over VPN every file operation crosses the internet and the VPN encryption. Copy the project to your PC and work locally, then copy it back."))
    )
  )
)

;;; ========================================================= SYSTEM EXTRAS

(defun pc:sys-extra (svc / r rows dpi sc names rb n cu)
  (if (setq r (car (pc:query svc "SELECT Manufacturer,Model FROM Win32_ComputerSystem" '("Manufacturer" "Model"))))
    (pc:kv "Computer" (strcat (pc:str (pc:f r "Manufacturer")) " " (pc:str (pc:f r "Model")))))
  ;; page file
  (setq rows (pc:query svc "SELECT Name,AllocatedBaseSize,CurrentUsage,PeakUsage FROM Win32_PageFileUsage"
                       '("Name" "AllocatedBaseSize" "CurrentUsage" "PeakUsage"))
        cu 0.0)
  (if rows
    (foreach r rows
      (setq cu (+ cu (pc:num (pc:f r "CurrentUsage"))))
      (pc:kv "Page file" (strcat (pc:str (pc:f r "Name")) ": " (rtos (/ (pc:num (pc:f r "AllocatedBaseSize")) 1024.0) 2 1)
                                 " GB, in use " (rtos (/ (pc:num (pc:f r "CurrentUsage")) 1024.0) 2 1)
                                 " GB, peak " (rtos (/ (pc:num (pc:f r "PeakUsage")) 1024.0) 2 1) " GB"))
    )
    (if (and *pc-ram-gb* (< *pc-ram-gb* 60))
      (progn
        (pc:kv "Page file" "NONE")
        (pc:issue 2 "Windows has no page file"
          "When RAM runs out Civil 3D will freeze or crash instead of just slowing down. Turn it back on: Settings > System > About > Advanced system settings > Performance Settings > Advanced > Virtual memory > 'Automatically manage paging file size'.")))
  )
  (if (> cu 3072)
    (pc:issue 1 (strcat (rtos (/ cu 1024.0) 2 1) " GB of memory is swapped out to the page file")
      "RAM ran short at some point, so Windows moved memory to disk. Switching back to those programs (or to Civil 3D) stutters while it is read back. Close what you don't need; more RAM is the real fix."))
  ;; display scaling
  (if (numberp (setq dpi (pc:try 'vl-registry-read '("HKEY_CURRENT_USER\\Control Panel\\Desktop\\WindowMetrics" "AppliedDPI"))))
    (pc:kv "Display scaling" (strcat (rtos (* 100.0 (/ dpi 96.0)) 2 0) "%")))
  ;; antivirus products
  (setq sc (pc:wmi "root\\SecurityCenter2"))
  (setq names (mapcar '(lambda (r) (pc:str (pc:f r "displayName")))
                      (pc:query sc "SELECT displayName FROM AntiVirusProduct" '("displayName"))))
  (if sc (pc:try 'vlax-release-object (list sc)))
  (if names
    (pc:kv "Antivirus" (apply 'strcat (mapcar '(lambda (x) (strcat x "  ")) names))))
  (if (> (length names) 1)
    (pc:issue 2 (strcat (itoa (length names)) " antivirus products are registered")
      "Two real-time scanners scan every file twice and can fight each other. Keep one (usually the company one) and uninstall / disable the other."))
  ;; pending restart
  (setq rb (append (mapcar 'strcase (pc:try 'vl-registry-descendents
                                      '("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\WindowsUpdate\\Auto Update")))
                   (mapcar 'strcase (pc:try 'vl-registry-descendents
                                      '("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Component Based Servicing")))))
  (if (or (member "REBOOTREQUIRED" rb) (member "REBOOTPENDING" rb))
    (progn
      (pc:kv "Pending restart" "YES - Windows is waiting to finish updates")
      (pc:issue 1 "Windows is waiting for a restart to finish updates"
        "Until you restart, update services keep working in the background. Save your work and restart Windows.")))
  ;; startup programs
  (setq n (length (pc:query svc "SELECT Name FROM Win32_StartupCommand" '("Name"))))
  (pc:kv "Programs starting with Windows" (itoa n))
  (if (> n 20)
    (pc:issue 1 (strcat (itoa n) " programs start with Windows")
      "Many of them keep running in the background. Task Manager > Startup apps: disable the ones you don't need."))
)

;;; ========================================================= ADD-INS / PLUGINS

(defun pc:adsk-path-p (u)
  (or (and (wcmatch u "*\\AUTODESK\\*,*AUTODESK SHARED*") (not (wcmatch u "*APPLICATIONPLUGINS*")))
      (wcmatch u "*APPLICATIONPLUGINS\\AUTODESK*,*APPLICATIONPLUGINS\\ADSK*"))
)

(defun pc:system-path-p (u / root)
  (setq root (strcase (pc:str (getenv "SystemRoot"))))
  (or (and (/= root "") (= (substr u 1 (1+ (strlen root))) (strcat root "\\")))
      (wcmatch u "*\\DOTNET\\*,*\\MICROSOFT SHARED\\*,*\\WINDOWSAPPS\\*,*\\MICROSOFT\\EDGEWEBVIEW\\*,*\\MICROSOFT OFFICE\\*,*\\MICROSOFT\\EDGE*"))
)

(defun pc:module-tip (u)
  (cond
    ((wcmatch u "*GAMEOVERLAY*,*DISCORD*,*RTSSHOOKS*,*RIVATUNER*,*NVSPCAP*,*NVIDIA SHARE*,*GRAPHICS-HOOK*,*OBS-STUDIO*,*OVERWOLF*,*FRAPS*,*AFTERBURNER*,*XSPLIT*,*BANDICAM*,*MEDAL*")
     (list 2 "a screen overlay / recorder (Discord, Steam, NVIDIA overlay, RivaTuner, OBS...) hooks into Civil 3D's graphics"
             "Overlays intercept every frame Civil 3D draws and are a known cause of display stutter. Turn the overlay off (or exclude acad.exe in that app), e.g. NVIDIA app > Settings > In-game overlay OFF; Discord > Settings > Game Overlay OFF."))
    ((wcmatch u "*CROWDSTRIKE*,*SENTINEL*,*CYLANCE*,*SYMANTEC*,*MCAFEE*,*TREND MICRO*,*SOPHOS*,*KASPERSKY*,*BITDEFENDER*,*ESET*,*CARBON BLACK*,*CBDEFENSE*,*TANIUM*,*MALWAREBYTES*,*WEBROOT*,*AVAST*,*AVG*,*DIGITAL GUARDIAN*,*FORCEPOINT*,*TRELLIX*,*CHECK POINT*,*NETSKOPE*,*CYBEREASON*")
     (list 1 "security software has code loaded inside Civil 3D"
             "Security tools that hook into programs check file and memory operations as they happen. If saves, opens or commands are slow, ask IT to add acad.exe and your project folders to the exclusions."))
    ((wcmatch u "*DROPBOX*,*ONEDRIVE*,*FILESYNC*,*GOOGLE*DRIVE*,*BOX*SYNC*,*EGNYTE*,*DESKTOP CONNECTOR*")
     (list 1 "a cloud-sync shell extension is loaded into Civil 3D"
             "These load into every file dialog and can make Open/Save dialogs and file browsing slow, especially over the network."))
    ((wcmatch u "*DISPLAYFUSION*,*ACTUAL WINDOW*,*WINDOWBLINDS*,*STARDOCK*,*LOGI*OPTIONS*,*LOGITECH*,*WACOM*,*3DCONNEXION*")
     (list 1 "a mouse / window-management tool hooks into Civil 3D"
             "Tools like DisplayFusion, Stardock, Logitech Options or 3Dconnexion drivers hook every window. Update them; if the lag stops when they are closed, keep them closed while working."))
    ((wcmatch u "*TEAMVIEWER*,*ANYDESK*,*SPLASHTOP*")
     (list 1 "a remote-control tool hooks into Civil 3D"
             "Remote-control tools capture the screen. Close them when no one is connected."))
  )
)

;; Every DLL loaded into acad.exe that is not from Windows or Autodesk.
;; This finds .NET plugins loaded with NETLOAD too.
(defun pc:addin-modules (svc / rows u dirs d a tips tip k)
  (if *pc-acadpid*
    (progn
      (pc:progress "Listing code loaded inside Civil 3D")
      (setq rows (pc:query svc (strcat "ASSOCIATORS OF {Win32_Process.Handle='" *pc-acadpid*
                                       "'} WHERE ResultClass = CIM_DataFile")
                           '("Name")))
      (foreach r rows
        (setq u (strcase (pc:str (pc:f r "Name"))))
        (if (and (/= u "") (not (pc:system-path-p u)) (not (pc:adsk-path-p u)))
          (progn
            (setq d (vl-filename-directory u))
            (if (setq a (assoc d dirs))
              (setq dirs (subst (cons d (1+ (cdr a))) a dirs))
              (setq dirs (cons (cons d 1) dirs)))
            (if (and (setq tip (pc:module-tip u)) (not (assoc (cadr tip) tips)))
              (setq tips (cons (list (cadr tip) (car tip) (caddr tip) u) tips)))
          )
        )
      )
      (pc:kv "Files loaded in acad.exe" (strcat (itoa (length rows)) " total, "
                                               (itoa (apply '+ (mapcar 'cdr dirs)))
                                               " from outside Windows / Autodesk"))
      (if (null rows) (pc:out "    (could not list loaded files - WMI query was refused)"))
      (if dirs
        (progn
          (pc:out "  Third-party code inside Civil 3D, by folder (includes NETLOAD-ed .NET plugins):")
          (setq k 0)
          (foreach x (reverse dirs)
            (if (< k 40) (pc:out (strcat "    " (pc:pad (car x) 80) (itoa (cdr x)) " file(s)")))
            (setq k (1+ k)))
        )
      )
      (foreach x tips
        (pc:issue (cadr x) (strcat "Found: " (car x))
                  (strcat (caddr x) "  (File: " (cadddr x) ")")))
      (setq *pc-thirdparty-dirs* (length dirs))
    )
  )
)

(defun pc:check-addins (svc full / arxs path third unknown bases d names b third-b key num i v suite lsps sup net missing
                                     cmds rx tot objrx mg mgn)
  (pc:section "ADD-INS / PLUGINS / CUSTOMIZATION")
  ;; ARX / DBX
  (setq arxs (pc:try 'arx nil) third nil unknown 0)
  (foreach a arxs
    (if (setq path (findfile a))
      (if (not (pc:adsk-path-p (strcase path))) (setq third (cons path third)))
      (setq unknown (1+ unknown))))
  (pc:kv "ARX/DBX modules loaded" (strcat (itoa (length arxs)) "  (" (itoa (length third)) " not from Autodesk)"))
  (foreach x third (pc:out (strcat "    third-party ARX: " x)))

  ;; Auto-loading bundles
  (setq bases (list (list "user" (getenv "APPDATA"))
                    (list "all users" (getenv "ProgramData"))
                    (list "Program Files" (getenv "ProgramFiles")))
        third-b nil)
  (foreach base bases
    (if (cadr base)
      (progn
        (setq d (strcat (cadr base) "\\Autodesk\\ApplicationPlugins"))
        (if (vl-file-directory-p d)
          (foreach b (vl-remove-if '(lambda (x) (member x '("." ".."))) (vl-directory-files d nil -1))
            (setq names (cons (strcat b "  [" (car base) "]") names))
            (if (not (wcmatch (strcase b) "AUTODESK*,ADSK*,ACAD*,AUTOCAD*"))
              (setq third-b (cons b third-b)))
          )
        )
      )
    )
  )
  (pc:kv "Plugin bundles (auto-load)" (strcat (itoa (length names)) "  (" (itoa (length third-b)) " not from Autodesk)"))
  (foreach x (reverse names) (pc:out (strcat "    " x)))

  ;; Startup Suite (APPLOAD > Contents)
  (setq key (pc:try 'vlax-product-key nil))
  (if key
    (progn
      (setq key (strcat "HKEY_CURRENT_USER\\" key "\\Profiles\\" (getvar "CPROFILE") "\\Dialogs\\Appload\\Startup")
            num (atoi (pc:str (pc:try 'vl-registry-read (list key "NumStartup"))))
            i 1)
      (repeat num
        (if (setq v (pc:try 'vl-registry-read (list key (strcat (itoa i) "Startup"))))
          (setq suite (cons (pc:str v) suite)))
        (setq i (1+ i)))
    )
  )
  (pc:kv "Startup Suite (APPLOAD)" (itoa (length suite)))
  (foreach x (reverse suite) (pc:out (strcat "    " x)))

  ;; acad*.lsp auto-load files
  (foreach f '("acad.lsp" "acaddoc.lsp" "acad2026.lsp" "acad2026doc.lsp")
    (if (setq path (findfile f)) (setq lsps (cons path lsps))))
  (pc:kv "Auto-load LISP files" (itoa (length lsps)))
  (foreach x (reverse lsps) (pc:out (strcat "    " x)))
  (pc:kv "ACADLSPASDOC" (pc:str (getvar "ACADLSPASDOC")))

  ;; LISP commands, reactors, menus
  (setq cmds (length (vl-remove-if-not '(lambda (s) (wcmatch s "C:*")) (atoms-family 1))))
  (pc:kv "LISP commands defined" (itoa cmds))
  (setq rx (pc:try 'vlr-reactors nil) tot 0 objrx 0)
  (foreach g rx
    (if (listp g)
      (progn
        (setq tot (+ tot (length (cdr g))))
        (if (= (car g) :VLR-Object-Reactor) (setq objrx (+ objrx (length (cdr g)))))
      )
    )
  )
  (pc:kv "LISP reactors (this drawing)" (strcat (itoa tot) "  (" (itoa objrx) " object reactors)"))
  (setq mg (pc:try 'vla-get-MenuGroups (list (pc:acad))) mgn nil)
  (if mg (vlax-for g mg (setq mgn (cons (pc:str (pc:try 'vla-get-Name (list g))) mgn))))
  (pc:kv "Menu groups (CUIx)" (strcat (itoa (length mgn)) "  " (apply 'strcat (mapcar '(lambda (x) (strcat x " ")) (reverse mgn)))))
  (pc:kv "SECURELOAD" (pc:str (getvar "SECURELOAD")))

  ;; Support file search path
  (setq sup (vl-remove "" (mapcar '(lambda (x) (vl-string-trim " " x)) (pc:split (pc:str (getenv "ACAD")) ";")))
        net nil missing nil)
  (foreach p sup
    (cond
      ((pc:pathkind p) (setq net (cons p net)))
      ((not (vl-file-directory-p p)) (setq missing (cons p missing)))
    )
  )
  (pc:kv "Support search path" (strcat (itoa (length sup)) " folders, " (itoa (length net)) " on network/cloud, "
                                        (itoa (length missing)) " missing"))
  (foreach p (reverse net) (pc:out (strcat "    network: " p)))
  (foreach p (reverse missing) (pc:out (strcat "    MISSING: " p)))

  ;; Code loaded inside acad.exe (finds NETLOAD-ed plugins too)
  (if (and svc full) (pc:addin-modules svc))

  ;; ---- flags
  (if (or third third-b (> (pc:n *pc-thirdparty-dirs*) 0) suite)
    (pc:issue 1 "Third-party add-ins are loaded into Civil 3D"
      (strcat "Found: " (itoa (length third)) " ARX, " (itoa (length third-b)) " plugin bundle(s), "
              (itoa (length suite)) " Startup Suite file(s)"
              (if *pc-thirdparty-dirs* (strcat ", " (itoa *pc-thirdparty-dirs*) " folder(s) of third-party code in acad.exe") "")
              " (see ADD-INS section). Add-ins hook into commands, saves, selection and object changes, so a slow one makes everything lag. To test: add /safemode at the end of the Civil 3D desktop shortcut's Target, start Civil 3D with it, and compare (also compare the 'Command overhead' benchmark). Then turn add-ins back on one at a time.")))
  (if net
    (pc:issue 2 (strcat (itoa (length net)) " support path folder(s) are on the network")
      "Civil 3D searches these folders every time it looks for a font, linetype, hatch pattern, block, menu, LISP or template. Over a slow network that adds delay everywhere. Move them to the BOTTOM of OPTIONS > Files > Support File Search Path, or copy those files to a local folder."))
  (if missing
    (pc:issue (if (vl-some 'pc:pathkind missing) 2 1) (strcat (itoa (length missing)) " support path folder(s) don't exist")
      "Missing folders (especially old network paths) are re-tried on every file search, and a missing network path can wait for a timeout each time. Remove them in OPTIONS > Files > Support File Search Path."))
  (if (> objrx 2000)
    (pc:issue 2 (strcat (itoa objrx) " LISP object reactors in this drawing")
      "A LISP routine is watching thousands of objects; every change fires code. Find which routine creates them (usually an add-in that 'links' objects) and update or remove it."))
  (if (> cmds 2000)
    (pc:issue 1 (strcat (itoa cmds) " LISP commands loaded")
      "Loading this many LISP routines into every drawing slows opening drawings. Load rarely used tools on demand (autoload) instead of in acaddoc.lsp / the Startup Suite."))
)

;;; ================================================== MORE DRAWING CHECKS

(defun pc:layer-xdict-count (name / tbl xd d)
  (setq tbl (cdr (assoc 330 (entget (tblobjname "LAYER" "0")))))
  (if (and tbl
           (setq xd (cdr (assoc 360 (member '(102 . "{ACAD_XDICTIONARY") (entget tbl)))))
           (setq d (dictsearch xd name)))
    (length (vl-remove-if-not '(lambda (x) (= (car x) 3)) d))
    0
  )
)

(defun pc:check-drawing-more (/ lf ls grp vp)
  (setq lf  (+ (pc:n (pc:try 'pc:layer-xdict-count '("ACAD_LAYERFILTERS")))
               (pc:n (pc:try 'pc:layer-xdict-count '("ACLYDICTIONARY"))))
        ls  (pc:n (pc:try 'pc:layer-xdict-count '("ACAD_LAYERSTATES")))
        grp (pc:dictcount "ACAD_GROUP")
        vp  (pc:count '((0 . "VIEWPORT"))))
  (pc:kv "Layer filters" (itoa lf))
  (pc:kv "Layer states" (itoa ls))
  (pc:kv "Groups" (itoa grp))
  (pc:kv "Viewports" (itoa vp))
  (if (> lf 100)
    (pc:issue 2 (strcat (itoa lf) " layer filters (layer filter bloat)")
      "Layer filters pile up when drawings with filters are xref'd or copied in, and they slow down the Layer Properties Manager, layer commands and opening. Delete the junk ones in Layer Properties Manager (right-click a filter > Delete), or use a layer filter cleanup tool."))
  (if (> ls 200)
    (pc:issue 1 (strcat (itoa ls) " layer states")
      "Delete old layer states in LAYERSTATE."))
  (if (> grp 2000)
    (pc:issue 1 (strcat (itoa grp) " groups")
      "Thousands of groups (often unnamed ones from copy/paste or imports) slow selection. Use GROUPEDIT / the Group Manager to remove ones you don't need."))
  (if (> vp 200)
    (pc:issue 1 (strcat (itoa vp) " viewports")
      "Every viewport has to be drawn. Split sheets into separate drawings or a sheet set."))
)

;;; =============================================== COMMAND OVERHEAD TEST

;; Runs a do-nothing command 200 times. Add-ins and reactors that run code on
;; every command show up here as extra milliseconds per command.
(defun pc:bench-cmd (/ old t0 d)
  (setq old (getvar "USERI5") t0 (pc:ms))
  (repeat 200 (command "_.SETVAR" "USERI5" 1))
  (setq d (pc:since t0))
  (setvar "USERI5" old)
  (/ d 200.0)
)

;;; ================================================================ SETTINGS

(defun pc:want (name okfn newval sev why / v ok)
  (setq v (pc:sv name))
  (if v
    (progn
      (setq ok (apply okfn (list v)))
      (pc:out (strcat "  " (pc:pad name 20) (pc:pad (vl-princ-to-string v) 8)
                      (if ok "ok" (strcat "<-- suggest " (vl-princ-to-string newval)))))
      (if (not ok)
        (progn
          (pc:issue sev (strcat name " is " (vl-princ-to-string v))
                    (strcat why " Fix: set " name " to " (vl-princ-to-string newval) " (PERFFIX can do it)."))
          (setq *pc-fixes* (cons (cons name newval) *pc-fixes*))
        )
      )
    )
  )
)

(defun pc:check-settings (/ big v)
  (setq *pc-fixes* nil
        big (> (pc:n *pc-ents*) 50000))
  (pc:section "SETTINGS THAT AFFECT SPEED")
  (pc:want "ROLLOVERTIPS" '(lambda (v) (= v 0)) 0 2
    "Rollover tooltips make Civil 3D query every object the cursor passes over (surfaces, alignments, corridors, points). One of the most common causes of a laggy, stuttering cursor.")
  (pc:want "SELECTIONPREVIEW" '(lambda (v) (= v 0)) 0 (if big 2 1)
    "Selection preview highlights objects as the cursor passes over them; on big Civil 3D objects that means redrawing them constantly.")
  (pc:want "QPMODE" '(lambda (v) (<= v 0)) 0 1
    "The Quick Properties popup reads all properties every time you select something.")
  (pc:want "VTENABLE" '(lambda (v) (= v 0)) 0 1
    "Smooth view transitions animate every zoom and pan.")
  (pc:want "LAYOUTREGENCTL" '(lambda (v) (= v 2)) 2 2
    "Below 2, switching layout tabs regenerates the drawing every time.")
  (pc:want "XLOADCTL" '(lambda (v) (= v 2)) 2 (if (> (pc:n *pc-xrefs*) 0) 2 1)
    "Demand-load xrefs from a copy, so only what is visible is loaded and the files are not locked.")
  (pc:want "SAVETIME" '(lambda (v) (or (= v 0) (>= v 10))) 10 2
    "Automatic save freezes Civil 3D while it writes the whole drawing. A short interval means a freeze every few minutes - a very typical 'regular stutter'.")
  (pc:want "LOGFILEMODE" '(lambda (v) (= v 0)) 0 1
    "Every command-line message is also written to a log file.")
  (pc:want "AUTOMATICPUB" '(lambda (v) (= v 0)) 0 2
    "A DWF/PDF is published every time you save.")
  (pc:want "COMMANDPREVIEW" '(lambda (v) (= v 0)) 0 1
    "Live previews for TRIM, FILLET, OFFSET etc. compute the result while you hover.")
  (pc:want "HPQUICKPREVIEW" '(lambda (v) (= v 0)) 0 1
    "The hatch preview calculates boundaries while you hover inside areas - very slow on Civil 3D geometry.")
  (pc:want "LINESMOOTHING" '(lambda (v) (= v 0)) 0 1
    "Anti-aliasing adds work for the graphics card.")
  (pc:want "GRIPOBJLIMIT" '(lambda (v) (and (> v 0) (<= v 100))) 100 1
    "Showing grips on a large selection takes a long time.")
  (if (= (pc:sv "SAVETIME") 0)
    (pc:out "  Note: automatic save is OFF (SAVETIME 0). No autosave freezes, but remember to save often."))
  (setq v (pc:n (pc:sv "OSMODE")))
  (if (and (= 0 (logand v 16384)) (= 512 (logand v 512))
           (> (+ (pc:cnt "surfaces") (pc:cnt "corridors")) 0))
    (pc:issue 1 "The Nearest object snap is always on"
      "Running Nearest snap makes Civil 3D test big objects (surfaces, corridors) under the cursor all the time. Turn Nearest off in OSNAP and pick it only when needed (Shift + right-click)."))
)

;;; ============================================================== BENCHMARKS

(defun pc:work (n / x i)
  (setq x 0.0 i 1)
  (repeat n (setq x (+ x (sqrt i)) i (1+ i)))
  x
)

(defun pc:bench-cpu (/ t0 a b c lst seed i s)
  (setq t0 (pc:ms))
  (pc:work 300000)
  (setq a (pc:since t0))
  (setq t0 (pc:ms) seed 12345 lst nil)
  (repeat 60000
    (setq seed (rem (+ (* seed 1103) 12345) 65521)
          lst  (cons seed lst)))
  (vl-sort-i lst '<)
  (setq b (pc:since t0))
  (setq t0 (pc:ms) i 0)
  (repeat 60000
    (setq s (strcase (strcat "item-" (itoa i) "-" (rtos (* i 0.5) 2 2)))
          i (1+ i)))
  (setq c (pc:since t0))
  (list a b c)
)

(defun pc:bench-db (/ base i ents t0 a b c d e ms ln lay)
  (setq lay (tblsearch "LAYER" "0"))
  (if (or (null lay) (/= 0 (logand 5 (cdr (assoc 70 lay)))))
    nil                                  ; layer 0 frozen or locked - skip
    (progn
      (setq *pc-clayer* (getvar "CLAYER"))
      (setvar "CLAYER" "0")
      (setq base (getvar "EXTMIN"))
      (if (> (abs (car base)) 1e19) (setq base '(0.0 0.0 0.0)))
      ;; create
      (setq t0 (pc:ms) i 0 ents nil)
      (repeat 2000
        (if (entmake (list '(0 . "LINE") '(8 . "0") '(410 . "Model")
                           (list 10 (+ (car base) (* i 0.001)) (cadr base) 0.0)
                           (list 11 (+ (car base) (* i 0.001)) (+ (cadr base) 0.001) 0.0)))
          (setq ents (cons (entlast) ents)))
        (setq i (1+ i))
      )
      (setq *pc-temp* ents
            a (pc:since t0))
      ;; modify
      (setq t0 (pc:ms))
      (foreach e ents
        (setq d (entget e))
        (entmod (subst (list 11 (cadr (assoc 10 d)) (+ (cadr base) 0.002) 0.0) (assoc 11 d) d))
      )
      (setq b (pc:since t0))
      ;; erase
      (setq t0 (pc:ms))
      (foreach e ents (entdel e))
      (setq c (pc:since t0)
            *pc-temp* nil)
      ;; ActiveX create + delete
      (setq ms (vla-get-ModelSpace (pc:doc)) t0 (pc:ms))
      (repeat 500
        (setq ln (vla-AddLine ms (vlax-3d-point base)
                              (vlax-3d-point (list (+ (car base) 0.001) (cadr base) 0.0))))
        (vla-Delete ln)
      )
      (setq d (pc:since t0))
      (setvar "CLAYER" *pc-clayer*)
      (setq *pc-clayer* nil)
      (list a b c d)
    )
  )
)

(defun pc:bench-regen (runs / l t0)
  (repeat runs
    (if (or (null l) (< (car l) 8000))   ; don't repeat a very slow regen
      (progn
        (setq t0 (pc:ms))
        (command "_.REGEN")
        (setq l (cons (pc:since t0) l))
      )
    )
  )
  (reverse l)
)

(defun pc:bench-zoom (/ t0 d)
  (setq *pc-vt* (getvar "VTENABLE"))
  (setvar "VTENABLE" 0)
  (setq t0 (pc:ms))
  (repeat 4 (command "_.ZOOM" "1.25X"))
  (repeat 4 (command "_.ZOOM" "0.8X"))
  (setq d (pc:since t0))
  (setvar "VTENABLE" *pc-vt*)
  (setq *pc-vt* nil)
  (/ d 8.0)
)

;; Runs the same small job many times. On a quiet PC every run takes about
;; the same time; runs that take much longer mean Civil 3D was paused by
;; something else (background programs, throttling, drivers).
(defun pc:bench-stutter (samples / n d t0 lst srt med mx spikes)
  (setq n 2000 d 0)
  (while (and (< d 40) (< n 20000000))
    (setq n (* n 2) t0 (pc:ms))
    (pc:work n)
    (setq d (pc:since t0))
  )
  (repeat samples
    (setq t0 (pc:ms))
    (pc:work n)
    (setq lst (cons (pc:since t0) lst))
  )
  (setq srt (mapcar '(lambda (i) (nth i lst)) (vl-sort-i lst '<))
        med (nth (/ samples 2) srt)
        mx  (last srt)
        spikes (length (vl-remove-if-not
                         '(lambda (x) (and (> x (* 1.6 med)) (> x (+ med 32))))
                         lst)))
  (list med mx spikes samples)
)

;; Test file for copy speed (about 8 MB), made once and kept.
(defun pc:bigfile (/ fn f s)
  (setq fn (strcat (pc:dir) "\\copytest.bin"))
  (if (< (pc:n (vl-file-size fn)) 8000000)
    (progn
      (setq s "0123456789")
      (repeat 6 (setq s (strcat s s)))   ; 640 chars
      (if (setq f (open fn "w"))
        (progn (repeat 13107 (write-line s f)) (close f))
      )
    )
  )
  (if (findfile fn) fn)
)

;; Returns (ms-per-small-file copy-ms size-MB) or nil if the folder is not writable
(defun pc:bench-folder (dir / fn h t0 lat src dst cp sz)
  (setq dir (vl-string-right-trim "\\/" dir)
        fn  (strcat dir "\\~perfcheck_latency.tmp"))
  (if (setq h (open fn "w")) (progn (write-line "x" h) (close h)))
  (if (findfile fn)
    (progn
      (vl-file-delete fn)
      (setq t0 (pc:ms))
      (repeat 20
        (if (setq h (open fn "w")) (progn (write-line "PerfCheck latency test" h) (close h)))
        (vl-file-delete fn)
      )
      (setq lat (/ (pc:since t0) 20.0))
      (if (setq src (pc:bigfile))
        (progn
          (setq dst (strcat dir "\\~perfcheck_copy.tmp")
                sz  (/ (vl-file-size src) 1048576.0))
          (if (findfile dst) (vl-file-delete dst))
          (setq t0 (pc:ms))
          (vl-file-copy src dst)
          (setq cp (pc:since t0))
          (if (findfile dst) (vl-file-delete dst) (setq cp nil))
        )
      )
      (list lat cp sz)
    )
  )
)

(defun pc:history-file () (strcat (pc:dir) "\\History.csv"))

;; Best (lowest) CPU benchmark time recorded earlier on this computer
(defun pc:history-best-cpu (/ f l parts v best comp)
  (setq comp (strcase (pc:str (getenv "COMPUTERNAME"))))
  (if (setq f (open (pc:history-file) "r"))
    (progn
      (while (setq l (read-line f))
        (setq parts (pc:split l ","))
        (if (and (> (length parts) 4) (= (strcase (nth 1 parts)) comp))
          (progn
            (setq v (atof (nth 4 parts)))
            (if (and (> v 0) (or (null best) (< v best))) (setq best v))
          )
        )
      )
      (close f)
    )
  )
  best
)

(defun pc:benchmarks (full / l avg cpu tot best db zoom st med mx spikes pause folders seen p r lat cp mbps)
  (pc:section "BENCHMARKS")
  (pc:out "  (Times are in milliseconds. Lower is better. They are measured from inside Civil 3D,")
  (pc:out "   so they show what you actually feel. Compare with earlier runs in History.csv.)")

  ;; REGEN
  (pc:progress "Timing REGEN")
  (setq l   (pc:bench-regen (if full 3 1))
        avg (/ (apply '+ l) (float (length l))))
  (pc:setres "regen" avg)
  (pc:kv "REGEN" (strcat (rtos avg 2 0) " ms"
                         (if (> (length l) 1)
                           (strcat "  (runs: " (apply 'strcat (mapcar '(lambda (x) (strcat (itoa x) " ")) l)) ")")
                           "")
                         (if (> (pc:n *pc-ents*) 1000)
                           (strcat "  = " (rtos (/ avg (/ *pc-ents* 1000.0)) 2 2) " ms per 1000 objects")
                           "")))
  (cond
    ((> avg 8000)
     (pc:issue 3 (strcat "REGEN takes " (pc:sec avg))
       "Every pan/zoom outside the cached view, layer freeze/thaw, layout switch or style change can trigger a regen, so you wait this long each time. Reduce what is displayed: freeze layers you don't need, use light surface styles (border only) in plan, hide point labels, unload xrefs/images you don't need, and see the DRAWING section."))
    ((> avg 3000)
     (pc:issue 2 (strcat "REGEN takes " (pc:sec avg))
       "Freeze layers you don't need, use lighter surface / point / label styles while working, and unload xrefs or images you don't need."))
  )

  ;; COMMAND OVERHEAD
  (pc:progress "Timing command overhead")
  (setq l (pc:bench-cmd))
  (pc:setres "cmd" l)
  (pc:kv "Command overhead" (strcat (rtos l 2 1) " ms per command  (200 do-nothing commands)"))
  (cond
    ((> l 40)
     (pc:issue 3 (strcat "Every command costs about " (rtos l 2 0) " ms before it even starts")
       "A do-nothing command should take only a few ms. Something runs code on every command - almost always an add-in / plugin or a LISP reactor. See the ADD-INS section, then test with /safemode (add it to the end of the Civil 3D shortcut's Target) and turn add-ins back on one at a time."))
    ((> l 15)
     (pc:issue 2 (strcat "Every command costs about " (rtos l 2 0) " ms of overhead")
       "A do-nothing command should take only a few ms. Add-ins or LISP reactors that react to every command are the usual reason. See the ADD-INS section and compare with a /safemode start."))
  )

  (if full
    (progn
      ;; ZOOM
      (pc:progress "Timing zoom")
      (setq zoom (pc:bench-zoom))
      (pc:setres "zoom" zoom)
      (pc:kv "Zoom step" (strcat (rtos zoom 2 0) " ms per zoom (average of 8)"))
      (cond
        ((> zoom 1000)
         (pc:issue 2 (strcat "Each zoom step takes " (rtos zoom 2 0) " ms")
           "Display is slow. Check the GRAPHICS items (driver, hardware acceleration via GRAPHICSCONFIG, integrated vs. dedicated GPU) and the drawing size items."))
        ((> zoom 300)
         (pc:issue 1 (strcat "Each zoom step takes " (rtos zoom 2 0) " ms")
           "A bit slow. Make sure Hardware Acceleration is on (GRAPHICSCONFIG) and the graphics driver is current."))
      )

      ;; CPU
      (pc:progress "CPU benchmark")
      (setq best (pc:history-best-cpu)
            cpu  (pc:bench-cpu)
            tot  (apply '+ cpu))
      (pc:setres "cpu" tot)
      (pc:kv "CPU (single core)" (strcat (itoa tot) " ms total  (math " (itoa (car cpu)) ", sort "
                                         (itoa (cadr cpu)) ", text " (itoa (caddr cpu)) ")"))
      (if (> tot 0)
        (pc:kv "CPU score" (strcat (rtos (/ 100000.0 tot) 2 0) "  (higher = faster)")))
      (if best
        (progn
          (pc:kv "  vs. your best run" (strcat (rtos best 2 0) " ms  ("
                                               (rtos (* 100 (- (/ tot best) 1)) 2 0) "% slower now)"))
          (if (> tot (* 1.25 best))
            (pc:issue 2 (strcat "The CPU is " (rtos (* 100 (- (/ tot best) 1)) 2 0)
                                "% slower right now than in your best run on this PC")
              "The same test ran faster before, so something is slowing the CPU right now: power plan / battery, overheating (dusty fans, laptop on a soft surface), or background programs (see BACKGROUND)."))
        )
        (pc:out "    (first run on this PC - later runs will be compared against this one)")
      )

      ;; DATABASE
      (pc:progress "Database benchmark")
      (if (setq db (pc:bench-db))
        (progn
          (pc:setres "db" (apply '+ db))
          (pc:kv "Database (2000 lines)" (strcat "create " (itoa (car db)) ", modify " (itoa (cadr db))
                                                 ", erase " (itoa (caddr db)) " ms"))
          (pc:kv "ActiveX (500 lines)" (strcat (itoa (cadddr db)) " ms"))
          (pc:out "    (test objects were deleted again)")
        )
        (pc:kv "Database" "skipped (layer 0 is frozen or locked)")
      )

      ;; DISK
      (pc:progress "Disk test")
      (setq folders (list (list "Temp folder" (getvar "TEMPPREFIX") "lat-temp")
                          (list "Autosave folder" (getvar "SAVEFILEPATH") "lat-save")))
      (if (= 1 (getvar "DWGTITLED"))
        (setq folders (cons (list "Drawing folder" (getvar "DWGPREFIX") "lat-dwg") folders)))
      (setq seen nil)
      (foreach x folders
        (setq p (strcase (vl-string-right-trim "\\/" (pc:str (cadr x)))))
        (if (and (/= p "") (not (member p seen)))
          (progn
            (setq seen (cons p seen))
            (if (setq r (pc:bench-folder (cadr x)))
              (progn
                (setq lat (car r) cp (cadr r))
                (pc:setres (caddr x) lat)
                (setq mbps (if (and cp (> cp 15)) (/ (caddr r) (/ cp 1000.0))))
                (pc:kv (strcat "Disk: " (car x))
                       (strcat (rtos lat 2 1) " ms per small file, copy "
                               (rtos (pc:n (caddr r)) 2 0) " MB: "
                               (cond ((null cp) "failed")
                                     ((<= cp 15) "<16 ms (fast)")
                                     (T (strcat (itoa cp) " ms = " (rtos mbps 2 0) " MB/s")))))
                (cond
                  ((> lat 40)
                   (pc:issue (if (= (car x) "Drawing folder") 3 2)
                     (strcat (car x) " storage is very slow (" (rtos lat 2 0) " ms per file)")
                     "Each save, autosave, .bak and lock file waits this long per file operation. Typical causes: network over Wi-Fi/VPN, cloud sync folders, antivirus scanning, or a failing/spinning disk."))
                  ((> lat 15)
                   (pc:issue 2 (strcat (car x) " storage is slow (" (rtos lat 2 0) " ms per file)")
                     "A local SSD normally takes under 2 ms. Network, cloud sync or antivirus scanning add delay to every save and autosave."))
                )
                (if (and mbps (< mbps 30))
                  (pc:issue 2 (strcat (car x) ": file copy only " (rtos mbps 2 0) " MB/s")
                    "Opening and saving large drawings here will be slow. Work on a local SSD copy if possible."))
              )
              (pc:kv (strcat "Disk: " (car x)) (strcat "not writable: " (pc:str (cadr x))))
            )
          )
        )
      )
    )
  )

  ;; STUTTER
  (pc:progress "Stutter test")
  (setq st (pc:bench-stutter (if full 60 30))
        med (car st) mx (cadr st) spikes (caddr st)
        pause (- mx med))
  (pc:setres "spikes" spikes)
  (pc:kv "Stutter test" (strcat (itoa spikes) " of " (itoa (cadddr st)) " runs were interrupted; normal run "
                                (itoa med) " ms, worst " (itoa mx) " ms"))
  (cond
    ((>= spikes (/ (cadddr st) 4))
     (pc:issue 3 (strcat "Civil 3D was interrupted in " (itoa spikes) " of " (itoa (cadddr st)) " test runs")
       "Something keeps taking the CPU away from Civil 3D - this is what stutter feels like. Look at the BACKGROUND section (antivirus, sync, updates, browsers), the power plan / battery, and overheating. Run PERFQUICK again with other programs closed to compare."))
    ((>= spikes (/ (cadddr st) 10))
     (pc:issue 2 (strcat "Civil 3D was interrupted in " (itoa spikes) " of " (itoa (cadddr st)) " test runs")
       "Background programs or CPU throttling are pausing Civil 3D now and then. See BACKGROUND and the power items."))
    ((> pause 250)
     (pc:issue 1 (strcat "One pause of about " (itoa pause) " ms during the stutter test")
       "A single hiccup can be normal, but if you feel regular freezes, check SAVETIME (autosave), antivirus and cloud sync."))
  )
)

;;; ================================================================ RESULTS

(defun pc:checklist ()
  (list
    ""
    "=== ALSO WORTH CHECKING (can't be read from LISP) ========================="
    "  1. GRAPHICSCONFIG: Hardware Acceleration must be ON."
    "  2. Laptops with two GPUs: Windows Settings > System > Display > Graphics >"
    "     acad.exe > High performance. NVIDIA Control Panel: 'Prefer maximum performance'."
    "  3. Antivirus: ask IT to exclude project folders / *.dwg *.sv$ *.dwl from real-time scanning."
    "  4. Surfaces: use a border-only or large-interval contour style while drafting."
    "  5. Corridors / surfaces: turn off 'Rebuild - Automatic' while editing."
    "  6. Hide what you don't need while drafting: give surfaces / points / alignments"
    "     you don't need to see a 'No Display' style."
    "  7. Use data shortcuts instead of one giant drawing with everything in it."
    "  8. Install the latest Civil 3D 2026 update (Autodesk Access) and graphics driver."
    "  9. Restart Civil 3D once a day and Windows at least once a week."
    " 10. Turn PERFWATCH on, work normally, then run PERFLOG to see exactly which"
    "     commands are slow."
    " 11. Plugins: add /safemode to the end of the Civil 3D shortcut's Target to start"
    "     without add-ins, run PERFCHECK again and compare the scores."
  )
)

(defun pc:report (full / h m l score verdict head k fn f best)
  (setq h 0 m 0 l 0)
  (foreach i *pc-issues*
    (cond ((>= (car i) 3) (setq h (1+ h))) ((= (car i) 2) (setq m (1+ m))) (T (setq l (1+ l))))
  )
  (setq score (max 0 (- 100 (* 15 h) (* 6 m) (* 2 l)))
        verdict (cond ((>= score 85) "GOOD - no major problems found")
                      ((>= score 65) "FAIR - a few things are slowing you down")
                      ((>= score 40) "POOR - several problems; fix the HIGH items first")
                      (T             "BAD - serious problems; fix the HIGH items first")))

  ;; ---- head of the report
  (setq head (list
               (strcat "C3D PerfCheck " *pc-version* "  -  " (if full "FULL CHECK" "QUICK CHECK")
                       "  -  " (pc:now "YYYY-MO-DD HH:MM:SS"))
               (strcat "Computer: " (pc:str (getenv "COMPUTERNAME")) "   Drawing: " (getvar "DWGNAME"))
               ""
               (strcat "HEALTH SCORE: " (itoa score) " / 100   " verdict)
               (strcat "Problems found: " (itoa h) " high, " (itoa m) " medium, " (itoa l) " low")
               ""
               "=== PROBLEMS FOUND (most important first) ================================"))
  (setq head (reverse head) k 0)
  (if (null *pc-issues*) (setq head (cons "  Nothing found. If it still stutters, use PERFWATCH + PERFLOG." head)))
  (foreach s '(3 2 1)
    (foreach i (reverse *pc-issues*)
      (if (= (min 3 (car i)) s)
        (progn
          (setq k (1+ k)
                head (cons (strcat (pc:lpad (itoa k) 3) ". [" (pc:sevname (car i)) "] " (cadr i)) head))
          (foreach w (pc:wrap (caddr i) 86) (setq head (cons (strcat "             " w) head)))
          (setq head (cons "" head))
        )
      )
    )
  )
  (setq head (reverse head))

  ;; ---- write the file
  (setq fn (strcat (pc:dir) "\\PerfReport_" (pc:stamp) ".txt"))
  (if (not (setq f (open fn "w")))
    (progn
      (princ (strcat "\nCould not write " fn " - trying the temp folder."))
      (setq fn (strcat (vl-string-right-trim "\\/" (getvar "TEMPPREFIX")) "\\PerfReport_" (pc:stamp) ".txt")
            f  (open fn "w"))
    )
  )
  (if f
    (progn
      (foreach x head (write-line x f))
      (foreach x (reverse *pc-lines*) (write-line x f))
      (foreach x (pc:checklist) (write-line x f))
      (close f)
    )
    (setq fn nil)
  )

  ;; ---- history
  (pc:history-append score h m l full)

  ;; ---- command line summary
  (princ "\n")
  (princ "\n================ C3D PerfCheck ================")
  (princ (strcat "\n HEALTH SCORE: " (itoa score) " / 100  -  " verdict))
  (princ (strcat "\n " (itoa h) " high, " (itoa m) " medium, " (itoa l) " low priority problems"))
  (setq k 0)
  (foreach s '(3 2)
    (foreach i (reverse *pc-issues*)
      (if (and (= (min 3 (car i)) s) (< k 10))
        (progn
          (setq k (1+ k))
          (princ (strcat "\n  [" (pc:sevname (car i)) "] " (cadr i)))
        )
      )
    )
  )
  (princ "\n===============================================")
  (if fn
    (progn
      (princ (strcat "\nFull report with fixes: " fn))
      (startapp "notepad" (strcat "\"" fn "\""))
    )
    (princ "\nCould not save the report file.")
  )
  (if *pc-fixes*
    (princ (strcat "\nTip: PERFFIX applies the " (itoa (length *pc-fixes*))
                   " recommended setting change(s); PERFUNDO puts them back.")))
)

(defun pc:csv (s) (vl-string-translate "," ";" (pc:str s)))

(defun pc:history-append (score h m l full / fn new f)
  (setq fn (pc:history-file)
        new (not (findfile fn)))
  (if (setq f (open fn "a"))
    (progn
      (if new
        (write-line "Date,Computer,Drawing,Score,CPU_ms,DB_ms,Regen_ms,Zoom_ms,Stutter_spikes,Temp_ms_per_file,Dwg_ms_per_file,Objects,High,Med,Low,Mode,Cmd_ms" f))
      (write-line (strcat (pc:now "YYYY-MO-DD HH:MM") "," (pc:csv (getenv "COMPUTERNAME")) ","
                          (pc:csv (getvar "DWGNAME")) "," (itoa score) ","
                          (pc:res "cpu") "," (pc:res "db") "," (pc:res "regen") "," (pc:res "zoom") ","
                          (pc:res "spikes") "," (pc:res "lat-temp") "," (pc:res "lat-dwg") ","
                          (itoa (pc:n *pc-ents*)) "," (itoa h) "," (itoa m) "," (itoa l) ","
                          (if full "full" "quick") "," (pc:res "cmd"))
                  f)
      (close f)
    )
  )
)

;;; ============================================================== MAIN RUN

(defun pc:run (full / svc t0)
  (pc:begin)
  (setq *pc-lines* nil *pc-issues* nil *pc-res* nil *pc-fixes* nil
        *pc-ram-gb* nil *pc-logical* nil *pc-netdrives* nil *pc-cnt* nil *pc-ents* nil
        *pc-xpaths* nil *pc-xrefs* nil *pc-laptop* nil *pc-cpuperf* nil
        *pc-netmap* nil *pc-acadpid* nil *pc-thirdparty-dirs* nil *pc-dwg-unc* nil
        t0 (pc:ms))
  (princ (strcat "\nC3D PerfCheck " *pc-version*
                 (if full
                   " - full check. Takes about 1-2 minutes; the screen will regen and zoom. Please wait..."
                   " - quick check. Please wait...")))
  (pc:progress "Reading Windows and hardware info")
  (setq svc (pc:wmi "root\\cimv2"))
  (if svc
    (progn
      (pc:sys-os svc)
      (pc:sys-cpu svc)
      (pc:sys-power svc)
      (pc:sys-gpu svc)
      (pc:sys-disk svc)
      (pc:sys-extra svc)
      (pc:progress "Checking the network")
      (setq *pc-dwg-unc* (if (= 1 (getvar "DWGTITLED")) (pc:unc-of (getvar "DWGPREFIX"))))
      (pc:sys-network svc full)
      (pc:progress "Checking what else is running")
      (pc:sys-procs svc)
    )
    (progn
      (pc:section "WINDOWS / HARDWARE")
      (pc:out "  Could not read system info (Windows WMI is not available or blocked).")
    )
  )
  (pc:progress "Scanning the drawing")
  (pc:check-drawing)
  (pc:check-paths)
  (pc:check-settings)
  (pc:progress "Checking add-ins and plugins")
  (pc:check-addins svc full)
  (if svc (pc:try 'vlax-release-object (list svc)))
  (pc:benchmarks full)
  (pc:out "")
  (pc:out (strcat "(PerfCheck took " (pc:sec (pc:since t0)) ")"))
  (pc:report full)
  (pc:finish)
)

(defun C:PERFCHECK () (pc:run T))
(defun C:PERFQUICK () (pc:run nil))

;;; ============================================================= PERFWATCH
;;; Times every command while you work and warns when one is slow.

(defun pc:warn-ms (/ v)
  (setq v (atoi (pc:str (getenv "C3DPerfCheckWarnMs"))))
  (if (< v 100) 1500 v)
)

(defun pc:record (name d / a f)
  (if (setq a (assoc name *pc-stats*))
    (setq *pc-stats* (subst (list name (1+ (cadr a)) (+ (caddr a) d) (max (cadddr a) d)) a *pc-stats*))
    (setq *pc-stats* (cons (list name 1 d d) *pc-stats*))
  )
  (if (>= d (pc:warn-ms))
    (progn
      (princ (strcat "\n[PerfCheck] " name " took " (pc:sec d)))
      (if (setq f (open (strcat (pc:dir) "\\SlowCommands.log") "a"))
        (progn
          (write-line (strcat (pc:now "YYYY-MO-DD HH:MM:SS") "\t" (rtos (/ d 1000.0) 2 2) " s\t"
                              name "\t" (getvar "DWGNAME"))
                      f)
          (close f)
        )
      )
    )
  )
)

(defun pc:cb-start (r args)
  (setq *pc-starts* (cons (cons (car args) (getvar "MILLISECS")) *pc-starts*))
)

(defun pc:cb-end (r args / s d)
  (if (setq s (assoc (car args) *pc-starts*))
    (progn
      (setq *pc-starts* (vl-remove s *pc-starts*)
            d (- (getvar "MILLISECS") (cdr s)))
      (if (>= d 0) (pc:record (car args) d))
    )
  )
)

(defun pc:cb-save-start (r args) (setq *pc-save-t0* (getvar "MILLISECS")))
(defun pc:cb-save-end (r args / d)
  (if *pc-save-t0*
    (progn
      (setq d (- (getvar "MILLISECS") *pc-save-t0*) *pc-save-t0* nil)
      (if (>= d 0) (pc:record "<file write: save/autosave>" d))
    )
  )
)

(defun pc:watch-off (/ groups)
  (setq groups (vl-catch-all-apply 'vlr-reactors '(:vlr-command-reactor :vlr-dwg-reactor)))
  (if (and groups (listp groups))
    (foreach grp groups
      (if (listp grp)
        (foreach r (cdr grp)
          (if (= (vlr-data r) "C3DPerfCheck") (vlr-remove r))
        )
      )
    )
  )
  (setq *pc-cmd-reactor* nil *pc-dwg-reactor* nil *pc-starts* nil)
)

(defun pc:watch-on ()
  (pc:watch-off)
  (setq *pc-cmd-reactor*
         (vlr-command-reactor "C3DPerfCheck"
           '((:vlr-commandWillStart . pc:cb-start)
             (:vlr-commandEnded     . pc:cb-end)
             (:vlr-commandCancelled . pc:cb-end)
             (:vlr-commandFailed    . pc:cb-end)))
        *pc-dwg-reactor*
         (vlr-dwg-reactor "C3DPerfCheck"
           '((:vlr-beginSave    . pc:cb-save-start)
             (:vlr-saveComplete . pc:cb-save-end))))
)

(defun C:PERFWATCH (/ ans v)
  (princ (strcat "\nPERFWATCH is " (if *pc-cmd-reactor* "ON" "OFF")
                 ". Warns when a command takes longer than " (pc:sec (pc:warn-ms)) "."))
  (initget "On Off Threshold")
  (setq ans (getkword (strcat "\nPERFWATCH [On/Off/Threshold] <" (if *pc-cmd-reactor* "Off" "On") ">: ")))
  (if (null ans) (setq ans (if *pc-cmd-reactor* "Off" "On")))
  (cond
    ((= ans "On")
     (pc:watch-on)
     (setenv "C3DPerfCheckWatch" "1")
     (princ "\nPERFWATCH is ON (stays on in new drawings and sessions). Work normally, then type PERFLOG."))
    ((= ans "Off")
     (pc:watch-off)
     (setenv "C3DPerfCheckWatch" "0")
     (princ "\nPERFWATCH is OFF."))
    ((= ans "Threshold")
     (initget 6)
     (if (setq v (getint (strcat "\nWarn when a command takes longer than how many milliseconds <"
                                 (itoa (pc:warn-ms)) ">: ")))
       (setenv "C3DPerfCheckWarnMs" (itoa (max 100 v))))
     (princ (strcat "\nWarning threshold: " (pc:sec (pc:warn-ms)))))
  )
  (princ)
)

(defun C:PERFLOG (/ k fn f l lines tot)
  (if (null *pc-stats*)
    (princ (strcat "\nNothing recorded yet in this drawing."
                   (if *pc-cmd-reactor* "" " Turn on PERFWATCH first,")
                   " work normally for a while, then run PERFLOG again."))
    (progn
      (princ "\n--- Slowest commands this session (this drawing) ---")
      (princ (strcat "\n  " (pc:pad "Command" 32) (pc:lpad "Runs" 6) (pc:lpad "Avg s" 9)
                     (pc:lpad "Max s" 9) (pc:lpad "Total s" 10)))
      (setq k 0)
      (foreach x (vl-sort *pc-stats* '(lambda (a b) (> (cadddr a) (cadddr b))))
        (if (< k 20)
          (princ (strcat "\n  " (pc:pad (car x) 32) (pc:lpad (itoa (cadr x)) 6)
                         (pc:lpad (rtos (/ (caddr x) (cadr x) 1000.0) 2 2) 9)
                         (pc:lpad (rtos (/ (cadddr x) 1000.0) 2 2) 9)
                         (pc:lpad (rtos (/ (caddr x) 1000.0) 2 1) 10))))
        (setq k (1+ k))
      )
      (setq tot 0)
      (foreach x *pc-stats* (setq tot (+ tot (caddr x))))
      (princ (strcat "\n  Time spent waiting on commands: " (pc:sec tot)))
    )
  )
  (setq fn (strcat (pc:dir) "\\SlowCommands.log"))
  (if (setq f (open fn "r"))
    (progn
      (while (setq l (read-line f))
        (setq lines (cons l lines))
        (if (> (length lines) 12) (setq lines (reverse (cdr (reverse lines)))))
      )
      (close f)
      (princ (strcat "\n--- Last slow commands from all sessions (" fn ") ---"))
      (foreach x (reverse lines) (princ (strcat "\n  " x)))
    )
  )
  (princ)
)

;;; ================================================================ PERFFIX

(defun pc:bak-file () (strcat (pc:dir) "\\PerfFix_Backup.txt"))

(defun pc:read-bak (/ f l v r)
  (if (setq f (open (pc:bak-file) "r"))
    (progn
      (while (setq l (read-line f))
        (if (and (setq v (pc:try 'read (list l))) (listp v)) (setq r (cons v r)))
      )
      (close f)
    )
  )
  (reverse r)
)

(defun pc:write-bak (lst / f)
  (if (setq f (open (pc:bak-file) "w"))
    (progn
      (foreach p lst (write-line (vl-prin1-to-string p) f))
      (close f)
      T
    )
  )
)

(defun C:PERFFIX (/ ans bak n)
  (pc:begin)
  (setq *pc-lines* nil *pc-issues* nil
        *pc-ents* (pc:count nil)
        *pc-xrefs* 0)
  (pc:civil-counts)
  (pc:check-settings)
  (setq *pc-lines* nil *pc-issues* nil)
  (if (null *pc-fixes*)
    (princ "\nAll speed-related settings are already OK.")
    (progn
      (princ "\nRecommended setting changes:")
      (foreach p (reverse *pc-fixes*)
        (princ (strcat "\n  " (pc:pad (car p) 18) (vl-princ-to-string (getvar (car p)))
                       "  ->  " (vl-princ-to-string (cdr p))))
      )
      (initget "Yes No")
      (setq ans (getkword "\nApply these changes? [Yes/No] <Yes>: "))
      (if (/= ans "No")
        (progn
          ;; keep the ORIGINAL values: only add variables not already backed up
          (setq bak (pc:read-bak))
          (foreach p *pc-fixes*
            (if (not (assoc (car p) bak))
              (setq bak (cons (cons (car p) (getvar (car p))) bak)))
          )
          (pc:write-bak bak)
          (setq n 0)
          (foreach p *pc-fixes*
            (if (vl-catch-all-error-p (vl-catch-all-apply 'setvar (list (car p) (cdr p))))
              (princ (strcat "\n  Could not set " (car p)))
              (setq n (1+ n))
            )
          )
          (princ (strcat "\nChanged " (itoa n) " setting(s). Old values saved - type PERFUNDO to put them back."))
        )
      )
    )
  )
  ;; Drawing cleanup (changes the drawing, so ask separately)
  (setq n (pc:tblcount "APPID"))
  (if (> n 300)
    (progn
      (initget "Yes No")
      (if (= "Yes" (getkword (strcat "\nThis drawing has " (itoa n)
                                     " registered applications. Purge the unused ones? [Yes/No] <No>: ")))
        (progn
          (command "_.-PURGE" "_R" "*")
          (while (> (getvar "CMDACTIVE") 0) (command "_N"))
          (princ (strcat "\nREGAPPs: " (itoa n) " -> " (itoa (pc:tblcount "APPID"))))
        )
      )
    )
  )
  (setq n (pc:dictcount "ACAD_SCALELIST"))
  (if (> n 60)
    (progn
      (initget "Yes No")
      (if (= "Yes" (getkword (strcat "\nThis drawing has " (itoa n)
                                     " annotation scales. Reset the scale list to the default? [Yes/No] <No>: ")))
        (progn
          (command "_.-SCALELISTEDIT" "_R" "_Y")
          (while (> (getvar "CMDACTIVE") 0) (command "_E"))
          (princ (strcat "\nScales: " (itoa n) " -> " (itoa (pc:dictcount "ACAD_SCALELIST"))))
        )
      )
    )
  )
  (if (> (pc:dictcount "ACAD_DGNLINESTYLECOMP") 0)
    (princ "\nDGN linestyle junk found: run PURGE, tick 'Purge orphaned data', click Purge All, then save."))
  (pc:finish)
)

(defun C:PERFUNDO (/ bak n)
  (setq bak (pc:read-bak) n 0)
  (if (null bak)
    (princ "\nNothing to undo - PERFFIX has not changed any settings.")
    (progn
      (foreach p bak
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'setvar (list (car p) (cdr p)))))
          (progn
            (setq n (1+ n))
            (princ (strcat "\n  " (pc:pad (car p) 18) "restored to " (vl-princ-to-string (cdr p))))
          )
        )
      )
      (vl-file-delete (pc:bak-file))
      (princ (strcat "\nRestored " (itoa n) " setting(s)."))
    )
  )
  (princ)
)

(defun C:PERFHELP ()
  (princ "\n--- C3D PerfCheck commands ---")
  (princ "\n  PERFCHECK   Full check + benchmarks (30-60 s). Opens a report in Notepad.")
  (princ "\n  PERFQUICK   Quick 'right now' check (5-10 s).")
  (princ "\n  PERFWATCH   Time every command while you work; warns when one is slow.")
  (princ "\n  PERFLOG     Show the slowest commands recorded by PERFWATCH.")
  (princ "\n  PERFFIX     Apply the recommended settings (old values are backed up).")
  (princ "\n  PERFUNDO    Put back the settings PERFFIX changed.")
  (princ (strcat "\n  Reports and history: " (pc:dir)))
  (princ)
)

;;; ------------------------------------------------------------------ load

(if (= (getenv "C3DPerfCheckWatch") "1") (pc:watch-on))

(princ (strcat "\nC3D PerfCheck " *pc-version* " loaded:  PERFCHECK (full)  |  PERFQUICK  |  PERFWATCH  |  PERFLOG  |  PERFFIX  |  PERFHELP"
               (if *pc-cmd-reactor* "   [PERFWATCH is ON]" "")))
(princ)
