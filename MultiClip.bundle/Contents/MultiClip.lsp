;;; ==========================================================================
;;;  MultiClip.lsp  -  Numbered clipboard slots for AutoCAD / Civil 3D
;;; --------------------------------------------------------------------------
;;;  Gives you 9 clipboards (slots 1-9) instead of one. Copy several things
;;;  one after another, then switch to another drawing and paste each one,
;;;  without going back and forth.
;;;
;;;  COMMANDS
;;;    MC        Copy.  Press 1-9 to choose a slot.  (Enter = normal Ctrl+C)
;;;    MCB       Copy with a base point you pick.  Press 1-9 to choose a slot.
;;;    MV        Paste. Press 1-9 to choose a slot.  (Enter = normal Ctrl+V)
;;;    MVO       Paste to original coordinates.  Press 1-9 to choose a slot.
;;;    C1..C9    Copy straight into slot 1..9
;;;    P1..P9    Paste slot 1..9 (drag into place)
;;;    PO1..PO9  Paste slot 1..9 at original coordinates
;;;    MCLIST    Show what is in each slot
;;;    MCCLEAR   Empty all slots
;;;    MCDIAG    Run a self-test and save a report (for troubleshooting)
;;;
;;;  Slots are saved as small DWG files in %LOCALAPPDATA%\C3D-MultiClip, so
;;;  they work across drawings and survive closing Civil 3D.
;;;
;;;  Elevations are preserved: the drag base point is the lower-left corner
;;;  of the copied objects at Z = 0, so a paste only moves things in X/Y.
;;; ==========================================================================

(vl-load-com)

(setq *mc-version* "1.1")

;;; ---------------------------------------------------------------- storage

(defun mc:dir (/ root dir)
  (setq root (or (getenv "LOCALAPPDATA") (getenv "TEMP") (getvar "TEMPPREFIX")))
  (setq dir (strcat (vl-string-right-trim "\\/" root) "\\C3D-MultiClip"))
  (if (not (vl-file-directory-p dir)) (vl-mkdir dir))
  dir
)

(defun mc:blkname (n) (strcat "MultiClip_Slot" (itoa n)))
(defun mc:dwg (n)  (strcat (mc:dir) "\\" (mc:blkname n) ".dwg"))
(defun mc:info (n) (strcat (mc:dir) "\\" (mc:blkname n) ".txt"))

(defun mc:pt->str (p)
  (strcat "(" (rtos (car p) 2 8) " " (rtos (cadr p) 2 8) " " (rtos (caddr p) 2 8) ")")
)

(defun mc:write-info (n base cnt / f)
  (if (setq f (open (mc:info n) "w"))
    (progn
      (write-line (mc:pt->str base) f)
      (write-line (itoa cnt) f)
      (write-line (getvar "DWGNAME") f)
      (write-line (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM)") f)
      (close f)
      T
    )
  )
)

;;; Returns (basepoint count sourceDwg time) or nil when the slot is empty.
(defun mc:read-info (n / f r)
  (cond
   ((not (findfile (mc:dwg n))) nil)
   ((not (setq f (open (mc:info n) "r"))) (list '(0.0 0.0 0.0) 0 "?" "?"))
   (T
      (setq r (vl-catch-all-apply
                '(lambda ()
                   (list (read (read-line f))
                         (atoi (read-line f))
                         (read-line f)
                         (read-line f)))))
      (close f)
      (if (vl-catch-all-error-p r)
        (list '(0.0 0.0 0.0) 0 "?" "?")
        r
      )
    )
  )
)

(defun mc:slot-text (n / i)
  (if (setq i (mc:read-info n))
    (strcat (itoa (cadr i)) " object(s) from " (caddr i) "  [" (cadddr i) "]")
    "(empty)"
  )
)

(defun mc:list (/ n)
  (princ (strcat "\n--- MultiClip slots ---  (" (mc:dir) ")"))
  (setq n 0)
  (repeat 9
    (setq n (1+ n))
    (princ (strcat "\n  " (itoa n) ": " (mc:slot-text n)))
  )
  (princ)
)

;; One-line summary of filled slots, e.g. "[1] 12 obj  [4] 3 obj"
(defun mc:summary (/ n i s)
  (setq n 0 s "")
  (repeat 9
    (setq n (1+ n))
    (if (setq i (mc:read-info n))
      (setq s (strcat s "  [" (itoa n) "] " (itoa (cadr i)) " obj"))
    )
  )
  (if (= s "") "  (all slots empty)" s)
)

;;; ------------------------------------------------------------ error / env

(defun mc:err (msg)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
    (princ (strcat "\nMultiClip error: " msg))
  )
  (if *mc-ucs* (progn (setq *mc-ucs* nil) (command-s "_.UCS" "_P")))
  (if *mc-undo*
    (progn
      (setq *mc-undo* nil)
      (vla-EndUndoMark (mc:doc))
      (command-s "_.U")            ; remove a half-finished paste
    )
  )
  (mc:finish)
)

(defun mc:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun mc:begin ()
  (setq *mc-olderr* *error*
        *error*     mc:err
        *mc-cmdecho* (getvar "CMDECHO")
        *mc-undo*   nil
        *mc-ucs*    nil)
  (setvar "CMDECHO" 0)
)

(defun mc:finish ()
  (if *mc-cmdecho* (setvar "CMDECHO" *mc-cmdecho*))
  (setq *error* *mc-olderr* *mc-olderr* nil *mc-cmdecho* nil)
  (princ)
)

;;; ------------------------------------------------------------- helpers

;; Wait for a single key press. Returns 1-9, 0 for Enter/Space/right-click.
(defun mc:getslot (msg / g k res)
  (princ msg)
  (while (null res)
    (setq g (grread nil 2))
    (cond
      ((= (car g) 2)
       (setq k (cadr g))
       (cond
         ((and (>= k 49) (<= k 57)) (setq res (- k 48)))   ; keys 1..9
         ((member k '(13 32))       (setq res 0))          ; Enter / Space
         ((= k 27)                  (exit))                ; Esc
       )
      )
      ((member (car g) '(11 25)) (setq res 0))             ; right click
    )
  )
  (if (> res 0) (princ (itoa res)))
  res
)

(defun mc:implied (/ ss)
  (if (setq ss (ssget "_I")) ss)
)

(defun mc:ss->objs (ss / i l)
  (setq i (sslength ss))
  (while (> i 0)
    (setq i (1- i)
          l (cons (vlax-ename->vla-object (ssname ss i)) l))
  )
  l
)

;; Lower-left corner of the selection (WCS). Objects that cannot report a
;; bounding box are skipped.
(defun mc:lowerleft (objs / mn mx r ll)
  (foreach o objs
    (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list o 'mn 'mx)))
    (if (not (vl-catch-all-error-p r))
      (progn
        (setq mn (vlax-safearray->list mn))
        (setq ll (if ll (mapcar 'min ll mn) mn))
      )
    )
  )
  ll
)

(defun mc:del-blockdef (n / b)
  (setq b (vl-catch-all-apply 'vla-Item
            (list (vla-get-Blocks (mc:doc)) (mc:blkname n))))
  (if (not (vl-catch-all-error-p b))
    (vl-catch-all-apply 'vla-Delete (list b))
  )
)

;;; ------------------------------------------------------------------ copy

(defun mc:copy (n ss base / doc objs file sets sso arr r)
  (setq doc  (mc:doc)
        objs (mc:ss->objs ss)
        file (mc:dwg n))
  (if (not base) (setq base (mc:lowerleft objs)))
  (if (not base) (setq base '(0.0 0.0 0.0)))
  (setq base (list (car base) (cadr base) 0.0))

  (if (findfile file) (vl-file-delete file))
  (if (findfile file)
    (progn (princ (strcat "\nCannot overwrite " file " (file in use?).")) (exit))
  )

  ;; Build an ActiveX selection set and write it out. Document.Wblock keeps
  ;; the objects in the drawing (unlike -WBLOCK) and keeps their coordinates.
  (setq sets (vla-get-SelectionSets doc))
  (if (not (vl-catch-all-error-p (setq sso (vl-catch-all-apply 'vla-Item (list sets "MULTICLIP_SS")))))
    (vla-Delete sso)
  )
  (setq sso (vla-Add sets "MULTICLIP_SS"))
  (setq arr (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objs)))))
  (vlax-safearray-fill arr objs)
  (vla-AddItems sso (vlax-make-variant arr))
  (setq r (vl-catch-all-apply 'vla-Wblock (list doc file sso)))
  (vl-catch-all-apply 'vla-Delete (list sso))

  ;; Fallback: if the ActiveX export did not produce a file, use the
  ;; -WBLOCK command (base point = WCS origin) and OOPS to restore the
  ;; originals, which -WBLOCK removes from the drawing.
  (if (not (findfile file))
    (progn
      (if (vl-catch-all-error-p r)
        (princ (strcat "\nMultiClip: ActiveX export failed (" (vl-catch-all-error-message r)
                       "), trying -WBLOCK..."))
      )
      (setq *mc-ucs* T)
      (command "_.UCS" "_W")
      (command "_.-WBLOCK" file "" "_non" "0,0,0" ss "")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (command "_.OOPS")
      (command "_.UCS" "_P")
      (setq *mc-ucs* nil)
    )
  )

  (if (not (findfile file))
    (progn
      (princ (strcat "\nCopy FAILED - slot file was not created:\n  " file))
      (exit)
    )
  )
  (if (not (mc:write-info n base (length objs)))
    (princ (strcat "\nWarning: could not write " (mc:info n)))
  )
  (princ (strcat "\nCopied " (itoa (length objs)) " object(s) to slot " (itoa n) "."))
)

(defun mc:do-copy (n ss askbase / base)
  (if (not ss) (setq ss (ssget)))
  (if (not ss) (progn (princ "\nNothing selected.") (exit)))
  (if askbase
    (progn
      (initget 1)
      (setq base (trans (getpoint "\nSpecify base point: ") 1 0))
    )
  )
  (mc:copy n ss base)
  (sssetfirst nil nil)
)

;;; ----------------------------------------------------------------- paste

(defun mc:paste (n orig / doc info file lastent e ss base)
  (setq doc  (mc:doc)
        file (mc:dwg n))
  (if (not (setq info (mc:read-info n)))
    (progn
      (princ (strcat "\nSlot " (itoa n) " is empty - copy something into it first with C"
                     (itoa n) " (not Ctrl+C).\n  Looked for: " file))
      (exit))
  )
  (setq base (car info)
        lastent (entlast))
  (sssetfirst nil nil)
  (mc:del-blockdef n)

  (vla-StartUndoMark doc)
  (setq *mc-undo* T)

  ;; Insert exploded ("*" prefix) at the WCS origin with no rotation, so
  ;; every object lands back on its original coordinates.
  (setq *mc-ucs* T)
  (command "_.UCS" "_W")
  (command "_.-INSERT" (strcat "*" file) "_non" "0,0,0" "1" "0")
  (command "_.UCS" "_P")
  (setq *mc-ucs* nil)
  (mc:del-blockdef n)

  ;; Collect the newly created objects.
  (setq ss (ssadd)
        e  (if lastent (entnext lastent) (entnext)))
  (while e
    (if (not (member (cdr (assoc 0 (entget e))) '("VERTEX" "SEQEND" "ATTRIB")))
      (ssadd e ss)
    )
    (setq e (entnext e))
  )

  (if (= (sslength ss) 0)
    (progn (princ "\nNothing was pasted.") (exit))
  )

  (if (not orig)
    (progn
      (princ (strcat "\nPaste slot " (itoa n) " - specify insertion point: "))
      (command "_.MOVE" ss "" "_non" (trans base 0 1) pause)
    )
  )

  (vla-EndUndoMark doc)
  (setq *mc-undo* nil)
  (princ (strcat "\nPasted " (itoa (sslength ss)) " object(s) from slot " (itoa n)
                 (if orig " at original coordinates." ".")))
)

(defun mc:run-native (cmd ss)
  (if ss
    (command cmd ss "")
    (progn
      (command cmd)
      (while (> (getvar "CMDACTIVE") 0) (command pause))
    )
  )
)

;;; ------------------------------------------------------------ diagnostics
;;; MCDIAG: checks every step MultiClip relies on, prints the results and
;;; saves them to MultiClip_Diag.txt (opened in Notepad) so they can be shared.

(defun mc:log (s) (setq *mc-diag* (cons s *mc-diag*)) (princ (strcat "\n" s)))
(defun mc:yn (x) (if x "OK" "FAILED"))
(defun mc:fsize (f / s) (if (setq s (vl-file-size f)) (strcat (itoa s) " bytes") "missing"))

(defun C:MCDIAG (/ dir f test ss tmp objs r n lastent e cnt info logf)
  (mc:begin)
  (setq *mc-diag* nil)
  (mc:log (strcat "=== MultiClip " *mc-version* " diagnostics  "
                  (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)") " ==="))
  (mc:log (strcat "Product: " (getvar "PRODUCT") "  ACADVER: " (getvar "ACADVER")
                  "  Drawing: " (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (mc:log (strcat "Space: " (if (= (getvar "CVPORT") 1) "Paper" "Model")
                  "  WORLDUCS: " (itoa (getvar "WORLDUCS"))
                  "  PICKFIRST: " (itoa (getvar "PICKFIRST"))
                  "  FILEDIA: " (itoa (getvar "FILEDIA"))
                  "  SECURELOAD: " (vl-princ-to-string (getvar "SECURELOAD"))
                  "  INSUNITS: " (itoa (getvar "INSUNITS"))))

  ;; 1. Folder
  (mc:log "--- 1. Slot folder ---")
  (mc:log (strcat "LOCALAPPDATA = " (vl-princ-to-string (getenv "LOCALAPPDATA"))))
  (mc:log (strcat "TEMP         = " (vl-princ-to-string (getenv "TEMP"))))
  (setq dir (mc:dir))
  (mc:log (strcat "Folder: " dir "  exists: " (mc:yn (vl-file-directory-p dir))))
  (setq test (strcat dir "\\MultiClip_WriteTest.txt"))
  (if (setq f (open test "w")) (progn (write-line "test" f) (close f)))
  (mc:log (strcat "Can write to folder: " (mc:yn (findfile test))))
  (if (findfile test) (vl-file-delete test))

  ;; 2. Commands loaded
  (mc:log "--- 2. Commands ---")
  (mc:log (strcat "C1: " (mc:yn (= (type C:C1) 'USUBR))
                  "  P1: " (mc:yn (= (type C:P1) 'USUBR))
                  "  MC: " (mc:yn (= (type C:MC) 'USUBR))
                  "  MV: " (mc:yn (= (type C:MV) 'USUBR))))

  ;; 3. Slots on disk
  (mc:log "--- 3. Slots ---")
  (setq n 0)
  (repeat 9
    (setq n (1+ n))
    (if (or (findfile (mc:dwg n)) (findfile (mc:info n)))
      (mc:log (strcat "Slot " (itoa n) ": dwg " (mc:fsize (mc:dwg n))
                      ", info " (mc:fsize (mc:info n))
                      (if (setq info (mc:read-info n))
                        (strcat ", base " (mc:pt->str (car info)) ", " (itoa (cadr info))
                                " obj from " (caddr info) " " (cadddr info))
                        "")))
    )
  )
  (mc:log (strcat "Files in folder: " (vl-princ-to-string (vl-directory-files dir "*.*" 1))))

  ;; 4. Export test
  (mc:log "--- 4. Copy test ---")
  (princ "\nSelect objects to test copying <Enter = use a temporary line>: ")
  (setq ss (ssget))
  (if (not ss)
    (progn
      (entmake (list '(0 . "LINE") '(10 0.0 0.0 0.0) '(11 10.0 10.0 0.0)))
      (setq tmp (entlast) ss (ssadd tmp))
      (mc:log "Using temporary LINE (0,0)-(10,10)")
    )
    (mc:log (strcat "Using " (itoa (sslength ss)) " selected object(s)"))
  )
  (setq objs (mc:ss->objs ss))
  (mc:log (strcat "Object types: "
                  (vl-princ-to-string (mapcar 'vla-get-ObjectName objs))))
  (mc:log (strcat "Lower-left: " (vl-princ-to-string (mc:lowerleft objs))))

  ;; 4a. ActiveX Wblock
  (setq f (strcat dir "\\MultiClip_Test.dwg"))
  (if (findfile f) (vl-file-delete f))
  (setq r (vl-catch-all-apply
            '(lambda (/ sets sso arr)
               (setq sets (vla-get-SelectionSets (mc:doc)))
               (if (not (vl-catch-all-error-p (setq sso (vl-catch-all-apply 'vla-Item (list sets "MULTICLIP_SS")))))
                 (vla-Delete sso))
               (setq sso (vla-Add sets "MULTICLIP_SS"))
               (setq arr (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objs)))))
               (vlax-safearray-fill arr objs)
               (vla-AddItems sso (vlax-make-variant arr))
               (vla-Wblock (mc:doc) f sso)
               (vla-Delete sso))))
  (mc:log (strcat "ActiveX Wblock: "
                  (if (vl-catch-all-error-p r) (strcat "ERROR " (vl-catch-all-error-message r)) "no error")
                  ", file " (mc:fsize f)))

  ;; 4b. -WBLOCK fallback
  (setq test (strcat dir "\\MultiClip_Test2.dwg"))
  (if (findfile test) (vl-file-delete test))
  (setq r (vl-catch-all-apply
            '(lambda ()
               (command "_.UCS" "_W")
               (command "_.-WBLOCK" test "" "_non" "0,0,0" ss "")
               (while (> (getvar "CMDACTIVE") 0) (command ""))
               (command "_.OOPS")
               (command "_.UCS" "_P"))))
  (mc:log (strcat "-WBLOCK: "
                  (if (vl-catch-all-error-p r) (strcat "ERROR " (vl-catch-all-error-message r)) "no error")
                  ", file " (mc:fsize test)
                  ", originals still there: " (mc:yn (entget (ssname ss 0)))))

  ;; 5. Insert test (then undone)
  (mc:log "--- 5. Paste test ---")
  (if (not (findfile f)) (setq f test))
  (if (findfile f)
    (progn
      (vla-StartUndoMark (mc:doc))
      (setq lastent (entlast))
      (setq r (vl-catch-all-apply
                '(lambda ()
                   (command "_.UCS" "_W")
                   (command "_.-INSERT" (strcat "*" f) "_non" "0,0,0" "1" "0")
                   (while (> (getvar "CMDACTIVE") 0) (command ""))
                   (command "_.UCS" "_P"))))
      (setq cnt 0 e (if lastent (entnext lastent) (entnext)))
      (while e (setq cnt (1+ cnt) e (entnext e)))
      (vla-EndUndoMark (mc:doc))
      (mc:log (strcat "-INSERT *: "
                      (if (vl-catch-all-error-p r) (strcat "ERROR " (vl-catch-all-error-message r)) "no error")
                      ", new entities: " (itoa cnt)))
      (command "_.U")
      (mc:log "(test paste undone)")
    )
    (mc:log "Skipped - no test file was created, so copying is what is broken.")
  )
  (foreach x (list (strcat dir "\\MultiClip_Test.dwg") test)
    (if (findfile x) (vl-file-delete x)))
  (vl-catch-all-apply 'vla-Delete
    (list (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks (mc:doc)) "MultiClip_Test"))))
  (vl-catch-all-apply 'vla-Delete
    (list (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks (mc:doc)) "MultiClip_Test2"))))
  (if (and tmp (entget tmp)) (entdel tmp))

  ;; Save report
  (setq logf (strcat dir "\\MultiClip_Diag.txt"))
  (if (setq f (open logf "w"))
    (progn
      (foreach l (reverse *mc-diag*) (write-line l f))
      (close f)
      (princ (strcat "\n\nReport saved to: " logf))
      (startapp "notepad" (strcat "\"" logf "\""))
    )
    (princ "\n\nCould not save report file - copy the lines above instead.")
  )
  (setq *mc-diag* nil)
  (mc:finish)
)

;;; -------------------------------------------------------------- commands

(defun C:MC (/ ss n)
  (mc:begin)
  (setq ss (mc:implied))
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nCopy to slot [1-9] <Enter = normal copy>: "))
  (if (= n 0)
    (mc:run-native "_.COPYCLIP" ss)
    (mc:do-copy n ss nil)
  )
  (mc:finish)
)

(defun C:MCB (/ ss n)
  (mc:begin)
  (setq ss (mc:implied))
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nCopy with base point to slot [1-9] <Enter = normal copybase>: "))
  (if (= n 0)
    (mc:run-native "_.COPYBASE" nil)
    (mc:do-copy n ss T)
  )
  (mc:finish)
)

(defun C:MV (/ n)
  (mc:begin)
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nPaste slot [1-9] <Enter = normal paste>: "))
  (if (= n 0)
    (mc:run-native "_.PASTECLIP" nil)
    (mc:paste n nil)
  )
  (mc:finish)
)

(defun C:MVO (/ n)
  (mc:begin)
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nPaste to original coordinates, slot [1-9] <Enter = normal>: "))
  (if (= n 0)
    (mc:run-native "_.PASTEORIG" nil)
    (mc:paste n T)
  )
  (mc:finish)
)

(defun C:MCLIST () (mc:list))

(defun C:MCCLEAR (/ n)
  (setq n 0)
  (repeat 9
    (setq n (1+ n))
    (if (findfile (mc:dwg n))  (vl-file-delete (mc:dwg n)))
    (if (findfile (mc:info n)) (vl-file-delete (mc:info n)))
  )
  (princ "\nAll MultiClip slots cleared.")
  (princ)
)

;; C1..C9, P1..P9, PO1..PO9
(defun mc:cmd-copy  (n) (mc:begin) (mc:do-copy n (mc:implied) nil) (mc:finish))
(defun mc:cmd-paste (n orig) (mc:begin) (mc:paste n orig) (mc:finish))

(foreach n '(1 2 3 4 5 6 7 8 9)
  (eval (list 'defun (read (strcat "C:C"  (itoa n))) '() (list 'mc:cmd-copy  n)))
  (eval (list 'defun (read (strcat "C:P"  (itoa n))) '() (list 'mc:cmd-paste n nil)))
  (eval (list 'defun (read (strcat "C:PO" (itoa n))) '() (list 'mc:cmd-paste n T)))
)

(princ (strcat "\nMultiClip " *mc-version* " loaded:  C1-C9 copy  |  P1-P9 paste  |  PO1-PO9 paste original  |  MC / MV pick a slot  |  MCLIST  |  MCDIAG"))
(princ)
