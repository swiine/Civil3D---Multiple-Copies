;;; ==========================================================================
;;;  MultiClip.lsp  v2.1  -  Nine numbered clipboards for AutoCAD / Civil 3D
;;; --------------------------------------------------------------------------
;;;  Load with APPLOAD (or add to the Startup Suite). AutoCAD / Civil 3D 2021+.
;;;
;;;  COMMANDS
;;;    C1..C9    Copy the selection into slot 1..9
;;;    P1..P9    Paste slot 1..9: choose Original coordinates or Select point
;;;    PO1..PO9  Paste slot 1..9 at its original coordinates
;;;    MC        Copy, then press 1-9 to pick the slot  (Enter = normal Ctrl+C)
;;;    MCB       Copy with a base point you pick, then press 1-9
;;;    MV        Paste, then press 1-9 to pick the slot (Enter = normal Ctrl+V)
;;;    MVO       Paste at original coordinates, then press 1-9
;;;    MCLIST    Show what is in each slot
;;;    MCCLEAR   Empty all slots
;;;    MCDIAG    Self-test, saves a report (troubleshooting)
;;;
;;;  Slots are small DWG files in %LOCALAPPDATA%\C3D-MultiClip, so they work
;;;  across drawings and survive restarting AutoCAD.
;;;
;;;  What changed in 2.0
;;;   - A copy is written to a temporary file first and only replaces the slot
;;;     when it succeeded, so a failed copy no longer wipes the slot.
;;;   - Every paste inserts a uniquely named temporary copy of the slot file.
;;;     The old version inserted "MultiClip_SlotN" each time, and when a block
;;;     of that name was left in the drawing, -INSERT asked "Redefine it?" and
;;;     every following answer went into the wrong prompt, so the paste failed.
;;;   - UCS is only switched when you are not already in World UCS, and is
;;;     always restored, also after Esc or an error.
;;;   - The -WBLOCK fallback only runs OOPS when -WBLOCK really erased the
;;;     originals, so it can never bring back unrelated erased objects.
;;;   - Esc while dragging a paste removes the half-done paste (one undo step).
;;;   - Leftover temporary files and blocks are cleaned up.
;;; ==========================================================================

(vl-load-com)
(setq *mc-version* "2.1")

;;; ------------------------------------------------------------ basics

(defun mc:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))

(defun mc:dir (/ root dir)
  (setq root (vl-some '(lambda (v) (if (and v (vl-file-directory-p (vl-string-right-trim "\\/" v))) v))
                      (list (getenv "LOCALAPPDATA") (getenv "TEMP") (getvar "TEMPPREFIX"))))
  (setq dir (strcat (vl-string-right-trim "\\/" root) "\\C3D-MultiClip"))
  (if (not (vl-file-directory-p dir)) (vl-mkdir dir))
  dir
)

(defun mc:blkname (n) (strcat "MultiClip_Slot" (itoa n)))
(defun mc:dwg  (n) (strcat (mc:dir) "\\" (mc:blkname n) ".dwg"))
(defun mc:info (n) (strcat (mc:dir) "\\" (mc:blkname n) ".txt"))

;; Unique id for temporary file / block / selection-set names.
(defun mc:uid ()
  (setq *mc-ctr* (1+ (if *mc-ctr* *mc-ctr* 0)))
  (strcat (itoa (getvar "MILLISECS")) "_" (itoa *mc-ctr*))
)

(defun mc:exists (f) (and f (findfile f)))
(defun mc:delfile (f) (if (mc:exists f) (vl-file-delete f)))

(defun mc:pt->str (p)
  (strcat "(" (rtos (car p) 2 8) " " (rtos (cadr p) 2 8) " " (rtos (caddr p) 2 8) ")")
)

;;; ------------------------------------------------------------ slot info

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

;; (basepoint count sourceDwg time), or nil when the slot is empty.
(defun mc:read-info (n / f r)
  (cond
    ((not (mc:exists (mc:dwg n))) nil)
    ((not (setq f (open (mc:info n) "r"))) (list '(0.0 0.0 0.0) 0 "?" "?"))
    (T
     (setq r (vl-catch-all-apply
               '(lambda (/ p)
                  (setq p (read (read-line f)))
                  (if (not (and (listp p) (= (length p) 3) (vl-every 'numberp p)))
                    (setq p '(0.0 0.0 0.0)))
                  (list p
                        (atoi (cond ((read-line f)) ("0")))
                        (cond ((read-line f)) ("?"))
                        (cond ((read-line f)) ("?"))))))
     (close f)
     (if (vl-catch-all-error-p r) (list '(0.0 0.0 0.0) 0 "?" "?") r)
    )
  )
)

(defun mc:slot-text (n / i)
  (if (setq i (mc:read-info n))
    (strcat (itoa (cadr i)) " object(s) from " (caddr i) "  [" (cadddr i) "]")
    "(empty)"
  )
)

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

;;; ------------------------------------------------------------ error / state

;; Globals that tell the error handler what to clean up:
;;   *mc-ucs*  T while we switched to World UCS
;;   *mc-undo* T while an undo group for a paste is open
;;   *mc-tmp*  temporary file to delete
;;   *mc-tblk* temporary block definition to delete

(defun mc:err (msg)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
    (princ (strcat "\nMultiClip error: " msg))
  )
  (if *mc-ucs*  (progn (setq *mc-ucs* nil) (command-s "_.UCS" "_P")))
  (if *mc-undo*
    (progn
      (setq *mc-undo* nil)
      (command-s "_.UNDO" "_End")
      (command-s "_.U")                    ; take back the half-finished paste
      (princ "\nPaste cancelled.")
    )
  )
  (mc:cleanup)
  (mc:finish)
)

(defun mc:cleanup ()
  (if *mc-tmp*  (progn (mc:delfile *mc-tmp*) (setq *mc-tmp* nil)))
  (if *mc-tblk* (progn (mc:del-block *mc-tblk*) (setq *mc-tblk* nil)))
)

(defun mc:begin ()
  (setq *mc-olderr*  *error*
        *error*      mc:err
        *mc-cmdecho* (getvar "CMDECHO")
        *mc-undo*    nil
        *mc-ucs*     nil
        *mc-tmp*     nil
        *mc-tblk*    nil)
  (setvar "CMDECHO" 0)
)

(defun mc:finish ()
  (if *mc-cmdecho* (setvar "CMDECHO" *mc-cmdecho*))
  (setq *error* *mc-olderr* *mc-olderr* nil *mc-cmdecho* nil)
  (princ)
)

;; Switch to World UCS only if needed.
(defun mc:ucs-world ()
  (if (= (getvar "WORLDUCS") 0)
    (progn (setq *mc-ucs* T) (command "_.UCS" "_W"))
  )
)
(defun mc:ucs-restore ()
  (if *mc-ucs* (progn (command "_.UCS" "_P") (setq *mc-ucs* nil)))
)

(defun mc:finish-cmd () (while (> (getvar "CMDACTIVE") 0) (command "")))

;;; ------------------------------------------------------------ helpers

;; Wait for one key. Returns 1-9, or 0 for Enter / Space / right-click.
(defun mc:getslot (msg / g k res)
  (princ msg)
  (while (null res)
    (setq g (grread nil 2))
    (cond
      ((= (car g) 2)
       (setq k (cadr g))
       (cond
         ((and (>= k 49) (<= k 57)) (setq res (- k 48)))
         ((member k '(13 32))       (setq res 0))
         ((= k 27)                  (exit))
       )
      )
      ((member (car g) '(11 25)) (setq res 0))
    )
  )
  (if (> res 0) (princ (itoa res)))
  res
)

(defun mc:implied () (ssget "_I"))

(defun mc:ss->objs (ss / i l)
  (setq i (sslength ss))
  (while (> i 0)
    (setq i (1- i)
          l (cons (vlax-ename->vla-object (ssname ss i)) l))
  )
  l
)

;; Lower-left corner (WCS) of everything that can report a bounding box.
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

(defun mc:del-block (name / b)
  (setq b (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks (mc:doc)) name)))
  (if (not (vl-catch-all-error-p b))
    (vl-catch-all-apply 'vla-Delete (list b))
  )
)

;; Main entity after which new entities will be appended.
(defun mc:lastent () (entlast))

;; Every main entity created after LAST.
(defun mc:new-since (last / ss e)
  (setq ss (ssadd)
        e  (if last (entnext last) (entnext)))
  (while e
    (if (not (member (cdr (assoc 0 (entget e))) '("VERTEX" "SEQEND" "ATTRIB")))
      (ssadd e ss)
    )
    (setq e (entnext e))
  )
  ss
)

;;; ------------------------------------------------------------ export / import

;; Write OBJS to FILE with Document.Wblock (keeps originals, keeps WCS coords).
;; Returns T when the file exists afterwards.
(defun mc:wblock-ax (file objs / sets sso arr r)
  (setq sets (vla-get-SelectionSets (mc:doc)))
  (setq r (vl-catch-all-apply
            '(lambda ()
               (setq sso (vla-Add sets (strcat "MC_SS_" (mc:uid))))
               (setq arr (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objs)))))
               (vlax-safearray-fill arr objs)
               (vla-AddItems sso arr)
               (vla-Wblock (mc:doc) file sso))))
  (if sso (vl-catch-all-apply 'vla-Delete (list sso)))
  (if (vl-catch-all-error-p r)
    (setq *mc-lasterr* (vl-catch-all-error-message r))
    (setq *mc-lasterr* nil)
  )
  (if (mc:exists file) T)
)

;; Fallback: -WBLOCK with base 0,0,0. -WBLOCK erases the originals, so they
;; are restored with OOPS - but only if they really were erased.
(defun mc:wblock-cmd (file ss / e0)
  (setq e0 (ssname ss 0))
  (mc:ucs-world)
  (command "_.-WBLOCK" file "" "_non" "0,0,0" ss "")
  (mc:finish-cmd)
  (if (not (entget e0)) (command "_.OOPS"))
  (mc:ucs-restore)
  (if (mc:exists file) T)
)

;; Insert FILE exploded at WCS 0,0,0, scale 1, rotation 0 so every object lands
;; on its original coordinates. FILE must have a unique name. Returns the
;; selection set of new objects.
(defun mc:insert-file (file / last bname)
  (setq bname (vl-filename-base file)
        *mc-tblk* bname
        last (mc:lastent))
  (mc:ucs-world)
  (command "_.-INSERT" (strcat "*" file) "_non" "0,0,0" "1" "0")
  (mc:finish-cmd)
  (mc:ucs-restore)
  (mc:del-block bname)
  (setq *mc-tblk* nil)
  (mc:new-since last)
)

;;; ------------------------------------------------------------ copy

(defun mc:copy (n ss base / objs file tmp ok)
  (setq objs (mc:ss->objs ss)
        file (mc:dwg n)
        tmp  (strcat (mc:dir) "\\MC_copy_" (mc:uid) ".dwg")
        *mc-tmp* tmp)
  (if (not base) (setq base (mc:lowerleft objs)))
  (if (not base) (setq base '(0.0 0.0 0.0)))
  (setq base (list (car base) (cadr base) 0.0))   ; drag only in X/Y

  (setq ok (mc:wblock-ax tmp objs))
  (if (not ok)
    (progn
      (princ (strcat "\nMultiClip: ActiveX export failed"
                     (if *mc-lasterr* (strcat " (" *mc-lasterr* ")") "")
                     ", trying -WBLOCK..."))
      (setq ok (mc:wblock-cmd tmp ss))
    )
  )
  (if (not ok)
    (progn
      (princ (strcat "\nCopy FAILED - nothing was written. Slot " (itoa n) " is unchanged."))
      (exit)
    )
  )

  ;; Swap the new file in.
  (mc:delfile file)
  (if (mc:exists file)
    (progn
      (princ (strcat "\nCopy FAILED - cannot replace " file " (is it open somewhere?)."))
      (exit)
    )
  )
  (if (not (vl-file-rename tmp file))
    (progn
      (princ (strcat "\nCopy FAILED - could not save " file "."))
      (exit)
    )
  )
  (setq *mc-tmp* nil)
  (if (not (mc:write-info n base (length objs)))
    (princ (strcat "\nWarning: could not write " (mc:info n)))
  )
  (princ (strcat "\nCopied " (itoa (length objs)) " object(s) to slot " (itoa n) "."))
)

(defun mc:do-copy (n ss askbase / base p)
  (if (not ss) (setq ss (ssget)))
  (if (not ss) (progn (princ "\nNothing selected.") (exit)))
  (if askbase
    (progn
      (initget 1)
      (setq p (getpoint "\nSpecify base point: "))
      (setq base (trans p 1 0))
    )
  )
  (mc:copy n ss base)
  (sssetfirst nil nil)
)

;;; ------------------------------------------------------------ paste

(defun mc:paste (n orig / file info base tmp ss)
  (setq file (mc:dwg n))
  (if (not (setq info (mc:read-info n)))
    (progn
      (princ (strcat "\nSlot " (itoa n) " is empty. Copy into it first with C" (itoa n)
                     " (Ctrl+C only fills the normal clipboard)."))
      (exit)
    )
  )
  (setq base (car info))

  ;; Insert a uniquely named copy so the block name never clashes with
  ;; anything already in the drawing and the slot file is never locked.
  (setq tmp (strcat (mc:dir) "\\MC_paste" (itoa n) "_" (mc:uid) ".dwg")
        *mc-tmp* tmp)
  (if (not (vl-file-copy file tmp))
    (progn (princ (strcat "\nCould not read " file ".")) (exit))
  )

  (sssetfirst nil nil)
  (command "_.UNDO" "_BEgin")
  (setq *mc-undo* T)

  (setq ss (mc:insert-file tmp))
  (mc:delfile tmp)
  (setq *mc-tmp* nil)

  (if (= (sslength ss) 0)
    (progn
      (command "_.UNDO" "_End")
      (setq *mc-undo* nil)
      (princ "\nNothing was pasted. Run MCDIAG to see why.")
      (exit)
    )
  )

  (if (not orig)
    (progn
      (princ (strcat "\nPaste slot " (itoa n) " - specify insertion point: "))
      (command "_.MOVE" ss "" "_non" (trans base 0 1) pause)
    )
  )

  (command "_.UNDO" "_End")
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

(defun mc:log (s) (setq *mc-diag* (cons s *mc-diag*)) (princ (strcat "\n" s)))
(defun mc:yn (x) (if x "OK" "FAILED"))
(defun mc:fsize (f / s) (if (setq s (vl-file-size f)) (strcat (itoa s) " bytes") "missing"))

(defun C:MCDIAG (/ dir f test ss tmp objs n info logf f1 f2 new cnt)
  (mc:begin)
  (setq *mc-diag* nil)
  (mc:log (strcat "=== MultiClip " *mc-version* " diagnostics  "
                  (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)") " ==="))
  (mc:log (strcat "Product: " (getvar "PRODUCT") "  ACADVER: " (getvar "ACADVER")
                  "  Drawing: " (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (mc:log (strcat "Space: " (if (= (getvar "CVPORT") 1) "Paper" "Model")
                  "  WORLDUCS: " (itoa (getvar "WORLDUCS"))
                  "  PICKFIRST: " (itoa (getvar "PICKFIRST"))
                  "  UNDOCTL: " (itoa (getvar "UNDOCTL"))
                  "  INSUNITS: " (itoa (getvar "INSUNITS"))))

  (mc:log "--- 1. Slot folder ---")
  (setq dir (mc:dir))
  (mc:log (strcat "Folder: " dir "  exists: " (mc:yn (vl-file-directory-p dir))))
  (setq test (strcat dir "\\MC_writetest.txt"))
  (if (setq f (open test "w")) (progn (write-line "test" f) (close f)))
  (mc:log (strcat "Can write to folder: " (mc:yn (mc:exists test))))
  (mc:delfile test)

  (mc:log "--- 2. Slots ---")
  (setq n 0)
  (repeat 9
    (setq n (1+ n))
    (if (setq info (mc:read-info n))
      (mc:log (strcat "Slot " (itoa n) ": dwg " (mc:fsize (mc:dwg n))
                      ", base " (mc:pt->str (car info)) ", " (mc:slot-text n)))
    )
  )

  (mc:log "--- 3. Copy test ---")
  (princ "\nSelect objects to test with <Enter = temporary line>: ")
  (setq ss (ssget))
  (if (not ss)
    (progn
      (entmake (list '(0 . "LINE") '(10 0.0 0.0 0.0) '(11 10.0 10.0 0.0)))
      (setq tmp (entlast) ss (ssadd tmp))
      (mc:log "Using a temporary LINE (0,0)-(10,10)")
    )
    (mc:log (strcat "Using " (itoa (sslength ss)) " selected object(s)"))
  )
  (setq objs (mc:ss->objs ss))
  (mc:log (strcat "Types: " (vl-princ-to-string (mapcar 'vla-get-ObjectName objs))))
  (mc:log (strcat "Lower-left: " (vl-princ-to-string (mc:lowerleft objs))))

  (setq f1 (strcat dir "\\MC_diagA_" (mc:uid) ".dwg"))
  (mc:log (strcat "ActiveX Wblock: " (mc:yn (mc:wblock-ax f1 objs))
                  (if *mc-lasterr* (strcat " (" *mc-lasterr* ")") "")
                  ", file " (mc:fsize f1)))
  (setq f2 (strcat dir "\\MC_diagB_" (mc:uid) ".dwg"))
  (mc:log (strcat "-WBLOCK fallback: " (mc:yn (mc:wblock-cmd f2 ss))
                  ", file " (mc:fsize f2)
                  ", originals still there: " (mc:yn (entget (ssname ss 0)))))

  (mc:log "--- 4. Paste test (undone afterwards) ---")
  (setq f (cond ((mc:exists f1) f1) ((mc:exists f2) f2)))
  (if f
    (progn
      (command "_.UNDO" "_BEgin")
      (setq new (vl-catch-all-apply 'mc:insert-file (list f)))
      (command "_.UNDO" "_End")
      (setq cnt (if (vl-catch-all-error-p new) -1 (sslength new)))
      (mc:log (strcat "-INSERT *: "
                      (if (vl-catch-all-error-p new)
                        (strcat "ERROR " (vl-catch-all-error-message new))
                        (strcat (itoa cnt) " new object(s), expected " (itoa (length objs))))))
      (command "_.U")
      (mc:log "(test paste undone)")
    )
    (mc:log "Skipped - no test file could be written, so copying is what is broken.")
  )
  (mc:delfile f1)
  (mc:delfile f2)
  (if (and tmp (entget tmp)) (entdel tmp))

  (setq logf (strcat dir "\\MultiClip_Diag.txt"))
  (if (setq f (open logf "w"))
    (progn
      (foreach l (reverse *mc-diag*) (write-line l f))
      (close f)
      (princ (strcat "\n\nReport saved to: " logf))
      (startapp "notepad" (strcat "\"" logf "\""))
    )
    (princ "\n\nCould not save the report file - copy the lines above instead.")
  )
  (setq *mc-diag* nil)
  (mc:finish)
)

;;; ------------------------------------------------------------ commands

(defun C:MC (/ ss n)
  (mc:begin)
  (setq ss (mc:implied))
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nCopy to slot [1-9] <Enter = normal copy>: "))
  (if (= n 0) (mc:run-native "_.COPYCLIP" ss) (mc:do-copy n ss nil))
  (mc:finish)
)

(defun C:MCB (/ ss n)
  (mc:begin)
  (setq ss (mc:implied))
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nCopy with base point to slot [1-9] <Enter = normal copybase>: "))
  (if (= n 0) (mc:run-native "_.COPYBASE" nil) (mc:do-copy n ss T))
  (mc:finish)
)

(defun C:MV (/ n)
  (mc:begin)
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nPaste slot [1-9] <Enter = normal paste>: "))
  (if (= n 0) (mc:run-native "_.PASTECLIP" nil) (mc:paste n nil))
  (mc:finish)
)

(defun C:MVO (/ n)
  (mc:begin)
  (princ (strcat "\nSlots:" (mc:summary)))
  (setq n (mc:getslot "\nPaste at original coordinates, slot [1-9] <Enter = normal>: "))
  (if (= n 0) (mc:run-native "_.PASTEORIG" nil) (mc:paste n T))
  (mc:finish)
)

(defun C:MCLIST (/ n)
  (princ (strcat "\n--- MultiClip slots ---  (" (mc:dir) ")"))
  (setq n 0)
  (repeat 9
    (setq n (1+ n))
    (princ (strcat "\n  " (itoa n) ": " (mc:slot-text n)))
  )
  (princ)
)

(defun C:MCCLEAR (/ n dir)
  (setq n 0 dir (mc:dir))
  (repeat 9
    (setq n (1+ n))
    (mc:delfile (mc:dwg n))
    (mc:delfile (mc:info n))
  )
  ;; leftovers from interrupted copies / pastes
  (foreach f (vl-directory-files dir "MC_*.dwg" 1) (mc:delfile (strcat dir "\\" f)))
  (princ "\nAll MultiClip slots cleared.")
  (princ)
)

(defun mc:cmd-copy  (n)      (mc:begin) (mc:do-copy n (mc:implied) nil) (mc:finish))
(defun mc:cmd-paste (n orig) (mc:begin) (mc:paste n orig) (mc:finish))

;; P1..P9: ask "Original coordinates" or "Select" (pick a point).
;; The last answer becomes the default and is remembered between sessions.
(defun mc:cmd-paste-ask (n / def k)
  (mc:begin)
  (if (not (mc:read-info n))
    (mc:paste n nil)                       ; prints the "slot is empty" message
    (progn
      (setq def (if (= (getenv "MultiClipPasteMode") "Original") "Original" "Select"))
      (initget "Original Select")   ; type O or S, or click an option
      (setq k (getkword (strcat "\nPaste slot " (itoa n) " [Original/Select] <"
                                def ">: ")))
      (if (not k) (setq k def))
      (setenv "MultiClipPasteMode" k)
      (mc:paste n (= k "Original"))
    )
  )
  (mc:finish)
)

;; C1..C9, P1..P9, PO1..PO9
(foreach n '(1 2 3 4 5 6 7 8 9)
  (eval (list 'defun (read (strcat "C:C"  (itoa n))) '() (list 'mc:cmd-copy  n)))
  (eval (list 'defun (read (strcat "C:P"  (itoa n))) '() (list 'mc:cmd-paste-ask n)))
  (eval (list 'defun (read (strcat "C:PO" (itoa n))) '() (list 'mc:cmd-paste n T)))
)

(princ (strcat "\nMultiClip " *mc-version* " loaded:  C1-C9 copy | P1-P9 paste (Original/Select) | PO1-PO9 paste at original"
               " | MC / MV pick a slot | MCLIST | MCCLEAR | MCDIAG"))
(princ)
