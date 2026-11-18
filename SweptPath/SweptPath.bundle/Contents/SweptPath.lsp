;;; ==========================================================================
;;;  SweptPath.lsp  -  Vehicle swept path / turning analysis for Civil 3D
;;; --------------------------------------------------------------------------
;;;  Drives a design vehicle along a path you draw (line, arc, polyline,
;;;  spline, 3D polyline or Civil 3D alignment) and draws what it sweeps:
;;;  outer tyre paths, the body envelope, vehicle outlines along the way,
;;;  and red markers where the vehicle cannot follow the path (steering or
;;;  articulation over its limit).
;;;
;;;  The front (steering) axle follows the path exactly. Each rear axle and
;;;  trailer follows using the exact tractrix solution, so off-tracking is
;;;  accurate for any path shape, not just simple circular turns.
;;;
;;;  COMMANDS
;;;    SWEPT     Run a swept path along a path you pick
;;;    SWDRIVE   Replay the animation of the last swept path, or drive it
;;;    SWVEH     Vehicle library: list, select, add or delete vehicles
;;;    SWSET     Settings (what gets drawn, outline spacing, units)
;;;    SWHELP    Show the command list
;;;
;;;  Vehicles you add are saved to %APPDATA%\SweptPath\vehicles.lsp
;;; ==========================================================================

(vl-load-com)

(setq *sw-version* "1.1")

;;; ------------------------------------------------------------ 2D vectors

(defun sw:2d (p) (list (float (car p)) (float (cadr p))))
(defun sw:add (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun sw:sub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun sw:mul (a s) (list (* (car a) s) (* (cadr a) s)))
(defun sw:len (a) (sqrt (+ (* (car a) (car a)) (* (cadr a) (cadr a)))))
(defun sw:dist (a b) (sw:len (sw:sub a b)))
(defun sw:unit (a / l)
  (setq l (sw:len a))
  (if (> l 1e-12) (sw:mul a (/ 1.0 l)) (list 1.0 0.0))
)
(defun sw:left (a) (list (- (cadr a)) (car a)))
(defun sw:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun sw:cross (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))
(defun sw:rot (u a / c s)
  (setq c (cos a) s (sin a))
  (list (- (* (car u) c) (* (cadr u) s)) (+ (* (car u) s) (* (cadr u) c)))
)
;; Signed angle (radians) that turns vector FROM onto vector TO. Left = +.
(defun sw:angle (from to / x y)
  (setq x (sw:dot from to) y (sw:cross from to))
  (if (and (< (abs x) 1e-14) (< (abs y) 1e-14)) 0.0 (atan y x))
)
(defun sw:r2d (r) (* r (/ 180.0 pi)))
(defun sw:asin (x) (atan x (sqrt (max 0.0 (- 1.0 (* x x))))))
(defun sw:fmt (x) (rtos x 2 1))
(defun sw:pt3 (p) (list (car p) (cadr p) *sw-elev*))

;;; ------------------------------------------------------- vehicle library
;;;
;;;  Each vehicle:  (name units minR maxArtic (unit unit ...) description)
;;;    units   "FT" or "M"  - the units the numbers below are in
;;;    minR    minimum turning radius of the CENTRE OF THE FRONT AXLE
;;;            (sets the steering limit: sin(limit) = wheelbase / minR)
;;;    maxArtic  maximum articulation angle between units, degrees (0 = n/a)
;;;
;;;  Each unit:  (L FO RO W T H)
;;;    L   wheelbase. Unit 1: front axle to rear axle.
;;;        Trailers: hitch (kingpin / drawbar eye) to trailer axle.
;;;    FO  body front overhang ahead of the front axle (trailers: ahead of hitch)
;;;    RO  body rear overhang behind the rear axle
;;;    W   body width
;;;    T   width over the tyres (outside edge to outside edge)
;;;    H   position of the hitch for the NEXT unit, measured forward from
;;;        this unit's rear axle (0 = over the axle, negative = behind it)
;;;
;;;  Tandem axle groups are modelled as one axle at the centre of the group,
;;;  the usual practice for swept path work.
;;;
;;;  The dimensions below follow the AASHTO design vehicles as commonly
;;;  published. The *-M vehicles are generic metric examples. Check every
;;;  vehicle against the design standard your project uses.

(setq *sw-lib*
 '(("P"       "FT" 21.0  0.0 ((11.0 3.0  5.0 7.0  6.0 0.0))
              "Passenger car, 19 ft")
   ("SU-30"   "FT" 38.0  0.0 ((20.0 4.0  6.0 8.0  8.0 0.0))
              "Single-unit truck, 30 ft")
   ("SU-40"   "FT" 47.4  0.0 ((25.0 4.0 10.5 8.0  8.0 0.0))
              "Single-unit truck, 39.5 ft")
   ("BUS-40"  "FT" 40.8  0.0 ((25.0 7.0  8.0 8.5  8.5 0.0))
              "Intercity bus, 40 ft")
   ("WB-40"   "FT" 36.0 70.0 ((12.5 3.0  2.0 8.0  8.0 0.0)
                              (25.5 3.0  4.5 8.0  8.0 0.0))
              "Intermediate semitrailer, 45.5 ft")
   ("WB-62"   "FT" 41.0 70.0 ((19.5 4.0  2.5 8.5  8.0 0.0)
                              (41.0 3.0  4.5 8.5  8.0 0.0))
              "Interstate semitrailer, 69 ft")
   ("WB-67"   "FT" 41.0 70.0 ((19.5 4.0  2.5 8.5  8.0 0.0)
                              (45.5 3.0  4.5 8.5  8.0 0.0))
              "Interstate semitrailer, 73.5 ft")
   ("CAR-M"   "M"   5.0  0.0 ((2.7  0.9  1.0 1.8  1.75 0.0))
              "Generic car, 4.6 m")
   ("TRUCK-M" "M"   8.6  0.0 ((5.0  1.5  2.3 2.5  2.5 0.0))
              "Generic rigid truck, 8.8 m")
   ("SEMI-M"  "M"   8.5 70.0 ((3.6  1.4  1.0 2.55 2.5 0.5)
                              (7.7  1.6  4.3 2.55 2.5 0.0))
              "Generic semitrailer, 16.5 m"))
)

;;; ------------------------------------------------------------- settings

(setq *sw-defaults*
 '(("Envelope" . "1") ("Wheels" . "1") ("Corners" . "0") ("Drive" . "1")
   ("Animate" . "1") ("Speed" . "10")
   ("Outlines" . "AUTO") ("Units" . "AUTO") ("Vehicle" . "WB-67"))
)

(defun sw:opt (k / v)
  (setq v (getenv (strcat "SweptPath_" k)))
  (if (and v (/= v "")) v (cdr (assoc k *sw-defaults*)))
)
(defun sw:put (k v) (setenv (strcat "SweptPath_" k) v))
(defun sw:on (k) (= (sw:opt k) "1"))
(defun sw:toggle (k) (sw:put k (if (sw:on k) "0" "1")))
(defun sw:onoff (k) (if (sw:on k) "On" "Off"))

;;; ---------------------------------------------------------------- units

(setq *sw-unit-m*
 '(("FT" . 0.3048) ("USFT" . 0.3048006096) ("IN" . 0.0254)
   ("M" . 1.0) ("MM" . 0.001) ("CM" . 0.01))
)

(defun sw:dwg-units (/ u i)
  (setq u (sw:opt "Units"))
  (if (= u "AUTO")
    (setq i (getvar "INSUNITS")
          u (cond ((= i 1) "IN") ((= i 2) "FT") ((= i 4) "MM") ((= i 5) "CM")
                  ((= i 6) "M") ((= i 21) "USFT") (T *sw-asked-units*)))
  )
  (if (not u)
    (progn
      (initget "Feet Meters")
      (setq u (if (= (getkword "\nThis drawing has no units set (INSUNITS). Drawing units [Feet/Meters] <Feet>: ")
                     "Meters")
                "M" "FT")
            *sw-asked-units* u)
    )
  )
  u
)

;;; ----------------------------------------------------- vehicle helpers

(defun sw:max-steer (minR L)
  (if (> minR L) (sw:r2d (sw:asin (/ L minR))) 60.0)
)

;; Overall length of a vehicle laid out straight.
(defun sw:overall (units / x fr rr)
  (setq x 0.0)
  (foreach u units
    (setq fr (if fr (max fr (+ x (nth 1 u))) (+ x (nth 1 u))))
    (setq x (- x (car u)))
    (setq rr (if rr (min rr (- x (nth 2 u))) (- x (nth 2 u))))
    (setq x (+ x (nth 5 u)))
  )
  (- fr rr)
)

(defun sw:user-file (/ root dir)
  (setq root (or (getenv "APPDATA") (getenv "LOCALAPPDATA") (getvar "TEMPPREFIX")))
  (setq dir (strcat (vl-string-right-trim "\\/" root) "\\SweptPath"))
  (if (not (vl-file-directory-p dir)) (vl-mkdir dir))
  (strcat dir "\\vehicles.lsp")
)

(defun sw:load-user (/ f)
  (setq *sw-user-veh* nil f (sw:user-file))
  (if (findfile f) (vl-catch-all-apply 'load (list f)))
  (if (listp *sw-user-veh*) *sw-user-veh* nil)
)

(defun sw:save-user (lst / f)
  (if (setq f (open (sw:user-file) "w"))
    (progn
      (princ ";; SweptPath custom vehicles - written by SWVEH\n" f)
      (princ "(setq *sw-user-veh* '" f)
      (prin1 lst f)
      (princ ")\n" f)
      (close f)
      T
    )
    (progn (princ (strcat "\nCould not write " (sw:user-file))) nil)
  )
)

(defun sw:all-veh () (append *sw-lib* (sw:load-user)))

(defun sw:find-veh (name lst)
  (car (vl-remove-if-not '(lambda (v) (= (strcase (car v)) (strcase name))) lst))
)

(defun sw:current-veh (/ lst)
  (setq lst (sw:all-veh))
  (cond ((sw:find-veh (sw:opt "Vehicle") lst))
        ((sw:find-veh "WB-67" lst))
        ((car lst)))
)

(defun sw:pad (s n) (while (< (strlen s) n) (setq s (strcat s " "))) s)

(defun sw:join (lst sep / r)
  (setq r (car lst))
  (foreach s (cdr lst) (setq r (strcat r sep s)))
  r
)

(defun sw:veh-line (v)
  (strcat (sw:pad (car v) 9)
          (sw:pad (nth 5 v) 34)
          " WB " (sw:join (mapcar '(lambda (u) (rtos (car u) 2 1)) (nth 4 v)) "+")
          "  L " (rtos (sw:overall (nth 4 v)) 2 1)
          "  minR " (rtos (nth 2 v) 2 1)
          " " (strcase (cadr v) T)
          "  steer " (sw:fmt (sw:max-steer (nth 2 v) (car (car (nth 4 v))))) "deg")
)

(defun sw:print-list (lst / i)
  (setq i 0)
  (foreach v lst
    (setq i (1+ i))
    (princ (strcat "\n " (sw:pad (itoa i) 3) (sw:veh-line v)))
  )
)

;; Asks for a vehicle by number. Returns the vehicle or nil.
(defun sw:choose-veh (/ lst cur i n)
  (setq lst (sw:all-veh) cur 1 i 0)
  (foreach v lst
    (setq i (1+ i))
    (if (= (strcase (car v)) (strcase (car (sw:current-veh)))) (setq cur i))
  )
  (princ "\n--- SweptPath vehicles ---")
  (sw:print-list lst)
  (initget 6)
  (setq n (getint (strcat "\nVehicle number <" (itoa cur) ">: ")))
  (if (null n) (setq n cur))
  (if (> n (length lst))
    (progn (princ "\nThere is no vehicle with that number.") nil)
    (progn (sw:put "Vehicle" (car (nth (1- n) lst))) (nth (1- n) lst))
  )
)

;;; Sets the globals for one run, converted to drawing units.
(defun sw:prep (veh / f tl)
  (setq f (/ (cdr (assoc (cadr veh) *sw-unit-m*))
             (cdr (assoc (sw:dwg-units) *sw-unit-m*))))
  (setq *sw-vname*    (car veh)
        *sw-units*    (mapcar '(lambda (u) (mapcar '(lambda (x) (* x f)) u)) (nth 4 veh))
        *sw-maxArt*   (nth 3 veh)
        *sw-maxSteer* (sw:max-steer (nth 2 veh) (car (car (nth 4 veh))))
        *sw-W0*       (nth 3 (car *sw-units*))
        *sw-txth*     (* 0.25 *sw-W0*))
  (setq tl (min (* f (if (= (cadr veh) "M") 1.0 3.3)) (* 0.2 (car (car *sw-units*)))))
  (setq *sw-tire* (list tl (* 0.3 tl)))
)

;;; ------------------------------------------------------------------ path

(defun sw:curve-p (e / r)
  (setq r (vl-catch-all-apply 'vlax-curve-getEndParam (list e)))
  (and r (not (vl-catch-all-error-p r)) (numberp r))
)

(defun sw:path-len (e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))

;;; Point and unit tangent at distance D along the path, in driving direction.
;;; Returns ((x y) (tx ty)).
(defun sw:path-at (d / dd par p dv tg)
  (setq dd (max 0.0 (min *sw-plen* (if *sw-flip* (- *sw-plen* d) d))))
  (setq par (vlax-curve-getParamAtDist *sw-path* dd))
  (if (null par)
    (setq par (if (> dd (* 0.5 *sw-plen*))
                (vlax-curve-getEndParam *sw-path*)
                (vlax-curve-getStartParam *sw-path*))))
  (setq p  (vlax-curve-getPointAtParam *sw-path* par)
        dv (vlax-curve-getFirstDeriv *sw-path* par))
  (setq tg (sw:unit (sw:2d dv)))
  (if *sw-flip* (setq tg (sw:mul tg -1.0)))
  (list (sw:2d p) tg)
)

;;; ------------------------------------------------------------ simulation
;;;
;;;  A state is (distance frontTangent poses). poses has one (P R h) per
;;;  unit: P = front reference point (front axle / hitch), R = rear axle,
;;;  h = unit heading vector.
;;;
;;;  Each step moves the pulling point P in a straight line by s. With phi
;;;  the angle between the move and the unit's axis, the exact tractrix gives
;;;      tan(phi'/2) = tan(phi/2) * exp(-s / L)

(defun sw:pull (Pold Pnew Rold L / m s w0 u phi phi1)
  (setq m (sw:sub Pnew Pold) s (sw:len m) w0 (sw:unit (sw:sub Pold Rold)))
  (if (< s 1e-12)
    w0
    (progn
      (setq u   (sw:mul m (/ 1.0 s))
            phi (sw:angle u w0))
      (if (> (abs phi) (- pi 1e-9))
        w0
        (progn
          (setq phi1 (* 2.0 (atan (* (/ (sin (* 0.5 phi)) (cos (* 0.5 phi)))
                                    (exp (/ (- s) L))))))
          (sw:rot u phi1)
        )
      )
    )
  )
)

(defun sw:simulate (/ minL n ds every k d st P tg poses new i o u h R states)
  (setq minL (apply 'min (mapcar 'car *sw-units*)))
  (setq n (fix (+ 0.999999 (/ *sw-plen* (/ minL 50.0)))))
  (setq n (max 1 (min n 30000)))
  (setq ds (/ *sw-plen* n))
  (setq every (max 1 (fix (+ 0.5 (/ minL 10.0 ds)))))
  (setq *sw-rec* (* every ds))
  ;; start: vehicle straight, lined up behind the path start
  (setq st (sw:path-at 0.0) P (car st) tg (cadr st))
  (foreach u *sw-units*
    (setq R (sw:sub P (sw:mul tg (car u))))
    (setq poses (cons (list P R tg) poses))
    (setq P (sw:add R (sw:mul tg (nth 5 u))))
  )
  (setq poses (reverse poses))
  (setq states (list (list 0.0 tg poses)) k 0)
  (while (< k n)
    (setq k (1+ k)
          d (if (= k n) *sw-plen* (* k ds))
          st (sw:path-at d)
          P (car st)
          tg (cadr st)
          new nil
          i 0)
    (foreach u *sw-units*
      (setq o (nth i poses)
            h (sw:pull (car o) P (cadr o) (car u))
            R (sw:sub P (sw:mul h (car u)))
            new (cons (list P R h) new)
            P (sw:add R (sw:mul h (nth 5 u)))
            i (1+ i))
    )
    (setq poses (reverse new))
    (if (or (= 0 (rem k every)) (= k n))
      (setq states (cons (list d tg poses) states))
    )
  )
  (reverse states)
)

(defun sw:steer (st) (sw:r2d (sw:angle (caddr (car (caddr st))) (cadr st))))

;;; Returns (maxSteer maxArtic warnings). Each warning is (point text).
(defun sw:analyse (states / maxS maxA runS runA warns hs s a pr)
  (setq maxS 0.0 maxA 0.0)
  (foreach st states
    (setq hs (mapcar 'caddr (caddr st))
          s  (abs (sw:steer st))
          maxS (max maxS s))
    (if (> s (+ *sw-maxSteer* 0.25))
      (progn
        (if (not runS)
          (setq warns (cons (list (car (car (caddr st)))
                                  (strcat "STEER " (sw:fmt s) "%%d > " (sw:fmt *sw-maxSteer*) "%%d"))
                            warns)))
        (setq runS T))
      (setq runS nil)
    )
    (setq a 0.0 pr (car hs))
    (foreach h (cdr hs)
      (setq a (max a (abs (sw:r2d (sw:angle pr h)))) pr h))
    (setq maxA (max maxA a))
    (if (and (> *sw-maxArt* 0) (> a *sw-maxArt*))
      (progn
        (if (not runA)
          (setq warns (cons (list (cadr (car (caddr st)))
                                  (strcat "ARTIC " (sw:fmt a) "%%d > " (sw:fmt *sw-maxArt*) "%%d"))
                            warns)))
        (setq runA T))
      (setq runA nil)
    )
  )
  (list maxS maxA (reverse warns))
)

;;; Largest distance from the last axle to the path once the whole vehicle
;;; is on it (off-tracking).
(defun sw:offtrack (states / sumL m R c)
  (setq sumL (apply '+ (mapcar 'car *sw-units*)) m 0.0)
  (foreach st states
    (if (>= (car st) sumL)
      (progn
        (setq R (cadr (last (caddr st)))
              c (vlax-curve-getClosestPointTo *sw-path* (sw:pt3 R)))
        (if c (setq m (max m (sw:dist R (sw:2d c)))))
      )
    )
  )
  m
)

;;; --------------------------------------------------------------- shapes

;; Rectangle: centre C, unit direction U, length A, width B.
;; Corners in order front-left, front-right, rear-right, rear-left.
(defun sw:rect (c u a b / v)
  (setq v (sw:mul (sw:left u) (* 0.5 b)) u (sw:mul u (* 0.5 a)))
  (list (sw:add (sw:add c u) v) (sw:sub (sw:add c u) v)
        (sw:sub (sw:sub c u) v) (sw:add (sw:sub c u) v))
)

(defun sw:body (u ps / h fr rr)
  (setq h  (caddr ps)
        fr (sw:add (car ps) (sw:mul h (nth 1 u)))
        rr (sw:sub (cadr ps) (sw:mul h (nth 2 u))))
  (sw:rect (sw:mul (sw:add fr rr) 0.5) h (sw:dist fr rr) (nth 3 u))
)

;;; Vehicle drawing for one state: list of (closed-flag points).
(defun sw:shape (st / tg poses i res ps P R h n hw off tl tw prevR)
  (setq tg (cadr st) poses (caddr st) i 0
        tl (car *sw-tire*) tw (cadr *sw-tire*))
  (foreach u *sw-units*
    (setq ps (nth i poses) P (car ps) R (cadr ps) h (caddr ps)
          n (sw:left h) hw (* 0.5 (nth 4 u)) off (- hw (* 0.5 tw)))
    (setq res (cons (list T (sw:body u ps)) res))
    (setq res (cons (list nil (list (sw:add R (sw:mul n hw)) (sw:sub R (sw:mul n hw)))) res)
          res (cons (list T (sw:rect (sw:add R (sw:mul n off)) h tl tw)) res)
          res (cons (list T (sw:rect (sw:sub R (sw:mul n off)) h tl tw)) res))
    (if (= i 0)
      ;; steering axle: tyres point along the path
      (setq res (cons (list nil (list (sw:add P (sw:mul n hw)) (sw:sub P (sw:mul n hw)))) res)
            res (cons (list T (sw:rect (sw:add P (sw:mul n off)) tg tl tw)) res)
            res (cons (list T (sw:rect (sw:sub P (sw:mul n off)) tg tl tw)) res))
      ;; drawbar from the previous axle to this hitch, when there is one
      (if (> (sw:dist prevR P) 1e-6)
        (setq res (cons (list nil (list prevR P)) res)))
    )
    (setq prevR R i (1+ i))
  )
  res
)

;; Outer tyre edge points for one state, in a fixed order.
(defun sw:wheel-pts (st / poses i res ps n hw)
  (setq poses (caddr st) i 0)
  (foreach u *sw-units*
    (setq ps (nth i poses) n (sw:left (caddr ps)) hw (* 0.5 (nth 4 u)))
    (if (= i 0)
      (setq res (append res (list (sw:add (car ps) (sw:mul n hw)) (sw:sub (car ps) (sw:mul n hw))))))
    (setq res (append res (list (sw:add (cadr ps) (sw:mul n hw)) (sw:sub (cadr ps) (sw:mul n hw))))
          i (1+ i))
  )
  res
)

(defun sw:corner-pts (st / poses i res)
  (setq poses (caddr st) i 0)
  (foreach u *sw-units*
    (setq res (append res (sw:body u (nth i poses))) i (1+ i)))
  res
)

;; Runs FN on every state and returns one point list per track.
(defun sw:collect (states fn / acc pts)
  (foreach st states
    (setq pts (apply fn (list st))
          acc (if acc (mapcar 'cons pts acc) (mapcar 'list pts))))
  (mapcar 'reverse acc)
)

;;; ------------------------------------------------------------- drawing

(defun sw:layer (name col)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 name) '(70 . 0)
                   (cons 62 col) '(6 . "Continuous")))
  )
)

(defun sw:layers ()
  (sw:layer "SWEPT-WHEELS" 1)
  (sw:layer "SWEPT-BODY" 4)
  (sw:layer "SWEPT-ENVELOPE" 30)
  (sw:layer "SWEPT-VEHICLE" 7)
  (sw:layer "SWEPT-WARN" 1)
  (sw:layer "SWEPT-LABEL" 2)
)

(defun sw:keep (e) (if e (setq *sw-made* (cons e *sw-made*))) e)

(defun sw:pline (pts closed lay)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
            '(100 . "AcDbPolyline") (cons 90 (length pts))
            (cons 70 (if closed 1 0)) (cons 38 *sw-elev*))
      (mapcar '(lambda (p) (cons 10 (sw:2d p))) pts)))
)

(defun sw:circle (c r lay)
  (entmakex (list '(0 . "CIRCLE") (cons 8 lay) (cons 10 (sw:pt3 c)) (cons 40 r)))
)

(defun sw:text (pt h str rot lay)
  (entmakex (list '(0 . "TEXT") (cons 8 lay) (cons 10 (sw:pt3 pt)) (cons 40 h)
                  (cons 1 str) (cons 50 rot) (cons 7 (getvar "TEXTSTYLE"))))
)

(defun sw:draw-shape (st lay)
  (foreach s (sw:shape st)
    (sw:keep (sw:pline (cadr s) (car s) lay)))
)

(defun sw:space ()
  (if (and (= (getvar "TILEMODE") 0) (= (getvar "CVPORT") 1))
    (vla-get-PaperSpace *sw-doc*)
    (vla-get-ModelSpace *sw-doc*))
)

;; Closed outline -> REGION object, or nil.
(defun sw:region (pts sp / o r)
  (setq o (vlax-ename->vla-object (sw:pline pts T "SWEPT-ENVELOPE")))
  (setq r (vl-catch-all-apply 'vlax-invoke (list sp 'AddRegion (list o))))
  (vla-Delete o)
  (if (or (vl-catch-all-error-p r) (null r)) nil (car r))
)

;;; Body envelope: union of every body position. Returns the number of
;;; pieces that could not be merged.
(defun sw:envelope (states / sp cnt step lst k i reg r fails)
  (setq sp (sw:space) cnt (length states)
        step (max 1 (fix (+ 0.999 (/ cnt 400.0)))) k 0 fails 0 i 0)
  (foreach st states
    (if (or (= 0 (rem k step)) (= k (1- cnt))) (setq lst (cons st lst)))
    (setq k (1+ k)))
  (foreach u *sw-units*
    (foreach st lst
      (setq r (sw:region (sw:body u (nth i (caddr st))) sp))
      (cond
        ((null r) (setq fails (1+ fails)))
        ((null reg) (setq reg r))
        ((vl-catch-all-error-p (vl-catch-all-apply 'vla-Boolean (list reg 0 r)))
         (setq fails (1+ fails))
         (vl-catch-all-apply 'vla-Delete (list r)))
      )
    )
    (setq i (1+ i))
  )
  (if reg
    (progn (vla-put-Layer reg "SWEPT-ENVELOPE")
           (sw:keep (vlax-vla-object->ename reg))))
  fails
)

;;; Puts everything this run made into one group, so it can be picked or
;;; erased as one. Returns the group name or nil.
(defun sw:make-group (enames / grps i name grp)
  (setq grps (vla-get-Groups *sw-doc*) i 1)
  (while (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-Item (list grps (setq name (strcat "SWEPT_" (itoa i)))))))
    (setq i (1+ i)))
  (setq grp (vla-Add grps name))
  (vlax-invoke grp 'AppendItems (mapcar 'vlax-ename->vla-object enames))
  name
)

(defun sw:finish-group (/ r lst)
  (setq lst (vl-remove-if-not '(lambda (e) (and e (entget e))) *sw-made*)
        *sw-made* nil)
  (if lst
    (progn
      (setq r (vl-catch-all-apply 'sw:make-group (list lst)))
      (if (vl-catch-all-error-p r) nil r))
  )
)

;;; ----------------------------------------------------------- drive mode

(defun sw:idx-at (upt n / w c d)
  (setq w (trans upt 1 0)
        c (vlax-curve-getClosestPointTo *sw-path* (list (car w) (cadr w) *sw-elev*))
        d (if c (vlax-curve-getDistAtPoint *sw-path* c)))
  (if (null d) (setq d 0.0))
  (if *sw-flip* (setq d (- *sw-plen* d)))
  (max 0 (min (1- n) (fix (+ 0.5 (/ d *sw-rec*)))))
)

(defun sw:grdraw (st col / v pts)
  (foreach s (sw:shape st)
    (setq pts (cadr s))
    (if (car s) (setq pts (append pts (list (car pts)))))
    (while (cdr pts)
      (setq v (cons (trans (sw:pt3 (cadr pts)) 0 1) (cons (trans (sw:pt3 (car pts)) 0 1) v))
            pts (cdr pts)))
  )
  (grvecs (cons col (reverse v)))
)

(defun sw:drive (states / n gr idx prev st done)
  (setq n (length states) prev -1)
  (princ "\nDrive: move the cursor along the path. Click = leave an outline there. Enter = finish.")
  (while (not done)
    (setq gr (grread T 15 0))
    (cond
      ((= (car gr) 5)
       (setq idx (sw:idx-at (cadr gr) n))
       (if (/= idx prev)
         (progn
           (setq prev idx st (nth idx states))
           (redraw)
           (sw:grdraw st (if (> (abs (sw:steer st)) (+ *sw-maxSteer* 0.25)) 1 2)))))
      ((= (car gr) 3)
       (setq st (nth (sw:idx-at (cadr gr) n) states))
       (sw:draw-shape st "SWEPT-VEHICLE")
       (princ (strcat "\n  Outline at " (sw:fmt (car st)) ": steering " (sw:fmt (sw:steer st))
                      " deg (limit " (sw:fmt *sw-maxSteer*) ")")))
      ((or (and (= (car gr) 2) (member (cadr gr) '(13 32)))
           (member (car gr) '(11 25)))
       (setq done T))
    )
  )
  (redraw)
)

;;; ------------------------------------------------------------ animation
;;;
;;;  Plays the vehicle along the path in real time. The speed is in mph
;;;  for feet / inch drawings and km/h for metric drawings. Each frame is
;;;  picked from the elapsed time, so the speed stays right even when the
;;;  screen is slow to redraw. DELAY lets the screen repaint between frames.

(defun sw:imperial-p () (member *sw-un* '("FT" "USFT" "IN")))
(defun sw:speed-name () (if (sw:imperial-p) "mph" "km/h"))

;; Animation speed in drawing units per second.
(defun sw:speed-du (/ v)
  (setq v (atof (sw:opt "Speed")))
  (if (<= v 0.0) (setq v 10.0))
  (/ (* v (if (sw:imperial-p) 0.44704 (/ 1.0 3.6)))
     (cdr (assoc *sw-un* *sw-unit-m*)))
)

;; Milliseconds from a running clock.
(defun sw:ms (/ v)
  (setq v (getvar "MILLISECS"))
  (if v (float v) (* 86400000.0 (rem (getvar "DATE") 1.0)))
)

(defun sw:animate (states / n spd t0 now el idx prev st)
  (setq n (length states) spd (sw:speed-du) prev -1 idx 0)
  (princ (strcat "
Playing " *sw-vname* " at " (sw:opt "Speed") " " (sw:speed-name)
                 " (" (sw:fmt (/ *sw-plen* spd)) " s). Red = steering over the limit."))
  (setq t0 (sw:ms))
  (while (< prev (1- n))
    (setq now (sw:ms) el (/ (- now t0) 1000.0))
    (if (< el 0.0) (setq t0 now el 0.0))      ; clock wrapped
    (setq idx (min (1- n) (fix (/ (* el spd) *sw-rec*))))
    (if (/= idx prev)
      (progn
        (setq prev idx st (nth idx states))
        (redraw)
        (sw:grdraw st (if (> (abs (sw:steer st)) (+ *sw-maxSteer* 0.25)) 1 2))))
    (command "_.DELAY" 20)
  )
)

;;; After a run: play the animation, then offer replay / speed / drive.
(defun sw:playback (states / k done v)
  (cond ((sw:on "Animate") (sw:animate states))
        ((sw:on "Drive") (sw:drive states)))
  (while (not done)
    (initget "Replay Faster Slower Drive eXit")
    (setq k (getkword (strcat "\nAnimation " (sw:opt "Speed") " " (sw:speed-name)
                              " [Replay/Faster/Slower/Drive/eXit] <eXit>: ")))
    (redraw)
    (cond
      ((or (null k) (= k "eXit")) (setq done T))
      ((= k "Replay") (sw:animate states))
      ((member k '("Faster" "Slower"))
       (setq v (* (atof (sw:opt "Speed")) (if (= k "Faster") 2.0 0.5)))
       (sw:put "Speed" (rtos (max 0.5 (min v 640.0)) 2 1))
       (sw:animate states))
      ((= k "Drive") (sw:drive states))
    )
  )
  (redraw)
)

;;; ------------------------------------------------------------ error / env

(defun sw:err (msg)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
    (princ (strcat "\nSweptPath error: " msg))
  )
  (redraw)
  (sw:finish-group)
  (sw:end)
)

(defun sw:begin ()
  (setq *sw-olderr* *error* *error* sw:err *sw-made* nil
        *sw-cmdecho* (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq *sw-doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark *sw-doc*)
  (setq *sw-undo* T)
)

(defun sw:end ()
  (if *sw-undo* (progn (setq *sw-undo* nil) (vla-EndUndoMark *sw-doc*)))
  (if *sw-cmdecho* (setvar "CMDECHO" *sw-cmdecho*))
  (setq *error* *sw-olderr*)
  (princ)
)

;;; ------------------------------------------------------------------ run

(defun sw:run (veh e pick / sp ep pw states res fails spc next st0 tg0 grp off un)
  (sw:prep veh)
  (setq un (sw:dwg-units) *sw-un* un)
  (setq *sw-path* e *sw-plen* (sw:path-len e))
  (if (or (null *sw-plen*) (<= *sw-plen* 0.0))
    (progn (princ "\nThat path has no length.") (exit)))
  (setq sp (vlax-curve-getStartPoint e) ep (vlax-curve-getEndPoint e)
        pw (sw:2d (trans pick 1 0)))
  (setq *sw-flip* (< (sw:dist pw (sw:2d ep)) (sw:dist pw (sw:2d sp))))
  (setq *sw-elev* (caddr (if *sw-flip* ep sp)))

  (princ (strcat "\nDriving " *sw-vname* " along " (sw:fmt *sw-plen*) " " un " of path..."))
  (setq states (sw:simulate)
        res (sw:analyse states)
        off (sw:offtrack states))
  (setq *sw-last* states)
  (sw:layers)

  (if (sw:on "Wheels")
    (foreach pts (sw:collect states 'sw:wheel-pts)
      (sw:keep (sw:pline pts nil "SWEPT-WHEELS"))))
  (if (sw:on "Corners")
    (foreach pts (sw:collect states 'sw:corner-pts)
      (sw:keep (sw:pline pts nil "SWEPT-BODY"))))
  (if (sw:on "Envelope")
    (progn (princ "\nBuilding body envelope...") (setq fails (sw:envelope states))))

  ;; vehicle outlines: start, end and every SPC along the way
  (sw:draw-shape (car states) "SWEPT-VEHICLE")
  (setq spc (sw:opt "Outlines")
        spc (cond ((= spc "AUTO") (sw:overall *sw-units*)) ((= spc "NONE") nil) (T (atof spc))))
  (if (and spc (> spc 0.0))
    (progn
      (setq next spc)
      (foreach st states
        (if (>= (car st) next)
          (progn
            (if (< (car st) (- *sw-plen* (* 0.5 spc))) (sw:draw-shape st "SWEPT-VEHICLE"))
            (while (<= next (car st)) (setq next (+ next spc))))))))
  (sw:draw-shape (last states) "SWEPT-VEHICLE")

  ;; problem spots
  (foreach w (caddr res)
    (sw:keep (sw:circle (car w) (* 0.75 *sw-W0*) "SWEPT-WARN"))
    (sw:keep (sw:text (sw:add (car w) (list (* 0.8 *sw-W0*) (* 0.8 *sw-W0*)))
                      *sw-txth* (cadr w) 0.0 "SWEPT-WARN")))

  ;; label beside the start
  (setq st0 (car states) tg0 (cadr st0))
  (sw:keep (sw:text (sw:add (car (car (caddr st0))) (sw:mul (sw:left tg0) (* 1.2 *sw-W0*)))
                    *sw-txth*
                    (strcat *sw-vname* "  steer max " (sw:fmt (car res)) "%%d (limit "
                            (sw:fmt *sw-maxSteer*) "%%d)")
                    (angle '(0.0 0.0) tg0) "SWEPT-LABEL"))

  ;; report
  (princ (strcat "\n" *sw-vname* ": path " (sw:fmt *sw-plen*) " " un))
  (princ (strcat "\n  Max steering angle:  " (sw:fmt (car res)) " deg  (limit " (sw:fmt *sw-maxSteer*) ")"
                 (if (> (car res) (+ *sw-maxSteer* 0.25)) "  ** OVER LIMIT **" "  OK")))
  (if (cdr *sw-units*)
    (princ (strcat "\n  Max articulation:    " (sw:fmt (cadr res)) " deg"
                   (if (> *sw-maxArt* 0)
                     (strcat "  (limit " (sw:fmt *sw-maxArt*) ")"
                             (if (> (cadr res) *sw-maxArt*) "  ** OVER LIMIT **" "  OK"))
                     ""))))
  (princ (strcat "\n  Max off-tracking:    " (sw:fmt off) " " un " (last axle from path)"))
  (if (caddr res)
    (princ (strcat "\n  " (itoa (length (caddr res)))
                   " problem spot(s) circled on layer SWEPT-WARN. Sharp corners need"
                   "\n  arcs: the vehicle cannot follow a tighter turn than its limit.")))
  (if (and fails (> fails 0))
    (princ (strcat "\n  Note: " (itoa fails) " envelope piece(s) could not be merged.")))

  (sw:playback states)
  (if (setq grp (sw:finish-group))
    (princ (strcat "\nEverything is in group " grp " on SWEPT-* layers. Erase the group to remove it.")))
)

;;; ------------------------------------------------------------- commands

(defun c:SWEPT (/ veh sel e pick)
  (sw:begin)
  (setq veh (sw:current-veh))
  (while (and veh (not e))
    (setvar "ERRNO" 0)
    (initget "Vehicle Settings")
    (setq sel (entsel (strcat "\nVehicle " (car veh)
                              ". Select path near the end to start from [Vehicle/Settings]: ")))
    (cond
      ((= sel "Vehicle") (setq veh (cond ((sw:choose-veh)) (veh))))
      ((= sel "Settings") (sw:settings))
      ((and sel (listp sel))
       (if (sw:curve-p (car sel))
         (setq e (car sel) pick (cadr sel))
         (princ "\nThat is not a line, arc, polyline, spline or alignment.")))
      ((= (getvar "ERRNO") 7) (princ "\nNothing there, try again."))
      (T (setq veh nil))
    )
  )
  (if e (sw:run veh e pick))
  (sw:end)
)

(defun c:SWDRIVE (/ grp)
  (sw:begin)
  (if (and *sw-last* *sw-path* (entget *sw-path*))
    (progn
      (sw:layers)
      (sw:playback *sw-last*)
      (if (setq grp (sw:finish-group)) (princ (strcat "\nOutlines added as group " grp "."))))
    (princ "\nRun SWEPT first. SWDRIVE replays the last swept path in this drawing.")
  )
  (sw:end)
)

(defun sw:settings (/ k v done)
  (while (not done)
    (princ (strcat "\nEnvelope=" (sw:onoff "Envelope") "  Wheels=" (sw:onoff "Wheels")
                   "  Corners=" (sw:onoff "Corners") "  Outlines=" (sw:opt "Outlines")
                   "  Drive=" (sw:onoff "Drive") "  Units=" (sw:opt "Units")
                   "\n  Animate=" (sw:onoff "Animate") "  Speed=" (sw:opt "Speed") " (mph in feet drawings, km/h in metric)"))
    (initget "Envelope Wheels Corners Outlines Drive Animate Speed Units eXit")
    (setq k (getkword "\nChange [Envelope/Wheels/Corners/Outlines/Drive/Animate/Speed/Units/eXit] <eXit>: "))
    (cond
      ((or (null k) (= k "eXit")) (setq done T))
      ((member k '("Envelope" "Wheels" "Corners" "Drive" "Animate")) (sw:toggle k))
      ((= k "Speed")
       (initget 6)
       (setq v (getreal (strcat "\nAnimation speed, mph (feet drawings) or km/h (metric) <" (sw:opt "Speed") ">: ")))
       (if v (sw:put "Speed" (rtos v 2 1))))
      ((= k "Outlines")
       (initget 6 "Auto None")
       (setq v (getdist "\nVehicle outline every [Auto/None] or distance along path <Auto>: "))
       (sw:put "Outlines" (cond ((or (null v) (= v "Auto")) "AUTO")
                                ((= v "None") "NONE")
                                (T (rtos v 2 4)))))
      ((= k "Units")
       (initget "Auto Feet Meters Inches MM CM")
       (setq v (getkword "\nDrawing units [Auto/Feet/Meters/Inches/MM/CM] <Auto>: "))
       (sw:put "Units" (cdr (assoc (if v v "Auto")
                                   '(("Auto" . "AUTO") ("Feet" . "FT") ("Meters" . "M")
                                     ("Inches" . "IN") ("MM" . "MM") ("CM" . "CM"))))))
    )
  )
)

(defun c:SWSET () (sw:settings) (princ))

;;; ------------------------------------------------------- vehicle editor

(defun sw:ask (msg def / v)
  (setq v (getreal (strcat msg " <" (rtos def 2 2) ">: ")))
  (if v v def)
)
(defun sw:ask-pos (msg def / v)
  (initget 6)
  (sw:ask msg def)
)

(defun sw:add-veh (/ name un m nt units u L fo ro w tw h minR art lst j)
  (setq name (strcase (getstring "\nName for the new vehicle: ")))
  (cond
    ((= name "") (princ "\nCancelled.") nil)
    ((sw:find-veh name *sw-lib*) (princ "\nThat name is a built-in vehicle. Pick another name.") nil)
    (T
     (initget "Feet Meters")
     (setq un (if (= (getkword "\nUnits for the numbers you type [Feet/Meters] <Feet>: ") "Meters") "M" "FT")
           m (if (= un "M") 0.3048 1.0))
     (initget 4)
     (setq nt (getint "\nNumber of trailers (0-3) <0>: "))
     (setq nt (min 3 (if nt nt 0)))
     (princ "\n-- Truck / towing unit --")
     (setq L    (sw:ask-pos "\nWheelbase, front axle to rear axle (centre of tandem)" (* m 20.0))
           fo   (sw:ask-pos "\nFront overhang, bumper to front axle" (* m 4.0))
           ro   (sw:ask-pos "\nRear overhang, rear axle to back of body" (* m 6.0))
           w    (sw:ask-pos "\nBody width" (* m 8.5))
           tw   (sw:ask-pos "\nWidth over tyres (outside to outside)" (* m 8.0))
           minR (sw:ask-pos "\nMinimum turning radius at the centre of the front axle" (* m 42.0)))
     (while (<= minR L)
       (princ "\nThe turning radius must be larger than the wheelbase.")
       (setq minR (sw:ask-pos "\nMinimum turning radius at the centre of the front axle" (* L 1.6))))
     (setq h (if (> nt 0)
               (sw:ask "\nHitch position ahead of the rear axle (negative = behind it)" 0.0)
               0.0))
     (setq units (list (list L fo ro w tw h)) j 0)
     (while (< j nt)
       (setq j (1+ j))
       (princ (strcat "\n-- Trailer " (itoa j) " --"))
       (setq L  (sw:ask-pos "\nHitch (kingpin or drawbar eye) to trailer axle (centre of group)" (* m 40.0))
             fo (sw:ask "\nBody front ahead of the hitch" (* m 3.0))
             ro (sw:ask-pos "\nRear overhang behind the trailer axle" (* m 4.5))
             w  (sw:ask-pos "\nBody width" (* m 8.5))
             tw (sw:ask-pos "\nWidth over tyres" (* m 8.0))
             h  (if (< j nt)
                  (sw:ask "\nHitch for the next trailer, ahead of this axle (negative = behind)" (* m -6.0))
                  0.0))
       (setq units (append units (list (list L fo ro w tw h))))
     )
     (setq art (if (> nt 0) (sw:ask-pos "\nMaximum articulation angle, degrees" 70.0) 0.0))
     (setq u (list name un minR art units
                   (strcat "Custom, " (rtos (sw:overall units) 2 1) (if (= un "M") " m" " ft"))))
     (setq lst (vl-remove-if '(lambda (v) (= (strcase (car v)) name)) (sw:load-user)))
     (if (sw:save-user (append lst (list u)))
       (progn
         (sw:put "Vehicle" name)
         (princ (strcat "\nSaved " name " and made it the current vehicle:\n  " (sw:veh-line u)))
         u))
    )
  )
)

(defun sw:delete-veh (/ lst n)
  (setq lst (sw:load-user))
  (if (null lst)
    (princ "\nThere are no custom vehicles. Built-in vehicles cannot be deleted.")
    (progn
      (princ "\n--- Custom vehicles ---")
      (sw:print-list lst)
      (initget 6)
      (setq n (getint "\nNumber to delete <none>: "))
      (if (and n (<= n (length lst)))
        (progn
          (princ (strcat "\nDeleted " (car (nth (1- n) lst)) "."))
          (sw:save-user (vl-remove (nth (1- n) lst) lst))))))
)

(defun c:SWVEH (/ k)
  (initget "List Select Add Delete")
  (setq k (getkword "\nVehicles [List/Select/Add/Delete] <Select>: "))
  (cond
    ((= k "List")
     (princ (strcat "\n--- SweptPath vehicles ---  (custom ones are kept in " (sw:user-file) ")"))
     (sw:print-list (sw:all-veh))
     (princ (strcat "\nCurrent: " (car (sw:current-veh)))))
    ((= k "Add") (sw:add-veh))
    ((= k "Delete") (sw:delete-veh))
    (T (if (setq k (sw:choose-veh)) (princ (strcat "\nCurrent vehicle: " (car k))))))
  (princ)
)

(defun c:SWHELP ()
  (princ (strcat "\n--- SweptPath " *sw-version* " ---"
    "\n  SWEPT    Pick a path (near the end to start from). The vehicle drives it and"
    "\n           SweptPath draws tyre paths, body envelope and outlines."
    "\n           Afterwards the vehicle is animated along the path."
    "\n  SWDRIVE  Replay the last animation, or drive it with the cursor"
    "\n  SWVEH    Vehicle library: List / Select / Add / Delete"
    "\n  SWSET    What gets drawn, outline spacing, drawing units"
    "\n  Draw the path with PLINE and use arcs (or FILLET) for the turns."))
  (princ)
)

(princ (strcat "\nSweptPath " *sw-version* " loaded: SWEPT, SWDRIVE, SWVEH, SWSET, SWHELP."))
(princ)
