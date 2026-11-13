;; 02 Erosion and Sediment Control.lsp - an example series script for 02xxx drawings.
;; To use it, copy it up one folder (into "Series Scripts") and put your changes in below.
;;
;; Series Scripts loads it on each drawing in the series, in a background console, and then
;; saves the drawing - so don't save here. These are set before it loads:
;;   ttw-dwg-number   "02000"
;;   ttw-series       2
;;   ttw-series-name  "Erosion and Sediment Control"
;;   ttw-superseded   the copy of the drawing as it was

(princ (strcat "\n" ttw-series-name " script on " ttw-dwg-number))

;; ---- your changes here, for example:
;; (command "_.-LAYER" "_Make" "C-ESC" "")

(princ)
