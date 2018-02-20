; trace-frames Version 0.3
; Copyright (C) 2018 Tamubun <http://bunysmc.exblog.jp/>
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(define (script-fu-trace-frames inImage inLayer fwidth fheight smooth tile-to-layer)
  (define (floor x) (- x (fmod x 1)))
  (define (div x y) (inexact->exact (floor (/ x y))))
  (define (large? array pos)
    (let* ((x (vector-ref array (+ 3 pos))) (y (vector-ref array (+ 4 pos))))
      (not (and (>= 5 (abs (- x (vector-ref array      pos ))))
		(>= 5 (abs (- y (vector-ref array (+ 1 pos)))))
		(>= 5 (abs (- x (vector-ref array (+ 6 pos)))))
		(>= 5 (abs (- y (vector-ref array (+ 7 pos)))))))))

  (define (frame-num x y iwidth iheight)
    (let* ((n-col (div iwidth  fwidth ))
	   (col (div x fwidth))
	   (row (div y fheight)))
      (if (< y 0)
	  0
	(inexact->exact
	 (min (+ col (* n-col row))
	      (- (* (div iwidth fwidth) (div iheight fheight)) 1))))))

  (define (single a pos x y type)
    (vector-set! a      pos  x)
    (vector-set! a (+ 1 pos) y)
    (vector-set! a (+ 2 pos) type))

  (define (simple-path array)
    (let* ((iwidth  (car (gimp-image-width  inImage)))
	   (iheight (car (gimp-image-height inImage)))
	   (x0 (vector-ref array  9)) (y0 (vector-ref array 10))
	   (x1 (vector-ref array 18)) (y1 (vector-ref array 19))
	   (start (frame-num x0 y0 iwidth iheight))
	   (end   (frame-num x1 y1 iwidth iheight))
;	   (x00 (min (vector-ref array 6) (vector-ref array 12)))
;	   (y00 (min (vector-ref array 7) (vector-ref array 13)))
;	   (cwidth  (abs (- (vector-ref array 6) (vector-ref array 12))))
;	   (cheight (abs (- (vector-ref array 7) (vector-ref array 13))))
	   (dx (if (< start end) fwidth (- fwidth)))
	   (path-len (+ 1 (abs (- end start))))
	   (len (+ 6 (* 9 path-len)))
	   (array2 (cons-array len 'double))
	   (idx))

      (set! idx 6)
      (while (< idx 15)
        (vector-set! array2 idx (vector-ref array idx))
	(set! idx (+ 1 idx)))

      (set! idx 15)
      (while (< idx len)
	(set! x0 (+ x0 dx))
;	(set! x00 (+ x00 dx))
	(if (>= x0 iwidth)
	    (begin
	      (set! x0 (- x0 iwidth ))
	      (set! y0 (+ y0 fheight))
	      )
;	      (set! x00 (max 0 (- x00 iwidth)))
;	      (set! y00 (+ y00 fheight)))
	  (if (< x0 0)
	      (begin
		(set! x0 (+ x0 iwidth ))
		(set! y0 (- y0 fheight))
		)))
;		(set! x00 (min iwidth (+ x00 iwidth)))
;		(set! y00 (- y00 fheight)))))
;	(single array2      idx     x00            y00          2)
	(single array2 (+ 3 idx)    x0             y0           1)
;	(single array2 (+ 6 idx) (+ x00 cwidth) (+ y00 cheight) 2)
	(set! idx (+ idx 9)))

      (single array2        6  (vector-ref array  6) (vector-ref array  7) 2)
      (single array2       12  (vector-ref array 12) (vector-ref array 13) 2)
      (single array2 (- len 9) (vector-ref array 15) (vector-ref array 16) 2)
      (single array2 (- len 3) (vector-ref array 21) (vector-ref array 22) 2)
      array2)) ; End of simple-path

  (define (smooth-path array step)
    (define (xy-value x-or-y idx d) (fmod (vector-ref array (+ 9 x-or-y (* 9 idx))) d))

    (define (triple a pos idx x-or-y) ; x-or-y: 0 .. x, 1 .. y
      (let* ((d (if (= x-or-y 0) fwidth fheight))
	     (y) (y1) (y2) (mean))

	(set! y  (xy-value x-or-y    idx    d))
	(set! y1 (xy-value x-or-y (- idx 1) d))
	(set! y2 (xy-value x-or-y (+ idx 1) d))
	(set! mean (/ (- y2 y1) 2))
	(set! pos (+ 6 (* 9 pos)))
	(single a      pos  (* 10 (- idx 2)) (- y mean) 2)
	(single a (+ 3 pos) (* 10    idx   )    y       1)
	(single a (+ 6 pos) (* 10 (+ idx 2)) (+ y mean) 2))) ; End of triple

    (define (get-hints)
      (let* ((pos (- (vector-length array) 18))
	     (hints '()))

	(while (>= pos 15)
	  (if (large? array pos)
	      (set! hints (cons (div (- pos 6) 9) hints)))
	  (set! pos (- pos 9)))
	hints)) ; End of get-hints

    (define (get-key-idx hints)
      (let* ((end (- (div (- (vector-length array) 6) 9) 1))
	     (s1 (* 1.5 step))
	     (h) (i)
	     (key-idx '()))
	(set! h (if (null? hints) #f (car hints)))
	(if (and h (< h s1))
	    (prog1
	     (set! i h)
	     (set! hints (cdr hints))
	     (set! h (car hints)))
	  (set! i step))
	(while (< i end)
	  (set! key-idx (cons i key-idx))
	  (if (and h (< h (+ i s1)))
	      (prog1
	       (set! i h)
	       (set! hints (if (null? hints) null (cdr hints)))
	       (set! h (if (null? hints) #f (car hints))))
	    (set! i (+ step i))))
	(if (null? key-idx) '() (reverse key-idx)))) ; End of get-key-idx

;  smoot-path
;  -----------------
    (let* ((len (/ (- (vector-length array) 6) 9))
	   (end (- len (/ step 2)))
	   (key-idx (get-key-idx (get-hints)))
	   (key-idx-cpy (map (lambda (x) x) key-idx)) ; copy-list
	   (seq (cons-array (+ 6 (* 9 (+ 2 (length key-idx)))) 'double))
	   (img) (layer)
	   (d)
	   (i) (j) (x) (y) (dist) (point) (x-or-y))

      (set! img (car (gimp-image-new (* 10 len) (max fwidth fheight) 0)))
      (set! layer (car (gimp-layer-new img (* 10 len) (max fwidth fheight)
				       0 "layer" 100 0)))
      (gimp-image-add-layer img layer 0)
      (gimp-image-undo-disable img)

      ; smooth x and y coordinates
      (set! x-or-y 0)
      (while (< x-or-y 2)
	(set! d (if (= x-or-y 0) fwidth fheight))
	(single seq 0 0  (xy-value x-or-y 0 d) 1)
	(single seq 3 20 (xy-value x-or-y 1 d) 2)

	(set! i step)
	(set! j 0)
	(while (pair? key-idx)
	  (triple seq j (car key-idx) x-or-y)
	  (set! key-idx (cdr key-idx))
	  (set! j (+ j 1)))

	(set! j (+ 6 (* 9 j)))
	(single seq    j    (* 10 (- len 3)) (xy-value x-or-y (- len 2) d) 2)
	(single seq (+ j 3) (* 10 (- len 1)) (xy-value x-or-y (- len 1) d) 1)
	(single seq (+ j 6) (* 10 (- len 1)) (xy-value x-or-y (- len 1) d) 2)

        ; Work around for get-point-at-dist.
	(single seq (+ j 9)  (* 10 len 3) 0 2)
	(single seq (+ j 12) (* 10 len 1) 0 2)
	(single seq (+ j 15) (* 10 len 1) 0 2)

	(gimp-path-set-points img
          (if (= x-or-y 0) "x" "y") 1 (vector-length seq) seq)
	(set! i 0)
	(while (< i len)
	  (set! dist (* 10 i))
	  (set! point (gimp-path-get-point-at-dist img dist))
	  (set! x (car point))
	  (while (< x (* 10 i))
	     (set! dist (+ 1 (max x dist)))
	     (set! point (gimp-path-get-point-at-dist img dist))
	     (set! x (car point)))
	  (set! y (* d (div (vector-ref array (+ 9 x-or-y (* 9 i))) d)))
	  (vector-set! array (+ 9 x-or-y (* 9 i)) (+ y (cadr point)))
	  (set! i (+ i 1)))

	(set! key-idx key-idx-cpy)
	(set! x-or-y (+ 1 x-or-y)))
      (gimp-image-delete img)
      )) ; End of smooth-path

;  script-fu-trace-frames
;  -----------------
  (let* ((outImage (car (gimp-image-new 10 10 RGB)))
	 (outLayer (car (gimp-layer-new outImage 10 10 RGB "Background" 100 NORMAL)))
	 (layers (cadr (gimp-image-get-layers inImage)))
	 (n-layers (vector-length layers))
	 (paths (cadr (gimp-path-list inImage)))
	 (item-layer)
	 (path)
	 (paste)
	 (iwidth  (car (gimp-image-width  inImage)))
	 (iheight (car (gimp-image-height inImage)))
	 (i) (l) (x) (y))

    (gimp-image-undo-disable inImage)
    (gimp-image-undo-disable outImage)
    (gimp-image-add-layer outImage outLayer 0)

    (set! l (vector-ref layers (- n-layers 1)))
    (gimp-image-resize outImage (car (gimp-drawable-width  l))
				(car (gimp-drawable-height l)) 0 0)
    (gimp-selection-clear inImage)
    (gimp-edit-copy l)
    (set! paste (car (gimp-edit-paste outLayer 0)))
    (gimp-floating-sel-to-layer paste)
    (gimp-layer-set-offsets paste 0 0)

    (set! path (if (null? paths) #f (car paths)))
    (while path
      (set! item-layer #f)
      (set! i 0)
      (while (< i n-layers)
	(set! l (vector-ref layers i))
        (if (string=? path (car (gimp-layer-get-name l)))
	    (begin
	      (set! item-layer l)
	      (set! i n-layers))
	  (set! i (+ 1 i))))

      (if item-layer
	  (let* ((points (gimp-path-get-points inImage path))
		 (array (nth 3 points))
		 (len (vector-length array))
		 (path-len (/ (- len 6) 9))
		 (offsets (gimp-drawable-offsets item-layer))
		 (base-x) (base-y)
		 (opacity 100) (opacity-step 0) (opacity-step2 0)
		 (start) (goal) (dimm-idx len)
		 (idx)
		 (copy))

	    (if (< path-len 2)
		(error (string-append "Path " path " is too short") path-len))

	    (cond ((= path-len 2)
		   (set! array (simple-path array))
		   (set! len (vector-length array))
		   (set! path-len (/ (- len 6) 9)))
		  (#t
		   (smooth-path array (* smooth 3))))

	    (set! base-x (vector-ref array 9))
	    (set! base-y (vector-ref array 10))

	    (if (large? array 6)
		(begin
		  (set! start (frame-num (vector-ref array 9)
					 (vector-ref array 10)
					 iwidth iheight))
		  (set! goal
			(max (frame-num (vector-ref array  6)
					(vector-ref array  7)
					iwidth iheight)
			     (frame-num (vector-ref array 12)
					(vector-ref array 13)
					iwidth iheight)))
		  (if (> goal start)
		      (begin
			(set! opacity-step (+ 1 (div 100 (+ 1 (- goal start)))))
			(set! opacity opacity-step)))))

	    (if (large? array (- len 9))
		(begin
		  (set! goal (frame-num (vector-ref array (- len 6))
					(vector-ref array (- len 5))
					iwidth iheight))
		  (set! start
			(min (frame-num (vector-ref array (- len 9))
					(vector-ref array (- len 8))
					iwidth iheight)
			     (frame-num (vector-ref array (- len 3))
					(vector-ref array (- len 2))
					iwidth iheight)))
		  (if (> goal start)
		      (begin
			(set! opacity-step2
			      (+ 1 (div 100 (+ 1 (- goal start)))))
			(set! dimm-idx
			      (+ 15 (* 9 (- path-len 1 (- goal start)))))))))

	    (gimp-edit-copy item-layer)
	    (set! copy (car (gimp-edit-paste paste 0)))
	    (gimp-floating-sel-to-layer copy)
	    (gimp-layer-set-offsets copy (car offsets) (cadr offsets))
	    (gimp-layer-set-opacity copy opacity)

	    (set! idx 18)
	    (while (< idx len)
	      (if (>= idx dimm-idx)
		  (set! opacity (max 0 (- opacity opacity-step2)))
	        (set! opacity (min 100 (+ opacity opacity-step))))

	      (set! copy (car (gimp-edit-paste paste 0)))
	      (gimp-floating-sel-to-layer copy)
	      (gimp-layer-set-offsets
	       copy
	       (+ (car offsets)  (- (vector-ref array      idx ) base-x))
	       (+ (cadr offsets) (- (vector-ref array (+ 1 idx)) base-y)))
	      (gimp-layer-set-opacity copy opacity)
	      (gimp-image-merge-down outImage copy 0)
	      (set! idx (+ 9 idx)))
	    ))
      (set! paths (cdr paths))
      (set! path (if (null? paths) #f (car paths))))

    (set! outLayer (car (gimp-image-merge-visible-layers outImage 0)))

    (if (= tile-to-layer 1)
	(begin
	  (set! y 0)
	  (while (< y iheight)
	    (set! x 0)
	    (while (< x iwidth)
	      (gimp-rect-select outImage x y fwidth fheight 2 0 0)
	      (gimp-edit-copy outLayer)
	      (set! paste (car (gimp-edit-paste outLayer 0)))
	      (gimp-floating-sel-to-layer paste)
	      (gimp-layer-set-offsets paste 0 0)
	      (set! x (+ x fwidth))
	      (if (>= x iwidth)
		  (set! y (+ y fheight)))))
	  (gimp-image-remove-layer outImage outLayer)
	  (gimp-image-resize outImage fwidth fheight 0 0)))

    (gimp-display-new outImage)
    (gimp-image-clean-all outImage)
    (gimp-image-undo-enable outImage)
    (gimp-image-undo-enable inImage))
)

; Register the function with the GIMP:

(script-fu-register
    "script-fu-trace-frames"
    _"<Image>/Script-Fu/Gymnp/Trace Frames..."
    "Write Comment to Each Frames from MPlayer's Tiled Output.
For more information, see
http://club.pep.ne.jp/~tamubun/computer/trace-frames.html"
    "Tamubun ( Contact http://bunysmc.exblog.jp/ )"
    "Tamubun"
    "6th April 2005"
    "*"
    SF-IMAGE "The Image" 0
    SF-DRAWABLE "The Layer" 0
    SF-ADJUSTMENT "Frame Width" '(160 10 1000 1 10 0 1)
    SF-ADJUSTMENT "Frame Height" '(120 10 1000 1 10 0 1)
    SF-ADJUSTMENT "Path Smoothing" '(2 0 5 1 10 0 0)
    SF-TOGGLE	"Tile to Layer"	TRUE
)
