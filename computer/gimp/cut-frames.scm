; cut-frames Version 0.4
; Copyright (C) 2005 Tamubun <http://bunysmc.exblog.jp/>
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

(define (script-fu-cut-frames inImage inLayer fwidth fheight orient smooth)
  (define (floor x) (- x (fmod x 1)))
  (define (div x y) (floor (/ x y)))
  (define (large? array pos)
    (let* ((x (aref array (+ 3 pos))) (y (aref array (+ 4 pos))))
      (not (and (>= 5 (abs (- x (aref array      pos ))))
		(>= 5 (abs (- y (aref array (+ 1 pos)))))
		(>= 5 (abs (- x (aref array (+ 6 pos)))))
		(>= 5 (abs (- y (aref array (+ 7 pos)))))))))

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
    (aset a      pos  x)
    (aset a (+ 1 pos) y)
    (aset a (+ 2 pos) type))

  (define (simple-path array)
    (let* ((iwidth  (car (gimp-image-width  inImage)))
	   (iheight (car (gimp-image-height inImage)))
	   (x0 (aref array  9)) (y0 (aref array 10))
	   (x1 (aref array 18)) (y1 (aref array 19))
	   (start (frame-num x0 y0 iwidth iheight))
	   (end   (frame-num x1 y1 iwidth iheight))
	   (x00 (min (aref array 6) (aref array 12)))
	   (y00 (min (aref array 7) (aref array 13)))
	   (cwidth  (abs (- (aref array 6) (aref array 12))))
	   (cheight (abs (- (aref array 7) (aref array 13))))
	   (dx (if (< start end) fwidth (- fwidth)))
	   (path-len (+ 1 (abs (- end start))))
	   (len (+ 6 (* 9 path-len)))
	   (array2 (cons-array len 'double))
	   (idx))

      (set! idx 6)
      (while (< idx 15)
        (aset array2 idx (aref array idx))
	(set! idx (+ 1 idx)))

      (set! idx 15)
      (while (< idx len)
	(set! x0 (+ x0 dx))
	(set! x00 (+ x00 dx))
	(if (>= x0 iwidth)
	    (begin
	      (set! x0 (- x0 iwidth ))
	      (set! y0 (+ y0 fheight))
	      (set! x00 (max 0 (- x00 iwidth)))
	      (set! y00 (+ y00 fheight)))
	  (if (< x0 0)
	      (begin
		(set! x0 (+ x0 iwidth ))
		(set! y0 (- y0 fheight))
		(set! x00 (min iwidth (+ x00 iwidth)))
		(set! y00 (- y00 fheight)))))
	(single array2      idx     x00            y00          2)
	(single array2 (+ 3 idx)    x0             y0           1)
	(single array2 (+ 6 idx) (+ x00 cwidth) (+ y00 cheight) 2)
	(set! idx (+ idx 9)))

      array2)) ; End of simple-path

  (define (smooth-path array step)
    (define (xy-value x-or-y idx d) (fmod (aref array (+ 9 x-or-y (* 9 idx))) d))

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
	     (hints nil))

	(while (>= pos 15)
	  (if (large? array pos)
	      (set! hints (cons (div (- pos 6) 9) hints)))
	  (set! pos (- pos 9)))
	hints)) ; End of get-hints

    (define (get-key-idx hints)
      (let* ((end (- (div (- (vector-length array) 6) 9) 1))
	     (s1 (* 1.5 step))
	     (h) (i)
	     (key-idx nil))
	(set! h (car hints))
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
	       (set! hints (cdr hints))
	       (set! h (car hints)))
	    (set! i (+ step i))))
	(reverse key-idx))) ; End of get-key-idx

;  smoot-path
;  -----------------
    (let* ((len (/ (- (vector-length array) 6) 9))
	   (end (- len (/ step 2)))
	   (key-idx (get-key-idx (get-hints)))
	   (key-idx-cpy (copy-list key-idx))
	   (seq (cons-array (+ 6 (* 9 (+ 2 (vector-length key-idx)))) 'double))
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
	(while key-idx
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
	  (set! y (* d (div (aref array (+ 9 x-or-y (* 9 i))) d)))
	  (aset array (+ 9 x-or-y (* 9 i)) (+ y (cadr point)))
	  (set! i (+ i 1)))

	(set! key-idx key-idx-cpy)
	(set! x-or-y (+ 1 x-or-y)))
      (gimp-image-delete img)
      )) ; End of smooth-path

;  script-fu-cut-frames
;  -----------------
  (let* ((outImage (car (gimp-image-new 10 10 RGB)))
	 (layer (car (gimp-layer-new outImage 10 10 RGB "Background" 100 NORMAL)))
	 (path (car (gimp-path-get-current inImage)))
	 (points (gimp-path-get-points inImage path))
	 (array (nth 3 points))
	 (len (vector-length array))
	 (path-len (/ (- len 6) 9))
	 (width 0) (total-width 0)
	 (height 0) (total-height 0)
	 (x) (y) (x1) (y1) (x2) (y2) (dx) (dy) (tmp) (w) (h)
	 (paste)
	 (full-frame #f)
	 (i))

    (gimp-image-undo-disable inImage)
    (gimp-image-undo-disable outImage)
    (gimp-image-add-layer outImage layer 0)
    (if (< path-len 2)
	(error "Selected path is too short" path-len))

    (if (not (large? array 6))
	(begin
	  (set! full-frame #t)
	  (set! x  (aref array 9))            (set! y  (aref array 10))
	  (set! x1 (* fwidth (div x fwidth))) (set! y1 (* fheight (div y fheight)))
	  (set! x  (+ x1 (/ fwidth 2)))       (set! y  (+ y1 (/ fheight 2)))
	  (set! x2 (+ x1 fwidth))             (set! y2 (+ y1 fheight))
	  (single array 6  x1 y1 2)
	  (single array 9  x  y  1)
	  (single array 12 x2 y2 2)))

    (cond ((= path-len 2)
	   (set! array (simple-path array))
	   (set! len (vector-length array))
	   (set! path-len (/ (- len 6) 9)))
	  (#t
	   (if (and (not full-frame) (= orient 2) (> smooth 0))
	       (smooth-path array (* smooth 3)))))

    (set! i 6)
    (while (< i len)
      (set! x1 (aref array i))
      (set! y1 (aref array (+ i 1)))
      (set! x (aref array (+ i 3)))
      (set! y (aref array (+ i 4)))
      (set! x2 (aref array (+ i 6)))
      (set! y2 (aref array (+ i 7)))
      (if full-frame
	  (begin
	    (set! x1 (* fwidth (div x fwidth))) (set! y1 (* fheight (div y fheight)))
	    (set! x  (+ x1 (/ fwidth 2)))       (set! y  (+ y1 (/ fheight 2)))
	    (set! x2 (+ x1 fwidth))             (set! y2 (+ y1 fheight))))

      (cond ((= orient 0)
	     (if (< i 15)
		 (prog1
		  (if (> y1 y2)
		      (prog1 (set! height (- y1 y2)) (set! dy (- y y2)))
		    (prog1   (set! height (- y2 y1)) (set! dy (- y y1))))
		  (set! total-height height)))
	     (if (> x1 x2)
		 (prog1 (set! width (- x1 x2)) (set! dx (- x x2)))
	       (prog1   (set! width (- x2 x1)) (set! dx (- x x1)))))

	    ((= orient 1)
	     (if (< i 15)
		 (prog1
		  (if (> x1 x2)
		      (prog1 (set! width (- x1 x2)) (set! dx (- x x2)))
		    (prog1   (set! width (- x2 x1)) (set! dx (- x x1))))
		  (set! total-width width)))
	     (if (> y1 y2)
		 (prog1 (set! height (- y1 y2)) (set! dy (- y y2)))
	       (prog1   (set! height (- y2 y1)) (set! dy (- y y1)))))

	    (#t
	     (if (< i 15)
		 (prog1
		  (if (> x1 x2)
		      (prog1 (set! width (- x1 x2)) (set! dx (- x x2)))
		    (prog1   (set! width (- x2 x1)) (set! dx (- x x1))))
		  (set! total-width width)
		  (if (> y1 y2)
		      (prog1 (set! height (- y1 y2)) (set! dy (- y y2)))
		    (prog1   (set! height (- y2 y1)) (set! dy (- y y1))))
		  (set! total-height height)))))

      (set! x1 (- x dx))
      (set! tmp (* fwidth (div x fwidth)))
      (if (>= x1 tmp)
	  (set! x2 0)
	(prog1 (set! x2 (- tmp x1)) (set! x1 tmp)))
      (set! w (if (<= (+ x1 width) (+ tmp fwidth)) width (- (+ tmp fwidth) x1)))
      (set! y1 (- y dy))
      (set! tmp (* fheight (div y fheight)))
      (if (>= y1 tmp)
	  (set! y2 0)
	(prog1 (set! y2 (- tmp y1)) (set! y1 tmp)))
      (set! h (if (<= (+ y1 height) (+ tmp fheight)) height (- (+ tmp fheight) y1)))

      (gimp-rect-select inImage x1 y1 w h 2 0 0)
      (gimp-edit-copy inLayer)
      (set! paste (car (gimp-edit-paste layer 0)))
      (gimp-floating-sel-to-layer paste)

      (cond ((= orient 0)
	     (gimp-layer-set-offsets paste (+ total-width x2) y2)
	     (set! total-width (floor (+ total-width width))))

	    ((= orient 1)
	     (gimp-layer-set-offsets paste x2 (+ total-height y2))
	     (set! total-height (floor (+ total-height height))))

	    ((= orient 2)
	     (gimp-layer-set-offsets paste x2 y2))

	    (#t nil))

      (set! i (+ i 9)))

      (gimp-selection-clear inImage)
      (gimp-image-resize outImage total-width total-height 0 0)
      (gimp-image-remove-layer outImage layer)
      (if (< orient 2)
	  (gimp-image-merge-visible-layers outImage 1))

      (gimp-display-new outImage)
      (gimp-image-clean-all outImage)
      (gimp-image-undo-enable outImage)
      (gimp-image-undo-enable inImage))
)

; Register the function with the GIMP:

(script-fu-register
    "script-fu-cut-frames"
    _"<Image>/Script-Fu/Gymnp/Cut Frames..."
    "Cut Frames from MPlayer's Tiled Output.
For more information, see
http://club.pep.ne.jp/~tamubun/computer/cut-frames.html"
    "Tamubun ( Contact http://bunysmc.exblog.jp/ )"
    "Tamubun"
    "31th March 2005"
    "*"
    SF-IMAGE "The Image" 0
    SF-DRAWABLE "The Layer" 0
    SF-ADJUSTMENT "Frame Width" '(160 10 1000 1 10 0 1)
    SF-ADJUSTMENT "Frame Height" '(120 10 1000 1 10 0 1)
    SF-OPTION "Orientation" '("Left to Right" "Up to Down" "Layer")
    SF-ADJUSTMENT "Path Smoothing" '(2 0 5 1 10 0 0)
)
