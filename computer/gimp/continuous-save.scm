; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Continuous Save --- save the file which named "base-name_(number).(type)"
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
;
; Modified by Tamubun <http://bunysmc.exblog.jp/>
;
; This script was born of miyoken's idea.
;
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  2001/05/20 iccii <iccii@hotmail.com>
;     - Initial relase
; version 0.2  2001/05/24 iccii <iccii@hotmail.com>
;     - Make better
; version 0.3  2001/05/24 iccii <iccii@hotmail.com>
;     - Chose the filename with directory
;     - Saved image type equal to original image type
; version 0.3a 2001/09/26 iccii <iccii@hotmail.com>
;     - Bug fixed in checking the file type
;
; ====
;
; version 0.4-tamubun
;              2005/03/11 tamubun <http://bunysmc.exblog.jp/>
;     - Added "Digits" option, and changed the file names for each layer.
;     - Added "Reverse Order" option, to control output ordering.
;         (To use the original ordering, i.e. from top layer to bottom
;          layer, "Reverse Ordering" must be checked.)
;     - Unified "JPEG Compression" option and "PNG Compression" option
;         to single "Quality" option.
;     - Deleted "Interactive" option.
; version 0.5-tamubun
;              2005/03/29 tamubun <http://bunysmc.exblog.jp/>
;     - Added "Start Number" option.
; version 0.6-tamubun
;              2005/04/06 tamubun <http://bunysmc.exblog.jp/>
;     - Added "#" convention.
; version 0.7-tamubun
;              2007/03/03 tamubun <http://bunysmc.exblog.jp/>
;     - Debugged: Removed junks when layers with alpha channel
;                 was saved.
; version 0.8-tamubun
;              2009/05/31 tamubun <http://bunysmc.exblog.jp/>
;     - Modified for Gimp 2.4
;
; --------------------------------------------------------------------
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
;


(define (script-fu-continuous-save
		img			; IMAGE
		drawable		; DRAWABLE (no need)
		save-type		; saving image type
		dir-name		; base file directory
		file                    ; file name
		digits			; trailing 0's
		start                   ; start number
		reverse			; If true, order from top to bottom
		quality			; compression quality for JPG, PNG
		)

  (define (floor x) (- x (fmod x 1)))

  (define (div x y) (floor (/ x y)))

  (define (to-save-bmp img layer file-name)
	  (file-bmp-save 1 img (car (gimp-image-flatten img)) file-name ""))

  (define (to-save-jpg img layer file-name jpg-comp)
	  (file-jpeg-save 1 img (car (gimp-image-flatten img)) file-name ""
			  (/ jpg-comp 100) 0.0 1 0 "" 0 0 0 1))

  (define (to-save-png img layer file-name png-comp)
	  (file-png-save 1 img layer file-name "" 0 png-comp 1 1 1 1 1))

  (define (gen-name file-name count ext)
	  (let* ((zeros "0000000000")
		 (str (number->string (+ start count)))
		 (num (string-append zeros str))
		 (pos (- (string-length num) (max digits (string-length str)))))
	    (string-append file-name (substring num pos) ext)))

  (let* (
	 (layers (gimp-image-get-layers img))
	 (file-name (string-append dir-name "/" file))
	 (count 0)
	 (number (car layers))
	 (image-type (car (gimp-image-base-type img)))
	 (len (string-length file-name))
	 (png-comp (- 9 (div quality 12)))

	) ; end variable definition

        (if (and (> len 0)
		 (not (eqv? #\/ (string-ref file-name (- len 1))))
		 (equal? (car (last (strbreakup file-name "/"))) "#"))
	    (set! file-name (substring file-name 0 (- len 1))))

	(while (< count number)
	  (let* (
	         (tmp-image-type
	           (cond
	             ((and (eqv? save-type 0) (eqv? image-type 2))
	               0)		;; convert to RGB because BMP can't treat INDEXED image
	             ((and (eqv? save-type 1) (or (eqv? image-type 1) (eqv? image-type 2)))
	               0)		;; convert to RGB because JPEG can't treat INDEXED and GRAY image
	             (image-type)))	;; otherwise, equal to origianl image type
	         (layer (vector-ref (cadr layers) (if (= reverse 1) count (- number 1 count))))
(print 2)
	         (tmp-img (car (gimp-image-new (car (gimp-drawable-width  layer))
	                                       (car (gimp-drawable-height layer))
	                                       tmp-image-type)))
	         (tmp-layer (car (gimp-layer-new tmp-img
	                                         (car (gimp-drawable-width  layer))
	                                         (car (gimp-drawable-height layer))
	                                         (+ 1 (* 2 tmp-image-type)) "Temp Layer" 100 NORMAL)))
	        )

	;; create an image with single layer, and remove the layer mask (if exists)
	    (gimp-image-add-layer tmp-img tmp-layer 0)
	    (gimp-edit-clear tmp-layer)
	    (gimp-edit-copy layer)
	    (gimp-floating-sel-anchor (car (gimp-edit-paste tmp-layer 0)))
	    (if (< 0 (car (gimp-layer-mask layer)))
	      (let* ((tmp-mask (car (gimp-layer-create-mask tmp-layer 0))))
	        (gimp-edit-copy (car (gimp-layer-mask layer)))
	        (gimp-floating-sel-anchor (car (gimp-edit-paste tmp-layer 0)))
	        (gimp-image-remove-layer-mask tmp-img tmp-layer APPLY)
	        (gimp-displays-flush)))
	    ;(set! tmp-display (car (gimp-display-new tmp-img)))
	    ;(gimp-displays-flush)

	;; save a resulting image by specified image type
	    (cond
	      ((eqv? save-type 0)
	        (to-save-bmp tmp-img tmp-layer (gen-name file-name count ".bmp")))
	      ((eqv? save-type 1)
	        (to-save-jpg tmp-img tmp-layer (gen-name file-name count ".jpg") quality))
	      ((eqv? save-type 2)
	        (to-save-png tmp-img tmp-layer (gen-name file-name count ".png") png-comp))
	    )
	    (gimp-image-delete tmp-img)
	    ;(gimp-display-delete tmp-display)
	  )
	  (set! count (+ count 1))
	) ;; repeat until all layer is saved
  )
)

(script-fu-register
	"script-fu-continuous-save"
	"<Image>/Script-Fu/Utils/Continuous Save..."
	"Save an Image by single layer with continuous number"
	"Tamubun <http://bunysmc.exblog.jp/>, Original: Iccii <iccii@hotmail.com>"
	"Iccii"
	"March, 2007"
	"RGB* INDEXED* GRAY*"
	SF-IMAGE	"Image"			0
	SF-DRAWABLE	"Drawable"		0
	SF-OPTION	"Saved File Type"	'("BMP" "JPG" "PNG")
	SF-DIRNAME	"Base File Directory"	""
	SF-STRING	"Base File Name"	"MyPicture_"
	SF-ADJUSTMENT	"Digits"		'(8 1 10 1 5 0 1)
	SF-ADJUSTMENT	"Start Number"		'(1 1 9999999 10 5 0 1)
	SF-TOGGLE	"Reverse Order"		FALSE
	SF-ADJUSTMENT	"Quality"	        '(75 0 100 1 5 0 0)
)
