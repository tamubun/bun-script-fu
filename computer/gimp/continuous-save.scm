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
; version 0.9-tamubun
;              2018/02/23 tamubun <http://bunysmc.exblog.jp/>
;     - Added "Layer to Image Size" option.
; version 1.0-tamubun
;              2019/07/14 tamubun <http://bunysmc.exblog.jp/>
;     - Modifed for Gimp 2.10
; version 1.1-tamubun
;              2020/01/23 tamubun <http://bunysmc.exblog.jp/>
;     - Debugged: Indexed source image is now treated correctly.
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
		resize-to-image         ; If true, resize all layers to image
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
	 (img-width (car (gimp-image-width img)))
	 (img-height (car (gimp-image-height img)))
	) ; end variable definition
      (if (and (> len 0)
		 (not (eqv? #\/ (string-ref file-name (- len 1))))
		 (equal? (car (last (strbreakup file-name "/"))) "#"))
	    (set! file-name (substring file-name 0 (- len 1))))

	(while (< count number)
	  (let* (
		 ;; tmp image type should be RGB because INDEXED or GRAY image makes trouble
	         (tmp-image-type 0)
	         (layer (vector-ref (cadr layers) (if (= reverse 1) count (- number 1 count))))
		 (layer-width  (car (gimp-drawable-width  layer)))
		 (layer-height (car (gimp-drawable-height layer)))
		 (tmp-img-width  (if (= 1 resize-to-image) img-width layer-width))
		 (tmp-img-height (if (= 1 resize-to-image) img-height layer-height))
	         (tmp-img (car (gimp-image-new tmp-img-width tmp-img-height tmp-image-type)))
		 ; NORMAL = LAYER-MODE-NORMAL-LEGACY = 0
	         (tmp-layer (car (gimp-layer-new tmp-img
	                                         layer-width layer-height
	                                         (+ 1 (* 2 tmp-image-type)) "Temp Layer" 100 0)))
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
		; APPLY = MASK-APPLY = 0
	        (gimp-image-remove-layer-mask tmp-img tmp-layer 0)
	        (gimp-displays-flush)))
	    (if (= 1 resize-to-image)
		(let* ((offsets (gimp-drawable-offsets layer)))
		  (gimp-layer-set-offsets tmp-layer (car offsets) (cadr offsets))
		  (gimp-layer-resize-to-image-size tmp-layer)))
	    ;(gimp-display-new tmp-img)
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
	SF-TOGGLE       "Layer to Image Size"   TRUE
	SF-ADJUSTMENT	"Quality"	        '(75 0 100 1 5 0 0)
)
