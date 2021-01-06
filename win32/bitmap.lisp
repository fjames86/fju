;;;; Copyright Frank James 2018 

;;; This file defines a program for generating C code for 32bpp bitmap resources
;;; in win32 gui applications. The reason for doing it this way, as opposed to 
;;; the standard resource compiler, is that it actually works for images with alpha (transparency).
;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "nibbles"))

(defpackage #:bitmap
  (:use #:cl)
  (:export #:generate-bitmap-resource))

(in-package #:bitmap)

(defun generate-bitmap-resource (filename &key (stream *standard-output*) (name "NAME") icon-p)
  "Parse a bitmap file and generate C code so that the resource can be embedded
within a C win32 program rather than having to deliver the image separately. 
The function prints out the code to be inserted into your project.
Generate the bitmap using gimp and save as a bmp file in 32bpp format (8a 8r 8g 8b).
" 
  (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
    (let ((len (file-length f)))
      (let ((bmp (make-array len :element-type '(unsigned-byte 8))))
	(read-sequence bmp f)

	;; extract dimensions from header -- see MSDN page for more info on bitmap stuctures 
	;; https://msdn.microsoft.com/en-us/library/windows/desktop/dd183391(v=vs.85).aspx
	(let ((offset (nibbles:ub32ref/le bmp 10))
	      (width (nibbles:sb32ref/le bmp 18))
	      (height (nibbles:sb32ref/le bmp 22))
	      (planes (nibbles:ub16ref/le bmp 26))
	      (bits-per-pixel (nibbles:ub16ref/le bmp 28)))

	  (ecase bits-per-pixel
	    (32 
	     ;; bitmap stores it as aa rr gg bb
	     ;; we want it as bb gg rr aa
	     ;; BUT: we need to use premultiplied alpha 
	     (do ((i 0 (+ i 4)))
		 ((= i (- (length bmp) offset)))
	       (let ((aa (aref bmp (+ offset i 0)))
		     (bb (aref bmp (+ offset i 1)))
		     (gg (aref bmp (+ offset i 2)))
		     (rr (aref bmp (+ offset i 3))))
		 (setf (aref bmp (+ offset i 0))
		       (truncate (* bb aa) #xff)
		       (aref bmp (+ offset i 1))
		       (truncate (* gg aa) #xff)
		       (aref bmp (+ offset i 3))
		       (truncate (* rr aa) #xff)
		       (aref bmp (+ offset i 2))
		       aa))))
	    (24
	     #+nil(do ((i 0 (+ i 3)))
		 ((= i (- (length bmp) offset)))
	       (setf (aref bmp (+ offset i 0)) #xff
		     (aref bmp (+ offset i 1)) 0
		     (aref bmp (+ offset i 2)) 0))

	     
	     nil))
	  
	  ;; print output for use with a C compiler 
	  (format stream "static uint8_t ~A_bits[] = {~%" name)
	  (do ((i offset (+ i 16))
	       (len (length bmp)))
	      ((>= i len))
	    (format stream "    ")
	    (dotimes (j 16)
	      (when (< (+ i j) len)
		(format stream "0x~2,'0X, " (aref bmp (+ i j)))))
	    (format stream "~%"))
	  (format stream "};~%")

	  ;; print function
	  (format stream "HBITMAP ~A_bm( void ) {~%" name)
	  (format stream "    static HBITMAP hbm;~%")
	  (format stream "    BITMAPINFO bmi;~%")
	  (format stream "    void *pbits;~%")
	  (format stream "~%")
	  (format stream "    if( !hbm ) {~%")
	  (format stream "        memset( &bmi, 0, sizeof(bmi) );~%")
	  (format stream "        bmi.bmiHeader.biBitCount = ~A;~%" bits-per-pixel)
	  (format stream "        bmi.bmiHeader.biHeight = ~A;~%" height)
	  (format stream "        bmi.bmiHeader.biPlanes = ~A;~%" planes)
	  (format stream "        bmi.bmiHeader.biSize = sizeof(bmi.bmiHeader);~%")
	  (format stream "        bmi.bmiHeader.biWidth = ~A;~%" width)
	  (format stream "~%")
	  (format stream "        hbm = CreateDIBSection( NULL, &bmi, 0, &pbits, NULL, 0 );~%")
	  (format stream "        SetDIBits( NULL, hbm, 0, ~A, ~A_bits, &bmi, BI_RGB );~%" height name)
	  (format stream "    }~%")
	  (format stream "    return hbm;~%")
	  (format stream "}~%")
	  (when icon-p
	    (format stream "HICON ~A_icon( void ) {~%" name)
	    (format stream "    static HICON hicon;~%")
	    (format stream "    if( !hicon ) {~%")
	    (format stream "        ICONINFO ico;~%")
	    (format stream "        memset( &ico, 0, sizeof(ico) );~%")
	    (format stream "        ico.fIcon = TRUE;~%")
	    (format stream "        ico.hbmMask = ~A_bm();~%" name)
	    (format stream "        ico.hbmColor = ~A_bm();~%" name)
	    (format stream "        hicon = CreateIconIndirect( &ico );~%")
	    (format stream "    }~%")
	    (format stream "    return hicon;~%")
	    (format stream "}~%")))))))

