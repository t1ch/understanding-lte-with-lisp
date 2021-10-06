;;; A simple implementation of serial CRC based on the paper "A PAINLESS GUIDE TO CRC ERROR DETECTION ALGORITHMS" by Ross N. Williams
;;; The Polynomial and message are passed as lists e.g (simple-crc '(1 0 0 1 0 1) '(1 0 0 1)) 
;;; The code then progresses into a parallel crc generator based on the paper "A Practical Parallel CRC Generation Method" by Evgeni Stavinov http://outputlogic.com/?p=158
(defun degree-of-polynomial (polynomial)
  (1- (length polynomial)))

(defun list-of-zeros (number-of-zeros)
  (loop for i from 1 upto number-of-zeros
	collect
	0))

(defun initialize-crc-register (degree-of-polynomial)
  (list-of-zeros degree-of-polynomial))

(defun augment-message (degree-of-polynomial message)
  (append message (list-of-zeros degree-of-polynomial)))

(defun most-significant-bit (crc-register)
  (car crc-register))

(defun update-crc-register (register message)
  (append (rest register) (list (car message))))

(defun xor (list-1 list-2)
  (map 'list #'(lambda (element-1 element-2) (logxor element-1 element-2)) list-1 list-2))
  
(defun crc (message crc-register polynomial)
  (if (null message)
      crc-register 
      (let* ((crc-register-msb (most-significant-bit crc-register))
	     (updated-crc-register (update-crc-register crc-register message))
	     (xored-register (xor updated-crc-register (rest polynomial))))
	(format t "~s~%" crc-register)
	(if (= 1 crc-register-msb)
	    (crc (rest message) xored-register polynomial)
	    (crc (rest message) updated-crc-register polynomial)))))

(defun crc-1 (message crc-register polynomial)
  (if (= (length message) 1)
      crc-register 
      (let* ((crc-register-msb (most-significant-bit crc-register))
	     (updated-crc-register (update-crc-register crc-register message))
	     (xored-register (xor updated-crc-register (rest polynomial))))
	(format t "~s~%" crc-register)
	(if (= 1 crc-register-msb)
	    (crc-1 (rest message) xored-register polynomial)
	    (crc-1 (rest message) updated-crc-register polynomial)))))

(defun simple-crc (polynomial message)
  (let* ((degree-of-polynomial (degree-of-polynomial polynomial))
	 (crc-register (initialize-crc-register degree-of-polynomial))
	 (augmented-message (augment-message degree-of-polynomial message)))
    (crc augmented-message crc-register polynomial)))

(defun simple-crc-1 (polynomial message)
  (let* ((degree-of-polynomial (degree-of-polynomial polynomial))
	 (crc-register (initialize-crc-register degree-of-polynomial))
	 (augmented-message (augment-message degree-of-polynomial message)))
    (crc-1 augmented-message crc-register polynomial)))
(defun list-shift-left (list number-of-shifts)
  (if (< number-of-shifts 1)
      list
      (list-shift-left (append (rest list) (list (car list))) (1- number-of-shifts))))


(defun generate-one-hot-lists (bit-width)
  (let ((seed (append (list-of-zeros (1- bit-width)) (list 1))))
   (loop for i from 0 upto (1- bit-width)  
		  collect
		  (list-shift-left seed i))))

(defun generate-NxM-matrix (polynomial message-width)
  (map 'list #'(lambda (x) (simple-crc polynomial x)) (generate-one-hot-lists message-width)))

(defun generate-MxM-matrix (polynomial)
  (let ((polynomial-width (degree-of-polynomial polynomial)))
    (map 'list #'(lambda (x) (simple-crc-1 polynomial x)) (generate-one-hot-lists polynomial-width))))
