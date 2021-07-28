(defun degree-of-polynomial (polynomial)
  (1- (length polynomial)))

(defun initialize-crc-register (degree-of-polynomial)
  (loop for i from 1 upto degree-of-polynomial
	collect
	0))

(defun augment-message (degree-of-polynomial message)
  (append message (loop for i from 1 upto degree-of-polynomial
			collect
			0)))

(defun most-significant-bit (crc-register)
  (car crc-register))

(defun update-crc-register (register message)
  (append (rest register) (list (car message))))

(defun xor (list-1 list-2)
  (loop for x in list-1
	for y in list-2
	collect
	(logxor x y)))

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


(defun simple-crc (polynomial message)
  (let* ((degree-of-polynomial (degree-of-polynomial polynomial))
	 (crc-register (initialize-crc-register degree-of-polynomial))
	 (augmented-message (augment-message degree-of-polynomial message)))
    (crc augmented-message crc-register polynomial)))
