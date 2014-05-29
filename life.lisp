(defparameter *field* (make-array '(20 20) :initial-element 0))
(defparameter *temp* (make-array '(20 20) :initial-element 0))

(defun life ()
  (sdl:with-init ()
    (sdl:window 600 600 :title-caption "Life")
    (setf (sdl:frame-rate) 1)

    ;; Initial state

    ;; Glider
    (setf (aref *field* 7 10) 1)
    (setf (aref *field* 8 10) 1)
    (setf (aref *field* 9 10) 1)
;    (setf (aref *field* 9 9) 1)
;    (setf (aref *field* 8 8) 1)

    ;; Blinker
    ;; (set-pixel 10 10 sdl:*green*)
    ;; (set-pixel 11 10 sdl:*green*)
    ;; (set-pixel 12 10 sdl:*green*)

    ;; (setf (aref *field* 9 7) 1)
    ;; (setf (aref *field* 9 8) 1)
    ;; (setf (aref *field* 9 9) 1)
    ;; (setf (aref *field* 9 10) 1)
    ;; (setf (aref *field* 9 11) 1)
    ;; (setf (aref *field* 9 12) 1)
    ;; (setf (aref *field* 9 13) 1)
    ;; (setf (aref *field* 9 14) 1)
    ;; (setf (aref *field* 9 15) 1)
    ;; (setf (aref *field* 9 16) 1)
    ;; (setf (aref *field* 9 17) 1)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event () (sdl:push-quit-event))
      (:idle ()
             (live)
             (set-grid 0 0)
             (sdl:update-display)
             (sleep 1)))))

(defun set-grid (x y)
  (if (or (>= x 600) (>= y 600))
      nil
      (progn (sdl:draw-rectangle (sdl:rectangle-from-edges-* x 0 x 600))
             (sdl:draw-rectangle (sdl:rectangle-from-edges-* 0 y 600 y))
             (set-grid (+ x 30) (+ y 30)))))

(defun get-coord (i)
  (+ 15 (* i 30)))

(defun set-pixel (i j color)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* (get-coord i) (get-coord j)  30 30)
                :color color))

(defun live ()
  (dotimes (i 20)
    (dotimes (j 20)
      (let ((n (neighbor-count i j)))
        (if (and (< n 2) (is-alive i j))
            (setf (aref *temp* i j) 0)
            (if (and (> n 3) (is-alive i j))
                (setf (aref *temp* i j) 0)
                (if (and (or (= n 3) (= n 2)) (is-alive i j))
                    (setf (aref *temp* i j) 1)
                    (if (and (= n 3) (not (is-alive i j)))
                        (setf (aref *temp* i j) 1)
                        (setf (aref *temp* i j) 0))))))))
  (dotimes (i 20)
    (dotimes (j 20)
      (if (= (aref *temp* i j) 1)
          (set-pixel i j sdl:*green*)
          (set-pixel i j sdl:*black*))
      (setf (aref *field* i j) (aref *temp* i j)))))

(defun is-alive (i j)
  (if (or (or (< i 0) (< j 0)) 
          (or (>= i 20) (>= j 20)))
      0
      (if (>= (aref *field* i j) 1) ; alive and in bounds
          1
          0)))

(defun neighbor-count (i j)
  (+ (is-alive (1- i) (1- j)) 
     (is-alive i (1- j)) 
     (is-alive (1+ i) (1- j)) 
     (is-alive (1- i) j) 
     (is-alive (1+ i) j) 
     (is-alive (1- i) (1+ j)) 
     (is-alive i (1+ j)) 
     (is-alive (1+ i) (1+ j))))

