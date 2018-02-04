(ql:quickload 'lispbuilder-sdl)
	(defvar *color* sdl:*white*)
	(defvar *arr*)
	(defvar *x_cor* 0)
	(defvar *y_cor* 0)
	(defvar *tmp*)
	(defvar *counter* 0)
	(defvar *dur* 10)
	(defvar *pause* -1)
(defvar *size* 5)

(defun gol(arr tmp)
 (dotimes (i (- (array-dimension arr 0) 1));go through y
  (dotimes (j (- (array-dimension arr 1) 1));go through x
   (let ((s 0));our sum
    (loop for q from -1 to 1;to access 3 neighbours
     do (loop for k from -1 to 1;same
	     do (if (and (> (+ i q) 0) (> (+ j k) 0));if we're not out of boundaries
		     (if (and
			  (or (not (= q 0)) (not (= k 0)));it's not the current point itself (offsets r 0 and 0)
			  (not (= (aref tmp (+ i q) (+ j k)) 0)));and if that neighbour is not dead
		      (incf s)))));inc sum

    (if (not (= (aref tmp i j) 0));if cur sell isn't dead
     (if (or (< s 2) (> s 3));and it has less then 2 or more than 3 neighbours
      (setf (aref arr i j) 0));it dies(
	      (if (= s 3);if it's dead and has 3 neighbours
	       (setf (aref arr i j) 1)));it lives!
    ))))

 (defun copy_arr (src dst);copy src to dst
  (dotimes (i (- (array-dimension src 0) 1))
   (dotimes (j (- (array-dimension src 1) 1))
    (setf (aref dst i j) (aref src i j)))))

(defun game (a b)
 "main function"
 (setq *arr* (make-array (list a b)))
 (setq *tmp* (make-array (list a b)))
 (dotimes (i (- (array-dimension *arr* 0) 1))
  (dotimes (j (- (array-dimension *arr* 1) 1))
   (setf (aref *arr* i j) (random 2))));randomly fill array

 (defun event (key)
  (when (sdl:key= key :sdl-key-a)
   (setf *x_cor* (- *x_cor* 10)))
  (when (sdl:key= key :sdl-key-s)
   (setf *y_cor* (+ *y_cor* 10)))
  (when (sdl:key= key :sdl-key-d)
   (setf *x_cor* (+ *x_cor* 10)))
  (when (sdl:key= key :sdl-key-w)
   (setf *y_cor* (- *y_cor* 10)))
  (when (sdl:key= key :sdl-key-escape)
   (exit))
  (when (sdl:key= key :sdl-key-minus)
   (if (> *size* 11)
    (setq *size* (- *size* 10))))
  (when (sdl:key= key :sdl-key-equals)
   (if (< *size* 100)
    (setq *size* (+ *size* 10))))
  (when (sdl:key= key :sdl-key-comma)
   (if (< *dur* 120)
    (setq *dur* (+ *dur* 10))))
  (when (sdl:key= key :sdl-key-p)
   (setq *pause* (* *pause* -1)))
	(when (sdl:key= key :sdl-key-period)
	 (if (> *dur* 0)
	  (setq *dur* (- *dur* 10)))))
	"handler"
(sdl:with-init ()
 (sdl:window 500 500 :title-caption "Potato" :FPS (make-instance 'sdl:fps-timestep))
 (setf (sdl:frame-rate) 60)
 (sdl:enable-key-repeat 1 1)
 (sdl:with-events ()
  (:quit-event () t)
  (:key-down-event (:key key)
   (event(key))
  )
  (:idle ()
   (if (and (> *counter* *dur*) (eq *pause* -1))
    (progn
     (sdl:clear-display sdl:*black*)
     (sdl:with-timestep ()
      (copy_arr *arr* *tmp*)
      (gol *arr* *tmp*))
     (dotimes (i a)
      (dotimes (j a)
       (sdl:draw-box (sdl:rectangle :x (+ *x_cor* (* *size* i)) :y (+ *y_cor* (* *size* j)) :w *size* :h *size* :fp nil) :color (if (= (aref *arr* i j) 1) sdl:*white* sdl:*cyan*))))
     (sdl:update-display)
     (setf *counter* 0)
    )
    (incf *counter*)
   )
  )
	)
	 )
	  )

	   (sb-int:with-float-traps-masked (:invalid :inexact :overflow) (game 100 100))
