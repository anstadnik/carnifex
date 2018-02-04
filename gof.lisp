(ql:quickload 'lispbuilder-sdl)
(defvar *color* sdl:*white*)

(defun game ()
  "main function"
  (sdl:with-init ()
				 (sdl:window 200 200 :title-caption "Potato")
				 (setf (sdl:frame-rate) 60)
				 (sdl:with-events ()
								  (:quit-event () t)
								  (:key-down-event (:key key)
												   (when (sdl:key= key :sdl-key-equals)
													   (setf *color* (sdl:color :r (random 255) :g (random 255) :b (random 255)))
													   )
												   )
								  (:idle ()
										 (sdl:clear-display sdl:*black*)
										 (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20) :color *color*)
										 (sdl:update-display)
										 )
								  )
				 )
  )

(sb-int:with-float-traps-masked (:invalid :inexact :overflow) (game))