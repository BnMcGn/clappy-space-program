;;;; clappy-space-program.lisp

(in-package #:clappy-space-program)

(defvar *canvas-width* 800)
(defvar *canvas-height* 800)

(gamekit:register-resource-package :keyword (asdf:system-relative-pathname :clappy-space-program "assets/"))

(gamekit:define-image :bg "bg.png")
(gamekit:define-image :bird-1 "bird-1.png")
(gamekit:define-image :bird-2 "bird-2.png")
(gamekit:define-image :bird-3 "bird-3.png")
(gamekit:define-image :bird-x "bird-x.png")
(gamekit:define-image :particle-1 "particle-1.png")
(gamekit:define-image :particle-2 "particle-2.png")

;; Doesn't support mp3
;;(gamekit:define-sound :boom "boom.mp3")

(gamekit:defgame clappy-space-program () ()
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Chubby Falcons"))  ; window's title

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *skyblue* (gamekit:vec4 0.296875 0.5234375 0.546875 1))
(defvar *origin* (gamekit:vec2 0 0))

(defmethod gamekit:draw ((app clappy-space-program))
  (gamekit:draw-rect
   *origin* *canvas-width* *canvas-height* :fill-paint *skyblue*)
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas
     (/ *canvas-width* 2) (/ *canvas-height* 2))
    (draw-background)
    (draw-particles)
    (draw-birds)
    (draw-altitude-limit)))

(defun make-clock (ticks-per-second)
  (unless (<= ticks-per-second internal-time-units-per-second)
    (error "Internal clock will not support that many ticks per second"))
  (let ((units (floor internal-time-units-per-second ticks-per-second))
        (start (get-internal-real-time)))
    (lambda ()
      (floor (- (get-internal-real-time) start) units))))

(defparameter *clock* (lambda ()
                        (*
                         (floor (get-internal-real-time) 60)
                         60)))
(defvar *time* 0)
(defvar *delta* 0)
(defvar *increment* 0.02)

(defmethod gamekit:act ((app clappy-space-program))

  (when *boost*
    (setf *the-bird* (apply #'boost-bird *the-bird*))
    (setf *boost* nil)
    (push-particles (apply #'make-particles *the-bird*)))
  (let ((now (funcall *clock*)))
    (when (eql 0 *time*)
      (setf *time* now))
    (incf *delta* (/ (- now *time*) 1000))
    (setf *time* now)

    (loop
      until (< *delta* 0)
      do (let ((live nil)
               (dead nil)
               (particles nil))
           (decf *delta* *increment*)
           (multiple-value-bind (bird sig)
               (apply #'grav *increment* nil *the-bird*)
             (if (eq :kill sig)
                 (progn
                   (push (apply #'kill-bird bird) dead)
                   (setf *the-bird* (new-bird)))
                 (setf *the-bird* bird)))
           (dolist (bird *live-birds*)
             (push (apply #'grav *increment* nil bird) live))
           (dolist (bird *dead-birds*)
             (multiple-value-bind (nbird sig)
                 (apply #'grav *increment* t bird)
               (unless (eq sig :grounded)
                 (push nbird dead))))
           (multiple-value-bind (live newdead killsig)
               (collisions (cons *the-bird* live))
             (when killsig
               (push (apply #'kill-bird *the-bird*) dead)
               (setf *the-bird* (new-bird)))
             (setf *live-birds* live)
             (setf *dead-birds*
                   (concatenate
                    'list dead
                    (mapcar
                     (lambda (b) (apply #'kill-bird b)) newdead))))

           (dolist (p *particles*)
             (alexandria:when-let
                 ((upd-part (apply #'step-particle *increment* p)))
               (push upd-part particles)))
           (setf *particles* particles)))
    (when (> (getf *the-bird* :tm) 5.0)
      (push *the-bird* *live-birds*)
      (setf *the-bird* (new-bird)))))

(defun draw-background ()
  (gamekit:draw-image (gamekit:vec2 -256 -256) :bg
                      :width 512 :height 512))

(defun draw-particles ()
  (dolist (pt *particles*)
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas
       (+ (getf pt :x) (* (getf pt :u) *delta*))
       (+ (getf pt :y) (* (getf pt :v) *delta*)))
      (funcall (getf pt :func)))))

(defun draw-birds ()
  (let ((interval (/ *time* 1000))
        (step (1+ (/ *delta* *increment*))))
    (dolist (bird *live-birds*)
      (draw-bird bird interval step))
    (dolist (bird *dead-birds*)
      (draw-bird bird interval step t))
    (draw-bird *the-bird* interval step)))

(defun draw-bird (bird time alpha &optional dead)
  (gamekit:with-pushed-canvas ()
    (let ((x (getf bird :x))
          (y (getf bird :y))
          (a (getf bird :a))
          (prevx (getf bird :prev-x))
          (prevy (getf bird :prev-y))
          (preva (getf bird :prev-a)))
      (gamekit:translate-canvas
       (+ (* alpha x) (* (1- alpha) prevx))
       (+ (* alpha y) (* (1- alpha) prevy)))
      (gamekit:rotate-canvas (interpolate-angle preva a alpha))
      ;(rotate-canvas-radians (interpolate-angle preva a alpha))
      (gamekit:translate-canvas -20 -20)
      (cond
        (dead
         (gamekit:draw-image *origin* :bird-x))
        ((< (mod time 0.8) 0.2)
         (gamekit:draw-image *origin* :bird-1))
        ((< (mod time 0.8) 0.4)
         (gamekit:draw-image *origin* :bird-2))
        ((< (mod time 0.8) 0.6)
         (gamekit:draw-image *origin* :bird-3))
        (t
         (gamekit:draw-image *origin* :bird-2))))))

(defun par1 ()
  (gamekit:draw-image (gamekit:vec2 0 0) :particle-1))

(defun par2 ()
  (gamekit:draw-image (gamekit:vec2 0 0) :particle-2))

(defun draw-altitude-limit ()
  (let ((quant 50))
    (dotimes (n quant)
      (gamekit:with-pushed-canvas ()
        (rotate-canvas-radians (* n (/ (* pi 2) quant)))
        (gamekit:translate-canvas 400 0)
        (rotate-canvas-radians (* (- n) (/ (* pi 2) quant)))
        (if (evenp n)
            (par1)
            (par2))))))

(defun run ()
  (setf *the-bird* (new-bird))
  (setf *live-birds* nil)
  (setf *dead-birds* nil)
  (setf *time* 0)
  (setf *delta* 0)
  (gamekit:start 'clappy-space-program)
  (gamekit:bind-button :space :pressed
                       (lambda () (setf *boost* t)))

  )



