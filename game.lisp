(in-package :clappy-space-program)

(defconstant +m+ 1000000)
(defconstant +g+ 10)
(defconstant +r+ 100)

(defvar *flap* 0.16)

(defvar *boost* nil)
(defvar *the-bird* nil)
(defvar *live-birds* nil)
(defvar *dead-birds* nil)
(defvar *particles* nil)

(defun new-bird ()
  (let ((y (* (- +r+) 1.25))
        (a (- (/ pi 2))))
    (list
     :x 0.0
     :y y
     :u 0.0
     :v 0.0
     :f 1.0
     :a a
     :tm 0.0
     :prev-x 0.0
     :prev-y y
     :prev-a a)))

(defun fake-bird ()
    '(:X 201.7880007883659d0 :Y 150.5168245808682d0 :U -51.67668839654161d0 :V
     195.8815563984243d0 :F 159.67324870775448d0 :A 2.196660293600592d0 :TM
     17.640213 :PREV-X 202.82153453319546d0 :PREV-Y 146.5991935404656d0 :PREV-A
     2.181462468385244d0))

(defun interpolate-angle (a1 a2 alpha)
  (let* ((pi2 (* 2 pi))
         (da (mod (- a2 a1) pi2))
         (da (cond
               ((> da pi) (- da pi2))
               ((< da (- pi)) (+ da pi2))
               (t da))))
    (+ a1 (* alpha da))))

(defun distance (x y)
  (sqrt (+ (* x x) (* y y))))

;; unused
(defun live-angle (x y)
  (let ((dist (distance x y))
        (pid (/ pi 2))
        (aa (atan y x)))
    (if (< dist 200)
        (+ aa (* pid (/ (- dist 100) 100)))
        (+ aa pid))))

(defun grav (delta dead &key x y u v f a tm prev-x prev-y
                          prev-a)
  (declare (ignore prev-x prev-y prev-a f))
  (let* ((dist (distance x y))
         (prev-x x)
         (prev-y y)
         (prev-a a)
         (f (/ (* +g+ +m+) (* dist dist)))
         (u (- u (* (/ x dist) f delta)))
         (v (- v (* (/ y dist) f delta)))
         (a (if dead
                (+ a (* 10 delta))
                (let ((pid (/ pi 2))
                      (aa (atan y x)))
                  (if (< dist 200)
                      (+ aa (* pid (/ (- dist 100) 100)))
                      (+ aa pid)))))
         (xx (+ x (* u delta)))
         (yy (+ y (* v delta)))
         (dd (distance xx yy))
         (state nil))
    (if (> dd +r+)
        (setf
         x xx
         y yy
         tm (+ tm delta)
         state (when (> dd 400)
                 :kill))
        (setf
         x (/ (* +r+ xx) dd)
         y (/ (* +r+ yy) dd)
         u 0
         v 0
         state :grounded))
    (values (list :x x :y y :u u :v v :f f :a a :tm tm 
                  :prev-x prev-x :prev-y prev-y :prev-a prev-a)
            state)))

(defun make-particles (&key x y u v a (quantity 10)
                       &allow-other-keys)
  (let* ((anglefunc
          (if a
              (lambda ()
                (+ a (* (- 0.5 (random 1.0) 0.25))))
              (lambda ()
                (* (random 1.0) pi 2))))
         (ufunc (if a (lambda (x) (- u x)) #'identity))
         (vfunc (if a (lambda (x) (- v x)) #'identity))
         (uu (funcall ufunc
                      (* (cos (funcall anglefunc))
                         100
                         (+ 1 (random 1.0)))))
         (vv (funcall vfunc
                      (* (sin (funcall anglefunc))
                         100
                         (+ 1 (random 1.0)))))
         (stor nil))
    (dotimes (i quantity)
      (push
       (list :x x :y y :u uu :v vv :tm (+ 0.5 (random 1.0))
             :func (alexandria:whichever #'par1 #'par2))
       stor))
    stor))

(defun step-particle (delta &key x y u v tm func)
  (incf x (* u delta))
  (incf y (* v delta))
  (decf tm delta)
  (when (> tm 0)
    (list :x x :y y :u u :v v :tm tm :func func)))

(defun push-particles (parts)
  (dolist (p parts)
    (push p *particles*)))

(defun boost-bird (&key x y u v f a tm prev-x prev-y prev-a)
  (let (;;(d (sqrt (+ (* u u) (* v v)))) ; unused apparently
        (u (+ u (* (cos a) f *flap*)))
        (v (+ v (* (sin a) f *flap*)))
        (tm 0))
    (list :x x :y y :u u :v v :f f :a a :tm tm
          :prev-x prev-x :prev-y prev-y :prev-a prev-a)))

(defun kill-bird (&key x y u v f a tm prev-x prev-y prev-a)
  ;;(gamekit:play-sound :boom)
  (let ((u (/ u 10))
        (v (/ v 10)))
    (push-particles (make-particles :x x :y y :u u :v v))
    (list :x x :y y :u u :v v :f f :a a :tm tm
          :prev-x prev-x :prev-y prev-y :prev-a prev-a)))

(defun collisions (birds)
  (let ((len (length birds))
        (deadlist nil))
    (dotimes (i len)
      (dotimes (j len)
        (unless (or (eq i j) (member i deadlist) (member j deadlist))
          (let ((d (distance (- (getf (elt birds i) :x)
                                (getf (elt birds j) :x))
                             (- (getf (elt birds i) :y)
                                (getf (elt birds j) :y)))))
            (when (< d 20)
              (push i deadlist)
              (push j deadlist))))))
    (let ((live (loop for i from 1 below len
                      unless (member i deadlist)
                        collect (elt birds i)))
          (dead (loop for i from 1 below len
                      when (member i deadlist)
                        collect (elt birds i))))
      (values live dead (when (eq 0 (car deadlist)) t)))))
