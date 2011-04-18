(setq c::*options* '((debug . compile)))
(defun foo (x y) "still equal" (declare (type list x y)) (equal x y))
(defun foo (x y) "to eq" (declare (type symbol y)) (equal x y))
(defun foo (x y) "to eq" (declare (type character x y)) (equal x y))
(defun foo (x y) "still equal" (equal x y))


;; 2009-02-08 Phi Values Decomposition
(defun foo (x)
  (multiple-value-call #'bar (if x (values 1 2) (values 3 4))) )

;; 2009-02-08 UpVar VarAnnex ClosedCell
(defun foo (x)
  (labels (
    (closure () (inner))
    (inner   () (setq x (list x))) ) (declare (notinline inner))
   #'closure ) )

;; 2009-02-08 UpVar VarAnnex LiteralCell
(defun foo (x)
  (labels (
    (closure () (inner))
    (inner   () x) ) (declare (notinline inner))
   #'closure ) )

;; 2009-02-08 GCMap with finally
(defun foo (x) (unwind-protect (error "foo ~S" x) (format t "clean ~S" x)))

;; 2009-02-08 GCMap with finally
(defun foo (x) (unwind-protect (format t "foo ~S" x) (format t "clean ~S" x)))


;; 2009-02-08 GCMap with nonlocal varible
(defun foo (x) (labels ((inner () (print x))) (declare (notinline inner)) (inner)))

;; 2009-02-02 values-list => values*
(defun foo (fn &rest args) (multiple-value-call fn (values-list args)))

;; 2009-02-02 tail merge
(defun ensure-output-stream (s)
    (declare (values stream))
    #+nil (declare (type output-stream-designator s))
  (case s
    ((nil) *standard-output*)
    ((t) *terminal-io*)
    (otherwise
      (if (output-stream-p s)
          s
        (error 'type-error
            :datum s
            :expected-type 'output-stream-designator )) )) )


;; 2009-01-13 UpVar inner closure r/w
(defun foo (x)
  (labels ((closure () (setq x (list x))) (bar () #'closure))
    (declare (notinline bar)) (bar) ) )

;; 2009-01-13 UpVar inner closure r/o
(defun foo (x)
  (labels ((closure () x) (bar () #'closure))
    (declare (notinline bar)) (bar) ) )


;; 2009-01-24 GC Map
(defun foo (&optional (x (init-x)) y) (bar x y))

;; 2009-01-24 function value
(defun foo (x) (declare (values fixnum)) x)

;; 2009-01-24 let binding
(defun foo ()
  (let ((*print-length* nil))
    (let ((*print-level* nil))
      (print '(1 2 3)) ) ) )

;; 2009-01-18 upvar with nonlocal exit
(defun foo (cond)
  (labels (
    (cmdl (cond)
      (let ((devel::*condition* cond))
        (bar) ) )
    )
  (loop
    (with-simple-restart (abort "Return to loop")
      (cmdl cond) ) ) ) )

;; 2009-01-18 sxhash
(defun foo (n)
  (let ((htb (make-hash-table :test 'eq :size (* n 2))))
    (dotimes (i n htb) (setf (gethash (list n) htb) n)) ) )

;; 2009-01-16 optimize and
(defun foo (x y) (if (and x y) (true) (false)))
(defun foo (x y z) (if (and x y z) (true) (false)))

;; 2009-01-16 restart
(defun foo () (restart-case (bar) (error (c) (format t "restart from ~S~%" c))))
(defun foo () (restart-case (bar) (error (c)  :report "Exit from foo." (format t "restart from ~S~%" c))))

;; 2009-01-15 UpVar Literal
(defun foo (x) (labels ((bar () x)) (declare (notinline bar)) (print (list x x)) (bar)))

;; 2009-01-15 UpVar Literal
(defun foo (x) (labels ((bar () x)) (declare (notinline bar)) (bar)))

;; 2009-01-13 UpVar Mixed ClosedCell
(defun foo (x)
  (labels ((bar () (setq x (list x))) (baz () (bar)))
    (declare (notinline baz))
    (print (baz)) #'bar))

;; 2009-01-13 UpVar Mixed ClosedCell
(defun foo (x) (labels ((bar () (setq x (list x)))) (print (bar)) #'bar))

;; 2009-01-12 UpVar Mixed Literal indirect
(defun foo (x) (labels ((bar () x) (baz () (bar))) (print (bar)) #'baz))

;; 2009-01-12 UpVar Mixed Literal
(defun foo (x) (labels ((bar () x)) (print (bar)) #'bar))

;; 2009-01-12 UpVar Closure LiteralMarker
(defun foo (x) (lambda () x))

;; 2009-01-12 UpVar Closure CellMarker
(defun foo (x) (lambda () (setq x (list x))))

;; 2009-01-12 UpVar Closure CellMarker
(defun foo (x)
  (labels ((bar () (setq x (list x))) (baz () (bar)))
    (declare (notinline bar)) #'baz ) )

;; 2009-01-12 UpVar Closure LiteralMarker
(defun foo (x)
  (labels ((bar () x) (baz () (bar)))
    (declare (notinline bar)) #'baz ) )

;; 2009-01-12 UpVar Closure LiteralMarker
(defun foo (x) (lambda () x))

;; 2009-01-11 destructuring-bind
(defun foo (x) (destructuring-bind (a b c) x (list a b c)))

;; 2009-01-11 UpVar
;;                  Path        Access
;;  mismatch        Variable    Base
;;  check-not-end   Fixed       Sp
(defun foo (expr)
    (labels (
        (mismatch (datum pattern)
            (error 'syntax-error
                :datum datum :form expr :pattern pattern :syntax '(a b c)))
        )
      (labels (
        (check-not-end (cur pat) (if (consp cur) cur (mismatch cur pat)))
        (check-end (cur pat) (if cur (mismatch cur pat)))
    )
    (declare (ignorable (function check-end) (function check-not-end)))
    (let* ((whole4 expr)
           (runner5 whole4)
           (a (progn (check-not-end runner5 (quote (a b c))) (pop runner5)))
           (b (progn (check-not-end runner5 (quote (b c))) (pop runner5)))
           (c (progn (check-not-end runner5 (quote (c))) (pop runner5)))
           (end6 (check-end runner5 (quote (a b c)))) )
    (declare (ignorable end6 c b a runner5 whole4))
    (list a b c)))) )


;; 2009-01-11 handler-bind x 2 = BUG only one frame.
(defun foo ()
  (handler-bind ((condition (lambda (c) (format t "Got[1] ~S~%" c) c)))
  (handler-bind ((condition (lambda (c) (format t "Got[2] ~S~%" c) c)))
    (signal (make-instance 'undefined-function :name 'test)) ) ) )


;; 2009-01-11 handler-bind x 2 = BUG only one frame.
(defun foo ()
  (handler-bind ((condition (lambda (c) (format t "Got[1] ~S~%" c) c)))
  (labels (
    (bar ()
      (handler-bind ((condition (lambda (c) (format t "Got[2] ~S~%" c) c)))
        (error "foo") ) )
    )
    (declare (notinline bar))
    (bar) ) ) )

;; 2009-01-24 let binding
(defun foo ()
  (let ((*print-length* nil))
    (let ((*print-level* nil))
      (print '(1 2 3)) ) ) )

;; 2009-01-11 handler-bind x 2 = BUG only one frame.
(defun foo ()
  (handler-bind ((condition (lambda (c) (format t "Got[1] ~S~%" c) c)))
  (handler-bind ((condition (lambda (c) (format t "Got[2] ~S~%" c) c)))
    (signal (make-instance 'undefined-function :name 'test)) ) ) )

;; 2009-01-11 handler-bind
(defun foo ()
   (handler-bind ((error (lambda (c) (format t "Got ~S~%" c)))) (bar)) )


;; 2007-12-07 Don't use $rn for local funcall
(defun foo (x)
  (labels ((bar (x) (print x))) (declare (notinline bar)) (bar x)) )

;; 2009-01-11 Branch+Phi+Ret => If+Ret
(defun foo (x) (typep x 'symbol))

;; 2009-01-11 Branch+Ret => If+Ret
(defun foo (x) (declare (type character x)) (if (char< x #\a) 10 20))

;; 2009-01-11 slot-value generic warning
(defun foo (x) (declare (type class x)) (slot-value x 'si::name))

;; 2009-01-04 RA should not spill, see also nconc/2
;; See BB22=>BB14 and RET %r23
(defun append/2 (list-1 list-2)
    (declare (type list list-1))
    (declare (type t list-2))
    (declare (values t))
  (if (null list-1)
      list-2
    (let* ((head (list (first list-1)))
           (tail head) )
      (dolist (elt (rest list-1))
        (setq tail (setf (rest tail) (list elt))) )
      (setf (rest tail) list-2)
      head )) )

;; 2009-01-04 RA should not spill, see also append/2
(defun nconc/2 (list-1 list-2)
  (if (endp list-1)
      list-2
    (let ((scan-1 list-1))
      (loop
        (let ((next-1 (rest scan-1)))
          (when (null next-1)
            (setf (rest scan-1) list-2)
            (return list-1) )
          (setq scan-1 next-1) )) )) )

;; 2009-01-03 RA physical in PhiI and ValuesI
(defmacro cl:psetq (&whole form &rest pair*)
  (let ((runner pair*))
    (loop
      (when (endp runner) (return `(psetf ,@pair*)))
      (let ((var (pop runner)))
        (unless (symbolp var)
          (error "Expect variable name but ~S." var) )
        (when (endp runner)
          (error "Missing value for ~S." var) )
        (pop runner) )) ) )


;; 2009-01-03 &key
(defun foo (sequence predicate &key (key 'identity))
  (values sequence predicate key) )

;; 2009-01-03 integrate apply
(defun foo (sequence predicate &rest args)
  (when sequence (apply 'stable-sort sequence predicate args)) )

;; 2009-01-02 fold !restify
(defun cl:sort (sequence predicate &rest args)
  (apply 'stable-sort sequence predicate args) )

;; 2009-01-02 type name
(defun foo (x) (declare (type function-name x)) x)

;; 2009-01-02 Type of rest parameter = ty of PROLOGUE
(defun foo (&rest x) x)

;; 2009-01-01 values declaration
(defun foo () (declare (values fixnum)) (let ((x (bar))) (baz) x))
(defun foo () (declare (values fixnum)) (bar))

;; 2008-12-31 inline nonlocal
(defun foo (x)
  (unwind-protect
      (labels ((bar () (return-from foo 1))) (when x (bar)))
    (format t "cleanup~%") ) )

;; 2008-12-31 inline nonlocal tagbody with close
(defun foo (x)
  (tagbody
    (labels ((bar () (unwind-protect (progn (format t "bar~%") (go exit)) (format t "clean~%"))))
      (when x (bar))
      (format t "foo~%")
      (return-from foo) )
    exit ) )



;; 2008-12-31 inline nonlocal tagbody
(defun foo (x)
  (tagbody
    (labels ((bar () (format t "bar~%") (go exit)))
      (when x (bar))
      (format t "foo~%")
      (return-from foo) )
    exit ) )

;; 2008-12-31 propagate unreachable
  (defmacro define+ (ty op* op/2 op/1)
   `(defun ,op* (x &rest y*)
        (declare (values real))
        (declare (type ,ty x))
        (declare (dynamic-extent y*))
      (if (null y*)
          (,op/1 x)
        (dolist (y y* x) (setq x (,op/2 x y))) )) )


;; 2008-12-30 inline mutual recursive call
(defun foo (x)
  (labels ((even? (n) (if (= n 0) t   (odd? (1- n))))
           (odd?  (n) (if (= n 0) nil (even? (1- n)))) )
    (odd? x) ) )

;; 2008-12-30 inline nonlocal block => expand
(defun foo (x) (labels ((bar () (return-from foo 1))) (unless x (bar))))

;; 2008-12-30 inline nonlocal block => stay
(defun foo (x) (labels ((bar () (labels ((baz () (return-from foo 1))) (baz)))) (unless x (bar))))


;; 2008-12-30 inline upvar r/w
(defun foo (x) (labels ((bar (y) (format t "bar: x=~S y=~S~%" (incf x) y))) (declare (notinline bar)) (bar 1) x))

;; 2008-12-30 inline upvar r/o
(defun foo (x) (labels ((bar (y) (format t "bar: x=~S y=~S~%" x y))) (declare (notinline bar)) (bar 1)))

;; 2008-12-30 inline simple
(defun foo () (labels ((bar (x) (format t "bar=~S~%" x))) (bar 1)))


;; 2008-12-30 Unused function
(labels ((foo () (bar)) (bar () (foo)) ) 1)

;; 2008-12-30 identity
(defun foo (x) (identity x))

;; 2008-12-30 notline
(defun foo () (labels ((bar () 1)) (declare (notinline bar)) (bar)))

;; 2008-12-29 opt-call closure and self-tail-call
(defun foo (a) (labels ((bar (x) (if (zerop x) a (bar (1- x))))) #'bar))

;; 2008-12-29 &rest dynamic-extent
(defun foo (&rest a) (declare (dynamic-extent a)) (format t "a=~S~%" a))

;; 2008-12-29 RA - release reg at the last use before call
(defun foo (x y)
    (declare (type list x y))
  (and (foo (car x) (car y)) (foo (cdr x) (cdr y))) )

(defun foo (x y)
    (declare (type list x y))
  (and (foo (car x) (car y)) (foo (cdr x) (cdr y))) )


;; 2008-12-29 self-tail-call - self tail call optimization should be called
;; after closure optimization pass.
(defun foo (n) (if n n (multiple-value-call #'foo (bar))))
(defun foo (n) (declare (notinline foo)) (if n n (multiple-value-call #'foo (bar))))

;; 2008-12-29 self-tail-call
(defun fact2 (n x) (if (= n 0) x (fact2 (1- n) (* n x))))

;; 2008-12-28 + integer
(defun foo (x) (declare (type (signed-byte 8) x)) (+ x 10))


;;; 2008-12-28 multiple-value-list
(defun foo (form) (format t "foo=~S~%" (multiple-value-list (eval form))))

;; 2008-12-26 values missing
(defun foo () (declare (values fixnum fixnum)) "ERROR" (values 1))
(defun foo () (declare (values fixnum fixnum)) "ERROR" 1)
(defun foo (x) (declare (values fixnum fixnum)) "ERROR" x)
(defun foo () (declare (values fixnum fixnum)) "ERROR" (values 1 'foo))

;; 2008-12-26 values extra
(defun foo () (declare (values fixnum fixnum)) (values 1 2 3))
(defun foo () (declare (values fixnum)) (values 1 2))
(defun foo (x) (declare (values t)) (gethash x *setf-table*))

;; 2008-12-26 values match
(defun foo ()  (declare (values fixnum fixnum)) (values 1 2))
(defun foo (x) (declare (values t t)) (gethash x *setf-table*))


;; 2008-12-26 primary value
(defun foo (x) (values (gethash x *setf-table*)))

;; 2008-12-26 define setf-function
(defun (setf foo) (x) (set-foo x))

;; 2008-12-26 extra values
(defun (setf foo) (x) (values x 1))

;; 2008-12-26 propagate eq
(defun foo (x) (if (eq x nil) x 'foo))
(defun foo (x) (if (eq x nil) nil 'foo))

;; 2008-12-26 (setf car)
(defun foo (x) (declare (type cons x)) (setf (cdr x) 1))
(defun foo (x) (declare (type cons x)) (setf (car x) 1))
(defun foo (x) (declare (type symbol x)) "ERROR" (setf (car x) 1))


;; 2008-12-25 (setf slot-value)
(defun foo (x) (declare (type symbol x)) (setf (slot-value x 'plist) nil))

;; 2008-12-25 tail merge
(defun foo (x y) (declare (type double-float x y)) (/ (+ x y) 2d0))

;; 2008-12-25 tinycl_c_ir_fundb.cpp float
(defun foo (x) (declare (type fixnum x)) (float x))
(defun foo (x) (declare (type double-float x)) (float x))
(defun foo (x) (declare (type single-float x)) (float x))
(defun foo (x) (declare (type fixnum x)) (float x 0d0))
(defun foo (x) (declare (type double-float x)) (float x 0d0))
(defun foo (x) (declare (type single-float x)) (float x 0d0))


;; 2008-12-25 tinycl_c_ir_fundb.cpp
(defun foo (x) (declare (type fixnum x)) (> x #x1234))
(defun foo (x) (declare (type fixnum x)) (= x #x1234))
(defun foo (x) (declare (type fixnum x)) (= x 0))
(defun foo (x) (declare (type fixnum x)) (> x 0))


;; 2008-12-25 tinycl_c_ir_fundb.cpp
(defun foo (x) (declare (type double-float x)) (+ x 2d0))
(defun foo (x) (declare (type double-float x)) (- x 2d0))
(defun foo (x) (declare (type double-float x)) (* x 2d0))
(defun foo (x) (declare (type double-float x)) (/ x 2d0))
(defun foo (x y) (declare (type double-float x y)) (/ (+ x y) 2d0))


;; 2008-12-24 tinycl_c_ir_fundb.cpp
(defun foo (x) (declare (type single-float x)) (+ x 1.0))
(defun foo (x) (declare (type single-float x)) (- x 1.0))
(defun foo (x) (declare (type single-float x)) (* x 1.0))
(defun foo (x) (declare (type single-float x)) (/ x 1.0))

;; 2008-12-24 tinycl_x86_c_lower .cpp float zero
(defun foo (x) (declare (type single-float x)) (> x 0f0))
(defun foo (x) (declare (type double-float x)) (> x 0d0))

;; 2008-12-24 tinycl_c_ir_fundb.cpp
(defun foo (x) (declare (type character x)) (char> x #\a))
(defun foo (x) (declare (type character x)) (if (char> x #\a) 10 20))
(defun foo (x) (declare (type character x)) (print (if (char> x #\a) 10 20)))
(defun foo (x) (declare (type fixnum x)) (= x 10))

;; 2008-12-24 tinycl_c_ir_fundb.cpp single-float compariosn
(defun foo (x) (declare (type single-float x)) (= x 1.0))
(defun foo (x) (declare (type single-float x)) (> x 1.0))

;; 2008-12-24 tinycl_c_ir_fundb.cpp double-float compariosn
(defun foo (x) (declare (type double-float x)) (= x 1d0))
(defun foo (x) (declare (type double-float x)) (= x pi))


;; 2008-12-24 tinycl_x86_c_lower.cpp
(defun foo (x) (declare (type character x)) (char-code x))

;; 2008-12-23 tinycl_x86_c_lower.cpp
(defun foo (x) (declare (type fixnum x)) (logeqv x 7))

;; 2008-12-23 tinycl_c_ir_fundb.cpp
(defun foo (x) (declare (type fixnum x)) (logand x 7))
(defun foo (x) (declare (type fixnum x)) (logxor x -1))
(defun foo (x) (declare (type fixnum x)) (lognot x))

;; 2008-12-23 fixnum
(defun foo (x) "ERROR" (declare (type symbol x)) (+ x 10))
(defun foo (x) (declare (type fixnum x)) (+ x 10))
(defun foo (x) (declare (type fixnum x)) (* x 10))
(defun foo (x) (declare (type fixnum x)) (ash x 10))

;; 2008-12-23 tinycl_c_ir_fundb.cpp
;; slot-value optimization
(defun foo (x) (declare (type simple-string x)) (slot-value x 'length))
(defun foo (x) (declare (type standard-class x)) (slot-value x 'si::names))


;; Branch Normalization for Not-Branch
;;  Typep BOOL %b9 <= %r3 CONS 
;;  If T %r10 <= %b9 %r3 'NIL 
;;  Eq BOOL %b11 <= %r10 'NIL 
;;  Branch    %b11 BB10 BB11 
(defun foo (x) (unless (consp x) (error "Expect cons")) (cdr x))

(load "/proj/evedit2/lisp/genesis/work")

;; special variable in lambda-list
(defun foo (x *print-length*) (print x))

;; 2008-12-27 &optional
(defun foo (x &optional *print-length*) (print x))
(defun foo (&optional (x 1 xp)) (list x xp))

;; 2009-01-03 &optional with type
(defun foo (x &optional s) (declare (type stream s)) (print x s))

;; 2008-12-27 &key
(defun foo (x &key ((:length *print-length*))) (print x))
(defun foo (&key (x 1 xp)) (list x xp))

;; Backquote
(let ((bar '#:bar) ``(foo ,',bar))) ; `(foo #:bar)

;; Propagate type
(defun foo (x) (values (cdr x) (car x)))

;; 2009-01-03 propage type
(defun foo (x) (when (typep x 'cons) (car x)))

;; 2009-01-03 propage type
(defun foo (x) (when (consp x) (car x)))

(defun foo (&optional (x (bar))) (declare (type fixnum x)) x)

(defun foo (&optional x) (declare (type fixnum x)) x)

(defun foo (x) (declare (type list x)) (car (the list x)))

(defun foo (x) (declare (type list x)) (car x))


(defun foo (n old-p) (<= 1 n (if old-p 4999 3999)))

;; from format-roman-aux
(defun foo (old-p) (<= 1 n (if old-p 4999 3999)))

(defun foo (x) (multiple-value-call #'bar (if x (baz) (quux))))

(defun foo (x) (typep x 'symbol))

;;; Clean: Jump+Phi+Ret
(defun foo (x) (if (not x) (bar) (baz)))
(defun foo (x) (if x (values 1 2) (values 3 4)))
(defun foo (x) (if x 'foo (values 3 4)))

;;; IfI::Optimize
(defun foo (x) (not (not x)))


(defun foo (x) (the list x))


(defun foo (x) (typep x 'list))

(defun foo (x) (typep x 'class-description))
(defun foo (x) (typep x 'symbol))
(defun foo (x) (when (typep x 'symbol) (bar)))


(defun foo (name fn attrs) (setf (gethash *commands* name) attrs))

(defun memq (item list)
    (declare (values list))
    (declare (type list list))
  (let ((runner list))
    (loop
      (when (endp runner) (return nil))
      (when (eq (car runner) item) (return runner))
      (setq runner (cdr runner)) ) ) )

;; -genesis $(SolutionDir)genesis\genesis.lisp
;; $(OutDir)

;;; slot-value
(defun foo (x)
  (mapcar (lambda (s) (slot-value (the standard-effective-slot s) 'name))
    (slot-value (find-class x) 'slots) ) )

;;; variable arity
(defun foo (x) (mapcar '1+ x))
(defun foo (x) (locally (declare (notinline mapcar)) (mapcar '1+ x)))

(load "/proj/evedit2/genesis/~map")

(defun foo (x) (char= x #\a))

(load "/proj/evedit2/regex/smoke.retest")

(setq re (compile-regex "(\\d*)+"))
(string-match re   "0123456789")


(defun foo () (let ((*load-print* t)) (values 1 2)))

(defun bar () (format t "*load-print*=~S~%" *load-print*))
(defun foo (x) (let ((*load-print* x)) (bar)))
(defun foo (x) (let ((*value-table* x)) (bar)))
(defun foo (x) (let ((*my-dynvar* x)) (declare (special *my-dynvar*)) (bar)))

;; Binding appeared more than once
(defun foo (a) (let ((x a) (x 1) (x 2)) (print x)))
(defun foo (a) (let* ((x a) (x 1) (x 2)) (print x)))
(defun foo (x) (let* ((x 0) (x 1) (x 2)) (print x)))

;; We must see "Use undefined variable X"
(defun foo () (let* ((x x)) x))


(macroexpand-1 '(ecase (key) (foo (foo)) (bar (bar))))

(defun not-function (name) (error 'not-function :name name) )

;; 2009-01-01 values declaration
(defmacro foo (a b) `(list 'foo ,a ,b))
(macrolet ((foo (a b) `(list 'foo ,a ,b))) (foo 1 2))
(macrolet ((foo (a b) `(list 'foo ,a ,b))) (foo 1))


(defun c::call-macro-expander (expander form env)
  (handler-case
      (values (funcall *macroexpand-hook* expander form env) nil)
    (error (c) (values nil c)) ) )

(setq *macroexpand-hook*
  (lambda (expander form env)
    (format t "~%macro=~S~%" form)
    (force-output *standard-output*)
    (let ((expansion (funcall expander form env)))
      (format t "~%expansion=~S~%" expansion)
      (force-output *standard-output*)
      expansion ) ))

(defmacro cl:ignore-errors (&rset form*)
 `(handler-case (progn ,@form*)
    (error (c) (values nil c)) ) )

(defmacro cl:incf (place &optional (delta 1) &environment env)
  (multiple-value-bind (var vals stores write-form read-form)
      (get-setf-expansion place env)
   `(let ,(mapcar #'list var vals)
      (let (,(first stores) (+ ,read-form ,delta))
        ,write-form ) ) ) )


(lambda () (declare (function-name foo)) 1)
(lambda () (declare (function-name foo bar)) 1)             ; error
(lambda () (let () (declare (function-name foo bar)) 1))    ; error
(lambda () (locally (declare (function-name foo)) 1))       ; error
(lambda (x) (locally (declare (ignore x)) (print x)) 1)


(defun foo (x) (declare (ignore x)) 1)
(defun foo (x) (setq x 1))
(defun foo () (labels ((bar () (print 'bar))) (bar)))

(defun foo () (declare (values fixnum)) 1)
(defun foo () (setq *load-verbose* (1+ *load-verbose*)))
(defun foo () *foo*)
(defun foo (x) (let ((*load-print* x)) (setq *load-print* (bar)) (foo *load-print*)))
(defun foo (&key ((:x y))) y)
(defun foo (&key (x :error) (y *load-print*)) (values x y))


(defun foo (x) (let ((*package* *package*)) (print x)))
(defun foo (&key a b) (list a b))
(defun foo (&rest x &key a b) (list a b))

(labels ((foo (x) (format t "foo ~S~%" x))) (foo 10))

(defun foo (x) (slot-value (the symbol x) 'si::package))

(defun foo (x) (slot-value (the si::regex-match x) 'si::captures))

(defun foo (x &optional (y x)) (list x y))

;; TODO yosi@msn.com 2008-06-21 We should revise bblock layout pass for
;; dolist.
(defun foo () (dolist (x '(a b c) 123) (print x)))
(defun foo (list) (dolist (x list) (print x)))
(defun foo () (dotimes (i 10) (print i)))

(defun foo (x y) (setf (car x) y))

#'(lambda () nil)
#'(lambda () t)
#'(lambda (a) a)
#'(lambda (a b) (if a a b))
#'(lambda (a b) (foo (if a a b)))
#'(lambda (a) (foo) a)
#'(lambda (a b) (format t "b=~S a=~S~%" b a) a)
#'(lambda (a b c d e f) (list a b c d e f))
#'(lambda (a b c d e f g) (list a b c d e f g))
#'(lambda (a b c d e f g) (list g f e d c b a))
#'(lambda (a) (setq x (foo)))
#'(lambda (a) (block nil x))
#'(lambda (a) (block nil (if x (return 1)) (print 'ok)))
#'(lambda (a) (tagbody (print 'ok)))
#'(lambda (a) (tagbody loop (print 'ok) (if (setq a (foo)) (go loop))))
#'(lambda (a) (let ()) a)
#'(lambda (a) (let ((x 1) (y 2)) (list x y)))
#'(lambda (a) (if a (prong (foo) (bar) (baz))))
#'(lambda () (block nil (let ((i 0)) (tagbody loop (if (>= i 10) (return i)) (print i) (setq i (+ i 1)) (go loop)))))

#'(lambda () (block nil (let ((i 0)) (tagbody loop (if (>= i 10) (return i)) (print i) (if (foo) (bar) (baz)) (setq i (+ i 1)) (go loop)))))

#'(lambda () (flet ((foo () 1)) (foo)))
(flet ((foo () 1)) #'foo)

#'(lambda (x) (flet ((foo () x)) (foo)))
#'(lambda (x) (labels ((foo () x) (bar () (foo))) (bar)))

#'(lambda (a &optional b) (list a b))
#'(lambda (a &optional (b 123)) (list a b))
#'(lambda (a &optional (b 123 b-p)) (list a b b-p))
#'(lambda (a &optional (b (foo))) (list a b))

;; closure
#'(lambda (x) (lambda () x))
#'(lambda (x) (lambda () (setq x (+ x 1))))



;; Path_Fixed
#'(lambda (x)
    (labels (
        (foo () (print x))
        (bar () (foo) (foo))
        (baz () (bar))
        )
        (baz) ) )

;; Path_Variable, Access_RegRo
#'(lambda (x)
    (labels (
        (foo () (print x))
        (bar () (foo))
        (baz () (bar))
        )
        (baz) (foo)) )

;; Path_Variable
#'(lambda (x)
    (labels (
        (foo () (print x))
        (bar () (if (foo) (baz)))
        (baz () (bar))
        )
        (baz) ) )

;; Path_Mixed
#'(lambda (x) (flet ((foo () (print x))) (foo) #'foo) )
#'(lambda (x) (labels ((foo () (print x)) (bar () (foo))) (bar) #'foo) )
#'(lambda (x) (flet ((foo () (print x) (setq x (1+ x)))) (foo) #'foo) )


#'(lambda ()
    (block nil
      (let ((i 0))
        (tagbody
          (go test)
         loop
          (print i)
          (setq i (+ i 1))
         test
          (if (>= i 10) (return i))
          (go loop) ) )) )

#'(lambda ()
    (block nil
      (let ((i 0))
        (tagbody
          (go test)
         loop
          (print i)
          (if (foo) (bar) (baz))
          (setq i (+ i 1))
         test
          (if (>= i 10) (return i))
          (go loop) ) )) )

#'(lambda ()
    (block nil
      (let ((i 0))
        (tagbody
         loop
          (print i)
          (if (foo) (bar) (baz))
          (setq i (+ i 1))
         test
          (if (>= i 10) (return i))
          (go loop) ) )) )

#'(lambda (x) (lambda () (setq x (+ x 1))))

;; &optional
(defun foo (a b &optional c d) (list a b c d))

;; &rest
(defun foo (&rest x) (format t "foo=~S~%" x))

;; 2008-12-27 apply
(defun foo (x) (apply x 1 2 (bar)))

;; block
(defun foo (x)
  (labels ((bar () (return-from foo 1)))
      (declare (notinline bar))
    (unless x (bar)) ) )

;; case
(defun foo (x) (case x (1 'one) (2 'two)))

;; catch
(defun foo (x) (catch 'foo (unless x (bar)) (format t "~S~%" x)))
(defun foo (x) (catch 'foo x))


;; eq
(defun foo (x) "Non portable eq" (eq x 1))

;; eql
(defun foo (x) (eql x 1))

;; flet
(defun foo () (flet ((bar () 1)) (declare (notinline bar)) (bar)))

;; funcall
(defun foo (x) (funcall x 1 2 (bar)))
(defun foo (x) (funcall 'list 1 2 x))
(defun foo (x) (funcall #'list 1 2 x))

;; 2008-12-29 go nonlocal, (foo 1)=456, (foo nil)=123
(defun foo (x)
  (block nil
    (tagbody
      (labels ((bar () (go exit))) (declare (notinline bar))
        (when x (bar)) )
     (return 123)
   exit
    (return 456) )) )

;; handler-case
(defun foo (x)
  (handler-case (bar x)
    (warning () (baz))
    (error (c) (format t "catch ~S~%" c)) ) )

(defun foo (x)
  (handler-case (bar x)
    (error (c) (format t "catch ~S~%" c))
    (:no-error () (format t "done~%")) ) )

(defun foo (x)
  (handler-case (bar x)
    (error (c) (format t "catch ~S~%" c))
    (:no-error (y) (format t "done~%") y) ) )

(defun foo () (handler-case 1 (error (c) c)))


;; lambda
(defun foo (x) x)
(defun foo (x) ((lambda (x) x) x))

;; let
(defun foo () (let ((*print-length* 10)) (print (make-list 20))))

;; multiple-value-bind
(defun foo () (multiple-value-bind (a b) (bar) (list a b)))

;; multiple-value-call
(defun foo () (multiple-value-call #'bar 1))
(defun foo (x) (multiple-value-call #'bar (if x (baz) (quux))))

;; multiple-value-prog1
(defun foo () (multiple-value-prog1 (bar) (baz)))

;; not
(defun foo (x) (not x))

;; return-from
(defun foo ()
  (labels ((bar () (return-from foo 123))) (declare (notinline bar))
    (bar) ) )

;; 2009-02-08 Nonlocal block
(defun foo (x)
    "z must be allocated onto stack, since it is used in nolocal bb."
  (let ((z (print 'baz)))
    (block nonlocal
      (labels ((bar () (print 'bar) (return-from nonlocal)))
          (declare (notinline bar))
        (unless x (bar)) ) )
    z ) )

;; throw
(defun bar () (throw 'foo 'bar))

;; unwind-protect
(defun foo () (unwind-protect (bar) (baz)))
(defun foo () (unwind-protect (return-from foo (bar)) (baz)))

(defun foo ()
  (let ((x nil)) (unwind-protect (progn (bar) (setq x t)) (print x))) )

(defun foo (a)
  (let ((b nil))
    (unwind-protect
        (progn (bar) (setq b t))
      (format t "Cleanup a=~S b=~S~%" a b) ) ) )

;; values
(defun foo () (values))
(defun foo () (values 'a))
(defun foo () (values 'a 'b))

(defmacro with-collector ((collect result) &body form*)
 `(let* ((head (list 0))
          (tail head) )
    (macrolet (
      (,collect (form) `(setq tail (setf (cdr tail) (list ,form))))
      (,result () '(cdr head))
      )
      ,@form* ) ) )

(defun mapcar (fn list-1)
  (with-collector (collect result)
    (dolist (item list-1 (result))
      (collect (funcall fn item)) ) ) )


(defun lit (itemize)
  (labels (
    (make-defun (names)
      (mapcar (lambda (x) `(,itemize ,x)) names) )
    )
    (make-defun '(foo bar baz)) ) )

;;; sxhash table test
(setq htb (make-hash-table :test 'eq))
(setq one '(one))
(setf (gethash one htb) 'one)
(room t) ; usedOf(sxhashtbl) = 40
(collect-garbage)
(room t) ; usedOf(sxhashtbl) = 40

(gethash one htb)
(setq one nil)
(collect-garbage)
(room t) ; usedOf(sxhashtbl) = 40

(clrhash htb)
(collect-garbage)
(room t) ; usedOf(sxhashtbl) = 40



;;; test GC
(setq one '(one))
(collect-garbage)

;; Until we support GC Map, below form signals AV.
(dotimes (i 10) (print i) (force-output *standard-output*) (collect-garbage))

(load "/proj/evedit2/genesis/genesis-runtime")
(si::collect-garbage)


(defun foo (x) (declare (type fixnum x)) (logxor x -3))
