
(defmacro with-collector ((collect result) &body form*)
 `(let* ((head (list 0))
          (tail head) )
    (macrolet (
      (,collect (form) `(setq tail (setf (cdr tail) (list ,form))))
      (,result () '(cdr head))
      )
      ,@form* ) ) )

(macrolet (
  (collect-nothing (&rest form*)
    `(macrolet ((collect (form) form)) ,@form* list-1) )

  (collect-item (&rest form*)
   `(macrolet (
      (collect (form) `(setq tail (setf (cdr tail) (list ,form))))
      )
     (let* ((head (list 0))
            (tail head) )
       ,@form*
       (cdr head) ) ) )

  (collect-list (&rest form*)
   `(macrolet (
      (collect (form)
       `(let ((list ,form))
          (setf (cdr tail) list)
          (setq tail (last list)) ) )
      )
      (let* ((head (list 0))
             (tail head) )
        ,@form*
        (cdr head) ) ) )

  (define (name collector itemize)
    (labels (
      (list-names (format count)
        (with-collector (collect result)
          (dotimes (nth count (result))
            (collect (intern (format nil format (+ nth 2)))) ) ) )

      (make-defun/n (name param* arg*)
       `(defun ,name (fn list-1 ,@param*)
            (declare (values list))
            (declare (type (or function symbol) fn))
            (declare (type list list-1 ,@param*))
          (,collector
            (let ((runner-1 list-1)
                  ,@(mapcar/2 (lambda (x y) (list x y)) arg* param*) )
              (loop
                (when (or (endp runner-1)
                      ,@(mapcar/1 (lambda (x) `(endp ,x)) arg*) )
                  (return) )
                (collect
                  (funcall fn
                    (,itemize runner-1)
                    ,@(mapcar/1 (lambda (x) (list itemize x)) arg*) ))
                (setq runner-1 (cdr runner-1))
                ,@(mapcar/1 (lambda (x) `(setq ,x (cdr ,x))) arg*) ) ) ) ) )

      (make-defun ()
       `(defun ,name (fn list-1 &rest list*)
            (declare (values list))
            (declare (type (or function symbol) fn))
            (declare (type list list-1))
            (declare (dynamic-extent list*))
          (if (null list*)
              (,(intern (format nil "~A/1" name)) fn list-1)
            (labels (
              (endp+ (list+)
                (dolist (list list+ nil)
                  (when (endp list) (return t)) ) )
              )
              (,collector
                (let ((list+ (cons list-1 list*)))
                  (loop
                    (when (endp+ list+) (return))
                    (let ((arg*
                            (with-collector (collect result)
                              (let ((runner list+))
                                (loop
                                  (when (endp runner) (return (result)))
                                  (collect (car runner))
                                  (setf (car runner) (cdr (car runner)))
                                  (setq runner (cdr runner)) ) ) ) ))
                      (collect (apply fn arg*)) )) )) )) ) )

      (mapcar/1 (fn list-1)
          (declare (values list))
          (declare (type list list-1))
        (with-collector (collect result)
          (let ((runner-1 list-1))
            (loop
             (when (endp runner-1) (return (result)))
             (let ((item-1 (car runner-1)))
               (collect (funcall fn item-1)) )
             (setq runner-1 (cdr runner-1)) ) ) ) )

      (mapcar/2 (fn list-1 list-2)
          (declare (values list))
          (declare (type list list-1 list-2))
        (with-collector (collect result)
          (let ((runner-1 list-1) (runner-2 list-2))
            (loop
              (when (or (endp runner-1) (endp runner-2)) (return (result)))
              (let ((item-1 (car runner-1))
                    (item-2 (car runner-2)) )
                (collect (funcall fn item-1 item-2)) )
              (setq runner-1 (cdr runner-1))
              (setq runner-2 (cdr runner-2)) ) ) ) )
      )
      ;;
      (with-collector (collect result)
        (dotimes (nth 2)
          (let ((name (intern (format nil "~A/~D" name (+ nth 2)))))
            (collect `(declaim (ftype
                        (function ((or function symbol)
                                   list ,@(list-names "LIST" nth) )
                                  list )
                        ,name )))
            (collect
              (make-defun/n
                  name
                  (list-names "LIST-~D" nth)
                  (list-names "RUNNER-~D" nth) )) ) )
        (collect (make-defun))
        `(progn ,@(result)) ) ) )
  )
  ;;
  (define cl-mapc   collect-nothing car)
  (define cl-mapcar collect-item    car)
  (define cl-mapcan collect-list    car)

  (define cl-mapl    collect-nothing identity)
  (define cl-maplist collect-item    identity)
  (define cl-mapcon  collect-list    identity)
 ) ; macrolet

