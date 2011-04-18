(in-package :c)

'(setq c::*options* '((debug . compile)))

(defmacro cl:restart-case (restartable-form &rest clause*)
  (let ((block '#:block)
        (cond  '#:cond)
        (var   '#:args) )
  (labels (
    (compute-condition (class arg*)
      `(si::coerce-to-condition ',class ,@arg*) )

    (parse-restartable-form (form)
      (let ((expansion (macroexpand form)))
        (case (first expansion)
          ((cerror) (parse-cerror expansion))
          ((error)  (parse-error  expansion 'simple-error))
          ((signal) (parse-signal expansion 'simple-condition))
          ((warn)   (parse-signal expansion 'simple-warning))
          (otherwise (values expansion nil)) ) ) )

    (parse-cerror (form)
      (values `(cerror ,(second form) ,cond)
              (compute-condition 'simple-error (cdddr form)) ) )

    (parse-clauses (clause*)
      (si::with-collector (collect-binding binding*)
      (si::with-collector (collect-handler handler*)
      (dolist (clause clause* (values (binding*) (handler*)))
        (destructuring-bind (case-name lambda-list &rest body) clause
          (si::with-collector (collect-keyval keyval*)
            (loop
              (case (first body)
                ((:interactive)
                  (pop body)
                  (collect-keyval :interactive-fucntion)
                  (collect-keyval `(function ,(pop body))) )
                ((:report)
                  (pop body)
                  (collect-keyval :report-function)
                  (let ((frob (pop body)))
                    (if (stringp frob)
                        (collect-keyval `(lambda (s) (write-string ,frob s)))
                      (collect-keyval `(function ,frob)) ) ) )
                ((:test)
                  (pop body)
                  (collect-keyval :test-fucntion)
                  (collect-keyval `(function ,(pop body))) )
                (otherwise
                  (return) )))
            (let ((tag (gensym)))
              (collect-binding
                `(,(first clause)
                    (lambda (&rest args) (setq ,var args) (go ,tag))
                    ,@(keyval*) ))
              (collect-handler tag)
              (collect-handler
                `(return-from ,block
                    (apply (lambda ,lambda-list ,@body) ,var) )) )))))) )

    (parse-error (form class)
      (values `(,(first form) ,cond) 
              (compute-condition class (cddr form)) ) )

    (parse-signal (form class)
      (multiple-value-bind (form cond-form)
          (parse-error form class)
        (values `(return-from ,block ,form) cond-form) ) )
    )
    (multiple-value-bind (rs-form cond-form)
        (parse-restartable-form restartable-form)
    (multiple-value-bind (binding* handler*)
        (parse-clauses clause*)
     `(block ,block
       (let (,var)
         (tagbody
           (let ((,cond ,cond-form))
             (restart-bind* ,cond ,binding* ,rs-form) )
           ,@handler* ) )) ) ) ) ) )
