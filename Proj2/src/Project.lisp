(defmacro def-class (supers &rest slots)
  (let ((realSupers nil)
        (className nil)
        (slots_make_instance nil)


       )
    (print slots)
    (if (listp supers)
      (progn  (setf className (car supers))
              (setf realSupers (cdr supers)))
      (setf className supers)
      )
      ; (loop for element in slots
      ;   do
      ;   (print element)
      ; )
      (setf slots_make_instance (concatenate 'list (list '&key) slots))
    `(progn
      ;   (write-line 5))
      (print '(defclass ,className ,realSupers ,slots))
      (print '(defun ,(intern (concatenate 'string (string '#:MAKE-) (string className))) ,slots_make_instance
      ;   ;  (write-line "5"))
          (make-instance ',classname)))
      ;;;; Define the class
      (defclass ,className ,realSupers ,slots)

      ;;;; make-className
      (defun ,(intern (concatenate 'string (string '#:MAKE-) (string className))) ,slots_make_instance
      ;   ;  (write-line "5"))
          (make-instance ',classname))
      ;; (make-instance 'b)
    )

  )
)

(defun xD (supers slots)
(let ((realSupers nil) (className nil))
  (if (listp supers)
    (set className (car supers))
    (set className supers)
    )
)
)


;(setf r (make-instance 'person))
(defclass person () ((ola :initform 0)))
