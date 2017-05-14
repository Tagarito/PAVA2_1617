(defmacro def-class (supers slots)
  (let ((realSupers nil) (className nil))
    (if (listp supers)
      (progn (setf className (car supers))
       (setf realSupers (cdr supers)))
      (setf className supers)
    )
    (progn
      `(defclass ,className ,realSupers ,slots)
      ;`(defun make-,className () 
      ;  (make-instance ',className))
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
