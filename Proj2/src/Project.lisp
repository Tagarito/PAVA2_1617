; (defmacro define_meta_hash()
; (define_meta_hash)
(defvar meta-hash (make-hash-table :test #'equal))

(defun print-hash ()
  (format t "printing~%")

  ; (print '\n)
  (defun print-hash-entry (key value)
    (format t "Key: ~S ,Value: ~S~%" key value)
    )
  (maphash #'print-hash-entry meta-hash)
)

(defmacro def-class (supers &optional slots &rest amaraleautista)
  (let ((realSupers nil)
        (className nil)
        (slots_make_instance nil)
        (vector_make_instance (vector))
        (loop_class_slots nil)
        (getter_template nil)
        (macro_value nil)

        (string_inheritance nil)
        (string_slots nil)

        )

    (if (listp supers)
      (progn  (setf className (car supers))
              (setf realSupers (cdr supers)))
      (setf className supers)
      )

    ;(print slots)
    ; (print amaraleautista)

    (if (not (null slots))
      (progn (setf slots_make_instance (cons '&key (cons slots amaraleautista)))
             (setf vector_make_instance (cons 'vector (cons slots amaraleautista)))
             (setf loop_class_slots (cons slots amaraleautista))
             )
      )
    (setf string_inheritance (concatenate 'string (string className) (string '-) (string 'inheritance)))
    (print string_inheritance)
    (setf string_slots (concatenate 'string (string className) (string '-) (string 'slots)))
    (print string_slots)
    (print supers)
    (print loop_class_slots)

    ;(print (remhash ,string_inheritance meta-hash))
    ;(print (remhash ,string_slots meta-hash))
    (setf (gethash string_inheritance meta-hash) supers)
    (setf (gethash string_slots meta-hash) loop_class_slots)

    ; for inheritance:
    ; juntar inheritance de todos
    ;
    ; for slots de todos:
    ; juntar slots
    ; (print slots_make_instance)
    ; (print vector_make_instance)
    ; (print loop_class_slots)

    (let ((index 0))
      (setf getter_template  (loop for element in loop_class_slots
                               ; collect (string element)
                               collect `(defun ,(intern (concatenate 'string (string classname) (string '-) (string element))) (instance) (aref instance ,index))
                               do
                               (setf index (+ index 1)

                                     )))
      ;  ;(defun ,(intern (concatenate 'string (string `,className) (string '-) (string `,element))) ())
      ;  )
      ; )

      ; (print
      ; (loop for element in getter_template
      ;   (element)
      ; )
      ; )

      (setf macro_value

            `(progn


              (print '(defun ,(intern (concatenate 'string (string '#:MAKE-) (string className))) ,slots_make_instance
                        ,vector_make_instance
                        ))
              (defun ,(intern (concatenate 'string (string '#:MAKE-) (string className))) ,slots_make_instance
                ,vector_make_instance
                )
              ,@getter_template



              ;  (print (string (concatenate 'string (string ',className) (string '-) (string element))))
              ;  (print `(defun ,(concatenate 'string (string ',className) (string '-) (string element)) ()
              ;                   )
              ;        )
              ;  )
              )
            )
      (print macro_value)
      macro_value
      )
    )
)



 ; (setf vector_make_instance (concatenate 'list (list 'vector) slots))
 ; `(progn
 ;
 ;   ;   (write-line 5))
 ;   ;(print '(defclass ,className ,realSupers ,slots))
 ;   ;(print '(defun ,(intern (concatenate 'string (string '#:MAKE-) (string className))) ,slots_make_instance
 ;   ;   ;  (write-line "5"))
 ;   ;    (make-instance ',classname)
 ;   ;    ,vector_make_instance)
 ;
 ;   ;)
 ;
 ;   ;;;; Define the class
 ;   ;(defclass ,className ,realSupers ,slots)
 ;   ;(print '(setf (symbol ',className) ,vector_make_instance))
 ;   ;(setf (symbol ',className) ,vector_make_instance)
 ;
 ;   ;;;; make-className
