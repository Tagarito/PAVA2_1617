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

(defun create-hash-map-key-inheritance (name)
  (concatenate 'string (string name) (string '-) (string 'inheritance))
)
(defun create-hash-map-key-slots (name)
  (concatenate 'string (string name) (string '-) (string 'slots))
)

(defun get-list-super-classes (name)
  (let (
        (direct_supers nil)
        (string_inheritance (create-hash-map-key-inheritance name))
        (toReturn (list name))
       )
       (setf direct_supers (gethash string_inheritance meta-hash))
       (if (listp direct_supers)
         (loop for element in direct_supers do
           (if (not (equal (string name) (string element))) (setf toReturn (concatenate 'list (get-list-super-classes element) toReturn )) ())
         )
       )
    ; (loop while (listp name)
    ; )
      (delete-duplicates toReturn)
  )
)
(defun get-list-slots (list-of-classes)
 (let ((toReturn nil)
       (string-slots nil)
      )
      (loop for element in list-of-classes do
            (setf string-slots (create-hash-map-key-slots element))
            (concatenate 'list toReturn (gethash string-slots meta-hash))
      )
   (delete-duplicates toReturn)
 )
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

        (lista_herancas nil)
        (lista_slots nil)

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
    (setf string_inheritance (create-hash-map-key-inheritance classname))
    (print string_inheritance)
    (setf string_slots (create-hash-map-key-slots classname))
    (print string_slots)
    (print supers)
    (print loop_class_slots)

    ;(print (remhash ,string_inheritance meta-hash))
    ;(print (remhash ,string_slots meta-hash))
    (setf (gethash string_inheritance meta-hash) supers)
    (setf (gethash string_slots meta-hash) loop_class_slots)

    ; herancas
    ; while (listp get-value )
    ;   do
    (setf lista_herancas (get-list-super-classes classname))
    (print lista_herancas)
    (setf lista_slots (get-list-slots classname))
    ; for inheritance:
    ; juntar inheritance de todos
    ;
    ; for slots de todos:
    ; juntar slots

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
