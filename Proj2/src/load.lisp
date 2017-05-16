; (defmacro define_meta_hash()
; (define_meta_hash)
(defun 123load-file()
  (progn

 (load "~/FAC/PAVA/PAVA2_1617/Proj2/src/load.lisp")
 (load "~/FAC/PAVA/PAVA2_1617/Proj2/src/classes.lisp")
)
)

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
    (delete-duplicates toReturn :from-end T)
    ; toreturn
  )
)
(defun get-list-slots (list-of-classes)
 (let ((toReturn nil)
       (string-slots nil)
      )
      (if (not (listp list-of-classes))
           (setf list-of-classes (list list-of-classes))
           ()
        )
      (loop for element in list-of-classes do
            (setf string-slots (create-hash-map-key-slots element))
            ; (print string-slots)
            ; (print (gethash string-slots meta-hash))
            (setf toReturn (concatenate 'list toReturn (gethash string-slots meta-hash)))
      )
    ; (print (delete-duplicates toReturn))
   (delete-duplicates toReturn)

 )
)

(defun transform-to-string (arg)
  (if (listp arg)
    (mapcar #'(lambda (x) (concatenate 'string (string x) (string ""))) arg)
    (concatenate 'string (string arg) (string ""))
  )
)
(defmacro def-class (supers &optional slots &rest amaraleautista)
  (let ((realSupers nil)
        (className nil)
        (lista_slots_with_&key nil)
        ; (vector_make_instance (vector))
        (loop_class_slots nil)
        (getter_template nil)
        (setter_template nil)
        (initial_definition_template nil)
        (macro_value nil)

        (string_inheritance nil)
        (string_slots nil)

        (lista_herancas nil)
        (lista_completa_slots nil)
        (inner_instance_hashtable nil)
        (validate_class_template nil)
        )

    (if (listp supers)
      (progn  (setf className (car supers))
              (setf realSupers (cdr supers)))
      (setf className supers)
    )

    (if (not (null slots))
      (progn
             (setf loop_class_slots (cons slots amaraleautista))
      )
    )
    (setf inner_instance_hashtable (make-hash-table :test #'equal))
    (setf string_inheritance (create-hash-map-key-inheritance classname))
    (setf string_slots (create-hash-map-key-slots classname))
    ; (format t "supers: ~a~%" supers)

    (setf (gethash string_inheritance meta-hash) supers)
    (setf (gethash string_slots meta-hash) loop_class_slots)

    (setf lista_herancas (get-list-super-classes classname))
    ; (format t "lista-herancas: ~a~%" lista_herancas)
    (setf lista_completa_slots (get-list-slots lista_herancas))
    ; (format t "lista_completa_slots: ~a~%" lista_completa_slots)

    (setf getter_template  (loop for element in lista_completa_slots
                               ; collect (string element)
                               collect `(defun ,(intern (concatenate 'string (string classname) (string '-) (string element))) (instance)
                                          (if (,(intern (concatenate 'string (string classname) (string '?) )) instance)
                                            (gethash (string ',element) (cdr instance))
                                            (progn
                                             (format t "Instance is not a ~S~%" ',classname)
                                             nil)
                                          )
                                     )
                            )
    )
    (setf setter_template  (loop for element in lista_completa_slots
                               collect `(defun ,(intern (concatenate 'string (string 'set-) (string classname) (string '-) (string element))) (instance value)
                                          (if (,(intern (concatenate 'string (string classname) (string '?) )) instance)
                                              (setf (gethash (string ',element) (cdr instance)) value)
                                              (progn
                                               (format t "Instance is not a ~S~%" ',classname)
                                               nil)
                                          )
                                     )
                            )
    )
    (setf initial_definition_template  (loop for element in lista_completa_slots
                                          collect `(,(intern (concatenate 'string (string 'set-) (string classname) (string '-) (string element))) instance ,element)
                                       )
    )
    (setf validate_class_template `(defun ,(intern (concatenate 'string (string classname) (string '?))) (instance)
                  (if (listp instance)
                   (not (null (position ',classname (car instance))))
                   nil
                  )
          )
    )

      (if (not (null lista_completa_slots))
        (setf lista_slots_with_&key (cons '&key lista_completa_slots))
        ()
      )
      (setf macro_value

            `(progn
              ,validate_class_template
              ,@getter_template
              ,@setter_template
               (defun ,(intern (concatenate 'string (string '#:MAKE-) (string className))) ,lista_slots_with_&key
                         (let ((instance nil))
                           (progn
                            (setf instance (cons ',lista_herancas ,inner_instance_hashtable))
                            ,@initial_definition_template
                            instance
                           )
                         )
               )
              )
        )

      (print macro_value)
      macro_value
  )
)
