;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defobject.lisp
;;;;
;;;; License: LLGPL
;;;;
;;;; Please see usage notes in the defobject macro.
;;;;
;;;; Copyright Paul Nathan 2012-2016
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :defobject
  (:use :common-lisp)
  (:export
   :defobject))

(in-package :defobject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-var (classname var &key undecorated)
  "Build the variable (slot) names"
  (let ((variable-symbol (if (consp var)
                             (first var)
                             var))
        (variable-init-val (if (consp var)
                               (second var)
                               nil)))
    (list variable-symbol
          :initform variable-init-val
          :accessor (if undecorated
                        (intern (string variable-symbol))
                        (intern (concatenate 'string (string classname) "-"
                                             (string variable-symbol))))
          :initarg (intern (string variable-symbol) :keyword))))

;; (with-running-unit-tests
;;     (expect
;;      '(BAR :INITFORM NIL :ACCESSOR FOO-BAR :INITARG :BAR)
;;      (build-var 'foo 'bar))

;;   (expect
;;    '(BAR :INITFORM QUUX :ACCESSOR FOO-BAR :INITARG :BAR)
;;    (build-var 'foo '(bar quux))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-ez-class (name &optional varlist &key documentation superclasses undecorated)
  "Defines the structure of the class"
  (let ((docpair nil)
        (vars (loop for var in varlist
                 collect (build-var name var :undecorated undecorated))))

    (if documentation
        (setf docpair (list :documentation documentation))
        (setf docpair (list :documentation "")))

    `(if ,superclasses
         ,(let ((classes (cadr superclasses)))
           `(defclass ,name
               ,classes
             ,vars
             ,docpair))
         ;;else
         (defclass ,name ()
           ,vars
           ,docpair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-ez-class-ctor (name varlist)
  "Defines a constructor `make-<name>` with keywords for the
different slots."
  (let ((initarg-list
         (loop for var in varlist collect
              (if (consp var)
                  (list (first var) (second var))
                  (list var nil))))
        (ctor-args
         (alexandria:flatten
          (loop for var in varlist
             collect
               (let ((var-symbol (if (consp var)
                                     (first var)
                                     var)))
                     (list (intern (string var-symbol) :keyword) var-symbol))))))

    ;;Add the kwarg notator
    (if initarg-list
        (push '&key initarg-list))

     `(defun ,(intern (concatenate 'string "MAKE-" (string name)))
          ,initarg-list
        (make-instance (quote ,name)
                       ,@ctor-args))))

(defmacro def-ez-equals (classname varlist)
  `(defmethod generic-comparability:equals ((obj1 ,classname) (obj2 ,classname)
                                            &rest keys
                                            &key recursive
                                            &allow-other-keys)
     "Generated EQUALS function"
     (declare (ignore keys))

     (every #'(lambda (slot)
                 (generic-comparability:equals
                  (slot-value obj1 slot)
                  (slot-value obj2 slot)
                  :recursive recursive))
             `,',varlist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defobject (name varlist
                     &key
                       (documentation nil)
                       (undecorated t)
                       (superclasses nil))
  "Defines a class `name`, along with helpers.

`name` will have its variables with these settings:
  initform as nil (or specified)
  accessor function as `var`
  initarg as :var

If `undecorated` is nil, then the accessor function will be `name-var`.

If the class will have no relation to others, it is better to decorate
the accessors.. However, if you anticipate the same accessors being
shared and related between objects, then it is better to leave them
undecorated.

If a slot name in the varlist is passed in as a pair (slotname value),
value will become the initform.

A make-`name` function definition will spring into existance, as will a
GENERIC-COMPARABILITY:EQUALS function, conforming to CDR-8.


Usage note: this macro makes for a good deal less typing for your
'average' POD class structure. It's similar to DEFSTRUCT, but instead
is a normal CLOS object suitable for normal CLOS relations. This macro
is suitable to embark on 'normal' and 'average' adventures. Genuinely
exciting and flexible adventures should require DEFCLASS directly.

DEFCLASS-STAR is a similar project, but the source code appears to
have gone travelling, and QUICKDOCS documentation appears to be
nil. This is all very depressing and is cause to find some whiskey.


Example usage:

(defobject world  (population-normals population-wizards population-dragons)
  :documentation ''Fun place!'' )"

  `(progn
     (def-ez-class ,name ,varlist
       :documentation ,documentation
       :superclasses ,superclasses
       :undecorated ,undecorated)
     (def-ez-class-ctor ,name ,varlist)
     (def-ez-equals ,name ,varlist)))
