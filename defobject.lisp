
(defpackage :defobject
  (:use :common-lisp)
  (:export
   :defobject
   ))

(in-package :defobject)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object creation macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object creation macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-var (classname var &key undecorated)
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
  "
;(defun make-name (&key (bar nil))
;(make-instance 'name :bar bar))"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this macro makes for a good deal less typing for your
;; 'average' POD class structure. It's similar to DEFSTRUCT, but
;; instead is a "normal" CLOS object.
(defmacro defobject (name varlist
                     &key
                       (documentation nil)
                       undecorated
                       (superclasses nil))
  "Defines a class `name`

`name` will have its variables with these settings:
  initform as nil (or specified)
  accessor function as `name-var`
  initarg as :var

If `undecorated` is T, then the accessor function will be `var`.

If a var is passed in as a pair (var val), val will become the
initform.

A make-`name` function definition will spring into existance

Example:
;(defobject world (population-normals population-wizards population-dragons)
  :documentation ''Fun place!'')"

  `(progn
     (def-ez-class ,name ,varlist
       :documentation ,documentation
       :superclasses ,superclasses
       :undecorated ,undecorated)
     (def-ez-class-ctor ,name ,varlist)))
