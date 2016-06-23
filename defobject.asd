(asdf:defsystem #:defobject
  :depends-on (#:alexandria
               #:generic-comparability)
  :components ((:file "defobject"))
  :name "DEFOBJECT"
  :version "2.1"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :license "LLGPL"
  :description "A DEFSTRUCT-esque approach to defining a CLOS class. "
  :long-description "Fast way to make classes; uses the POWER of
  MACROS to deliver efficient class definitions and mangement.")
