(asdf:defsystem #:defobject
  :depends-on (#:alexandria
	       #:closer-mop)
  :components ((:file "defobject"))
  :name "DEFOBJECT"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :license "LLGPL"
  :description "A DEFSTRUCT-esque approach to defining a CLOS class"
  :long-description "Fast way to make classes with slots")
