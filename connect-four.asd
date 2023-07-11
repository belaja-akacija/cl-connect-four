(defsystem "connect-four"
  :description "Connect four game implementation in Common Lisp"
  :author "belaja-akacija"
  :depends-on ("cl-ppcre" "vlime")
  :components ((:file "main"))
  :build-operation "program-op"
  :build-pathname "connect-four"
  :entry-point "main")
