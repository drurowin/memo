(asdf:defsystem :org.drurowin.memo
  :author "Lucien Pullen"
  :mailto "drurowin@gmail.com"
  :description "Functions that memoize calls and locally memoizing existing functions."
  :version "1.0.1"
  :license "MIT"
  :depends-on (:org.drurowin.sequence)
  :components ((:file "memo")))
