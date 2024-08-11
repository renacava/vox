;;;; vox.asd

(asdf:defsystem #:vox
  :description ""
  :author "renacava"
  :license  "pending"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl
               #:cepl.sdl2
               #:cepl.sdl2-image
               #:sdl2-ttf
               #:sdl2-image
               #:nineveh
               #:temporal-functions
               #:cepl.skitter.sdl2
               #:dirt
               #:cl-soil
               #:rtg-math
               #:sdl2-game-controller-db
               #:fiveam
               #:deploy)
  :components ((:file "package")
               (:file "cube")
               (:file "chunk")
               (:file "main"))
	:defsystem-depends-on (:deploy)
	:build-operation "deploy-op"
	:build-pathname "vox"
	:entry-point "vox::main")
