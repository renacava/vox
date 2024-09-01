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
               #:deploy
               #:lparallel
               #:precise-time
               #:vox-world-sample
               #:vox-cam)
  :components ((:file "package")
               (:file "utilities")
               (:file "gpu-funcs")
               (:file "cube")
               (:file "cube-faces")
               (:file "sky")
               (:file "chunk")
               (:file "meshes")
               (:file "cull-hidden")
               (:file "sunlight")
               (:file "main"))
	:defsystem-depends-on (:deploy)
	:build-operation "deploy-op"
	:build-pathname "vox"
	:entry-point "vox::main")
