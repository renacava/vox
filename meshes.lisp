(in-package #:vox)

(defclass block-mesh ()
  ((verts :initarg :verts
          :initform *cube-verts*
          :accessor verts)
   (indices :initarg :indices
            :initform *cube-indices*
            :accessor indices)
   (n-verts :initarg :n-verts
            :initform *cube-n-verts*
            :accessor n-verts)
   (atlas-column :initarg :atlas-column
                 :initform 1.0
                 :accessor atlas-column)
   (atlas-row :initarg :atlas-row
              :initform 1.0
              :accessor atlas-row)
   (solid-p :initarg :solid-p
            :initform 1.0
            :accessor solid-p)))

(defun bind-block-symbol-to-mesh (block-symbol mesh-verts mesh-indices texture-atlas-column texture-atlas-row &optional (solid-p t))
  "Mesh should be a list of lists, where each sublist contains a list of sublists each each sublist contains a vec3 for vertices, and a vec2 for uv's."
  (bt:with-lock-held (symbol-mesh-table-lock)
    (setf (gethash block-symbol *symbol-mesh-table*)
          (make-block-mesh mesh-verts mesh-indices texture-atlas-column texture-atlas-row block-symbol solid-p)))
  )

(defun make-block-mesh (mesh-verts mesh-indices texture-atlas-column texture-atlas-row block-symbol &optional (solid-p t))
  (let (n-verts)
    (unless mesh-verts
      (setf mesh-verts *cube-verts*
            mesh-indices *cube-indices*
            n-verts *cube-n-verts*
            texture-atlas-column (or texture-atlas-column 1.0)
            texture-atlas-row (or texture-atlas-row 1.0)
            solid-p (resolve solid-p)))
    (setf (gethash block-symbol *block-solidity-table*) solid-p)
    (make-instance 'block-mesh
                   :verts mesh-verts
                   :indices mesh-indices
                   :n-verts (or n-verts (length mesh-verts))
                   :atlas-column (ensure-float texture-atlas-column)
                   :atlas-row (ensure-float texture-atlas-row)
                   :solid-p (resolve solid-p))))

(defun get-mesh-bound-to-block-symbol (block-symbol)
  (let ((block-mesh (or (gethash block-symbol *symbol-mesh-table*)
                        (setf (gethash block-symbol *symbol-mesh-table*)
                              (gethash nil *symbol-mesh-table*)))))
    (list :verts (verts block-mesh)
          :indices (indices block-mesh)
          :n-verts (n-verts block-mesh)
          :atlas-column (atlas-column block-mesh)
          :atlas-row (atlas-row block-mesh)
          :solid-p (solid-p block-mesh))))

(defparameter *block-solidity-table* (make-hash-table))

(defun get-symbol-mesh-solid-p (block-symbol)
  (gethash block-symbol *block-solidity-table*))

(defun setup-mesh-table ()
  (defparameter *symbol-mesh-table* (make-hash-table))
  (defparameter symbol-mesh-table-lock (bt:make-lock "symbol-mesh-table-lock"))
  (setup-default-cube-mesh)
  (mapcar (lambda (data) (apply #'bind-block-symbol-to-mesh data))
          (vox-world-sample:get-default-blocks-for-binding))
  
  )

(setup-mesh-table)


(defparameter lod-chunk-verts (list (list (vec3 0.0 0.0 0.0) (vec2 0.0 0.0))
                                    (list (vec3 1.0 0.0 0.0) (vec2 0.0 0.0))
                                    (list (vec3 1.0 1.0 0.0) (vec2 0.0 0.0))
                                    (list (vec3 0.0 1.0 0.0) (vec2 0.0 0.0))
                                    (list (vec3 0.0 0.0 1.0) (vec2 0.0 0.0))
                                    (list (vec3 1.0 0.0 1.0) (vec2 0.0 0.0))
                                    (list (vec3 1.0 1.0 1.0) (vec2 0.0 0.0))
                                    (list (vec3 0.0 1.0 1.0) (vec2 0.0 0.0))))
