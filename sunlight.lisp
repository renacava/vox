(in-package #:vox)

(defun calculate-sunlight-on-chunk (chunk-data)
  (let* ())

  (loop for column in (chunk-columns chunk-data)
        collect (get-topmost-verts column))

  )

(defun get-topmost-solid-block (chunk-data))

(defun get-topmost-verts (chunk-data))

(defun sunlight-given-verts (verts))

(defun set-sunlight-level (level))

