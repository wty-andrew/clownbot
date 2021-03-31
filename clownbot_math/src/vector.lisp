(uiop:define-package #:clownbot-math/vector
  (:use #:cl)
  (:export #:make-vector #:copy-vector
           #:v+ #:v- #:.+ #:.- #:.* #:./
           #:dot #:cross
           #:norm #:normalize))

(in-package #:clownbot-math/vector)

(defun make-vector (size &key data (element-type 'single-float))
  "Create a vector."
  (apply #'make-array size :element-type element-type
         (when data
           `(:initial-contents ,(mapcar (lambda (x) (coerce x element-type))
                                        data)))))

(defun copy-vector (vec)
  "Return a copy of the vector."
  (let* ((size (length vec))
         (copy (make-vector size :element-type (array-element-type vec))))
    (dotimes (i size copy)
      (setf (aref copy i) (aref vec i)))))

(defun vector-map (fn &rest vectors)
  (when vectors
    (apply #'map `(vector ,(array-element-type (first vectors))) fn vectors)))

(defun v+ (&rest vectors)
  "Add vectors."
  (apply #'vector-map #'+ vectors))

(defun v- (&rest vectors)
  "Subtract vectors."
  (apply #'vector-map #'- vectors))

(defgeneric .+ (obj c)
  (:documentation "Add a scalar on all elements of an object."))

(defgeneric .- (obj c)
  (:documentation "Subtract a scalar on all elements of an object."))

(defgeneric .* (obj c)
  (:documentation "Multiply a scalar on all elements of an object."))

(defgeneric ./ (obj c)
  (:documentation "Divide a scalar on all elements of an object."))

(defmacro def-vector-scalar-op (name op)
  `(defmethod ,name ((vec vector) (c number))
     (vector-map (lambda (val) (,op val c)) vec)))

(def-vector-scalar-op .+ +)
(def-vector-scalar-op .- -)
(def-vector-scalar-op .* *)
(def-vector-scalar-op ./ /)

(defun dot (v1 v2)
  "Return the dot product of v1 and v2."
  (reduce #'+ (map `(vector ,(array-element-type v1)) #'* v1 v2)))

(defun cross (v1 v2)
  "Return the cross product of v1 and v2."
  (let ((x1 (aref v1 0))
        (y1 (aref v1 1))
        (z1 (aref v1 2))
        (x2 (aref v2 0))
        (y2 (aref v2 1))
        (z2 (aref v2 2)))
    (make-vector 3 :data `(,(- (* y1 z2) (* y2 z1))
                           ,(- (* x2 z1) (* x1 z2))
                           ,(- (* x1 y2) (* x2 y1))))))

(defun norm (vec)
  "Return the Euclidean norm of the vector."
  (sqrt (loop for val across vec
              sum (* val val))))

(defun normalize (vec)
  "Return the normalized vector."
  (./ vec (norm vec)))
