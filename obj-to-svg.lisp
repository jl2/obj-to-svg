;; Copyright (c) 2025 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :obj-to-svg)

(defun ensure-obj-file (thing)
  (etypecase thing
    (pathname (obj-reader:read-obj-from-file thing))
    (obj-file thing)
    (string (with-input-from-string (ins thing)
              (obj-reader:read-obj ins)))
    (stream (obj-reader:read-obj thing))))

(defclass point-light ()
  ((location :initarg :location
             :accessor location
             :initform (vec3 0 0 10))

   (color :initarg :color
          :accessor color
          :initform (vec3 10.0 10.0 10.0)))
  (:documentation "A point light source.  The color is pre-multiplied by the power."))

(defun obj-to-svg (obj-file
                   &key
                     (svg-file (merge-pathnames (make-pathname :type "svg")
                                                (typecase obj-file
                                                  (pathname obj-file)
                                                  (obj-file (path obj-file))
                                                  (t #P"~/images/object.obj"))
                                                ))
                     (tri-limit nil)
                     (eye-position (vec3 0 0 -100))
                     (look-at (obj-reader:bb-center (obj-reader:bounding-box obj-file)))
                     (up-vector (vec3 0 0 1))
                     (scale-factor 0.01)
                     (edge-color (vec4 1.0 0 0 0.5))
                     (show-edges nil)
                     (shade t)
                     (point-lights (list (make-instance 'point-light)))
                     (show-centers nil)
                     (svg-height 1200)
                     (svg-width 1200)

                     (perspective t)
                     (line-color nil)
                     (field-of-view 50.0)
                     (sort-predicate #'<)
                     (view-transform (if perspective
                                         (mperspective field-of-view (/ svg-height svg-width 1.0) -100.0 100.0)
                                         (mortho -1.0 1.0 -1.0 1.0 -1.0 1.0)))
                     (open-file "firefox"))
  "Render a 3D obj-file as SVG into svg-file, as viewed from eye-position, looking at look-at.
eye-position, look-at, and up-vector describe the view orientation.
default-color is the color if no materials are given in the object file.
If fill is non-nil then face polygons will be filled.
If hidden-line-removal is non-nil then an attempt will be made to hide lines behind other lines
If shade is non-nil then Gouraud shading will be used to render faces.
point-lights is a list of light positions used for shading.
If perspective is nil then orthographic rendering (no perspective, parallel lines remain parallel) will be used.
If perspective is non-nil then perspective rendering is used (parallel lines eventually merge at infinity) with the specified field of view.
filed-of-view is ignored if perspective is nil.
"
  (declare (ignorable shade point-lights))
  (let ((obj-file (ensure-obj-file obj-file))
        (really-open t)
        )

    (let ((the-mat (m*
                    view-transform
                    (mscaling (vec3 scale-factor scale-factor scale-factor))
                    (mlookat eye-position look-at up-vector)))
          (tris-and-lines (collect-geometry obj-file)))
      (sort-geometry tris-and-lines eye-position :predicate sort-predicate)
      (when tri-limit
        (setf tris-and-lines (if (> tri-limit (length tris-and-lines))
                                 tris-and-lines
                                 (subseq tris-and-lines
                                         (- (length tris-and-lines) tri-limit)))))
      (labels ((shade-geometry (geo)
                 (declare (type obj-reader:geometry geo))
                 (with-slots (vertices material geo-type) geo
                   (cond ((and shade (has-normals geo) (eq geo-type :face))
                          (let ((color (vec3 0 0 0))
                                (kd (vxyz (obj-reader:get-attribute material "Kd")))
                                (ks (vxyz (obj-reader:get-attribute material "Ks")))
                                (ns (if-let (ns (obj-reader:get-attribute material "Ns"))
                                      ns
                                      1.0))
                                (alpha (typecase (obj-reader:get-attribute material "Kd")
                                         (vec4 (vw (obj-reader:get-attribute material "Kd")))
                                         (t svg:*default-alpha*))))

                            (loop
                              :with first-vert = (obj-reader:center-point (vertices geo))
                              :with norm = (obj-reader:center-point (normals geo))
                              :for light :in point-lights
                              :for l-location = (slot-value light 'location)
                              :for l-color = (slot-value light 'color)
                              :for l-dir = (v- first-vert l-location)
                              :for distance = (vlength l-dir)
                              :for dsquared = (* distance distance)
                              :for light-dir = (vunit l-dir)
                              :for lambertian = (v. light-dir norm)
                              :for view-dir = (vunit (v- first-vert eye-position))
                              :for half-dir = (vunit (v- light-dir view-dir))
                              :for spec-angle = (max 0.0 (v. norm half-dir))
                              :for specular = (expt (* 0.24 spec-angle) ns)
                              :do
                                 (setf color (v+ color
                                                 (v* (/ specular dsquared) l-color ks)
                                                 (v* (/ lambertian distance) l-color kd))))
                            (vec4 (vx color) (vy color) (vz color) alpha)))
                         ((and line-color (eq geo-type :line))
                          line-color)
                         (t
                          (obj-reader:get-attribute (material geo) "Kd")))))

               (xform (geo)
                 (declare (type obj-reader:geometry geo))
                 (loop :for i :below (length (obj-reader:vertices geo))
                       :when (obj-reader:has-vertices geo)
                         :do
                            (setf (aref (obj-reader:vertices geo) i)
                                  (m*
                                   the-mat
                                   (aref (obj-reader:vertices geo) i)))
                       :when (obj-reader:has-normals geo)
                         :do
                            (setf (aref (obj-reader:normals geo) i)
                                  (vunit (m*
                                          (minv the-mat)
                                          (aref (obj-reader:vertices geo) i))))))

               (%obj-to-svg (svg-stream)
                 (svg:with-svg (svg-stream svg-width svg-height)
                   (loop :for geo :across tris-and-lines
                         :for color = (shade-geometry geo)
                         :do
                            (xform geo)
                            (with-slots (vertices normals material geo-type) geo
                              (case geo-type
                                (:points
                                 (loop :for vert :across vertices
                                       :do
                                          (svg:circle svg-stream vert 0.001)))
                                (:line

                                 (loop 
                                   :for idx :from 1 :below (length vertices)
                                   
                                   :do
                                      (svg:line svg-stream
                                                (aref vertices idx)
                                                (aref vertices (- idx 1))
                                                :stroke-color color)
                                   ))
                                (:face
                                 (svg:polygon svg-stream vertices
                                              :stroke-color (if show-edges
                                                                edge-color
                                                                color)
                                              :fill-color color)))

                              (when show-centers
                                (svg:circle svg-stream (loop :with average = (vec3 0 0 0)
                                                             :for vert :across vertices
                                                             :do
                                                                (setf average (v+ vert average))
                                                             :finally (return (v* (/ 1 3.0) average)))
                                            0.001)))))))

        (if (null svg-file)
            (with-output-to-string (outs)
              (%obj-to-svg outs))
            (etypecase svg-file
              (stream
               (%obj-to-svg svg-file)
               (setf really-open nil))

              (pathname
               (with-output-to-file (outs svg-file)
                 (%obj-to-svg outs)))))))
    (when (and really-open open-file)
      (cond ((eq :emacs open-file)
             #+swank (swank:eval-in-emacs `(find-file ,(namestring svg-file)))
             #-swank (error "Can't open in Emacs without Swank."))
            (open-file
             (uiop:launch-program (format nil "~a ~s &" open-file (namestring svg-file))))))))
