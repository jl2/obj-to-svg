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

(defun %obj-to-svg (obj-file svg-stream
                    &key
                      (eye-position (vec3 0 0 1))
                      (look-at (vec3 0 0 0))
                      (up-vector (vec3 0 1 0))
                      (default-color (vec4 0 0.9 0 0.75))
                      (fill t)
                      (hidden-line-removal t)
                      (shade t)
                      (point-lights (vec3 0 10.0 0))
                      (perspective nil)
                      (field-of-view 50.0))
  (declare (ignorable
            obj-file
            eye-position look-at up-vector default-color fill hidden-line-removal shade point-lights perspective field-of-view))
  (svg:with-svg (svg-stream 1200 1200 :default-stroke-width 0.0015)
    (let ((count (+ 180 (random 180)))
          (center (vec2 0 0))
          (sides (+ 3 (random 7)))
          (spins (1+ (random 5.0)))
          (offset (random (/ pi 2)))
          (step (1+ (random 5))))
      (loop :for i :below count :by step :do
        (svg:regular-polygon
         svg-stream center sides (- 1.0 (/ i count))
         :angle-offset (+ offset (* i (/ pi (/ count spins))))
         :stroke-color (vec4 0 0.8 0 1)
         :fill-color (vec4 (random 1.0) (+ 0.2 (random 0.5)) (random 1.0) 0.25))))))


(defun obj-to-svg (obj-file svg-file
                   &rest keys
                   &key
                     (eye-position (vec3 0 0 1))
                     (look-at (vec3 0 0 0))
                     (up-vector (vec3 0 1 0))
                     (default-color (vec4 0 0.9 0 0.75))
                     (fill t)
                     (hidden-line-removal t)
                     (shade t)
                     (point-lights (vec3 0 10.0 0))
                     (perspective nil)
                     (field-of-view 50.0))
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
  (declare (ignorable
            eye-position look-at up-vector default-color fill hidden-line-removal shade point-lights perspective field-of-view))
  (let ((obj-file (typecase obj-file
                    (pathname (obj-reader:read-obj-from-file obj-file))
                    (string (with-input-from-string (ins obj-file)
                              (obj-reader:read-obj ins)))
                    (stream (obj-reader:read-obj obj-file))))
        )

    (typecase svg-file
      (stream
       (apply #'%obj-to-svg obj-file svg-file keys))
      (pathname
       (with-output-to-file (outs svg-file)
         (apply #'%obj-to-svg obj-file outs keys)))
      (t
       (if (null svg-file)
           (with-output-to-string (outs)
             (apply #'%obj-to-svg obj-file outs keys))
           (error "svg-file must be a stream, pathname, or nil"))))))
