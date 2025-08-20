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

(defun main (args)
  (handler-case
      (let* ((parser (cl-argparse:create-main-parser
                      (main-parser "Render a Wavefront OBJ file to SVG." "obj-to-svg")
                      (cl-argparse:add-positional main-parser
                                            :name "obj-file"
                                            :help "The Wavefront OBJ file name.")
                      (cl-argparse:add-positional  main-parser
                                             :name "svg-file"
                                             :help"The SVG output file name.")))
             (pargvs (cl-argparse:parse parser (cdr args)))
             (obj-pathname (pathname  (cl-argparse:get-value "obj-file" pargvs)))
             (svg-pathname (pathname (cl-argparse:get-value "svg-file" pargvs)))
             )
        (format t "obj-file ~a~%" obj-pathname)
        (format t "svg-file ~a~%" svg-pathname)
        (obj-to-svg obj-pathname svg-pathname))
    (cl-argparse:cancel-parsing-error (e)
      (format t "~a~%" e)))
  0)
