# Build obj-to-svg

obj-to-svg: *.lisp *.asd Makefile
	buildapp --output obj-to-svg \
			 --load ~/quicklisp/setup.lisp \
			 --eval '(ql:quickload :obj-to-svg)' \
			 --entry 'obj-to-svg:main'

test: t/*.lisp *.lisp *.asd Makefile
	sbcl --eval "(ql:quickload :obj-to-svg.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
