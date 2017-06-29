.PHONY: binary mac clean

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

build/rldt: $(lisps)
	sbcl --load "build/build.lisp"

build/rldt-mac: $(lisps)
	sbcl --load "build/build-mac.lisp"

mac: build/rldt-mac build/Info.plist lib/libBearLibTerminal.dylib assets/*
	mkdir -p build/rldt.app/Contents/MacOS
	mkdir -p build/rldt.app/Contents/Resources
	mkdir -p build/rldt.app/Contents/Frameworks
	cp build/rldt-mac build/rldt.app/Contents/MacOS/rldt
	cp build/Info.plist build/rldt.app/Contents/
	cp build/rldt.icns build/rldt.app/Contents/Resources/
	cp lib/libBearLibTerminal.dylib build/rldt.app/Contents/Frameworks/
	rsync -av assets/ build/rldt.app/Contents/Resources/assets

binary: build/rldt

# Clean -----------------------------------------------------------------------

clean:
	rm -rf build/rldt.app
	rm -f build/rldt
	rm -f build/rldt-mac
