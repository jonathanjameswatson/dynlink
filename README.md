# dynlink

Attempting dynamic linking of Haskell modules using the GHC library.

## GHC version

9.0.2

## Usage

```
$ stack build
$ stack exec dynlink-exe
```

I am using hpack, but I have included the generated Cabal files as well.

## Current output

```
----- Linker state -----
Pkgs: [base-4.15.1.0, ghc-bignum-1.1, ghc-prim-0.7.0, rts-1.0.2]
Objs: []
BCOs: []
-----------
name: dynlink-example-package
version: 0.1.0.0
id: dynlink-example-package-0.1.0.0
exposed: True
exposed-modules: [(ExamplePackageLib, Nothing)]
hidden-modules: Paths_dynlink_example_package
trusted: False
import-dirs: /home/jonathan/Code/dynlink/.stack-work/install/x86_64-linux/6407cefbeb9c83528d7c6c7f6ea5b4b7736c8711606985b4ad476a8c4745ba30/9.0.2/lib/x86_64-linux-ghc-9.0.2/dynlink-example-package-0.1.0.0-Bvl5Gky6QHFDyGCGrCEPA
library-dirs: /home/jonathan/Code/dynlink/.stack-work/install/x86_64-linux/6407cefbeb9c83528d7c6c7f6ea5b4b7736c8711606985b4ad476a8c4745ba30/9.0.2/lib/x86_64-linux-ghc-9.0.2/dynlink-example-package-0.1.0.0-Bvl5Gky6QHFDyGCGrCEPA
dynamic-library-dirs: /home/jonathan/Code/dynlink/.stack-work/install/x86_64-linux/6407cefbeb9c83528d7c6c7f6ea5b4b7736c8711606985b4ad476a8c4745ba30/9.0.2/lib/x86_64-linux-ghc-9.0.2
hs-libraries: HSdynlink-example-package-0.1.0.0-Bvl5Gky6QHFDyGCGrCEPA
extra-libraries:
extra-ghci-libraries:
include-dirs:
includes:
depends: base-4.15.1.0
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces: /home/jonathan/Code/dynlink/.stack-work/install/x86_64-linux/6407cefbeb9c83528d7c6c7f6ea5b4b7736c8711606985b4ad476a8c4745ba30/9.0.2/doc/dynlink-example-package-0.1.0.0/dynlink-example-package.haddock
haddock-html: /home/jonathan/Code/dynlink/.stack-work/install/x86_64-linux/6407cefbeb9c83528d7c6c7f6ea5b4b7736c8711606985b4ad476a8c4745ba30/9.0.2/doc/dynlink-example-package-0.1.0.0
-----------
----- Linker state -----
Pkgs: [dynlink-example-package-0.1.0.0, base-4.15.1.0,
       ghc-bignum-1.1, ghc-prim-0.7.0, rts-1.0.2]
Objs: []
BCOs: []
-----------
Bad interface file: /home/jonathan/Code/dynlink/.stack-work/install/x86_64-linux/6407cefbeb9c83528d7c6c7f6ea5b4b7736c8711606985b4ad476a8c4745ba30/9.0.2/lib/x86_64-linux-ghc-9.0.2/dynlink-example-package-0.1.0.0-Bvl5Gky6QHFDyGCGrCEPA/ExamplePackageLib.hi
    mismatched interface file ways (wanted "", got "dyn")
```
