
default : Euler

GHC_EXTENSIONS = \
-XScopedTypeVariables

GHC_WARNINGS = \
-fwarn-deprecations \
-fwarn-dodgy-imports \
-fwarn-hi-shadowing \
-fwarn-implicit-prelude \
-fwarn-incomplete-patterns \
-fwarn-missing-fields \
-fwarn-missing-methods \
-fwarn-missing-signatures \
-fwarn-orphans \
-fwarn-overlapping-patterns \
-fwarn-tabs \
-fwarn-type-defaults \
-fwarn-unused-binds \
-fwarn-unused-imports \
-fwarn-unused-matches \
-fno-warn-incomplete-record-updates \
-fno-warn-simple-patterns \
-fno-warn-monomorphism-restriction \

push :
	darcs push seanmcl@gs4083.sp.cs.cmu.edu:/usr0/wwwsrv/htdocs/repos/euler

Euler : *.lhs
	ghc --make Main.lhs $(GHC_WARNINGS) $(GHC_EXTENSIONS) -o Euler

clean : 
	-rm -f *.o *.hi Euler a.out
