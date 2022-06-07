
── R CMD check results ─────────────────────── ACSampling 0.0.0.9000 ────
Duration: 3m 39s

❯ checking for missing documentation entries ... WARNING
Undocumented code objects:
   ‘Thompson1990Figure1Sample’
Undocumented data sets:
   ‘Thompson1990Figure1Sample’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

❯ checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'randomizeClusters'
‘yvar’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

❯ checking compilation flags used ... WARNING
Compilation used the following non-portable flag(s):
   ‘-Wno-unused’
including flag(s) suppressing warnings

❯ checking for unstated dependencies in ‘tests’ ... WARNING
Warning: unable to access index for repository https://CRAN.R-project.org/src/contrib:
   cannot open URL 'https://CRAN.R-project.org/src/contrib/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.15/bioc/src/contrib:
   cannot open URL 'https://bioconductor.org/packages/3.15/bioc/src/contrib/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.15/data/annotation/src/contrib:
   cannot open URL 'https://bioconductor.org/packages/3.15/data/annotation/src/contrib/PACKAGES'
Warning: unable to access index for repository https://bioconductor.org/packages/3.15/data/experiment/src/contrib:
   cannot open URL 'https://bioconductor.org/packages/3.15/data/experiment/src/contrib/PACKAGES'

❯ checking if this is a source package ... NOTE
Found the following apparent object files/libraries:
   ACSampling.so
Object files/libraries should not be included in a source package.

❯ checking for future file timestamps ... NOTE
unable to verify current time

❯ checking top-level files ... NOTE
Non-standard files/directories found at top level:
   ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’
