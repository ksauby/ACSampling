Warning: [sampleRealizations.R:85] @param requires name and description
Warning: [variance_wo_jointinclusionprobabilities.R:36] @references requires a value
Warning: [y_HT.R:102] @references requires a value

── Building ────────────────────────────────────────────────── ACSampling ──
Setting env vars:
   • CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
────────────────────────────────────────────────────────────────────────────
✔  checking for file ‘/Users/KSauby/Documents/Projects/ACS/DESCRIPTION’ (423ms)
─  preparing ‘ACSampling’: (3.7s)
✔  checking DESCRIPTION meta-information ...
─  cleaning src
─  checking for LF line-endings in source and make files and shell scripts (1.3s)
─  checking for empty or unneeded directories
Removed empty directory ‘ACSampling/vignettes/drafts’
Removed empty directory ‘ACSampling/vignettes’
─  building ‘ACSampling_0.0.0.9000.tar.gz’

── Checking ────────────────────────────────────────────────── ACSampling ──
Setting env vars:
   • _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
• _R_CHECK_CRAN_INCOMING_           : FALSE
• _R_CHECK_FORCE_SUGGESTS_          : FALSE
• NOT_CRAN                          : true
── R CMD check ─────────────────────────────────────────────────────────────
─  using log directory ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpiRBQq8/ACSampling.Rcheck’ (392ms)
─  using R version 4.2.0 (2022-04-22)
─  using platform: x86_64-apple-darwin17.0 (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’ (358ms)
✔  checking for file ‘ACSampling/DESCRIPTION’
─  this is package ‘ACSampling’ version ‘0.0.0.9000’
─  package encoding: UTF-8
✔  checking package namespace information ...
N  checking package dependencies (11.7s)
Package suggested but not available for checking: ‘covr’
N  checking if this is a source package ...
Found the following apparent object files/libraries:
   ACSampling.so
Object files/libraries should not be included in a source package.
✔  checking if there is a namespace
✔  checking for executable files (1.8s)
✔  checking for hidden files and directories ...
✔  checking for portable file names ...
✔  checking for sufficient/correct file permissions
W  checking whether package ‘ACSampling’ can be installed (56.2s)
Found the following significant warnings:
   Note: possible error in 'rvarMultDatCalc(dats, ': unused argument (m) 
See ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpiRBQq8/ACSampling.Rcheck/00install.out’ for details.
Information on the location(s) of code generating the ‘Note’s can be
obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
to ‘yes’.
✔  checking installed package size ...
✔  checking package directory (392ms)
✔  checking for future file timestamps ...
✔  checking DESCRIPTION meta-information (378ms)
N  checking top-level files
Non-standard files/directories found at top level:
   ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’
✔  checking for left-over files ...
✔  checking index information ...
✔  checking package subdirectories ...
✔  checking R files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (6.4s)
✔  checking whether the package can be loaded with stated dependencies (6.5s)
✔  checking whether the package can be unloaded cleanly (6.5s)
✔  checking whether the namespace can be loaded with stated dependencies (6.6s)
✔  checking whether the namespace can be unloaded cleanly (6.7s)
✔  checking dependencies in R code (6.6s)
✔  checking S3 generic/method consistency (7.4s)
✔  checking replacement functions (6.5s)
✔  checking foreign function calls (6.6s)
N  checking R code for possible problems (33.8s)
calcPopSummaryStats: no visible binding for global variable ‘estimate’
prepDatasets: no visible binding for global variable ‘Sampling’
sampleRealizations: possible error in rvarMultDatCalc(dats, rvar, N,
                                                      n1, m): unused argument (m)
Undefined global functions or variables:
   Sampling estimate
✔  checking Rd files (563ms)
✔  checking Rd metadata ...
N  checking Rd line widths ...
Rd file 'createACS.Rd':
   \examples lines wider than 100 characters:
   # plot ACS sample and population from which sampled was collected. In the plot, the open squares correspond to population units that we ... [TRUNCATED]
   
   These lines will be truncated in the PDF manual.
✔  checking Rd cross-references (907ms)
W  checking for missing documentation entries (6.5s)
Undocumented code objects:
   ‘Thompson1990Figure1Sample’
Undocumented data sets:
   ‘Thompson1990Figure1Sample’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
✔  checking for code/documentation mismatches (19.8s)
W  checking Rd \usage sections (339ms)
Undocumented arguments in documentation object 'sampleRealizations'
‘seeds’

Undocumented arguments in documentation object 'summarizeNetworkInfo'
‘popgroupvar’
Documented arguments not in \usage in documentation object 'summarizeNetworkInfo':
   ‘groupvar’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
W  checking Rd contents (7.9s)
Argument items with no description in Rd object 'calcSpatStats':
   ‘weights’

E  checking for unstated dependencies in examples (397ms)
Warning: parse error in file 'lines':
   1: unexpected '@'
801: 
   802: @
   ^
   ─  will not attempt to run examples
✔  checking contents of ‘data’ directory ...
✔  checking data for non-ASCII characters (379ms)
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves (359ms)
✔  checking line endings in C/C++/Fortran sources/headers
✔  checking pragmas in C/C++ headers and code ...
W  checking compilation flags used
Compilation used the following non-portable flag(s):
   ‘-Wno-unused’
including flag(s) suppressing warnings
✔  checking compiled code ...
─  checking examples ... SKIPPED
W  checking for unstated dependencies in ‘tests’ (4.2s)
'::' or ':::' import not declared from: ‘tibble’
─  checking tests ...
─  Running ‘testthat.R’ [10s/11s] (10.9s)
E  Some test files failed
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
   ── Error (test_sampleRealizations.R:462:4): test rvarMultDatCalc ───────────────
Error in `get(dats[n])`: object 'R_smd' not found
Backtrace:
   ▆
1. ├─rvarMultDatCalc(dats, rvar = "CACAonStricta", N, n1) %>% ... at test_sampleRealizations.R:462:3
2. ├─dplyr::mutate(...)
3. ├─ACSampling:::rvarMultDatCalc(...)
4. │ ├─... %>% mutate(Plots = dats[[n]])
5. │ ├─ACSampling:::rvarMultVarCalc(...)
6. │ └─base::get(dats[n])
7. └─dplyr::mutate(., Plots = dats[[n]])

[ FAIL 1 | WARN 0 | SKIP 0 | PASS 175 ]
Error: Test failures
Execution halted
✔  checking for non-standard things in the check directory ...
✔  checking for detritus in the temp directory

See
‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpiRBQq8/ACSampling.Rcheck/00check.log’
for details.


── R CMD check results ────────────────────────── ACSampling 0.0.0.9000 ────
Duration: 3m 39.8s

❯ checking for unstated dependencies in examples ... ERROR
Warning: parse error in file 'lines':
   1: unexpected '@'
801: 
   802: @
   ^
   
   ❯ checking tests ...
See below...

❯ checking whether package ‘ACSampling’ can be installed ... WARNING
See below...

❯ checking for missing documentation entries ... WARNING
Undocumented code objects:
   ‘Thompson1990Figure1Sample’
Undocumented data sets:
   ‘Thompson1990Figure1Sample’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

❯ checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'sampleRealizations'
‘seeds’

Undocumented arguments in documentation object 'summarizeNetworkInfo'
‘popgroupvar’
Documented arguments not in \usage in documentation object 'summarizeNetworkInfo':
   ‘groupvar’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

❯ checking Rd contents ... WARNING
Argument items with no description in Rd object 'calcSpatStats':
   ‘weights’

❯ checking compilation flags used ... WARNING
Compilation used the following non-portable flag(s):
   ‘-Wno-unused’
including flag(s) suppressing warnings

❯ checking for unstated dependencies in ‘tests’ ... WARNING
'::' or ':::' import not declared from: ‘tibble’

❯ checking package dependencies ... NOTE
Package suggested but not available for checking: ‘covr’

❯ checking if this is a source package ... NOTE
Found the following apparent object files/libraries:
   ACSampling.so
Object files/libraries should not be included in a source package.

❯ checking top-level files ... NOTE
Non-standard files/directories found at top level:
   ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’

❯ checking R code for possible problems ... NOTE
calcPopSummaryStats: no visible binding for global variable ‘estimate’
prepDatasets: no visible binding for global variable ‘Sampling’
sampleRealizations: possible error in rvarMultDatCalc(dats, rvar, N,
                                                      n1, m): unused argument (m)
Undefined global functions or variables:
   Sampling estimate

❯ checking Rd line widths ... NOTE
Rd file 'createACS.Rd':
   \examples lines wider than 100 characters:
   # plot ACS sample and population from which sampled was collected. In the plot, the open squares correspond to population units that we ... [TRUNCATED]
   
   These lines will be truncated in the PDF manual.

── Test failures ───────────────────────────────────────────── testthat ────

> library(testthat)
> library(ACSampling)
> 
   > testthat::test_check("ACSampling")
[ FAIL 1 | WARN 0 | SKIP 0 | PASS 175 ]

══ Failed tests ════════════════════════════════════════════════════════════════
── Error (test_sampleRealizations.R:462:4): test rvarMultDatCalc ───────────────
Error in `get(dats[n])`: object 'R_smd' not found
Backtrace:
   ▆
1. ├─rvarMultDatCalc(dats, rvar = "CACAonStricta", N, n1) %>% ... at test_sampleRealizations.R:462:3
2. ├─dplyr::mutate(...)
3. ├─ACSampling:::rvarMultDatCalc(...)
4. │ ├─... %>% mutate(Plots = dats[[n]])
5. │ ├─ACSampling:::rvarMultVarCalc(...)
6. │ └─base::get(dats[n])
7. └─dplyr::mutate(., Plots = dats[[n]])

[ FAIL 1 | WARN 0 | SKIP 0 | PASS 175 ]
Error: Test failures
Execution halted

2 errors ✖ | 6 warnings ✖ | 5 notes ✖
> 