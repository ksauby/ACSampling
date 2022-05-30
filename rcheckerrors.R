> devtools::check()
ℹ Updating ACSampling documentation
ℹ Loading ACSampling
N  checking package dependencies (6.4s)
   Package suggested but not available for checking: ‘covr’
N  checking if this is a source package ...
   Found the following apparent object files/libraries:
     ACSampling.so
   Found the following significant warnings:
     Note: possible error in 'rvarMultDatCalc(dats, ': unused argument (m) 
   See ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpomoCLp/ACSampling.Rcheck/00install.out’ for details.
   Information on the location(s) of code generating the ‘Note’s can be
   obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
   to ‘yes’.
N  checking top-level files
   Non-standard files/directories found at top level:
     ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’
N  checking R code for possible problems (34.9s)
   calcPopSummaryStats: no visible binding for global variable ‘estimate’
   prepDatasets: no visible binding for global variable ‘Sampling’
   sampleRealizations: possible error in rvarMultDatCalc(dats, rvar, N,
     n1, m): unused argument (m)
   Undefined global functions or variables:
     Sampling estimate
   Rd file 'createACS.Rd':
     \examples lines wider than 100 characters:
        # plot ACS sample and population from which sampled was collected. In the plot, the open squares correspond to population units that we ... [TRUNCATED]
   
✔  checking Rd cross-references (1.2s)
W  checking for missing documentation entries (6.5s)
   Undocumented code objects:
     ‘Thompson1990Figure1Sample’
   Undocumented data sets:
     ‘Thompson1990Figure1Sample’
   All user-level objects in a package should have documentation entries.
   See chapter ‘Writing R documentation files’ in the ‘Writing R
   Extensions’ manual.
✔  checking for code/documentation mismatches (21s)
W  checking Rd \usage sections (374ms)
   Undocumented arguments in documentation object 'sampleRealizations'
     ‘seeds’
   
   Functions with \usage entries need to have the appropriate \alias
   entries, and all their arguments documented.
   The \usage entries must correspond to syntactically valid R code.
   See chapter ‘Writing R documentation files’ in the ‘Writing R
   Extensions’ manual.
W  checking Rd contents (13.5s)
   Argument items with no description in Rd object 'calcSpatStats':
     ‘weights’
   
E  checking for unstated dependencies in examples (432ms)
   Warning: parse error in file 'lines':
   1: unexpected '@'
   801: 
   802: @
        ^
─  will not attempt to run examples
W  checking compilation flags used
   Compilation used the following non-portable flag(s):
     ‘-Wno-unused’
   including flag(s) suppressing warnings
─  checking examples ... SKIPPED
W  checking for unstated dependencies in ‘tests’ (5s)
   '::' or ':::' import not declared from: ‘tibble’
─  Running ‘testthat.R’ [11s/13s] (12.6s)
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
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory ... 
   
   See
     ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpomoCLp/ACSampling.Rcheck/00check.log’
   for details.
   
   
── R CMD check results ─────────────────────────────── ACSampling 0.0.0.9000 ────
Duration: 3m 37.4s

❯ checking for unstated dependencies in examples ... ERROR
  Warning: parse error in file 'lines':
  1: unexpected '@'
  801: 
  802: @
       ^


❯ checking for missing documentation entries ... WARNING
  Undocumented code objects:
    ‘Thompson1990Figure1Sample’
  Undocumented data sets:
    ‘Thompson1990Figure1Sample’
  All user-level objects in a package should have documentation entries.
  See chapter ‘Writing R documentation files’ in the ‘Writing R
  Extensions’ manual.

❯ checking Rd \usage sections ... WARNING

  Functions with \usage entries need to have the appropriate \alias
  entries, and all their arguments documented.
  The \usage entries must correspond to syntactically valid R code.
  See chapter ‘Writing R documentation files’ in the ‘Writing R
  Extensions’ manual.


❯ checking if this is a source package ... NOTE
  Found the following apparent object files/libraries:
    ACSampling.so
  Object files/libraries should not be included in a source package.

❯ checking top-level files ... NOTE
  Non-standard files/directories found at top level:
    ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’

  Undefined global functions or variables:
    Sampling estimate


── Test failures ────────────────────────────────────────────────── testthat ────

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