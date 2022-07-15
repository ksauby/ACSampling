devtools::check()
ℹ Updating ACSampling documentation
ℹ Loading ACSampling
Warning: [sampleRealizations_dependencies.R:5] @param requires name and description
Warning: [sampleRealizations_depends_estimators.R:9] @param requires name and description
Writing var_pi.Rd
── Building ─────────────────────────────────────────────────────────────── ACSampling ──
Setting env vars:
   • CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
─────────────────────────────────────────────────────────────────────────────────────────
✔  checking for file ‘/Users/KSauby/Documents/Projects/ACS/DESCRIPTION’ (502ms)
─  preparing ‘ACSampling’: (3.7s)
✔  checking DESCRIPTION meta-information ...
─  cleaning src
─  installing the package to process help pages
Loading required namespace: ACSampling
─  saving partial Rd database (6.2s)
─  cleaning src
─  checking for LF line-endings in source and make files and shell scripts (828ms)
─  checking for empty or unneeded directories
Removed empty directory ‘ACSampling/tests/drafts’
Removed empty directory ‘ACSampling/tests/testthat/_snaps’
Removed empty directory ‘ACSampling/vignettes/drafts’
Removed empty directory ‘ACSampling/vignettes’
─  building ‘ACSampling_0.0.0.9000.tar.gz’

── Checking ─────────────────────────────────────────────────────────────── ACSampling ──
Setting env vars:
   • _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
• _R_CHECK_CRAN_INCOMING_           : FALSE
• _R_CHECK_FORCE_SUGGESTS_          : FALSE
• NOT_CRAN                          : true
── R CMD check ──────────────────────────────────────────────────────────────────────────
─  using log directory ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpCq4HIH/ACSampling.Rcheck’
─  using R version 4.2.0 (2022-04-22)
─  using platform: x86_64-apple-darwin17.0 (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✔  checking for file ‘ACSampling/DESCRIPTION’
─  this is package ‘ACSampling’ version ‘0.0.0.9000’
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (3.4s)
N  checking if this is a source package ...
Found the following apparent object files/libraries:
   ACSampling.so
Object files/libraries should not be included in a source package.
✔  checking if there is a namespace
✔  checking for executable files (1.7s)
✔  checking for hidden files and directories ...
✔  checking for portable file names ...
✔  checking for sufficient/correct file permissions
✔  checking whether package ‘ACSampling’ can be installed (38.3s)
✔  checking installed package size ...
✔  checking package directory ...
✔  checking for future file timestamps (393ms)
✔  checking DESCRIPTION meta-information ...
N  checking top-level files
Non-standard files/directories found at top level:
   ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’
✔  checking for left-over files ...
✔  checking index information
✔  checking package subdirectories ...
✔  checking R files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (6.5s)
✔  checking whether the package can be loaded with stated dependencies (6.3s)
✔  checking whether the package can be unloaded cleanly (6.3s)
✔  checking whether the namespace can be loaded with stated dependencies (6.1s)
✔  checking whether the namespace can be unloaded cleanly (6.4s)
✔  checking dependencies in R code (6.6s)
✔  checking S3 generic/method consistency (8.6s)
✔  checking replacement functions (6.3s)
✔  checking foreign function calls (6.7s)
N  checking R code for possible problems (40.1s)
calcSpatStats: no visible binding for global variable ‘newval’
var_Tille: no visible global function definition for ‘Hajek’
var_pi: no visible global function definition for ‘Hajek’
Undefined global functions or variables:
   Hajek newval
✔  checking Rd files (10.6s)
✔  checking Rd metadata ...
✔  checking Rd line widths ...
✔  checking Rd cross-references (1.2s)
W  checking for missing documentation entries (6.3s)
Undocumented code objects:
   ‘Thompson1990Figure1Sample’
Undocumented data sets:
   ‘Thompson1990Figure1Sample’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
✔  checking for code/documentation mismatches (20.9s)
W  checking Rd \usage sections ...
Undocumented arguments in documentation object 'calcPopSummaryStats'
‘popdata’ ‘summaryvar’ ‘rvar’ ‘popgroupvar’ ‘spatweights’ ‘nrow’
‘ncol’

Undocumented arguments in documentation object 'calcSpatStats'
‘yvar’

Documented arguments not in \usage in documentation object 'ovar':
   ‘popdata’ ‘summaryvar’ ‘popgroupvar’ ‘rvar’ ‘spatweights’ ‘nrow’
‘ncol’

Undocumented arguments in documentation object 'randomizeClusters'
‘yvar’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
✔  checking Rd contents (8.3s)
✔  checking for unstated dependencies in examples ...
✔  checking contents of ‘data’ directory ...
✔  checking data for non-ASCII characters ...
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ...
✔  checking line endings in C/C++/Fortran sources/headers ...
✔  checking pragmas in C/C++ headers and code ...
W  checking compilation flags used
Compilation used the following non-portable flag(s):
   ‘-Wno-unused’
including flag(s) suppressing warnings
✔  checking compiled code ...
E  checking examples (27.4s)
Running examples in ‘ACSampling-Ex.R’ failed
The error most likely occurred in:
   
   > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: var_pi
   > ### Title: Variance estimator free of joint inclusion probability
   > ###   calculations for unequal probability sampling
   > ### Aliases: var_pi
   > 
   > ### ** Examples
   > 
   > # Hajek Approximation
   > library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
   
   filter, lag

The following objects are masked from ‘package:base’:
   
   intersect, setdiff, setequal, union

> library(magrittr)
> Z = createACS(Thompson1990Fig1Pop, seed=3, n1=30, "y_value", criterion=0)
> Z_summary <- Z %>% 
   + 	dplyr::filter(Sampling!="Edge") %>%
   + 	group_by(NetworkID) %>%
   + 	filter(NetworkID > 0) %>%
   + 	dplyr::summarise(
      + 		m = m[1],
      + 		y_total = sum(y_value, na.rm=TRUE)
      + 	)
> var_y_HT(
   + 	N = dim(Thompson1990Fig1Pop)[1], 
   + 	n1 = dim(Thompson1990Figure1Sample)[1], 
   + 	m = Z_summary$m, 
   + 	y = Z_summary$y_total
   + )
[1] 0.8933236
> pi_i_values <- pi_i(N=900,n1=30, m=Z_summary$m)
> Hajek_b(pi_i=pi_i_values, n=30)
[1] 0.22234482 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[7] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[13] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[19] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[25] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
> var_pi(
   + 	n = 30, 
   + 	y = Z_summary$y_total, 
   + 	pi_i_values = pi_i_values,
   + 	estimator = "Hajek"
   + )
Error in Hajek(pi_i_values, n) : could not find function "Hajek"
Calls: var_pi
Execution halted
W  checking for unstated dependencies in ‘tests’ (4.6s)
'::' or ':::' import not declared from: ‘tibble’
─  checking tests (351ms)
✔  Running ‘testthat.R’ [34s/36s] (35.7s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory

See
‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/RtmpCq4HIH/ACSampling.Rcheck/00check.log’
for details.


── R CMD check results ─────────────────────────────────────── ACSampling 0.0.0.9000 ────
Duration: 4m 23.6s

❯ checking examples ... ERROR
Running examples in ‘ACSampling-Ex.R’ failed
The error most likely occurred in:
   
   > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: var_pi
   > ### Title: Variance estimator free of joint inclusion probability
   > ###   calculations for unequal probability sampling
   > ### Aliases: var_pi
   > 
   > ### ** Examples
   > 
   > # Hajek Approximation
   > library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
   
   filter, lag

The following objects are masked from ‘package:base’:
   
   intersect, setdiff, setequal, union

> library(magrittr)
> Z = createACS(Thompson1990Fig1Pop, seed=3, n1=30, "y_value", criterion=0)
> Z_summary <- Z %>% 
   + 	dplyr::filter(Sampling!="Edge") %>%
   + 	group_by(NetworkID) %>%
   + 	filter(NetworkID > 0) %>%
   + 	dplyr::summarise(
      + 		m = m[1],
      + 		y_total = sum(y_value, na.rm=TRUE)
      + 	)
> var_y_HT(
   + 	N = dim(Thompson1990Fig1Pop)[1], 
   + 	n1 = dim(Thompson1990Figure1Sample)[1], 
   + 	m = Z_summary$m, 
   + 	y = Z_summary$y_total
   + )
[1] 0.8933236
> pi_i_values <- pi_i(N=900,n1=30, m=Z_summary$m)
> Hajek_b(pi_i=pi_i_values, n=30)
[1] 0.22234482 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[7] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[13] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[19] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
[25] 0.03333333 0.03333333 0.03333333 0.03333333 0.03333333
> var_pi(
   + 	n = 30, 
   + 	y = Z_summary$y_total, 
   + 	pi_i_values = pi_i_values,
   + 	estimator = "Hajek"
   + )
Error in Hajek(pi_i_values, n) : could not find function "Hajek"
Calls: var_pi
Execution halted

❯ checking for missing documentation entries ... WARNING
Undocumented code objects:
   ‘Thompson1990Figure1Sample’
Undocumented data sets:
   ‘Thompson1990Figure1Sample’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

❯ checking Rd \usage sections ... WARNING
Undocumented arguments in documentation object 'calcPopSummaryStats'
‘popdata’ ‘summaryvar’ ‘rvar’ ‘popgroupvar’ ‘spatweights’ ‘nrow’
‘ncol’

Undocumented arguments in documentation object 'calcSpatStats'
‘yvar’

Documented arguments not in \usage in documentation object 'ovar':
   ‘popdata’ ‘summaryvar’ ‘popgroupvar’ ‘rvar’ ‘spatweights’ ‘nrow’
‘ncol’

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
'::' or ':::' import not declared from: ‘tibble’

❯ checking if this is a source package ... NOTE
Found the following apparent object files/libraries:
   ACSampling.so
Object files/libraries should not be included in a source package.

❯ checking top-level files ... NOTE
Non-standard files/directories found at top level:
   ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’

❯ checking R code for possible problems ... NOTE
calcSpatStats: no visible binding for global variable ‘newval’
var_Tille: no visible global function definition for ‘Hajek’
var_pi: no visible global function definition for ‘Hajek’
Undefined global functions or variables:
   Hajek newval

1 error ✖ | 4 warnings ✖ | 3 notes ✖
> 