devtools::check()
ℹ Updating ACSampling documentation
ℹ Loading ACSampling
Warning: [sampleRealizations_dependencies.R:54] @param requires name and description
Warning: [sampleRealizations_dependencies.R:70] @param requires name and description
Warning: [sampleRealizations_dependencies.R:71] @param requires name and description
Warning: [sampleRealizations_dependencies.R:72] @param requires name and description
Warning: [sampleRealizations_dependencies.R:73] @param requires name and description
Warning: [sampleRealizations_dependencies.R:74] @param requires name and description
Warning: [sampleRealizations_dependencies.R:101] @param requires name and description
Warning: [sampleRealizations_dependencies.R:102] @param requires name and description
Warning: [sampleRealizations_dependencies.R:103] @param requires name and description
Warning: [sampleRealizations_dependencies.R:104] @param requires name and description
Warning: [sampleRealizations_dependencies.R:106] @param requires name and description
Warning: [sampleRealizations_dependencies.R:107] @param requires name and description
Warning: [sampleRealizations_dependencies.R:108] @param requires name and description
── Building ────────────────────────────────────────────────────────────────────────────────────── ACSampling ──
Setting env vars:
   • CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
────────────────────────────────────────────────────────────────────────────────────────────────────────────────
✔  checking for file ‘/Users/KSauby/Documents/Projects/ACS/DESCRIPTION’ (355ms)
─  preparing ‘ACSampling’: (3.7s)
✔  checking DESCRIPTION meta-information ...
─  cleaning src
─  installing the package to process help pages
Loading required namespace: ACSampling
─  saving partial Rd database (6.6s)
─  cleaning src
─  checking for LF line-endings in source and make files and shell scripts (800ms)
─  checking for empty or unneeded directories
Removed empty directory ‘ACSampling/tests/drafts’
Removed empty directory ‘ACSampling/vignettes/drafts’
Removed empty directory ‘ACSampling/vignettes’
─  building ‘ACSampling_0.0.0.9000.tar.gz’

── Checking ────────────────────────────────────────────────────────────────────────────────────── ACSampling ──
Setting env vars:
   • _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
• _R_CHECK_CRAN_INCOMING_           : FALSE
• _R_CHECK_FORCE_SUGGESTS_          : FALSE
• NOT_CRAN                          : true
── R CMD check ─────────────────────────────────────────────────────────────────────────────────────────────────
─  using log directory ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/Rtmp3ttK8n/ACSampling.Rcheck’
─  using R version 4.2.0 (2022-04-22)
─  using platform: x86_64-apple-darwin17.0 (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✔  checking for file ‘ACSampling/DESCRIPTION’
─  this is package ‘ACSampling’ version ‘0.0.0.9000’
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (2.8s)
N  checking if this is a source package ...
Found the following apparent object files/libraries:
   ACSampling.so
Object files/libraries should not be included in a source package.
✔  checking if there is a namespace
✔  checking for executable files (1.8s)
✔  checking for hidden files and directories ...
W  checking for portable file names ...
Found the following file with a non-portable file name:
   man-roxygen/popvar copy 2.R
These are not fully portable file names.
See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
✔  checking for sufficient/correct file permissions ...
W  checking whether package ‘ACSampling’ can be installed (39.9s)
Found the following significant warnings:
   Note: possible error in 'calcPopSummaryStats(popdata = CactusRealizations, ': unused argument (popgroupvar = "population") 
Note: possible error in 'createSample(SamplingDesign = SamplingDesign, ': unused argument (sampling_seed = tseed2) 
See ‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/Rtmp3ttK8n/ACSampling.Rcheck/00install.out’ for details.
Information on the location(s) of code generating the ‘Note’s can be
obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
to ‘yes’.
✔  checking installed package size ...
✔  checking package directory ...
✔  checking for future file timestamps ...
✔  checking DESCRIPTION meta-information ...
N  checking top-level files
Non-standard files/directories found at top level:
   ‘ACSampling.so’ ‘dontuse’ ‘man-roxygen’ ‘rcheckerrors.R’
✔  checking for left-over files ...
✔  checking index information
✔  checking package subdirectories ...
✔  checking R files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (6.7s)
✔  checking whether the package can be loaded with stated dependencies (6.4s)
✔  checking whether the package can be unloaded cleanly (6.7s)
✔  checking whether the namespace can be loaded with stated dependencies (6.4s)
✔  checking whether the namespace can be unloaded cleanly (6.6s)
✔  checking dependencies in R code (6.5s)
✔  checking S3 generic/method consistency (8s)
✔  checking replacement functions (6.5s)
✔  checking foreign function calls (6.7s)
N  checking R code for possible problems (33.3s)
createCactusRealizationSummary: possible error in
calcPopSummaryStats(popdata = CactusRealizations, summaryvar =
                       c("Stricta", "Pusilla", "Cactus", "MEPR_on_Stricta",
                         "CACA_on_Stricta", "Percent_Cover_Stricta", "Height_Stricta",
                         "Old_Moth_Evidence_Stricta"), popgroupvar = "population", rvar =
                       c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta",
                         "Height_Stricta", "Old_Moth_Evidence_Stricta")): unused argument
(popgroupvar = "population")
sampleRealizations: possible error in createSample(SamplingDesign =
                                                      SamplingDesign, popdata = P, sampling_seed = tseed2, n1 = n1, yvar =
                                                      yvar, f_max = f_max): unused argument (sampling_seed = tseed2)
✔  checking Rd files (7.2s)
✔  checking Rd metadata ...
✔  checking Rd line widths (341ms)
✔  checking Rd cross-references (1s)
W  checking for missing documentation entries (6.5s)
Undocumented code objects:
   ‘Thompson1990Figure1Sample’
Undocumented data sets:
   ‘Thompson1990Figure1Sample’
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
✔  checking for code/documentation mismatches (19.6s)
W  checking Rd \usage sections ...
Undocumented arguments in documentation object 'calcSpatStats'
‘yvar’

Undocumented arguments in documentation object 'estim_pi_i'
‘y_variable’
Documented arguments not in \usage in documentation object 'estim_pi_i':
   ‘yvar’

Undocumented arguments in documentation object 'estim_pi_ij'
‘y_variable’
Documented arguments not in \usage in documentation object 'estim_pi_ij':
   ‘yvar’

Functions with \usage entries need to have the appropriate \alias
entries, and all their arguments documented.
The \usage entries must correspond to syntactically valid R code.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
✔  checking Rd contents (8.4s)
✔  checking for unstated dependencies in examples (401ms)
✔  checking contents of ‘data’ directory ...
✔  checking data for non-ASCII characters (354ms)
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ...
✔  checking line endings in C/C++/Fortran sources/headers
✔  checking pragmas in C/C++ headers and code ...
W  checking compilation flags used
Compilation used the following non-portable flag(s):
   ‘-Wno-unused’
including flag(s) suppressing warnings
✔  checking compiled code ...
✔  checking examples (1m 11.3s)
Examples with CPU (user + system) or elapsed time > 5s
user system elapsed
calcPopSummaryStats 42.255  0.246  42.985
W  checking for unstated dependencies in ‘tests’ (4.4s)
'::' or ':::' import not declared from: ‘tibble’
─  checking tests ...
─  Running ‘testthat.R’ [17s/18s] (18.3s)
E  Some test files failed
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
   fillSpatStatsNA(alldata_all, weights)
})
}
}
do.call(rbind.data.frame, A[[i]][[j]])
}`: task 1 failed - "unused argument (sampling_seed = tseed2)"
Backtrace:
   ▆
1. └─ACSampling::sampleRealizations(...) at test_sampleRealizations_SampleEstimators.R:29:0
2.   └─... %dopar% ...
3.     └─e$fun(obj, substitute(ex), parent.frame(), e$data)

[ FAIL 2 | WARN 1 | SKIP 0 | PASS 197 ]
Error: Test failures
Execution halted
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory

See
‘/private/var/folders/97/nlts05gx1sx8qysxpnjjhsnc0000gn/T/Rtmp3ttK8n/ACSampling.Rcheck/00check.log’
for details.


── R CMD check results ────────────────────────────────────────────────────────────── ACSampling 0.0.0.9000 ────
Duration: 4m 40.4s

❯ checking tests ...
See below...

❯ checking for portable file names ... WARNING
Found the following file with a non-portable file name:
   man-roxygen/popvar copy 2.R
These are not fully portable file names.
See section ‘Package structure’ in the ‘Writing R Extensions’ manual.

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
Undocumented arguments in documentation object 'calcSpatStats'
‘yvar’

Undocumented arguments in documentation object 'estim_pi_i'
‘y_variable’
Documented arguments not in \usage in documentation object 'estim_pi_i':
   ‘yvar’

Undocumented arguments in documentation object 'estim_pi_ij'
‘y_variable’
Documented arguments not in \usage in documentation object 'estim_pi_ij':
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
createCactusRealizationSummary: possible error in
calcPopSummaryStats(popdata = CactusRealizations, summaryvar =
                       c("Stricta", "Pusilla", "Cactus", "MEPR_on_Stricta",
                         "CACA_on_Stricta", "Percent_Cover_Stricta", "Height_Stricta",
                         "Old_Moth_Evidence_Stricta"), popgroupvar = "population", rvar =
                       c("MEPR_on_Stricta", "CACA_on_Stricta", "Percent_Cover_Stricta",
                         "Height_Stricta", "Old_Moth_Evidence_Stricta")): unused argument
(popgroupvar = "population")
sampleRealizations: possible error in createSample(SamplingDesign =
                                                      SamplingDesign, popdata = P, sampling_seed = tseed2, n1 = n1, yvar =
                                                      yvar, f_max = f_max): unused argument (sampling_seed = tseed2)

── Test failures ───────────────────────────────────────────────────────────────────────────────── testthat ────

> library(testthat)
> library(ACSampling)
> 
   > testthat::test_check("ACSampling")
nextElem.ixforeach called with redo FALSE
evaluation # 1:
$i
[1] 1

$j
[1] 1

1_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 1
accumulating result with tag 1
fired:
   [1] 1
nextElem.ixforeach called with redo FALSE
evaluation # 2:
$i
[1] 1

$j
[1] 2

1_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 2
accumulating result with tag 2
fired:
   [1] 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 1, numResults: 1, stopped: FALSE
returning status FALSE
evaluation # 3:
$i
[1] 2

$j
[1] 1

2_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 3
accumulating result with tag 3
fired:
   [1] 2 1
nextElem.ixforeach called with redo FALSE
evaluation # 4:
$i
[1] 2

$j
[1] 2

2_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 4
accumulating result with tag 4
fired:
   [1] 2 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 2, numResults: 2, stopped: FALSE
first call to combine function
evaluating call object to combine results:
   fun(result.1, result.2)
returning status FALSE
evaluation # 5:
$i
[1] 3

$j
[1] 1

3_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 5
accumulating result with tag 5
fired:
   [1] 2 2 1
nextElem.ixforeach called with redo FALSE
evaluation # 6:
$i
[1] 3

$j
[1] 2

3_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 6
accumulating result with tag 6
fired:
   [1] 2 2 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 3, numResults: 3, stopped: FALSE
calling combine function
evaluating call object to combine results:
   fun(accum, result.3)
returning status FALSE
evaluation # 7:
$i
[1] 4

$j
[1] 1

4_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 7
accumulating result with tag 7
fired:
   [1] 2 2 2 1
nextElem.ixforeach called with redo FALSE
evaluation # 8:
$i
[1] 4

$j
[1] 2

4_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 8
accumulating result with tag 8
fired:
   [1] 2 2 2 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 4, numResults: 4, stopped: FALSE
calling combine function
evaluating call object to combine results:
   fun(accum, result.4)
returning status FALSE
numValues: 4, numResults: 4, stopped: TRUE
nextElem.ixforeach called with redo FALSE
evaluation # 1:
$i
[1] 1

$j
[1] 1

1_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 1
accumulating result with tag 1
fired:
   [1] 1
nextElem.ixforeach called with redo FALSE
evaluation # 2:
$i
[1] 1

$j
[1] 2

1_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 2
accumulating result with tag 2
fired:
   [1] 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 1, numResults: 1, stopped: FALSE
returning status FALSE
evaluation # 3:
$i
[1] 2

$j
[1] 1

2_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 3
accumulating result with tag 3
fired:
   [1] 2 1
nextElem.ixforeach called with redo FALSE
evaluation # 4:
$i
[1] 2

$j
[1] 2

2_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 4
accumulating result with tag 4
fired:
   [1] 2 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 2, numResults: 2, stopped: FALSE
first call to combine function
evaluating call object to combine results:
   fun(result.1, result.2)
returning status FALSE
evaluation # 5:
$i
[1] 3

$j
[1] 1

3_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 5
accumulating result with tag 5
fired:
   [1] 2 2 1
nextElem.ixforeach called with redo FALSE
evaluation # 6:
$i
[1] 3

$j
[1] 2

3_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 6
accumulating result with tag 6
fired:
   [1] 2 2 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 3, numResults: 3, stopped: FALSE
calling combine function
evaluating call object to combine results:
   fun(accum, result.3)
returning status FALSE
evaluation # 7:
$i
[1] 4

$j
[1] 1

4_1result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 7
accumulating result with tag 7
fired:
   [1] 2 2 2 1
nextElem.ixforeach called with redo FALSE
evaluation # 8:
$i
[1] 4

$j
[1] 2

4_2result of evaluating expression:
   <simpleError in createSample(SamplingDesign = SamplingDesign, popdata = P, sampling_seed = tseed2,     n1 = n1, yvar = yvar, f_max = f_max): unused argument (sampling_seed = tseed2)>
   got results for task 8
accumulating result with tag 8
fired:
   [1] 2 2 2 2
nextElem.ixforeach called with redo FALSE
propagating accumulated result up to the next level from nextElem
NULL
numValues: 4, numResults: 4, stopped: FALSE
calling combine function
evaluating call object to combine results:
   fun(accum, result.4)
returning status FALSE
numValues: 4, numResults: 4, stopped: TRUE
[ FAIL 2 | WARN 1 | SKIP 0 | PASS 197 ]

══ Warnings ════════════════════════════════════════════════════════════════════
── Warning (test_sampleRealizations.R:29:1): (code run outside of `test_that()`) ──
executing %dopar% sequentially: no parallel backend registered
Backtrace:
   1. ACSampling::sampleRealizations(...)
at test_sampleRealizations.R:29:0
2. ... %dopar% ...
3. foreach:::getDoPar()

══ Failed tests ════════════════════════════════════════════════════════════════
── Error (test_sampleRealizations.R:29:1): (code run outside of `test_that()`) ──
Error in `{
   cat(paste(i, j, sep = "_"))
   P <- popdata %>% filter(!!POPVAR == unique(eval(parse(text = paste("popdata$", 
                                                                      popvar, sep = ""))))[i])
   N <- dim(P)[1]
   n1 <- n1_vec[j]
   A[[i]][[j]] <- list()
   if (!all(is.na(seeds))) {
      tseed1 <- (i - 1) * (nsample.length) + j
      set.seed(tseed1)
      sim_seeds <- runif(sims)
   }
   else {
      sim_seeds <- runif(sims)
   }
   for (k in 1:sims) {
      tseed2 <- sim_seeds[k]
      set.seed(tseed2)
      alldata <- createSample(SamplingDesign = SamplingDesign, 
                              popdata = P, sampling_seed = tseed2, n1 = n1, yvar = yvar, 
                              f_max = f_max)
      alldata_all <- alldata
      if (SampleEstimators == TRUE) {
         datasetprep <- prepDatasets(SamplingDesign, alldata)
         SRSWOR_data <- datasetprep$SRSWOR_data
         alldata <- datasetprep$alldata
         SampleMeanVar <- list()
         for (n in 1:length(datasetprep)) {
            dat <- datasetprep[[n]] %>% select(!!!OAVAR) %>% 
               ungroup() %>% summarise(across(everything(), 
                                              list(mean = ~mean(.x, na.rm = T), var = ~sum(.x, 
                                                                                           na.rm = T)), na.rm = T, .names = "{col}_{fn}_obs"))
            dat$Plots <- names(datasetprep)[n]
            SampleMeanVar[[n]] <- dat
         }
         SampleMeanVar %<>% bind_rows
         if (!(is.null(rvar))) {
            SmpR <- rvarMultDatCalc(datasetprep, rvar, N, 
                                    n1)
            SampleMeanVar %<>% merge(SmpR)
         }
      }
      else {
         alldata %<>% filter(Sampling != "Edge")
      }
      if (SamplingDesign == "ACS" | SamplingDesign == "RACS") {
         HTres <- list()
         HTres[[1]] <- yHTMultVarCalc(alldata, OAVAR, N, n1, 
                                      m, m_threshold, y_HT_formula)
         HTres[[2]] <- varyMultVarCalc(alldata, OAVAR, var_formula, 
                                       N, n1)
         if (!(is.null(rvar))) {
            R_smd <- createSummaryforVarCalcs(alldata, rvar, 
                                              ovar)
            HTres[[3]] <- rvarMultVarCalc(R_smd, rvar, ovar, 
                                          N, n1)
         }
         All_HT <- HTres %>% as.data.frame %>% mutate(Plots = "Horvitz Thompson Mean (All Plots)")
         if (SampleEstimators == TRUE) {
            A[[i]][[j]][[k]] = bind_rows(SampleMeanVar, All_HT)
         }
         else {
            A[[i]][[j]][[k]] <- All_HT
         }
      }
      else {
         A[[i]][[j]][[k]] <- SampleMeanVar
      }
      A[[i]][[j]][[k]] %<>% addMiscInfo(k, tseed2, P, alldata_all, 
                                        n1, realvar, popvar, .)
      if (mChar == TRUE) {
         if (sum(alldata_all$Cactus) > 0) {
            A[[i]][[j]][[k]] %<>% fillmChar(alldata_all, 
                                            ., yvar, popvar, realvar)
         }
         else {
            A[[i]][[j]][[k]] %<>% fillmCharNA()
         }
      }
      if (SpatStat == TRUE) {
         A[[i]][[j]][[k]] %<>% cbind(if (sum(alldata_all[[yvar]]) > 
                                         1) {
            calcSpatStats(alldata_all, weights, yvar)
         }
         else {
            fillSpatStatsNA(alldata_all, weights)
         })
      }
   }
   do.call(rbind.data.frame, A[[i]][[j]])
}`: task 1 failed - "unused argument (sampling_seed = tseed2)"
Backtrace:
   ▆
1. └─ACSampling::sampleRealizations(...) at test_sampleRealizations.R:29:0
2.   └─... %dopar% ...
3.     └─e$fun(obj, substitute(ex), parent.frame(), e$data)
── Error (test_sampleRealizations_SampleEstimators.R:29:1): (code run outside of `test_that()`) ──
Error in `{
   cat(paste(i, j, sep = "_"))
   P <- popdata %>% filter(!!POPVAR == unique(eval(parse(text = paste("popdata$", 
                                                                      popvar, sep = ""))))[i])
   N <- dim(P)[1]
   n1 <- n1_vec[j]
   A[[i]][[j]] <- list()
   if (!all(is.na(seeds))) {
      tseed1 <- (i - 1) * (nsample.length) + j
      set.seed(tseed1)
      sim_seeds <- runif(sims)
   }
   else {
      sim_seeds <- runif(sims)
   }
   for (k in 1:sims) {
      tseed2 <- sim_seeds[k]
      set.seed(tseed2)
      alldata <- createSample(SamplingDesign = SamplingDesign, 
                              popdata = P, sampling_seed = tseed2, n1 = n1, yvar = yvar, 
                              f_max = f_max)
      alldata_all <- alldata
      if (SampleEstimators == TRUE) {
         datasetprep <- prepDatasets(SamplingDesign, alldata)
         SRSWOR_data <- datasetprep$SRSWOR_data
         alldata <- datasetprep$alldata
         SampleMeanVar <- list()
         for (n in 1:length(datasetprep)) {
            dat <- datasetprep[[n]] %>% select(!!!OAVAR) %>% 
               ungroup() %>% summarise(across(everything(), 
                                              list(mean = ~mean(.x, na.rm = T), var = ~sum(.x, 
                                                                                           na.rm = T)), na.rm = T, .names = "{col}_{fn}_obs"))
            dat$Plots <- names(datasetprep)[n]
            SampleMeanVar[[n]] <- dat
         }
         SampleMeanVar %<>% bind_rows
         if (!(is.null(rvar))) {
            SmpR <- rvarMultDatCalc(datasetprep, rvar, N, 
                                    n1)
            SampleMeanVar %<>% merge(SmpR)
         }
      }
      else {
         alldata %<>% filter(Sampling != "Edge")
      }
      if (SamplingDesign == "ACS" | SamplingDesign == "RACS") {
         HTres <- list()
         HTres[[1]] <- yHTMultVarCalc(alldata, OAVAR, N, n1, 
                                      m, m_threshold, y_HT_formula)
         HTres[[2]] <- varyMultVarCalc(alldata, OAVAR, var_formula, 
                                       N, n1)
         if (!(is.null(rvar))) {
            R_smd <- createSummaryforVarCalcs(alldata, rvar, 
                                              ovar)
            HTres[[3]] <- rvarMultVarCalc(R_smd, rvar, ovar, 
                                          N, n1)
         }
         All_HT <- HTres %>% as.data.frame %>% mutate(Plots = "Horvitz Thompson Mean (All Plots)")
         if (SampleEstimators == TRUE) {
            A[[i]][[j]][[k]] = bind_rows(SampleMeanVar, All_HT)
         }
         else {
            A[[i]][[j]][[k]] <- All_HT
         }
      }
      else {
         A[[i]][[j]][[k]] <- SampleMeanVar
      }
      A[[i]][[j]][[k]] %<>% addMiscInfo(k, tseed2, P, alldata_all, 
                                        n1, realvar, popvar, .)
      if (mChar == TRUE) {
         if (sum(alldata_all$Cactus) > 0) {
            A[[i]][[j]][[k]] %<>% fillmChar(alldata_all, 
                                            ., yvar, popvar, realvar)
         }
         else {
            A[[i]][[j]][[k]] %<>% fillmCharNA()
         }
      }
      if (SpatStat == TRUE) {
         A[[i]][[j]][[k]] %<>% cbind(if (sum(alldata_all[[yvar]]) > 
                                         1) {
            calcSpatStats(alldata_all, weights, yvar)
         }
         else {
            fillSpatStatsNA(alldata_all, weights)
         })
      }
   }
   do.call(rbind.data.frame, A[[i]][[j]])
}`: task 1 failed - "unused argument (sampling_seed = tseed2)"
Backtrace:
   ▆
1. └─ACSampling::sampleRealizations(...) at test_sampleRealizations_SampleEstimators.R:29:0
2.   └─... %dopar% ...
3.     └─e$fun(obj, substitute(ex), parent.frame(), e$data)

[ FAIL 2 | WARN 1 | SKIP 0 | PASS 197 ]
Error: Test failures
Execution halted

1 error ✖ | 6 warnings ✖ | 3 notes ✖
> 