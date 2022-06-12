#' Sample species patch realizations simulations according to the selected sampling design

#' @template  SamplingDesign
#' @template popdata
#' @template seed
#' @template n1
#' @template yvar
#' @template f_max
#' 
#' @noRd

createSample <- function(SamplingDesign, popdata, seed, n1, yvar, f_max) {
   if (SamplingDesign=="ACS") {
      alldata <- createACS(
         popdata=popdata, seed=seed, n1=n1, yvar=yvar)
   } else if (SamplingDesign=="RACS") {
      alldata <- createRACS(
         popdata=popdata, seed=seed, n1=n1, yvar=yvar, f_max=f_max)
   } else if (SamplingDesign=="SRS") {
      alldata <- createSRS(
         popdata=popdata, seed=seed, n1=n1)
   }
}

#' prep dataset and names of output data
#' @template  SamplingDesign
#' @param alldata NEED DEF
#' @noRd
prepDatasets <- function(SamplingDesign, alldata) {
   if (SamplingDesign!="ACS" & SamplingDesign!="RACS") {
      # datasets to apply simple mean/variance/ratio estimator
      #dats <- "alldata"
      SRSWOR_data <- NA
      #alldata <- NA
      result <- list(SRSWOR_data)
      names(result) <- "SRSWOR_data"
      return(result)
   }
   if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
      SRSWOR_data <- alldata %>% filter(.data$Sampling=="SRSWOR")
      alldata %<>% filter(.data$Sampling!="Edge")
      # apply simple mean/variance & simple ratio estimator to:
      #dats <- c("SRSWOR_data", "alldata")
      result <- list(SRSWOR_data, alldata)
      names(result) <- c("SRSWOR_data", "alldata")
      return(result)
   }
}


#' Fill m fields with NAs if statistics are not gathered
#' 
#' @noRd
fillmCharNA <- function(dat) {
   dat$mean_m <- NA
   dat$median_m <- NA
   dat$max_m <- NA
   dat$min_m <- NA
   dat$mean_uniq_m <- NA
   dat$median_uniq_m <- NA
   dat$max_uniq_m <- NA
   dat$min_uniq_m <- NA
   return(dat)
}

#' calculate statistics about m
#' 
#' @noRd
fillmChar <- function(dat, results, yvar, popvar, realvar) {
   YVAR <- sym(yvar)
   POPVAR <- sym(popvar)
   REALVAR <- sym(realvar)
   temp <- dat %>% filter(!!YVAR > 0)
   prelim_results <- temp %>%
      ungroup() %>%
      #group_by(!!POPVAR, !!REALVAR) %>%
      dplyr::summarise(
         mean_m = mean(m),
         median_m = median(m),
         max_m = max(m),
         min_m = min(m),
         mean_uniq_m = mean(unique(m)),
         median_uniq_m = median(unique(m)),
         max_uniq_m = max(unique(m)),
         min_uniq_m = min(unique(m))
      )# %>%
      #ungroup() %>%
      #select(-c(.data[[realvar]], .data[[popvar]]))
   results %>% cbind(prelim_results)
}

#' Save misc info about the
#' 
#' @noRd
addMiscInfo <- function(k, tseed, pop, dat, n1, realvar, popvar, results){
   results$simulation = k
   results$seed = tseed
   results$N.ACS.plots = dim(dat)[1] - n1
   results$N.Total.plots = dim(dat)[1]
   results$realvar = pop %$% unique(eval(parse(text=realvar))) #eval(parse(text=paste({{pop}}, "$", realvar, sep="")))[1]
   results$popvar = pop %$% unique(eval(parse(text=popvar))) #eval(parse(text=paste(deparse(substitute(pop)), "$", popvar, sep="")))[1]
   results$N.SRSWOR.plots = n1
   return(results)
}
