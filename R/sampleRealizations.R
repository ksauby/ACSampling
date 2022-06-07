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
      dats <- "alldata"
      SRSWOR_data <- NA
   }
   if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
      SRSWOR_data <- alldata %>% filter(.data$Sampling=="SRSWOR")
      alldata %<>% filter(.data$Sampling!="Edge")
      # apply simple mean/variance & simple ratio estimator to:
      dats <- c("SRSWOR_data", "alldata")
   }
   return(list(SRSWOR_data, alldata, dats))
}



#' Calculate the Horvitz-Thompson mean for multiple variables

#' @template alldata
#' @param OAVAR the variables with which to calculate the Horvitz-Thompson mean
#' @template N
#' @template n1
#' @template m
#' @template m_threshold
#' @param y_HT_formula
#' 
#' @noRd
#' 
yHTMultVarCalc <- function(alldata, OAVAR, N, n1, m, m_threshold, y_HT_formula) {
   # summarise data for mean calculations
   O <- alldata %>% 
      filter(.data$Sampling!="Edge") %>%
      select(!!!OAVAR, NetworkID, m)
   # calculate y_HT
   m <- O$m
   if (y_HT_formula == "y_HT_RACS") {
      O %>%
         select(!!!OAVAR) %>%
         summarise_all(
            list(yHT = new_y_HT),
            N = N, n1 = n1, m = m, 
            m_threshold = m_threshold
         )
   } else if (y_HT_formula == "y_HT") {
      O %>%
         select(!!!OAVAR) %>%
         summarise_all(
            list(yHT = y_HT),
            N = N, n1 = n1, m = m
         )
   }
}

#' Calculate the Horvitz-Thompson variance for multiple variables

#' @template alldata
#' @param OAVAR the variables with which to calculate the Horvitz-Thompson variance
#' @template N
#' @template n1
#' @param var_formula
#' 
#' @noRd
#' 
varyMultVarCalc <- function(alldata, OAVAR, var_formula, N, n1) {
   # summarise data for variance calculations
   O_smd <- alldata %>% 
      select(!!!OAVAR, NetworkID, m) %>%
      filter(!(is.na(NetworkID))) %>%
      group_by(NetworkID) %>%
      filter(row_number()==1) %>%
      ungroup()
   m <- O_smd$m
   # var_y_HT
   if (var_formula == "var_y_HT_RACS") {
      O_smd %>% 
         select(!!!OAVAR) %>%
         summarise_all(
            list(var_yHT_RACS = var_y_HT_RACS),
            N=N,  n1=n1,  m=m, m_threshold=2
         )
   } else if (var_formula == "var_y_HT") {
      O_smd %>% 
         select(!!!OAVAR) %>%
         summarise_all(
            list(var_yHT=var_y_HT),
            N=N, n1=n1, m=m
         )
   } else if (var_formula == "var_pi") {
      O_smd %>% 
         select(!!!OAVAR) %>%
         summarise_all(
            list(var_pi=var_pi),
            N=N, n1=n1, m=m
         )
   }
}

#' Prepare the data for calculating the Horvitz-Thompson variance

#' @template alldata
#' @template rvar
#' @template ovar
#' 
#' @noRd
#' 
createSummaryforVarCalcs <- function(alldata, rvar, ovar) {
   rovar <- c(rvar, ovar)
   ROVAR <- syms(rovar)
   # summarise data for variance calculations
   mvals <- alldata %>%
      group_by(.data$NetworkID) %>%
      summarise(m = m[1])
   R_smd <- alldata %>%
      filter(.data$Sampling!="Edge") %>%
      select(!!!ROVAR, "NetworkID") %>%
      group_by(.data$NetworkID) %>%
      #as.data.table %>%
      summarise_all(
         list(sum = sum),
         na.rm = T
      ) %>%
      merge(mvals, by="NetworkID")
}

#' Calculate the ratio mean and variance for multiple variables

#' @param R_smd dataset?
#' @template rvar
#' @template ovar
#' @template N
#' @template n1
#' 
#' @noRd
#' 
rvarMultVarCalc <- function(R_smd, rvar, ovar, N, n1) {
   tempdat <- data.frame(Var1 = NA)
   for (l in 1:length(rvar)) {
      x = R_smd[, rvar[l]]
      y = R_smd[, str_sub(rvar,-7,-1)]
      #y = eval(parse(text=paste("R_smd$", rvar[l], sep="")))
      #x = eval(parse(text = paste("R_smd$",
      # GENERALIZE THIS 
      #  str_sub(rvar[l],-7,-1), sep="")))
      tempdat$Var1 = R_hat(y = y, x = x, N = N, n1 = n1, m_vec = R_smd$m)
      tempdat$Var2 = var_R_hat(y=y, x=x, N=N, n1=n1, m_vec=R_smd$m)
      names(tempdat)[(dim(tempdat)[2] - 1) : dim(tempdat)[2]] <- c(
         paste(rvar[l], "RMeanObs", sep=""),
         paste(rvar[l], "RVarObs", sep="")
      )
   }
   tempdat
}

#' Calculate the ratio mean and variance for multiple variables and datasets

#' @param dats names of the datasets
#' @template rvar
#' @template N
#' @template n1
#' 
#' @noRd

rvarMultDatCalc <- function(dats, rvar, N, n1) {
   Ratio <- data.frame(row.names = 1:length(rvar)) 
   SmpR <- list()
   for (n in 1:length(dats)) {
      R_smd <- get(dats[n])
      SmpR[[n]] <- rvarMultVarCalc(
         R_smd = R_smd, 
         rvar=rvar, N=N, n1=n1
      ) %>% 
         mutate(
            Plots = dats[[n]]
            #Plots = deparse(substitute(dataset))
            )
   }
   do.call(rbind.data.frame, SmpR)
}


#' Calculate the Join Count Test estimate

#' @template spatdata
#' @template lwb
#' 
getJoinCountTestEst <- function(spatdata, lwb) {
   # I think cells are indexed by row, then column
   joincount.test(as.factor(spatdata$y_value), lwb)[[2]]$estimate[1]
}

#' Calculate the Moran Test estimate

#' @template spatdata
#' @template lwb
#' 
getMoranTestEst <- function(spatdata, lwb) {
   moran.test(spatdata$y_value, lwb)$estimate[1]
}

#' Calculate Spatial Statistics

#' @template alldata_all
#' @param weights String identifying the weight to use. See REFERENCE FOR?
#' 
calcSpatStats <- function(alldata_all, weights) {
   temp <- alldata_all %>%
      as.data.frame %>%
      # I DONT UNDERSTAND HOW I GOT THIS TO WORK
      # IF I REMOVE SOME UNITS THEN ITS HARD TO CALCULATE THE REALIZATION SIZE 
      # AND THUS GET TEST RESULTS
      # get rid of edge units - not involved in calculation of m
      # filter(!(is.na(NetworkID))) %>%
      arrange(x, y)
   
   # dnearneigh - why was this here? showed up April 23 2017, I don't think 
   # I ever used the function
   
   # generate neighbor list
   coordinates(temp) = ~ x+y
   # has to be a full rectangle to use this
   nb <- cell2nb(
      nrow = max(temp$x) - min(temp$x) + 1, 
      ncol = max(temp$y) - min(temp$y) + 1
   )
   #data_dist <- dim(as.matrix(dist(cbind(temp$x, temp$y))))[1]
   tempdat <- data.frame(JoinCountTest = NA)
   for (i in length(weights)) {
      lwb <- nb2listw(nb, style = weights[i]) # convert to neighbor list to weights list
      tempdat$JoinCountTest <- getJoinCountTestEst(temp, lwb)
      tempdat$MoranI <- getMoranTestEst(temp, lwb)
      colnames(tempdat)[which(names(tempdat) == "JoinCountTest")] <- 
         paste("JoinCountTest", weights[i], sep=".")
      colnames(tempdat)[which(names(tempdat) == "MoranI")] <-
         paste("MoranI", weights[i], sep=".")
   }
   return(tempdat)
}

#' Name data columns?

#' @template tempdat
#' @template weights
#' 
#' @noRd
fillSpatStatsNA <- function(tempdat, weights) {
   for (i in length(weights)) {
      tempdat <- data.frame(JoinCountTest = NA)
      tempdat$MoranI <- NA
      colnames(tempdat)[which(names(tempdat) == "JoinCountTest")] <- 
         paste("JoinCountTest", weights[i], sep=".")
      colnames(tempdat)[which(names(tempdat) == "MoranI")] <-
         paste("MoranI", weights[i], sep=".")
   }
   return(tempdat)
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
fillmChar <- function(dat, results, yvar) {
   YVAR <- sym(yvar)
   temp <- dat %>% filter(!!!yvar > 0)
   prelim_results <- temp %>% 
      summarise(
         mean_m = mean(m),
         median_m = median(m),
         max_m = max(m),
         min_m = min(m),
         mean_uniq_m = mean(unique(m)),
         median_uniq_m = median(unique(m)),
         max_uniq_m = max(unique(m)),
         min_uniq_m = min(unique(m))
      )
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


#' Sample species patch realizations simulations

#' @template yvar
#' @param popdata patch realizations
#' @param sims Number of simulations per population.
#' @template n1_vec
#' @template avar
#' @template ovar
#' @template rvar
#' @param SamplingDesign Whether simple random sampling (SRS), unrestricted adaptive cluster sampling ("ACS"), or restricted ACS ("RACS") should be performed; defaults to \code{ACS}.
#' @param y_HT_formula The formula used to estimate the population total: either the Horvitz-Thompson estimator, 'y_HT,' or or the RACS-corrected Horvitz-Thompson estimator, 'y_HT_RACS'.
#' @param var_formula The formula used to estimate the variance of the population total: either the Horvitz-Thompson variance estimator, 'var_y_HT', or the RACS-corrected Horvitz-Thompson variance estimator, "var_y_HT_RACS." Defaults to "var_y_HT".
#' @param mThreshold Default is NULL. OPTIONS
#' @param f_max Default is 2. OPTIONS
#' @param SampleEstimators If "TRUE", calculate the sample mean and sample variance for each simulation. Default is FALSE.
#' @param SpatStat TRUE or FALSE. If "TRUE", for each simulation calculate Moran's I, and the nugget, sill, and range of the semivariogram. Default is TRUE.
#' @param weights TRUE or FALSE. If SpatStat is "TRUE", this is a vector giving spatial weight matrix styles to use to calculate the Join Count and Moran's I statistics. Can take on values "W", "B", "C", "U", "S", and "minmax". See nb2listw for more details.
#' @param mChar TRUE or FALSE. If "TRUE", for each simulation calculate summary statistics (median, mean, min, and max) for the sample's m values. Also, for each simulation and for the set of unique m values, calculate the same summary statistics. If "FALSE," no summary statistics are calculated.
#' @param popvar Default is "n.networks"
#' @param realvar Default is "realization"
#' @param seeds Vector of numbers to be used to set random seeds. HOW TO CALCULATE HOW MANY NEEDED?

#' @description This function simulates sampling of multiple realizations of patches of the species of interest within the grid of locations created with \code{createPop}.

#' @references 
#' \insertRef{saubyadaptive}{ACSampling}

#' @importFrom foreach foreach %dopar%
#' @importFrom dplyr summarise_all arrange_at row_number
#' @importFrom rlang sym syms
#' @importFrom stringr str_replace
#' @import sp
 
#' @export
#' @examples
#' # sims=20
#' # n1_vec=c(5,10,20,40)
#' # population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
#' # #' avar = NULL
#' # ovar = c(
#' # 	"Stricta",
#' # 	"Pusilla",
#'#  "Cactus",
#'#  "CACA_on_Pusilla",
#' # "CACA_on_Stricta",
#' # "MEPR_on_Pusilla",
#' # "MEPR_on_Stricta",
#' # "Old_Moth_Evidence_Pusilla",
#' # "Old_Moth_Evidence_Stricta"
#' # )
#' # data(CactusRealizations)
#' # popdata = CactusRealizations # WHY IS THERE ISLAND=NA
#' # simulation_data <- sampleRealizations(
#' # popdata = popdata,
#' # 	sims = sims,
#' # 	n1_vec = n1_vec,
#' # 	avar = avar,
#' # 	ovar = ovar,
#' # popvar="Island",
#' # yvar="Cactus"
#' # )
#' # sims=200
#' # n1_vec=c(75,150,225,300,350)
#' # simulation_data_SRSWOR <- sampleRealizations(
#' # popdata = popdata,
#' # sims = sims,
#' # n1_vec = n1_vec,
#' # avar = avar,
#' # ovar = ovar,
#' # popvar="Island"
#' # )
sampleRealizations <- function(
   popdata, 
   sims, 
   n1_vec, 
   avar=NULL, 
   ovar, 
   rvar=NULL,
   #ACS=TRUE, 
   SamplingDesign="ACS",
   yvar,
   y_HT_formula = "y_HT",
   var_formula = "var_y_HT",
   mThreshold = NULL,
   f_max = 2,
   SampleEstimators = FALSE,
   SpatStat = TRUE,
   mChar = TRUE,
   popvar = "n.networks",
   realvar = "realization",
   weights="S",
   seeds = NA
) 
{
   handleError_popdata(popdata)
   handleError_n1vector(n1_vec)
   handleError_yvar(yvar)
   handleError_LogicalVar(SampleEstimators, "SampleEstimators")
   handleError_LogicalVar(SpatStat, "SpatStat")
   handleError_LogicalVar(mChar, "mChar")
   
   vars <- c(ovar, avar, rvar)
   handleError_vars(vars)
   
   
   if (!(SamplingDesign %in% c("ACS", "RACS", "SRS"))) {
      stop("SamplingDesign must be supplied as either 'SRS', ACS', or 'RACS'.")
   }
   if (!(y_HT_formula %in% c("y_HT", "y_HT_RACS"))) {
      stop("y_HT_formula must be supplied as either 'y_HT' or 'y_HT_RACS'.")
   }
   if (!(var_formula %in% c("var_y_HT", "var_y_HT_RACS"))) {
      stop("var_formula must be supplied as either 'var_y_HT' or 'var_y_HT_RACS'.")
   }
   if (!(weights %in% c("W", "B", "C", "U", "S"))) {
      stop("weights must be supplied as 'W', 'B', 'C', 'U', or 'S'.")
   }
   
   if (is.character(popvar)==FALSE) {
      stop("The argument popvar must be a character string.")
   }
   if (is.character(realvar)==FALSE) {
      stop("The argument realvar must be a character string.")
   }
   
   
   
   POPVAR <- sym(popvar)
   REALVAR <- sym(realvar)
   n.networks <- realization <- i <- j <- Sampling <- . <- NetworkID <- NULL
   TIME <- Sys.time()
   popdata %<>% arrange_at(c(popvar, realvar))
   n.patches <- length(unique(eval(parse(text=paste(
      "popdata$", popvar, sep="")))))
   nsample.length <- length(n1_vec)
   A <- vector("list", n.patches)
   # c() - same code calculates the HT estimators for occupancy and abundance
   oavar <- c(ovar, avar)
   OAVAR <- syms(oavar)
   # empty dataframes will be cbind'd together after HT estimators calculated
   occ_abund_var <- data.frame(row.names = 1:length(c(ovar, avar))) 
   occ_abund_mean <- data.frame(row.names = 1:length(c(ovar, avar)))
   # the names to assign the estimates
   # occ_abund_mean_names <- paste(ovar, avar, "MeanObs", sep="")
   #occ_abund_var_names <- paste(ovar, avar, "VarObs", sep="")
   ratio_mean_names <- paste(rvar, "RMeanObs", sep="")
   ratio_var_names <- paste(rvar, "RVarObs", sep="")
   # i=1;j=1;k=1
   Z = foreach(
      i = 1:n.patches, # for each species density
      .inorder = FALSE, 
      .packages = c("magrittr", "foreach", "plyr", "dplyr", "data.table",
                    "ACSampling", "intergraph", "network", "igraph", "stringr", 
                    "spdep"), 
      .combine = "bind_rows",
      #.errorhandling = "pass",
      .verbose = TRUE
   ) %:%
      foreach(
         j = 1:nsample.length, # for each sampling effort
         .combine = "bind_rows",
         .inorder = FALSE
      ) %dopar% {
         cat(paste(i,j, sep="_"))
         P <- popdata %>% 
            filter(!!POPVAR == unique(eval(parse(text=paste(
               "popdata$", popvar, sep=""
            ))))[i])
         N <- dim(P)[1]
         n1 <- n1_vec[j]
         A[[i]][[j]] <- list()
         r <- (i - 1) * j + j
         if (is.na(seeds)) {randnos <- runif(sims)} else {randnos <- seeds}
         for (k in 1:sims) {
            tseed <- randnos[k]*100000
            alldata <- createSample(SamplingDesign, P, tseed, n1, yvar, f_max)
            alldata_all <- alldata
            if (SampleEstimators == TRUE) {
               datasetprep <- prepDatasets(SamplingDesign, alldata)
               SRSWOR_data <- datasetprep[[1]]
               alldata <- datasetprep[[2]]
               dats <- datasetprep[[3]]
               SampleMeanVar <- list()
               for (n in 1:length(dats)) {
                  dat <- eval(parse(text=dats[[n]])) %>%
                     select(!!!OAVAR) %>%
                     summarise_all(list(mean), na.rm=T)
                  names(dat) <- str_replace(names(dat), "(.*)", "\\1_obs")
                  dat$Plots <- dats[n]
                  SampleMeanVar[[n]] <- dat
               }
               SampleMeanVar %<>% bind_rows
               # simple ratio estimators applied to alldata, SRSWOR_data
               if (!(is.null(rvar))) {
                  SmpR <- rvarMultDatCalc(dats, rvar, N, n1)
                  SampleMeanVar %<>% merge(SmpR)
               }
            } else
            {
               alldata %<>% filter(Sampling!="Edge")
            }
            if (SamplingDesign=="ACS" | SamplingDesign=="RACS") {
               ################ HORVITZ-THOMPSON ESTIMATORS ##########
               HTres <- list()
               HTres[[1]] <- yHTMultVarCalc(
                  alldata, OAVAR, N, n1, m, mThreshold, y_HT_formula)
               HTres[[2]] <- varyMultVarCalc(alldata, OAVAR, var_formula, N, n1)
               # RATIO DATA
               if (!(is.null(rvar))) {
                  R_smd <- createSummaryforVarCalcs(alldata, rvar, ovar)
                  HTres[[3]] <- rvarMultVarCalc(R_smd, rvar, ovar, N, n1)
               }
               # merge together			
               All_HT <- HTres %>% 
                  as.data.frame %>%
                  mutate(Plots = "Horvitz Thompson Mean (All Plots)")
               # merge estimates
               if (SampleEstimators == TRUE) {
                  A[[i]][[j]][[k]] = bind_rows(SampleMeanVar, All_HT)
               } else
               {
                  A[[i]][[j]][[k]] <- All_HT
               }
            } else
            {
               A[[i]][[j]][[k]] <- SampleMeanVar
            }
            # add other information
            A[[i]][[j]][[k]] %<>% addMiscInfo(k, tseed, P, alldata_all, n1, 
               realvar, popvar, results)
            # m characteristics
            if (mChar == TRUE) {
               if (sum(alldata_all$Cactus) > 0) {
                  A[[i]][[j]][[k]] %<>% fillmChar(., yvar)
               } else {
                  A[[i]][[j]][[k]] %<>% fillmCharNA()
               }
            }
            # Spatial Statistics
            if (SpatStat == TRUE) {
               A[[i]][[j]][[k]] %<>% cbind(
                  if (sum(alldata_all$Cactus) > 1) {
                     calcSpatStats(alldata_all, weights)
                  } else {
                     fillSpatStatsNA(alldata_all, weights)
                  }
               )
            }
         }
         do.call(rbind.data.frame, A[[i]][[j]])
      }
   Z$f_max = f_max
   Z$mThreshold = mThreshold
   Z$nSims = sims
   Z$SimDate = format(Sys.time(), "%m-%d-%y")
   Z$y_HT_formula = y_HT_formula
   Z$SmplngDsgn = SamplingDesign
   Z$MrnsIWghtMtrx = weights
   print(Sys.time() - TIME)
   return(Z)
}