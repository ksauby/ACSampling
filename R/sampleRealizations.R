#' Sample species patch realizations simulations

#' @template yvar
#' @param popdata patch realizations
#' @param sims Number of simulations per population.
#' @template n1_vec
#' @template avar
#' @template ovar
#' @template rvar
#' @param SamplingDesign Whether simple random sampling (SRS), unrestricted 
#' adaptive cluster sampling ("ACS"), or restricted ACS ("RACS") should be 
#' performed; defaults to \code{ACS}.
#' @param y_HT_formula The formula used to estimate the population total: 
#' either the Horvitz-Thompson estimator, 'y_HT,' or or the RACS-corrected 
#' Horvitz-Thompson estimator, 'y_HT_RACS'.
#' @param var_formula The formula used to estimate the variance of the 
#' population total: either the Horvitz-Thompson variance estimator, 'var_y_HT',
#' or the RACS-corrected Horvitz-Thompson variance estimator, "var_y_HT_RACS." 
#' Defaults to "var_y_HT".
#' @param mThreshold Default is NULL. OPTIONS
#' @param f_max Default is 2. OPTIONS
#' @param SampleEstimators If "TRUE", calculate the sample mean and sample 
#' variance for each simulation. Default is FALSE.
#' @param SpatStat TRUE or FALSE. If "TRUE", for each simulation calculate 
#' Moran's I, and the nugget, sill, and range of the semivariogram. Default is 
#' TRUE.
#' @param weights TRUE or FALSE. If SpatStat is "TRUE", this is a vector giving 
#' spatial weight matrix styles to use to calculate the Join Count and Moran's I
#'  statistics. Can take on values "W", "B", "C", "U", "S", and "minmax". See 
#'  nb2listw for more details.
#' @param mChar TRUE or FALSE. If "TRUE", for each simulation calculate summary 
#' statistics (median, mean, min, and max) for the sample's m values. Also, for 
#' each simulation and for the set of unique m values, calculate the same 
#' summary statistics. If "FALSE," no summary statistics are calculated.
#' @param popvar Variable identifying each population. Default is "n.networks"
#' @param realvar Variable identifying each realization. Default is "realization"
#' @param seeds Vector of numbers to be used to set random seeds. HOW TO 
#' CALCULATE HOW MANY NEEDED?

#' @description This function simulates sampling of multiple realizations of 
#' patches of the species of interest within the grid of locations created with 
#' \code{createPop}. The number of total simulations is
#' length(n1_vec) x length(popvar) x length(realvar)

#' @references 
#' \insertRef{saubyadaptive}{ACSampling}

#' @importFrom foreach foreach %dopar%
#' @importFrom dplyr summarise_all arrange_at row_number
#' @importFrom rlang sym syms enquo
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
      .packages = c("magrittr", "foreach", #"plyr", 
                    "dplyr",# "data.table",
                    "ACSampling"#,  "intergraph", "network", "igraph", "stringr", 
                    #"spdep"
                    ), 
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
         #r <- (i - 1) * j + j
         if (!all(is.na(seeds))) {
            tseed1 <- (i-1)*(nsample.length) + j
            set.seed(tseed1)
            sim_seeds <- runif(sims)
         }
         for (k in 1:sims) {
            if (!all(is.na(seeds))) {
               tseed2 <- sim_seeds[k]
               set.seed(tseed2)
            } else {tseed2 <- runif(1)}
            alldata <- createSample(SamplingDesign, popdata=P, seed=tseed2, n1=n1, yvar=yvar, f_max=f_max)
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
                  SmpR <- rvarMultDatCalc(datasetprep, rvar, N, n1)
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
            A[[i]][[j]][[k]] %<>% addMiscInfo(k, tseed2, P, alldata_all, n1, 
               realvar, popvar, .)
            # m characteristics
            if (mChar == TRUE) {
               if (sum(alldata_all$Cactus) > 0) {
                  A[[i]][[j]][[k]] %<>% fillmChar(alldata_all, ., yvar, popvar, realvar)
               } else {
                  A[[i]][[j]][[k]] %<>% fillmCharNA()
               }
            }
            # Spatial Statistics
            if (SpatStat == TRUE) {
               A[[i]][[j]][[k]] %<>% cbind(
                  if (sum(alldata_all$Cactus) > 1) {
                     calcSpatStats(alldata_all, weights, yvar)
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
   Z$SamplingDesign = SamplingDesign
   Z$MoransIWeightMatrix = weights
   print(Sys.time() - TIME)
   return(Z)
}