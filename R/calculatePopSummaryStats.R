#' Calculate Summary Statistics for Patch Population Data
#' 
#' @param popdata Data on multiple realizations of patches of the species of interest within the grid of locations (created by \code{createSpeciesPatchPopulations} function).
#' @param summaryvar Vector of variables for which summary statistics should be calculated.
#' @template popgroupvar
#' @template rvar
#' @param spatweights Vector of spatial weight matrix styles. Can take on values "W", "B", "C", "U", "S", and "minmax". See \code{nb2listw} for more details.
#' @param nrow the number of rows in the grid that creates the population
#' @param ncol the number of columns in the grid that creates the population
#' @description Calculates summary statistics for patch population data, including summary statistics about $m$ (minimum, maximum, mean, and variance) for each network and population as a whole, and the number of networks with $m>1$.

#' @return A list of two dataframes, where the first dataframe includes network- and population-specific summary statistics about $m$, summarizes across unique networks as well as unique units within each population, and where the second list includes summary statistics about the \code{summaryvar}.

#' @export
#' @importFrom dplyr ungroup sym group_by_at
#' @importFrom stringr str_sub
#' @importFrom stats var
#' @importFrom sp coordinates
#' @importFrom spdep cell2nb nb2listw joincount.test moran.test 

#' @examples
#' library(magrittr)
#' library(dplyr)
#' ovar = c(
#' 	"Stricta",
#' 	"Pusilla",
#' 	"Cactus",
#' 	"CACA_on_Pusilla",
#' 	"CACA_on_Stricta",
#' 	"MEPR_on_Pusilla",
#' 	"MEPR_on_Stricta",
#' 	"Old_Moth_Evidence_Pusilla",
#' 	"Old_Moth_Evidence_Stricta"
#' 	"Percent_Cover_Pusilla", #how do I do these? they are occupancy nor abundance
#' 	"Percent_Cover_Stricta",
#' 	"Height_Pusilla",
#' 	"Height_Stricta",
#' )		
#' summaryvar = ovar
#' # WHAT WAS I THINK HERE? for grouping variables?
#' popgroupvar = "n.networks" # c("n.networks", "realization")
#' # create realizations
#' x_start = 1
#' x_end = 30
#' y_start = 1
#' y_end = 30
#' n.networks = c(5, 15, 10, 20, 30, 40)
#' n.realizations = 1
#' SpeciesInfo = PlotSurveys_season1
#' start.seed=1
#' buffer=5
#' rvar = c("CACA_on_Stricta", "CACA_on_Pusilla")
#' cactus.realizations <- createRealizations(x_start, x_end,
#' 	y_start, y_end, buffer, n.networks, n.realizations, SpeciesInfo, start.seed,
#' 	ovar)
#' patch_data_summary <- calcPopSummaryStats(cactus.realizations, 
#' 	summaryvar=ovar, popgroupvar=popgroupvar, nrow=30, ncol=30)

calcPopSummaryStats <- function(
      popdata, 
      summaryvar, 
      rvar=NULL, 
      popgroupvar,
      spatweights="S",
      nrow,
      ncol
) {
   handleError_var_in_df(popgroupvar, popdata)
   handleError_var_in_df(summaryvar, popdata)
   
   POPVAR <- sym(popgroupvar)
   
   popdata %<>% arrange(!!POPVAR)
   # for each popgroupvar combo, calculate summary statistics for m and number of species patches
   # this calculates the m statistics for the unique Network sizes
   Y1 <- popdata %>%
      group_by_at(c("NetworkID", popgroupvar)) %>%
      summarise(m = .data$m[1]) %>%
      group_by_at(popgroupvar) %>%
      summarise(
         networks_m_min = min(.data$m),
         networks_m_max = max(.data$m),
         networks_m_mean = mean(.data$m),
         networks_m_var = popVar(.data$m),
         n_networks_mGreaterThan1 = 
            length(unique(.data$NetworkID[which(.data$m>1)]))
      ) %>%
      ungroup %>%
      as.data.frame
   # this calculates the m statistics for all units
   Y2 = popdata %>%
      group_by_at(popgroupvar) %>%
      summarise(
         units_m_mean = mean(.data$m),
         units_m_var = popVar(.data$m)
      ) %>%
      ungroup %>%
      as.data.frame
   Z = popdata %>%
      group_by_at(popgroupvar) %>%
      summarise(N = length(.data$m)) %>%
      ungroup %>%
      as.data.frame
   Y1 %<>% 
      merge(Y2, by=popgroupvar) %>%
      merge(Z, by=popgroupvar)
   for (i in 1:length(summaryvar)) {
      names(Y1) <- ifelse(
         names(Y1) %in% summaryvar[i],
         paste("n_networks_", summaryvar[i], sep=""),
         names(Y1)
      )
   }
   # spatial statistics and other characteristics of variables
   A <- list()
   popvar <- paste(
      "popdata$", 
      popgroupvar, 
      sep=""
   )
   for (i in 1:length(unique(eval(parse(text=popvar))))) {
      temp <- popdata %>%
         filter(!!POPVAR == unique(!!POPVAR)[i])
      temp %<>% arrange(.data$x,.data$y)
      # spatial statistics
      coordinates(temp) = ~ x + y
      A[[i]] <- list()
      for (j in 1:length(summaryvar)) {
         A[[i]][[j]] <- data.frame(variable = summaryvar[j])
         if (summaryvar[j] %in% rvar) {
            tr <- temp %>% as.data.frame
            tr %<>% 
               .[.[colnames(.)==str_sub(summaryvar[j],-7,-1)]==1, ]	
            tv <- eval(parse(text = paste("tr$", summaryvar[j], sep="")))
            coordinates(tr) = ~ x + y
         } else {
            tv <- eval(parse(text = paste("temp$", summaryvar[j], sep="")))
         }
         A[[i]][[j]]$Mean_tempvar <- Mean(tv)
         A[[i]][[j]]$Var_tempvar <- popVar(tv)
         A[[i]][[j]]$CV_tempvar <- popCV(tv)
         A[[i]][[j]]$Total_tempvar <- Sum(tv)
         A[[i]][[j]]$SSQ_R <- calcSSQR(
            popdata = as.data.frame(temp),
            variable = summaryvar[j],
            popgroupvar
         )$SSQ_R
         # for join counts and moran's i, change NAs in rvar's to zeros
         t2 <- temp
         t2@data[rvar][is.na(t2@data[rvar])] <- 0
         if (length(tv[which(tv > 0)]) > 0) {
            # join counts and moran's i
            nb <- cell2nb(nrow = nrow, ncol = ncol)
            svar <-  eval(parse(text=paste("t2$", summaryvar[j], sep="")))
            if ("W" %in% spatweights) {
               lwb <- nb2listw(nb, style = "W") # convert to spatweights
               # I think cells are indexed by row, then column
               A[[i]][[j]]$JoinCountTest.W <- joincount.test(as.factor(svar), lwb)[[2]]$estimate[1]
               A[[i]][[j]]$MoranI.W <- moran.test(svar, lwb)$estimate[1]
            }
            if ("B" %in% spatweights) {
               lwb <- nb2listw(nb, style = "B") # convert to spatweights
               # I think cells are indexed by row, then column
               A[[i]][[j]]$JoinCountTest.B <- joincount.test(as.factor(svar), lwb)[[2]]$estimate[1]
               A[[i]][[j]]$MoranI.B <- moran.test(svar, lwb)$estimate[1]
            }	
            if ("C" %in% spatweights) {
               lwb <- nb2listw(nb, style = "C") # convert to spatweights
               # I think cells are indexed by row, then column
               A[[i]][[j]]$JoinCountTest.C <- joincount.test(as.factor(svar), lwb)[[2]]$estimate[1]
               A[[i]][[j]]$MoranI.C <- moran.test(svar, lwb)$estimate[1]
            }	
            if ("U" %in% spatweights) {
               lwb <- nb2listw(nb, style = "U") # convert to spatweights
               # I think cells are indexed by row, then column
               A[[i]][[j]]$JoinCountTest.U <- joincount.test(as.factor(svar), lwb)[[2]]$estimate[1]
               A[[i]][[j]]$MoranI.U <- moran.test(svar, lwb)$estimate[1]
            }	
            if ("S" %in% spatweights) {
               lwb <- nb2listw(nb, style = "S") # convert to spatweights
               # I think cells are indexed by row, then column
               A[[i]][[j]]$JoinCountTest.S <- joincount.test(as.factor(svar), lwb)[[2]]$estimate[1]
                  # NEED TO FIGURE OUT HOW TO GET RID OF NAs
                  # OR CAN YOU JUST NOT DO JOINT COUNT TEST FOR RVAR
                  # Get this error: Error in joincount.test(as.factor(eval(parse(text = paste("t2$", summaryvar[j],  :   objects of different length
               A[[i]][[j]]$MoranI.S <- moran.test(svar, lwb)$estimate[1]
            }	
            if ("minmax" %in% spatweights) {
               lwb <- nb2listw(nb, style = "minmax") # convert to spatweights
               # I think cells are indexed by row, then column
               A[[i]][[j]]$JoinCountTest.minmax <- joincount.test(as.factor(svar), lwb)[[2]]$estimate[1]
               A[[i]][[j]]$MoranI.minmax <- moran.test(svar, lwb)$estimate[1]
            }	
         } else {
            if ("W" %in% spatweights) {
               A[[i]][[j]]$JoinCountTest.W <- NA
               A[[i]][[j]]$MoranI.W <- NA
            }
            if ("B" %in% spatweights) {
               A[[i]][[j]]$JoinCountTest.B <- NA
               A[[i]][[j]]$MoranI.B <- NA
            }	
            if ("C" %in% spatweights) {
               A[[i]][[j]]$JoinCountTest.C <- NA
               A[[i]][[j]]$MoranI.C <- NA
            }	
            if ("U" %in% spatweights) {
               A[[i]][[j]]$JoinCountTest.U <- NA
               A[[i]][[j]]$MoranI.U <- NA
            }	
            if ("S" %in% spatweights) {
               A[[i]][[j]]$JoinCountTest.S <- NA
               A[[i]][[j]]$MoranI.S <- NA
            }	
            if ("minmax" %in% spatweights) {
               A[[i]][[j]]$JoinCountTest.minmax <- NA
               A[[i]][[j]]$MoranI.minmax <- NA
            }	
         }
      }
      A[[i]] <- do.call(rbind.data.frame, A[[i]])
      A[[i]]$population <- unique(eval(parse(
         text=paste(
            "popdata$", 
            popgroupvar, 
            sep=""
         )
      )))[i]
   }
   B <- do.call(rbind.data.frame, A)
   B %<>% arrange(variable, population) %>%
      dplyr::rename(
         Mean = Mean_tempvar,
         Var = Var_tempvar,
         CV = CV_tempvar,
         Total = Total_tempvar
      )
   return(list(Y1, B))
}