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
         ungroup() %>%
         select(!!!OAVAR) %>%
         summarise(across(everything(),
            new_y_HT, N = N, n1 = n1, m_vec = m, 
            m_threshold = m_threshold,
            .names="{col}_yHT"
         ))
   } else if (y_HT_formula == "y_HT") {
      O %>%
         ungroup() %>%
         select(!!!OAVAR) %>%
         summarise(across(everything(), y_HT, N = N, n1 = n1, m_vec = m, .names="{col}_yHT")
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

rvarMultDatCalc <- function(datasetprep, rvar, N, n1) {
   #Ratio <- data.frame(row.names = 1:length(rvar)) 
   SmpR <- list()
   for (i in 1:length(datasetprep)) {
      #R_smd <- get(dats[n])
      dat <-  datasetprep[[i]]
      SmpR[[i]] <- rvarMultVarCalc(
         R_smd = dat,
         rvar=rvar, N=N, n1=n1
      ) %>% 
         mutate(
            Plots = names(datasetprep)[[i]]
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
   joincount.test(as.factor(spatdata), lwb)[[2]]$estimate[1]
}

#' Calculate the Moran Test estimate

#' @template spatdata
#' @template lwb
#' 
getMoranTestEst <- function(spatdata, lwb) {
   moran.test(spatdata, lwb)$estimate[1]
}

#' Calculate Spatial Statistics

#' @template alldata_all
#' @param weights String identifying the weight to use. See REFERENCE FOR?
#' 
calcSpatStats <- function(alldata_all, weights, yvar) {
   temp <- alldata_all %>%
      as.data.frame %>%
      # I DONT UNDERSTAND HOW I GOT THIS TO WORK
      # IF I REMOVE SOME UNITS THEN ITS HARD TO CALCULATE THE REALIZATION SIZE 
      # AND THUS GET TEST RESULTS
      # get rid of edge units - not involved in calculation of m
      filter(!(is.na(NetworkID))) %>%
      arrange(x, y)
   
   # dnearneigh - why was this here? showed up April 23 2017, I don't think 
   # I ever used the function
   
   # generate neighbor list
   #coordinates(temp) = ~ x+y
   # has to be a full rectangle to use this
   ROW = max(temp$x) - min(temp$x)
   COL = max(temp$y) - min(temp$y)
   
   nb <- cell2nb(
      nrow = ROW + 1, 
      ncol = COL + 1
   )
   
   
   YVAR <- sym(yvar)
   
   complete.grid <- expand.grid(
      x = min(temp$x):max(temp$x),
      y = min(temp$y):max(temp$y),
      newval = 0
   ) %>%
   as.data.frame() %>%
   merge(
      temp %>% select(x, y, !!YVAR),
      by=c("x", "y"),
      all=T
   ) %>%
      mutate(
         !!YVAR := ifelse(
            !is.na(!!YVAR),
            !!YVAR,
            newval
         )
      ) %>%
      dplyr::select(-newval)
   
   
   #data_dist <- dim(as.matrix(dist(cbind(temp$x, temp$y))))[1]
   tempdat <- data.frame(JoinCountTest = NA)
   for (i in length(weights)) {
      lwb <- nb2listw(nb, style = weights[i]) # convert to neighbor list to weights list
      tempdat$JoinCountTest <- getJoinCountTestEst(complete.grid[[yvar]], lwb)
      tempdat$MoranI <- getMoranTestEst(complete.grid[[yvar]], lwb)
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