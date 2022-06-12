#' sims = 1
#' n1_vec = c(5, 10)
#' population <-
#'    createPop(
#'       x_start = 1,
#'       x_end = 30,
#'       y_start = 1,
#'       y_end = 30
#'    )
#' #' avar = NULL
#' ovar = c("Stricta",
#'          "CACA_on_Stricta")
#' avar = NULL
#' data(CactusRealizations)
#' popdata = CactusRealizations # WHY IS THERE ISLAND=NA
#' 
#' 
#' test_that("test sampleRealizations", {
#'    expect_error(
#'       sampleRealizations(
#'          popdata = popdata,
#'          sims = sims,
#'          n1_vec = 1,
#'          avar = avar,
#'          ovar = ovar,
#'          popvar = "Island",
#'          yvar = "Cactus",
#'          realvar = 5
#'       ),
#'       "The argument realvar must be a character string."
#'    )
#' 
#' })



#' # 

sims=2
n1_vec=c(5,10)
population <- createPop(x_start = 1, x_end = 30, y_start = 1, y_end = 30)
avar = NULL
ovar = c(
   "Stricta",
   "Pusilla",
   "Cactus"
)
yvar="Cactus"
rvar=NULL
SamplingDesign="ACS"
y_HT_formula = "y_HT"
var_formula = "var_y_HT"
mThreshold = NULL
f_max = 2
SampleEstimators = FALSE
SpatStat = TRUE
mChar = TRUE
popvar = "n.networks"
realvar = "realization"
weights="S"
seeds = 1:1000
data(CactusRealizations)
popdata = CactusRealizations %>%
   filter(n.networks==5 | n.networks==10 | n.networks==15 | n.networks==20)

# WHY IS THERE ISLAND=NA
simulation_data <- sampleRealizations(
   popdata,
   sims,
   n1_vec,
   avar,
   ovar,
   rvar,
   #ACS=TRUE,
   SamplingDesign,
   yvar,
   y_HT_formula,
   var_formula,
   mThreshold,
   f_max,
   SampleEstimators,
   SpatStat,
   mChar,
   popvar,
   realvar,
   weights,
   seeds
)

#simulation_data %<>% filter(popvar < 30)
realvar <- 1
yvar <- "Cactus"
# n1=5, popvar=5 --------------------------------------------------------------#
tseed1 <- 1
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 5
popvar <- 5
popdata05 <- popdata %>% filter(n.networks == popvar)

# sim 1
seed_05_05_01 <- sim_seeds[1]
set.seed(seed_05_05_01)
alldata <- createACS(popdata=popdata05, seed=seed, n1=n1, yvar=yvar)
alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
N.ACS.plots_05_05_01 <- dim(alldata)[1] - n1
N.Total.plots_05_05_01 <- dim(alldata)[1]

O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_05_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_05_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_05_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_05_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_05_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_05_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_05_01 = mean(alldata$m)
median_m_05_05_01 = median(alldata$m)
max_m_05_05_01 = max(alldata$m)
min_m_05_05_01 = min(alldata$m)
mean_uniq_m_05_05_01 = mean(unique(alldata$m))
median_uniq_m_05_05_01 = median(unique(alldata$m))
max_uniq_m_05_05_01 = max(unique(alldata$m))
min_uniq_m_05_05_01 = min(unique(alldata$m))

# sim 2
seed_05_05_02 <- sim_seeds[2]
set.seed(seed_05_05_02)
alldata <- createACS(popdata=popdata05, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_05_02 <- dim(alldata)[1] - n1
N.Total.plots_05_05_02 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_05_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_05_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_05_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_05_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_05_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_05_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_05_02 = mean(alldata$m)
median_m_05_05_02 = median(alldata$m)
max_m_05_05_02 = max(alldata$m)
min_m_05_05_02 = min(alldata$m)
mean_uniq_m_05_05_02 = mean(unique(alldata$m))
median_uniq_m_05_05_02 = median(unique(alldata$m))
max_uniq_m_05_05_02 = max(unique(alldata$m))
min_uniq_m_05_05_02 = min(unique(alldata$m))

# n1=10, popvar=5 --------------------------------------------------------------#
tseed1 <- 2
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 10
popvar <- 5

# sim 1
seed_10_05_01 <- sim_seeds[1]
set.seed(seed_10_05_01)
alldata <- createACS(popdata=popdata05, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_05_01 <- dim(alldata)[1] - n1
N.Total.plots_10_05_01 <- dim(alldata)[1]


alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_05_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_05_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_05_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_05_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_05_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_05_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_05_01 = mean(alldata$m)
median_m_10_05_01 = median(alldata$m)
max_m_10_05_01 = max(alldata$m)
min_m_10_05_01 = min(alldata$m)
mean_uniq_m_10_05_01 = mean(unique(alldata$m))
median_uniq_m_10_05_01 = median(unique(alldata$m))
max_uniq_m_10_05_01 = max(unique(alldata$m))
min_uniq_m_10_05_01 = min(unique(alldata$m))

# sim 2
seed_10_05_02 <- sim_seeds[2]
set.seed(seed_10_05_02)
alldata <- createACS(popdata=popdata05, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_05_02 <- dim(alldata)[1] - n1
N.Total.plots_10_05_02 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_05_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_05_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_05_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_05_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_05_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_05_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_05_02 = mean(alldata$m)
median_m_10_05_02 = median(alldata$m)
max_m_10_05_02 = max(alldata$m)
min_m_10_05_02 = min(alldata$m)
mean_uniq_m_10_05_02 = mean(unique(alldata$m))
median_uniq_m_10_05_02 = median(unique(alldata$m))
max_uniq_m_10_05_02 = max(unique(alldata$m))
min_uniq_m_10_05_02 = min(unique(alldata$m))

# n1=5, popvar=10 --------------------------------------------------------------#
tseed1 <- 3
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 5
popvar <- 10
popdata10 <- popdata %>% filter(n.networks == popvar)

# sim 1
seed_05_10_01 <- sim_seeds[1]
set.seed(seed_05_10_01)
alldata <- createACS(popdata=popdata10, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_10_01 <- dim(alldata)[1] - n1
N.Total.plots_05_10_01 <- dim(alldata)[1]


alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_10_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_10_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_10_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_10_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_10_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_10_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_10_01 = mean(alldata$m)
median_m_05_10_01 = median(alldata$m)
max_m_05_10_01 = max(alldata$m)
min_m_05_10_01 = min(alldata$m)
mean_uniq_m_05_10_01 = mean(unique(alldata$m))
median_uniq_m_05_10_01 = median(unique(alldata$m))
max_uniq_m_05_10_01 = max(unique(alldata$m))
min_uniq_m_05_10_01 = min(unique(alldata$m))

# sim 2
seed_05_10_02 <- sim_seeds[2]
set.seed(seed_05_10_02)
alldata <- createACS(popdata=popdata10, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_10_02 <- dim(alldata)[1] - n1
N.Total.plots_05_10_02 <- dim(alldata)[1]


alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_10_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_10_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_10_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_10_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_10_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_10_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_10_02 = mean(alldata$m)
median_m_05_10_02 = median(alldata$m)
max_m_05_10_02 = max(alldata$m)
min_m_05_10_02 = min(alldata$m)
mean_uniq_m_05_10_02 = mean(unique(alldata$m))
median_uniq_m_05_10_02 = median(unique(alldata$m))
max_uniq_m_05_10_02 = max(unique(alldata$m))
min_uniq_m_05_10_02 = min(unique(alldata$m))
# sim 2
# n1=10, popvar=10 --------------------------------------------------------------#
tseed1 <- 4
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 10
popvar <- 10

# sim 1
seed_10_10_01 <- sim_seeds[1]
set.seed(seed_10_10_01)
alldata <- createACS(popdata=popdata10, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_10_01 <- dim(alldata)[1] - n1
N.Total.plots_10_10_01 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_10_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_10_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_10_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_10_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_10_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_10_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_10_01 = mean(alldata$m)
median_m_10_10_01 = median(alldata$m)
max_m_10_10_01 = max(alldata$m)
min_m_10_10_01 = min(alldata$m)
mean_uniq_m_10_10_01 = mean(unique(alldata$m))
median_uniq_m_10_10_01 = median(unique(alldata$m))
max_uniq_m_10_10_01 = max(unique(alldata$m))
min_uniq_m_10_10_01 = min(unique(alldata$m))

# sim 2
seed_10_10_02 <- sim_seeds[2]
set.seed(seed_10_10_02)
alldata <- createACS(popdata=popdata10, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_10_02 <- dim(alldata)[1] - n1
N.Total.plots_10_10_02 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_10_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_10_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_10_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_10_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_10_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_10_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_10_02 = mean(alldata$m)
median_m_10_10_02 = median(alldata$m)
max_m_10_10_02 = max(alldata$m)
min_m_10_10_02 = min(alldata$m)
mean_uniq_m_10_10_02 = mean(unique(alldata$m))
median_uniq_m_10_10_02 = median(unique(alldata$m))
max_uniq_m_10_10_02 = max(unique(alldata$m))
min_uniq_m_10_10_02 = min(unique(alldata$m))

# n1=5, popvar=15 --------------------------------------------------------------#
tseed1 <- 5
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 5
popvar <- 15
popdata15 <- popdata %>% filter(n.networks == popvar)

# sim 1
seed_05_15_01 <- sim_seeds[1]
set.seed(seed_05_15_01)
alldata <- createACS(popdata=popdata15, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_15_01 <- dim(alldata)[1] - n1
N.Total.plots_05_15_01 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_15_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_15_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_15_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_15_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_15_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_15_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_15_01 = mean(alldata$m)
median_m_05_15_01 = median(alldata$m)
max_m_05_15_01 = max(alldata$m)
min_m_05_15_01 = min(alldata$m)
mean_uniq_m_05_15_01 = mean(unique(alldata$m))
median_uniq_m_05_15_01 = median(unique(alldata$m))
max_uniq_m_05_15_01 = max(unique(alldata$m))
min_uniq_m_05_15_01 = min(unique(alldata$m))

# sim 2
seed_05_15_02 <- sim_seeds[2]
set.seed(seed_05_15_02)
alldata <- createACS(popdata=popdata15, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_15_02 <- dim(alldata)[1] - n1
N.Total.plots_05_15_02 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_15_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_15_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_15_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_15_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_15_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_15_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_15_02 = mean(alldata$m)
median_m_05_15_02 = median(alldata$m)
max_m_05_15_02 = max(alldata$m)
min_m_05_15_02 = min(alldata$m)
mean_uniq_m_05_15_02 = mean(unique(alldata$m))
median_uniq_m_05_15_02 = median(unique(alldata$m))
max_uniq_m_05_15_02 = max(unique(alldata$m))
min_uniq_m_05_15_02 = min(unique(alldata$m))
# n1=10, popvar=15 --------------------------------------------------------------#
tseed1 <- 6
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 10
popvar <- 15

# sim 1
seed_10_15_01 <- sim_seeds[1]
set.seed(seed_10_15_01)
alldata <- createACS(popdata=popdata15, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_15_01 <- dim(alldata)[1] - n1
N.Total.plots_10_15_01 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_15_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_15_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_15_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_15_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_15_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_15_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_15_01 = mean(alldata$m)
median_m_10_15_01 = median(alldata$m)
max_m_10_15_01 = max(alldata$m)
min_m_10_15_01 = min(alldata$m)
mean_uniq_m_10_15_01 = mean(unique(alldata$m))
median_uniq_m_10_15_01 = median(unique(alldata$m))
max_uniq_m_10_15_01 = max(unique(alldata$m))
min_uniq_m_10_15_01 = min(unique(alldata$m))

# sim 2
seed_10_15_02 <- sim_seeds[2]
set.seed(seed_10_15_02)
alldata <- createACS(popdata=popdata15, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_15_02 <- dim(alldata)[1] - n1
N.Total.plots_10_15_02 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_15_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_15_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_15_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_15_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_15_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_15_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_15_02 = mean(alldata$m)
median_m_10_15_02 = median(alldata$m)
max_m_10_15_02 = max(alldata$m)
min_m_10_15_02 = min(alldata$m)
mean_uniq_m_10_15_02 = mean(unique(alldata$m))
median_uniq_m_10_15_02 = median(unique(alldata$m))
max_uniq_m_10_15_02 = max(unique(alldata$m))
min_uniq_m_10_15_02 = min(unique(alldata$m))

# n1=5, popvar=20 --------------------------------------------------------------#
tseed1 <- 7
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 5
popvar <- 20
popdata20 <- popdata %>% filter(n.networks == popvar)

# sim 1
seed_05_20_01 <- sim_seeds[1]
set.seed(seed_05_20_01)
alldata <- createACS(popdata=popdata20, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_20_01 <- dim(alldata)[1] - n1
N.Total.plots_05_20_01 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_20_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_20_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_20_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_20_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_20_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_20_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_20_01 = mean(alldata$m)
median_m_05_20_01 = median(alldata$m)
max_m_05_20_01 = max(alldata$m)
min_m_05_20_01 = min(alldata$m)
mean_uniq_m_05_20_01 = mean(unique(alldata$m))
median_uniq_m_05_20_01 = median(unique(alldata$m))
max_uniq_m_05_20_01 = max(unique(alldata$m))
min_uniq_m_05_20_01 = min(unique(alldata$m))

# sim 2
seed_05_20_02 <- sim_seeds[2]
set.seed(seed_05_20_02)
alldata <- createACS(popdata=popdata20, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_05_20_02 <- dim(alldata)[1] - n1
N.Total.plots_05_20_02 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_05_20_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_05_20_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_05_20_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_05_20_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_05_20_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_05_20_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_05_20_02 = mean(alldata$m)
median_m_05_20_02 = median(alldata$m)
max_m_05_20_02 = max(alldata$m)
min_m_05_20_02 = min(alldata$m)
mean_uniq_m_05_20_02 = mean(unique(alldata$m))
median_uniq_m_05_20_02 = median(unique(alldata$m))
max_uniq_m_05_20_02 = max(unique(alldata$m))
min_uniq_m_05_20_02 = min(unique(alldata$m))
# n1=10, popvar=20 --------------------------------------------------------------#
tseed1 <- 8
set.seed(tseed1)
sim_seeds <- runif(sims)

n1 <- 10
popvar <- 20

# sim 1
seed_10_20_01 <- sim_seeds[1]
set.seed(seed_10_20_01)
alldata <- createACS(popdata=popdata20, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_20_01 <- dim(alldata)[1] - n1 
N.Total.plots_10_20_01 <- dim(alldata)[1]

alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_20_01 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_20_01 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_20_01 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_20_01 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_20_01 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_20_01 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_20_01 = mean(alldata$m)
median_m_10_20_01 = median(alldata$m)
max_m_10_20_01 = max(alldata$m)
min_m_10_20_01 = min(alldata$m)
mean_uniq_m_10_20_01 = mean(unique(alldata$m))
median_uniq_m_10_20_01 = median(unique(alldata$m))
max_uniq_m_10_20_01 = max(unique(alldata$m))
min_uniq_m_10_20_01 = min(unique(alldata$m))

# sim 2
seed_10_20_02 <- sim_seeds[2]
set.seed(seed_10_20_02)
alldata <- createACS(popdata=popdata20, seed=seed, n1=n1, yvar=yvar)
N.ACS.plots_10_20_02 <- dim(alldata)[1] - n1
N.Total.plots_10_20_02 <- dim(alldata)[1]


alldata_noedge <- alldata %>%
   filter(Sampling!="Edge")
O_smd <- alldata %>% 
   select(Cactus, Stricta, Pusilla, NetworkID, m) %>%
   filter(!(is.na(NetworkID))) %>%
   group_by(NetworkID) %>%
   filter(row_number()==1) %>%
   ungroup()

Stricta_yHT_10_20_02 <- y_HT(y=alldata_noedge$Stricta, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Pusilla_yHT_10_20_02 <- y_HT(y=alldata_noedge$Pusilla, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Cactus_yHT_10_20_02 <- y_HT(y=alldata_noedge$Cactus, N = 900, n1 = n1, m_vec = alldata_noedge$m)
Stricta_var_yHT_10_20_02 <- var_y_HT(O_smd$Stricta, N = 900, n1=n1, m_vec=O_smd$m)
Pusilla_var_yHT_10_20_02 <- var_y_HT(O_smd$Pusilla, N = 900, n1=n1, m_vec=O_smd$m)
Cactus_var_yHT_10_20_02 <- var_y_HT(O_smd$Cactus, N = 900, n1=n1, m_vec=O_smd$m)

mean_m_10_20_02 = mean(alldata$m)
median_m_10_20_02 = median(alldata$m)
max_m_10_20_02 = max(alldata$m)
min_m_10_20_02 = min(alldata$m)
mean_uniq_m_10_20_02 = mean(unique(alldata$m))
median_uniq_m_10_20_02 = median(unique(alldata$m))
max_uniq_m_10_20_02 = max(unique(alldata$m))
min_uniq_m_10_20_02 = min(unique(alldata$m))





seed <- c(
   seed_05_05_01, seed_05_05_02, 
   seed_10_05_01, seed_10_05_02,
   
   seed_05_10_01, seed_05_10_02, 
   seed_10_10_01, seed_10_10_02,
   
   seed_05_15_01, seed_05_15_02, 
   seed_10_15_01, seed_10_15_02,
   
   seed_05_20_01, seed_05_20_02, 
   seed_10_20_01, seed_10_20_02
)

Stricta_yHT <- c(
   Stricta_yHT_05_05_01, Stricta_yHT_05_05_02,
   Stricta_yHT_10_05_01, Stricta_yHT_10_05_02,

   Stricta_yHT_05_10_01, Stricta_yHT_05_10_02,
   Stricta_yHT_10_10_01, Stricta_yHT_10_10_02,

   Stricta_yHT_05_15_01, Stricta_yHT_05_15_02,
   Stricta_yHT_10_15_01, Stricta_yHT_10_15_02,

   Stricta_yHT_05_20_01, Stricta_yHT_05_20_02,
   Stricta_yHT_10_20_01, Stricta_yHT_10_20_02
)

Pusilla_yHT <- c(
   Pusilla_yHT_05_05_01, Pusilla_yHT_05_05_02,
   Pusilla_yHT_10_05_01, Pusilla_yHT_10_05_02,

   Pusilla_yHT_05_10_01, Pusilla_yHT_05_05_02,
   Pusilla_yHT_10_10_01, Pusilla_yHT_10_05_02,

   Pusilla_yHT_05_15_01, Pusilla_yHT_05_15_02,
   Pusilla_yHT_10_15_01, Pusilla_yHT_10_15_02,

   Pusilla_yHT_05_20_01, Pusilla_yHT_05_20_02,
   Pusilla_yHT_10_20_01, Pusilla_yHT_10_20_02
)

Cactus_yHT <- c(
   Cactus_yHT_05_05_01, Cactus_yHT_05_05_02,
   Cactus_yHT_10_05_01, Cactus_yHT_10_05_02,

   Cactus_yHT_05_10_01, Cactus_yHT_05_10_02,
   Cactus_yHT_10_10_01, Cactus_yHT_10_10_02,

   Cactus_yHT_05_15_01, Cactus_yHT_05_15_02,
   Cactus_yHT_10_15_01, Cactus_yHT_10_15_02,

   Cactus_yHT_05_20_01, Cactus_yHT_05_20_02,
   Cactus_yHT_10_20_01, Cactus_yHT_10_20_02
)

Stricta_var_yHT <- c(
   Stricta_var_yHT_05_05_01, Stricta_var_yHT_05_05_02,
   Stricta_var_yHT_10_05_01, Stricta_var_yHT_10_05_02,

   Stricta_var_yHT_05_10_01, Stricta_var_yHT_05_10_02,
   Stricta_var_yHT_10_10_01, Stricta_var_yHT_10_10_02,

   Stricta_var_yHT_05_15_01, Stricta_var_yHT_05_15_02,
   Stricta_var_yHT_10_15_01, Stricta_var_yHT_10_15_02,

   Stricta_var_yHT_05_20_01, Stricta_var_yHT_05_20_02,
   Stricta_var_yHT_10_20_01, Stricta_var_yHT_10_20_02
)


Pusilla_var_yHT <- c(
   Pusilla_var_yHT_05_05_01, Pusilla_var_yHT_05_05_02,
   Pusilla_var_yHT_10_05_01, Pusilla_var_yHT_10_05_02,

   Pusilla_var_yHT_05_10_01, Pusilla_var_yHT_05_10_02,
   Pusilla_var_yHT_10_10_01, Pusilla_var_yHT_10_10_02,

   Pusilla_var_yHT_05_15_01, Pusilla_var_yHT_05_15_02,
   Pusilla_var_yHT_10_15_01, Pusilla_var_yHT_10_15_02,

   Pusilla_var_yHT_05_20_01, Pusilla_var_yHT_05_20_02,
   Pusilla_var_yHT_10_20_01, Pusilla_var_yHT_10_20_02
)

Cactus_var_yHT <- c(
   Cactus_var_yHT_05_05_01, Cactus_var_yHT_05_05_02,
   Cactus_var_yHT_10_05_01, Cactus_var_yHT_10_05_02,

   Cactus_var_yHT_05_10_01, Cactus_var_yHT_05_10_02,
   Cactus_var_yHT_10_10_01, Cactus_var_yHT_10_10_02,

   Cactus_var_yHT_05_15_01, Cactus_var_yHT_05_15_02,
   Cactus_var_yHT_10_15_01, Cactus_var_yHT_10_15_02,

   Cactus_var_yHT_05_20_01, Cactus_var_yHT_05_20_02,
   Cactus_var_yHT_10_20_01, Cactus_var_yHT_10_20_02
)

mean_m <- c(
   mean_m_05_05_01, mean_m_05_05_02,
   mean_m_10_05_01, mean_m_10_05_02,

   mean_m_05_10_01, mean_m_05_10_02,
   mean_m_10_10_01, mean_m_10_10_02,

   mean_m_05_15_01, mean_m_05_15_02,
   mean_m_10_15_01, mean_m_10_15_02,

   mean_m_05_20_01, mean_m_05_20_02,
   mean_m_10_20_01, mean_m_10_20_02
)

median_m <- c(
   median_m_05_05_01, median_m_05_05_02,
   median_m_10_05_01, median_m_10_05_02,

   median_m_05_10_01, median_m_05_10_02,
   median_m_10_10_01, median_m_10_10_02,

   median_m_05_15_01, median_m_05_15_02,
   median_m_10_15_01, median_m_10_15_02,

   median_m_05_20_01, median_m_05_20_02,
   median_m_10_20_01, median_m_10_20_02
)

max_m <- c(
   max_m_05_05_01, max_m_05_05_02,
   max_m_10_05_01, max_m_10_05_02,

   max_m_05_10_01, max_m_05_10_02,
   max_m_10_10_01, max_m_10_10_02,

   max_m_05_15_01, max_m_05_15_02,
   max_m_10_15_01, max_m_10_15_02,

   max_m_05_20_01, max_m_05_20_02,
   max_m_10_20_01, max_m_10_20_02
)

min_m <- c(
   min_m_05_05_01, min_m_05_05_02,
   min_m_10_05_01, min_m_10_05_02,

   min_m_05_10_01, min_m_05_10_02,
   min_m_10_10_01, min_m_10_10_02,

   min_m_05_15_01, min_m_05_15_02,
   min_m_10_15_01, min_m_10_15_02,

   min_m_05_20_01, min_m_05_20_02,
   min_m_10_20_01, min_m_10_20_02
)

mean_uniq_m <- c(
   mean_uniq_m_05_05_01, mean_uniq_m_05_05_02,
   mean_uniq_m_10_05_01, mean_uniq_m_10_05_02,

   mean_uniq_m_05_10_01, mean_uniq_m_05_10_02,
   mean_uniq_m_10_10_01, mean_uniq_m_10_10_02,

   mean_uniq_m_05_15_01, mean_uniq_m_05_15_02,
   mean_uniq_m_10_15_01, mean_uniq_m_10_15_02,

   mean_uniq_m_05_20_01, mean_uniq_m_05_20_02,
   mean_uniq_m_10_20_01, mean_uniq_m_10_20_02
)

median_uniq_m <- c(
   median_uniq_m_05_05_01, median_uniq_m_05_05_02,
   median_uniq_m_10_05_01, median_uniq_m_10_05_02,

   median_uniq_m_05_10_01, median_uniq_m_05_10_02,
   median_uniq_m_10_10_01, median_uniq_m_10_10_02,

   median_uniq_m_05_15_01, median_uniq_m_05_15_02,
   median_uniq_m_10_15_01, median_uniq_m_10_15_02,

   median_uniq_m_05_20_01, median_uniq_m_05_20_02,
   median_uniq_m_10_20_01, median_uniq_m_10_20_02
)

max_uniq_m <- c(
   max_uniq_m_05_05_01, max_uniq_m_05_05_02,
   max_uniq_m_10_05_01, max_uniq_m_10_05_02,
   
   max_uniq_m_05_10_01, max_uniq_m_05_10_02,
   max_uniq_m_10_10_01, max_uniq_m_10_10_02,
   
   max_uniq_m_05_15_01, max_uniq_m_05_15_02,
   max_uniq_m_10_15_01, max_uniq_m_10_15_02,
   
   max_uniq_m_05_20_01, max_uniq_m_05_20_02,
   max_uniq_m_10_20_01, max_uniq_m_10_20_02
)

min_uniq_m <- c(
   min_uniq_m_05_05_01, min_uniq_m_05_05_02,
   min_uniq_m_10_05_01, min_uniq_m_10_05_02,
   
   min_uniq_m_05_10_01, min_uniq_m_05_10_02,
   min_uniq_m_10_10_01, min_uniq_m_10_10_02,

   min_uniq_m_05_15_01, min_uniq_m_05_15_02,
   min_uniq_m_10_15_01, min_uniq_m_10_15_02,

   min_uniq_m_05_20_01, min_uniq_m_05_20_02,
   min_uniq_m_10_20_01, min_uniq_m_10_20_02
)

N.ACS.plots <- c(
   N.ACS.plots_05_05_01, N.ACS.plots_05_05_02,
   N.ACS.plots_10_05_01, N.ACS.plots_10_05_02,
   
   N.ACS.plots_05_10_01, N.ACS.plots_05_10_02,
   N.ACS.plots_10_10_01, N.ACS.plots_10_10_02,
   
   N.ACS.plots_05_15_01, N.ACS.plots_05_15_02,
   N.ACS.plots_10_15_01, N.ACS.plots_10_15_02,
   
   N.ACS.plots_05_20_01, N.ACS.plots_05_20_02,
   N.ACS.plots_10_20_01, N.ACS.plots_10_20_02
)

N.Total.plots <- c(
   N.Total.plots_05_05_01, N.Total.plots_05_05_02,
   N.Total.plots_10_05_01, N.Total.plots_10_05_02,
   
   N.Total.plots_05_10_01, N.Total.plots_05_10_02,
   N.Total.plots_10_10_01, N.Total.plots_10_10_02,
   
   N.Total.plots_05_15_01, N.Total.plots_05_15_02,
   N.Total.plots_10_15_01, N.Total.plots_10_15_02,
   
   N.Total.plots_05_20_01, N.Total.plots_05_20_02,
   N.Total.plots_10_20_01, N.Total.plots_10_20_02
)


simulation_data %<>%
   dplyr::select(
      -c(JoinCountTest.S, MoranI.S, SimDate)
   )
simulation_data %<>%
   rename(
      SamplingDesign = SmplngDsgn
   )
simulation_data %<>%
   rename(
      MoransIWeightMatrix = MrnsIWghtMtrx
   )

simulation_data %<>%
   dplyr::select(
      -c(mean_m,
         median_m,
         max_m,
         min_m,
         mean_uniq_m,
         median_uniq_m,
         max_uniq_m,
         min_uniq_m
         #JoinCountTest.S,
      )
   )


manually_calculated <- data.frame(
   Stricta_yHT,
   Pusilla_yHT,
   Cactus_yHT,
   Stricta_var_yHT,
   Pusilla_var_yHT,
   Cactus_var_yHT,
   Plots = "Horvitz Thompson Mean (All Plots)",
   simulation = rep(1:2, 2),
   seed,
   N.ACS.plots,
   N.Total.plots,
   realvar = 1,
   popvar = c(rep(5,4), rep(10, 4), rep(15, 4), rep(20, 4)),
   N.SRSWOR.plots = rep(c(rep(5, 2), rep(10, 2)), 4),
   #mean_m,
   #median_m,
   #max_m,
   #min_m,
   #mean_uniq_m,
   #median_uniq_m,
   #max_uniq_m,
   #min_uniq_m,
   #JoinCountTest.S,
   #MoranI.S,
   f_max = 2,
   nSims = 2,
   y_HT_formula = "y_HT",
   SamplingDesign = "ACS",
   MoransIWeightMatrix = weights
)

test_that("test sampleRealizations", {
expect_equal(
   simulation_data,
   manually_calculated
)
})
#' sims=200
#' n1_vec=c(75,150,225,300,350)
#' simulation_data_SRSWOR <- sampleRealizations(
#'    popdata = popdata,
#'    sims = sims,
#'    n1_vec = n1_vec,
#'    avar = avar,
#'    ovar = ovar,
#'    popvar="Island"
#' )
#' 
#' test_that("test sampleRealizations", {
#'    expect_error(
#'       sampleRealizations(
#'          popdata = popdata,
#'          sims = sims,
#'          n1_vec = 1,
#'          avar = avar,
#'          ovar = ovar,
#'          popvar = "Island",
#'          yvar = "Cactus",
#'          realvar = 5
#'       ),
#'       "The argument realvar must be a character string."
#'    )
#' 
#' })
# # data(Thompson1990Fig1Pop)
# # alldata_all <- createACS(Thompson1990Fig1Pop, 20, "y_value", seed=24)
# #
# # temp <- alldata_all %>%
# #         as.data.frame %>%
# #         # get rid of edge units - not involved in calculation of m
# #         filter(!(is.na(NetworkID))) %>%
# #         arrange(x, y)
# #
# # # dnearneigh - why was this here?
# #
# # nb <- cell2nb(
# #         nrow = max(temp$x) - min(temp$x) + 1,
# #         ncol = max(temp$y) - min(temp$y) + 1
# # )
# # coordinates(temp) = ~ x+y
# # data_dist <- dim(as.matrix(dist(cbind(temp$x, temp$y))))[1]
# # tempdat <- data.frame(JoinCountTest.W = NA)
# # for (i in length(weights)) {
# #         lwb <- nb2listw(nb, style = weights[i]) # convert to weights
# #         # I think cells are indexed by row, then column
# #         tempdat$JoinCountTest <- getJoinCountTestEst(temp, lwb)
# #         tempdat$MoranI <- getMoranTestEst(temp, lwb)
# #         colnames(tempdat)[which(names(tempdat) == "JoinCountTest")] <-
# #                 paste("JoinCountTest", weights[i], sep=".")
# #         colnames(tempdat)[which(names(tempdat) == "MoranI")] <-
# #                 paste("MoranI", weights[i], sep=".")
# # }
# # return(tempdat)