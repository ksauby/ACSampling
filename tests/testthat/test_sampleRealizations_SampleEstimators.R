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
   popdata=popdata,
   sims=sims,
   n1_vec=n1_vec,
   avar=avar,
   ovar=ovar,
   rvar=rvar,
   #ACS=TRUE,
   SamplingDesign=SamplingDesign,
   yvar=yvar,
   y_HT_formula=y_HT_formula,
   var_formula=var_formula,
   mThreshold=mThreshold,
   f_max=f_max,
   SampleEstimators=SampleEstimators,
   SpatStat=SpatStat,
   mChar=mChar,
   popvar=popvar,
   realvar=realvar,
   weights=weights,
   seeds=seeds
)
