rm(list = ls()) # clear working space

# Let's use the package GRF - Generalized Random Forest
library(grf)
library(tidyverse)
library(cowplot)
library(tictoc)

# packages for parallel computing
# parellel https://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/
# Best explain https://blog.aicry.com/r-parallel-computing-in-5-minutes/index.html
library(foreach)
library(doParallel)


setwd(paste0("SET PATH"))

wd1 <- (paste0("SET PATH"))
wd2 <- (paste0("SET PATH"))

# My favourite explanation is here: https://www.markhw.com/blog/causalforestintro
# Upload data of the RCT
dfbase <- read.table(paste0(wd1,"A3_crime_t3_hete_ML.csv"),header=T,sep=",")

 ###################################################################################
 ## 1. Heterogeneous effects on lictec2
 ###################################################################################
 ss<- 165158 # first time ive run the file hhmmss
  set.seed(ss)

df = dfbase
dfbase = NULL
#cases <- sample(seq_len(nrow(dfbase)), round(nrow(dfbase) * .1)) # random sample
#df <- df[cases, ]
#df = dfbase[1:100000,]

ntrees = 1000 # 100
loops = 10 # 60
nodesize = 5000

## creating cluste
n_cores <- 10 # detectCores()*0.8
cl <- makeCluster(n_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(grf)
  library(tidyverse)
  library(cowplot)
})

# run causal forest in parallel
tic()
cf_parallel = NULL
cf_parallel <- foreach(i=1:loops)  %dopar% { # %dopar% .combine = data_frame, .packages='grf'
      causal_forest(
      X = model.matrix(~ ., data = select(df, agecont, tencont, schcont, inccont, homcont, informcont, grtcnaecont, grtocupcont, pibpccont, popcont, ginicont) ),
      Y = df$psuit,
      W = df$treat,
      num.trees = ntrees, ## michela uses 100k - 6 hours to run 500 trees - command default is 2000
      clusters= df$cpf,
      num.threads = 8,
      min.node.size = nodesize,
      honesty.fraction = .5, ## .5 default
      seed = ss
    )
}
toc()  

# stop clusters
stopCluster(cl)

tic()
cf = merge_forests(cf_parallel)
cf_parallel = NULL
toc()


  # I then predict thetestata, tellÂrfÂo include variance estimates, and then I assign the predictions (i.e., the estimated treatment effects) to theÂtestÂdata frame so that we can use these in subsequent analyses:
  preds <- predict(
    object = cf, 
    newdata = NULL, 
    estimate.variance = TRUE
  )
  
  df$preds <- preds$predictions
  df$sqrt <-  sqrt(preds$variance.estimates)
  df$excesserror <-  preds$excess.error
  df$debiasederror <-  preds$debiased.error
  df$y_hat <- cf$Y.hat
  df$w_hat <- cf$W.hat
  
  
  # Variable Importance
  cf %>% 
    variable_importance() %>% 
    as.data.frame() %>% 
    mutate(variable = colnames(cf$X.orig)) %>% 
    arrange(desc(V1))

### Estimate E[Y|W=0,X] ###

## creating cluste
n_cores <- 10 # detectCores()*0.8
cl <- makeCluster(n_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(grf)
  library(tidyverse)
  library(cowplot)
})

  
# Delta_control - E[Delta_Y|treat=0,X]
dfadd = df %>% filter(treat==0)

tic()
# Let's run the causal forest now
cf_y_parallel = NULL
cf_y_parallel <- foreach(i=1:loops)  %dopar% { # %dopar% .combine = data_frame, .packages='grf'
    regression_forest(
    X = model.matrix(~ ., data = select(dfadd, agecont, tencont, schcont, inccont, homcont, informcont, grtcnaecont, grtocupcont, pibpccont, popcont, ginicont) ),
    Y = dfadd$psuit, # psuit is the delta
    num.trees = ntrees, ## michela uses 100k - 6 hours to run 500 trees - command default is 2000
    clusters= dfadd$cpf,
    num.threads = 8,
    min.node.size = nodesize,
    honesty.fraction = .5, ## .5 default
    seed = ss
  )
}
toc()

# stop clusters
stopCluster(cl)

tic()
cf_y = merge_forests(cf_y_parallel)
cf_y_parallel = NULL
toc()

df$delta_control <- NULL
df$delta_control <- predict(cf_y)$predictions
df %>% summarize(mean(delta_control, na.rm = TRUE))

# Y_pretreat - E[Y|post=0,treat=1,X]

## creating cluste
n_cores <- 10 # detectCores()*0.8
cl <- makeCluster(n_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(grf)
  library(tidyverse)
  library(cowplot)
})


dfadd = df %>% filter(treat==1)

tic()
# Let's run the causal forest now
cf_y_parallel = NULL
cf_y_parallel <- foreach(i=1:loops)  %dopar% { # %dopar% .combine = data_frame, .packages='grf'
    regression_forest(
    X = model.matrix(~ ., data = select(dfadd, agecont, tencont, schcont, inccont, homcont, informcont, grtcnaecont, grtocupcont, pibpccont, popcont, ginicont) ),
    Y = dfadd$psuitpre,
    num.trees = ntrees, ## michela uses 100k - 6 hours to run 500 trees - command default is 2000
    clusters= dfadd$cpf,
    num.threads = 8,
    min.node.size = nodesize,
    honesty.fraction = .5, ## .5 default
    seed = ss
  )
}
toc()

# stop clusters
stopCluster(cl)

tic()
cf_y = merge_forests(cf_y_parallel)
cf_y_parallel = NULL
toc()


df$y_pretreat <- NULL
df$y_pretreat <- predict(cf_y)$predictions
df %>% summarize(mean(y_pretreat, na.rm = TRUE))


# export data
library(foreign)
write.dta(df, "A3_crime_t3_hete_ML_output_parallel.dta") 
