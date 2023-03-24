##############   Load Packages   ################

library(amt)
library(lubridate)
library(dplyr)
library(momentuHMM)
library(parallel)




#############   Subset Hours to Periods of High Human Activity   ############

load("gpsDataFinal.rdata")
gps_data <- subset(gpsDataFinal, select = c("COY_ID", "LONGITUDE", "LATITUDE", 
                                           "CHI_TIME"))

# convert UTC to CT
gps_data$date_timeCT <- format(gps_data$CHI_TIME, tz = "America/Chicago", 
                             usetz = TRUE)

# subset between 6 and 21 
gps_sub <- with(gps_data, gps_data[lubridate::hour(date_timeCT) > 6 & 
                                      lubridate::hour(date_timeCT) < 21, ])
gps_sub$date_timeCT <- NULL




###############   Convert GPS Data to Track   ##################

# create tracks
gps_track <- gps_sub %>% nest(data = -COY_ID) %>%
  mutate(trk = map(data, function(i) {
    make_track(i, LONGITUDE, LATITUDE, CHI_TIME, crs = "+init=epsg:26971")})) %>%
  unnest(cols = COY_ID)

# resample to 15 mins and bursts of at least 3 locations
trk_re <- do.call(rbind, lapply(1:nrow(gps_track), function(i){
  re <- amt::track_resample(gps_track$trk[[i]], rate = minutes(15), 
                            tolerance = minutes(3))
  re$ID<- gps_track$COY_ID[[i]]
  filter_min_n_burst(re, min_n = 3)
})) %>% as.data.frame()




############   Identify Periods of Movement and Rest   ##############

gps_hmm <- prepData(data = trk_re, type = "LL", coordNames = c("x_", "y_"))

plot(gps_hmm) #plot tracks, turning angles and step lengths 

# fix erroneous steps by removing steps with a speed over 2.25 km/15 min or 9 km/hr 
## the threshold Ellington et al. 2019 used
long_steps <- gps_hmm[gps_hmm$step > 2.25,] 
gps_hmm <- gps_hmm[gps_hmm$step < 2.25,]

# estimate priors (mu and sd) based on histogram of sl and angles
hist(gps_hmm$step, breaks = 10000) 
hist(gps_hmm$angle) 

# lots of zero step lengths so use a zero-inflated step length distribution 
## which assumes that there is a probability z of observing a 0 and a probability 
## of 1 − z of observing a positive value distributed according to a standard 
## positive distribution (e.g. a gamma distribution)

# find starting zero-mass value by finding the proportion of 0 step lengths 
whichzero <- which(gps_hmm$step == 0)
length(whichzero)/nrow(gps_hmm) #0.006

# (mu_1, mu_2, sd_1, sd_2, zero-mass_1, zero-mass_2)
stepPar0 <- c(0.1, 2, 0.1, 2, 0.006, 0.006) 

# initial angle distribution natural scale parameters: the von Mises distribution 
## has 2 parameters, the mean turning angle and the concentration, mean turning 
## angle is between -3.14 and 3.14 and the concentration is a positive number that 
## is inversely related to variance
## coyote turning angles are not really centered around 0 so give them a low concentration
anglePar0 <- c(pi, 0, .2, 1) 

# fit model
hmm_fit <- fitHMM(data = gps_hmm, nbStates = 2, 
              dist = list(step = "gamma", angle = "vm"),
              Par0 = list(step = stepPar0, angle = anglePar0),
              estAngleMean = list(angle=TRUE),
              formula = ~ 1)

hmm_fit

# check model fit
residuals <- pseudoRes(hmm_fit)
step_residuals <- residuals[1]$stepRes
hist(step_residuals)  # if the model is a good fit pseudo-residuals should be normally distributed

angle_residuals <- as.numeric(residuals[2]$angleRes)
hist(angle_residuals)

plotPR(hmm_fit) # qq-plots of pseudo residuals


# issues with fit suggest we need to adjust priors
stepPar0_fix <- c(0.01, .4, 0.1, 2, 0.012, 0.002) 
anglePar0_fix <- c(3, 0, 1, 1) 

hmm_fit_fix <- fitHMM(data = gps_hmm, nbStates = 2, 
                  dist = list(step = "gamma", angle = "vm"),
                  Par0 = list(step = stepPar0_fix, angle = anglePar0_fix),
                  estAngleMean = list(angle=TRUE),
                  formula = ~ 1,
                  optMethod = "Nelder-Mead")

hmm_fit_fix
plotPR(hmm_fit_fix)

# run model with priors pulled randomly from distributions of plausible parameters
# speed this process up with parallel computing

# create cluster of size ncores
ncores <- detectCores() - 1
cl <- makeCluster(getOption("cl.cores", ncores))

# export objects needed in parallelised function to cluster
clusterExport(cl, list("gps_hmm", "fitHMM"))

# number of tries with different starting values
niter <- 100

# Save list of fitted models
allm <- list()
allPar0 <- lapply(as.list(1:niter), function(x) {
  # Step length mean
  stepMean0 <- c(runif(1, 0.0001, 0.1),
                 runif(1, 0.1, 1))
  # Step length standard deviation
  stepSD0 <- c(runif(1, 0.001, 0.1),
                   runif(1, 0.1, 1))
  # Zero-inflation param
  zero_inf <- c(runif(1, 0.001, 0.01),
                runif(1, 0, 0.0005))
  # Turning angle mean
  angleMean0 <- c(runif(1, 0, 3),
                  runif(1, 0, 1))
  # Turning angle concentration
  angleCon0 <- c(runif(1, 0.1, 1),
                 runif(1, 0.5, 3))
  
  # Fit model
  stepPar0 <- c(stepMean0, stepSD0, zero_inf)
  anglePar0 <- c(angleMean0, angleCon0)
  
  # Optimizer
  opt <- sample(c("nlm", "Nelder-Mead"), size = 1, replace = "TRUE")
  
  return(list(step = c(stepMean0, stepSD0, zero_inf), 
              angle = c(angleMean0, angleCon0), opt = opt))
})


allm <- parLapply(cl = cl, X = allPar0, fun = function(par0) {
  m <- fitHMM(data = gps_hmm, 
              nbStates = 2, 
              dist = list(step = "gamma", angle = "vm"),
              Par0 = list(step = par0$step, angle = par0$angle),
              estAngleMean = list(angle=TRUE),
              optMethod = par0$opt)
  return(m)
})

# find best fitting model
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))
best_fit <- allm[[which.min(allnllk)]]
plotPR(best_fit)

# confidence intervals
ci(best_fit)
plot(m, plotCI=TRUE)

# classify movement data into states using the viterbi function which 
## finds the most likely sequence of states to have generated the observation based 
## on the fitted model

states <- viterbi(best_fit)




