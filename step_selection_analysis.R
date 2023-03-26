#####################   Load Packages   ################

library(raster)
library(amt)
library(sp)
library(ggplot2)
library(glmmTMB)
library(ggeffects)




#####################    Prep Data    ###################

# load gps data
load("gps_ssf.rdata")

# load raster with covariates
raster <- stack("raster.grd")

# organize data for SSF 
ssf_data <- function(data){
  
  # data frame to spatial points data frame
  coordinates(data)<-c("x", "y")
  proj4string(data)<-CRS("+proj=longlat +datum=WGS84")
  data <- spTransform(data, crs(raster))
  
  # group points into tracks and then bursts with 15 min interlocation periods
  coyIDs <- unique(data$ID)
  
  data1 <- lapply(1:length(coyIDs), function(c) {
    ID <- coyIDs[c]
    df <- data[data$ID == ID,]
    tr <- make_track(df, x, y, CHI_TIME)
    tr15 <- track_resample(tr, rate = minutes(15), tolerance = minutes(3))
    mb <- filter_min_n_burst(tr15, min_n = 3)
    mb$id <- ID
    return(mb)
  })
  
  # generate real and random steps
  data2 <- do.call(rbind, data1)
  data2 <- steps_by_burst(data2, keep_cols = "start")
  data2 <- random_steps(data2)
  
  # extract covariates
  data3 <- extract_covariates(data2, raster)
  
  # scale covariates
  data3[c("propWST", "propLST", "propBST", "medIncST", "propAST", "popDensST", 
          "distST", "agST", "natST")] <- 
    lapply(data3[c("propW", "propL", "propB", "medInc", "propA", "popDens", 
                   "dist", "ag", "nat")], scale)
  
  return(data3)
}

data <- ssf_data(gps_ssf)
data$caseN <- ifelse(data$case_ == TRUE, 1, 0)




###################    Step Selection Analysis   #################

ssf_fit <-  fit_clogit(caseN ~ 
                     medIncST + propWST + distST + natST + agST + popDensST + 
                     medIncST:popDensST + propWST:popDensST + distST:popDensST + 
                      natST:popDensST + agST:popDensST + strata(step_id_), 
                     data = data)
                   
summary(ssf_fit)

save(data, file = "ssf_data.rdata")
save(ssf_fit, file = "ssf_fit.rdata")


