##Library the Package
library(readxl)
library(INLA)
library(tidyverse)
library(evd)
library(extRemes)
library(lwgeom)
library(raster)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(sp)
library(lattice)     
library(gridExtra)
library(lwgeom)
library(raster)
library(sf)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(sf)
library(patchwork)
library(ggsci)
library(ROOPSD)

## Read the Data
d <- read.csv('airdata.csv')

#data standarlization
standardize <- function(x) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  z <- (x - mean_x) / sd_x
  return(z)
}

d$`population`=standardize(d$`population`)
d$precipitation=standardize(d$precipitation)
d$surface_pressure=standardize(d$surface_pressure)
d$wind=standardize(d$wind)
d$`GDP`=standardize(d$`GDP`)
d$fire_burnt_area=standardize(d$fire_burnt_area)
d$temperature=standardize(d$temperature)

#generate the extreme drought indicator
d$drought <-ifelse(d$drought>2,1,0) 


## construct indices
d$id <- 1:dim(d)[1]
d$id.year <- d$year-2016

##preparation for full prediction
#Standardization the full prediction data
d_full_pre<-

d_full_pre$`population`=standardize(d_full_pre$`population`)
d_full_pre$precipitation=standardize(d_full_pre$precipitation)
d_full_pre$surface_pressure=standardize(d_full_pre$surface_pressure)
d_full_pre$wind=standardize(d_full_pre$wind)
d_full_pre$`GDP`=standardize(d_full_pre$`GDP`)
d_full_pre$fire_burnt_area=standardize(d_full_pre$fire_burnt_area)
d_full_pre$temperature = standardize(d_full_pre$temperature)

#generate extreme drought indicator
d_full_pre$ed <-ifelse(d$drought>2,1,0)

##Create ozone pm column.
d_full_pre$log_oz_max <- rep(NA,dim(d_full_pre)[2])
d_full_pre$log_pm_mean <- rep(NA,dim(d_full_pre)[2])

##extract data from the fuull prediction dataset. We choose the 2021 as the full prediction year.
d2021 <- subset(d_full_pre,d_full_pre$year == 2021)
d2021$id.year <- rep(5,length(d2021$year)) 


##Dealing with response variable 
# Define the quantile of each response variable
d$o_max <- d$o_max*1000 # transfer from unit of ppm to ppb
d$log_oz_max <- log(d$o_max)
d$log_pm_mean <- log(d$pm_mean)

##Constructing mesh
coor <- data.frame(long = d[, "longitude"], lat = d[, "latitude"])
max.edge <- diff(range(d$long))/15 ## max.edge between 1/3 and 1/10 of spatial range
bound.outer <- diff(range(d$lat))/3 ## 1/3 of spatial range
bdy <- inla.nonconvex.hull(as.matrix(coor), convex=0.8, concave = -0.1)
mesh <- inla.mesh.2d(loc = coor, boundary = bdy,
                     max.edge = c(0.8,6)*max.edge, 
                     cutoff = max.edge/5, 
                     offset = c(max.edge, bound.outer))

jpeg("mesh.jpeg", quality = 100, units = "in", width = 6, height = 6, res = 300)

plot(mesh); points(coor,col = "red")

dev.off()

coordinate <- coordinates(SpatialPoints(coor))

##Index for training sets
index_oz <- which(d$year < 2021)
index_pm <- which(d$year < 2021)

##Index for validation sets
index_val_oz <- which(d$year == 2021) 
index_val_pm <- which(d$year == 2021) 

##Projector matrix for training set
A1 <- inla.spde.make.A(mesh, coordinate[index_pm,], group = d$month[index_pm], n.group=12)
A2 <- inla.spde.make.A(mesh, coordinate[index_oz,], group = d$month[index_oz], n.group=12)

##Projector matrix for validation set
A_val1 <- inla.spde.make.A(mesh, coordinate[index_val_pm,], group = d$month[index_val_pm], n.group=12)
A_val2 <- inla.spde.make.A(mesh, coordinate[index_val_oz,], group = d$month[index_val_oz], n.group=12)

##Projector matrix for prediction set
Ap <- inla.spde.make.A(mesh, coordinate1, group = d2021$month, n.group = 12)


##Coordinate of all points
coor1 <- data.frame(long = d2021[, "longitude"], lat = d2021[, "latitude"])
coordinate1 <- coordinates(SpatialPoints(coor1))

#PC prior for spde
spde <- inla.spde2.pcmatern(mesh = mesh,
                            alpha = 2,
                            prior.range = c(10, 0.9), 
                            prior.sigma = c(0.5,0.1) 
)


##Create the spde index.
iset_pm<- inla.spde.make.index("s_pm",n.spde = spde$n.spde,
                               n.group = 12
)


iset_oz<- inla.spde.make.index("s_oz",n.spde = spde$n.spde,
                               n.group = 12
)

iset_pm2<- inla.spde.make.index("s_pm2",n.spde = spde$n.spde,
                                n.group = 12
)

iset_oz2<- inla.spde.make.index("s_oz2",n.spde = spde$n.spde,
                                n.group = 12
)


stack_pm_train <- inla.stack(data = list(y = cbind(as.vector(d$log_pm_mean[index_pm]), NA)),
                             A = list(A1, 1), 
                             effects = list(c(list(intercept=1), #the intercept
                                              iset_pm,iset_pm2), 
                                            data.frame(Intercept_pm = rep(1,length(d$log_pm_mean[index_pm])),
                                                       Precipitation_pm = d$precipitation[index_pm],
                                                       Surface_pressure_pm = d$surface_pressure[index_pm],
                                                       GDP_pm = d$`GDP`[index_pm],
                                                       Burnt_area_pm = d$fire_burnt_area[index_pm],
                                                       Pop_pm = d$`population`[index_pm],
                                                       Month_pm = d$month[index_pm],
                                                       Wind_pm = d$wind[index_pm],
                                                       Year_pm = d$id.year[index_pm],
                                                       Id_pm = d$id[index_pm],
                                                       Tmax_pm = d$temperature[index_pm],
                                                       Extreme_drought_pm = d$drought[index_pm]
                                                       
                                            )),
                             tag = "pm_mean.train") 


stack_oz_train <- inla.stack(data = list(y = cbind(NA,as.vector(d$log_oz_max[index_oz]))),
                             A = list(A2, 1),
                             effects = list(c(list(intercept=1), #the intercept
                                              iset_oz,iset_oz2),
                                            data.frame(Intercept_oz = rep(1,length(d$log_oz_max[index_oz])),
                                                       Precipitation_oz = d$precipitation[index_oz],
                                                       Surface_pressure_oz = d$surface_pressure[index_oz],
                                                       GDP_oz = d$`GDP`[index_oz],
                                                       Burnt_area_oz = d$fire_burnt_area[index_oz],
                                                       Pop_oz = d$`population`[index_oz],
                                                       Month_oz = d$month[index_oz],
                                                       Wind_oz = d$wind[index_oz],
                                                       Year_oz = d$id.year[index_oz],
                                                       Id_oz = d$id[index_oz],
                                                       Tmax_oz = d$temperature[index_oz],
                                                       Extreme_drought_oz = d$drought[index_oz]
                                            )),
                             tag = "Ozone.train")


stack_pm_val <- inla.stack(data = list(y = cbind(rep(NA, length(d$log_pm_mean[index_val_pm])),NA)),
                           A = list(A_val1, 1), 
                           effects = list(c(list(intercept=1), #the intercept
                                            iset_pm,iset_pm2), 
                                          data.frame(Intercept_pm = rep(1,length(d$log_pm_mean[index_val_pm])),
                                                     Precipitation_pm = d$precipitation[index_val_pm],
                                                     Surface_pressure_pm = d$surface_pressure[index_val_pm],
                                                     GDP_pm = d$`GDP`[index_val_pm],
                                                     Burnt_area_pm = d$fire_burnt_area[index_val_pm],
                                                     Pop_pm = d$`population`[index_val_pm],
                                                     Month_pm = d$month[index_val_pm],
                                                     Wind_pm = d$wind[index_val_pm],
                                                     Year_pm = d$id.year[index_val_pm],
                                                     Id_pm = d$id[index_val_pm],
                                                     Tmax_pm = d$temperature[index_val_pm],
                                                     Extreme_drought_pm = d$drought[index_val_pm]
                                          )),
                           tag = "pm_mean.val")


stack_oz_val <- inla.stack(data = list(y = cbind(NA,rep(NA, length(d$log_oz_max[index_val_oz])))),
                           A = list(A_val2, 1), 
                           effects = list(c(list(intercept=1), #the intercept
                                            iset_oz,iset_oz2), 
                                          data.frame(Intercept_oz = rep(1,length(d$log_oz_max[index_val_oz])),
                                                     Precipitation_oz = d$precipitation[index_val_oz],
                                                     Surface_pressure_oz = d$surface_pressure[index_val_oz],
                                                     GDP_oz = d$`GDP`[index_val_oz],
                                                     Burnt_area_oz = d$fire_burnt_area[index_val_oz],
                                                     Pop_oz = d$`population`[index_val_oz],
                                                     Month_oz = d$month[index_val_oz],
                                                     Wind_oz = d$wind[index_val_oz],
                                                     Year_oz = d$id.year[index_val_oz],
                                                     Id_oz = d$id[index_val_oz],
                                                     Tmax_oz = d$temperature[index_val_oz],
                                                     Extreme_drought_oz = d$drought[index_val_oz]
                                          )),
                           tag = "Ozone.val")


stack_pm_pred <- inla.stack(data = list(y = cbind(rep(NA, length(d2021$log_pm_mean)),NA)),
                            A = list(Ap, 1), 
                            effects = list(c(list(intercept=1), #the intercept
                                             iset_pm,iset_pm2), 
                                           data.frame(Intercept_pm = rep(1,length(d2021$log_pm_mean)),
                                                      Precipitation_pm = d2021$precipitation,
                                                      Surface_pressure_pm = d2021$surface_pressure,
                                                      GDP_pm = d2021$`GDP`,
                                                      Burnt_area_pm = d2021$fire_burnt_area,
                                                      Pop_pm = d2021$`population`,
                                                      Month_pm = d2021$month,
                                                      Wind_pm = d2021$wind,
                                                      Year_pm = d2021$id.year,
                                                      Tmax_pm = d2021$temperature,
                                                      Extreme_drought_pm = d2021$drought
                                           )),
                            tag = "pm_mean.pred")


stack_oz_pred <- inla.stack(data = list(y = cbind(NA,rep(NA, length(d2021$log_oz_max)))),
                            A = list(Ap, 1), 
                            effects = list(c(list(intercept=1), #the intercept
                                             iset_oz,iset_oz2), 
                                           data.frame(Intercept_oz = rep(1,length(d2021$log_oz_max)),
                                                      Precipitation_oz = d2021$precipitation,
                                                      Surface_pressure_oz = d2021$surface_pressure,
                                                      GDP_oz = d2021$`GDP`,
                                                      Burnt_area_oz = d2021$fire_burnt_area,
                                                      Pop_oz = d2021$`population`,
                                                      Month_oz = d2021$month,
                                                      Wind_oz = d2021$wind,
                                                      Year_oz = d2021$id.year,
                                                      Tmax_oz = d2021$temperature,
                                                      Extreme_drought_oz = d2021$drought
                                                      
                                           )),
                            tag = "Ozone.pred")

stack<-inla.stack.join(stack_pm_train,  stack_oz_train,stack_pm_val,stack_oz_val,stack_pm_pred,stack_oz_pred)

##Hyper setting
rprior <- list(theta = list(prior = "pccor1", param = c(0, 0.95)))

hyper1 <- list(theta = list(prior = 'loggamma', param = c(20,0.01))) 

hyper <- list(beta = list(prior = 'normal', param = c(1,10))) # default choice

hyper.gev = list(
  tail = list(initial = 0, fixed = TRUE),
  prec  = list(prior = "pcprec", param = c(5, 0.01))
) 


##Modelling M1
formula_m1 <- y ~ 0 + Intercept_pm + Intercept_oz +
  f(Id_pm, model = "iid", hyper = hyper1) + 
  f(Id_oz, model = "iid", hyper = hyper1)+
  Tmax_pm+Tmax_oz+
  Precipitation_pm+Precipitation_oz+
  Surface_pressure_pm+Surface_pressure_oz+
  Wind_pm+Wind_oz+
  Burnt_area_pm+Burnt_area_oz+
  Extreme_drought_pm + Extreme_drought_oz+
  GDP_pm+GDP_oz+
  Pop_pm+Pop_oz

model1 <- inla(formula_m1,
               family = c("gaussian", "gev"),  
               data = inla.stack.data(stack),
               control.family = list(list(), list(hyper = hyper.gev)),
               #                      list(beta.censor.value = 0.05), list()),
               control.predictor = list(A = inla.stack.A(stack), link = 1,
                                        compute = TRUE),
               #Ntrials=1,
               #Ntrials = c(length(d$max[ipos.max]),length(d$ba[ipos.ba])),
               
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
                                      config = T, return.marginals.predictor = T), 
               verbose = T)


##Modelling M2

formula_m2 <- y ~ 0 + Intercept_pm + Intercept_oz +
  f(s_pm, model=spde)+
  f(s_oz, copy ="s_pm", fixed = F, hyper = hyper)+
  f(Month_pm, model = "ar1",cyclic = T) + 
  f(Month_oz, copy = "Month_pm", fixed = F, hyper = hyper)+
  f(Year_pm, model = "ar1")+
  f(Year_oz, copy = "Year_pm", fixed = F, hyper = hyper)+
  f(Id_pm, model = "iid", hyper = hyper1) + 
  f(Id_oz, model = "iid", hyper = hyper1)+
  f(s_oz2, model = spde)+
  Tmax_pm+Tmax_oz+
  Precipitation_pm+Precipitation_oz+
  Surface_pressure_pm+Surface_pressure_oz+
  Wind_pm+Wind_oz+
  Burnt_area_pm+Burnt_area_oz+
  Extreme_drought_pm + Extreme_drought_oz+
  GDP_pm+GDP_oz+
  Pop_pm+Pop_oz

model2 <- inla(formula_m2,
               family = c("gaussian", "gev"),  
               data = inla.stack.data(stack),
               control.family = list(list(), list(hyper = hyper.gev)),
               #                      list(beta.censor.value = 0.05), list()),
               control.predictor = list(A = inla.stack.A(stack), link = 1,
                                        compute = TRUE),
               #Ntrials=1,
               #Ntrials = c(length(d$max[ipos.max]),length(d$ba[ipos.ba])),
               
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
                                      config = T, return.marginals.predictor = T), 
               verbose = T)


##Modelling M3

formula_m3 <- y ~ 0 + Intercept_pm + Intercept_oz +
  f(s_pm, model=spde, group = s_pm.group,
    control.group = list(model = "ar1", hyper = rprior,cyclic = T))+
  f(s_oz, copy ="s_pm", fixed = F, hyper = hyper)+
  f(s_oz2, model=spde, group = s_oz2.group,
    control.group = list(model = "ar1", hyper = rprior,cyclic = T))+
  f(Year_pm, model = "ar1",hyper = rprior) + 
  f(Year_oz, copy ="Year_pm", fixed = F, hyper = hyper)+
  f(Id_pm, model = "iid", hyper = hyper1) + 
  f(Id_oz, model = "iid", hyper = hyper1) +
  Tmax_pm+Tmax_oz+
  Precipitation_pm+Precipitation_oz+
  Surface_pressure_pm+Surface_pressure_oz+
  Wind_pm+Wind_oz+
  Burnt_area_pm+Burnt_area_oz+
  Extreme_drought_pm + Extreme_drought_oz+
  GDP_pm+GDP_oz+
  Pop_pm+Pop_oz



model3 <- inla(formula_m3,
               family = c("gaussian", "gev"),  
               data = inla.stack.data(stack),
               control.family = list(list(), list(hyper = hyper.gev)),
               #                      list(beta.censor.value = 0.05), list()),
               control.predictor = list(A = inla.stack.A(stack), link = 1,
                                        compute = TRUE),
               #Ntrials=1,
               #Ntrials = c(length(d$max[ipos.max]),length(d$ba[ipos.ba])),
               
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
                                      config = T, return.marginals.predictor = T), 
               verbose = T)

summary(model3)

##Caculate the RMSE and the Cor for the model.
#For pm training
validation1 = list()
index = inla.stack.index(stack,"pm_mean.train")$data
pm.train.mean = model3$summary.linear.predictor[index,"mean"]
validation1$res =d$log_pm_mean[index_pm] - pm.train.mean
validation1$rmse = sqrt(mean(validation1$res^2, na.rm=TRUE))
validation1$cor = cor(d$log_pm_mean[index_pm], pm.train.mean,
                      use="pairwise.complete.obs", method="pearson")

#For ozone training
validation2 = list()
index = inla.stack.index(stack,"Ozone.train")$data
oz.train.mean = model3$summary.linear.predictor[index,"mean"]
validation2$res =d$log_oz_max[index_oz] - oz.train.mean
validation2$rmse = sqrt(mean(validation2$res^2, na.rm=TRUE))
validation2$cor = cor(d$log_oz_max[index_oz], oz.train.mean,
                      use="pairwise.complete.obs", method="pearson")

#For pm testing
validation3 = list()
index = inla.stack.index(stack,"pm_mean.val")$data
pm.val.mean = model3$summary.linear.predictor[index,"mean"]
validation3$res =d$log_pm_mean[index_val_pm] - pm.val.mean
validation3$rmse = sqrt(mean(validation3$res^2, na.rm=TRUE))
validation3$cor = cor(d$log_pm_mean[index_val_pm], pm.val.mean,
                      use="pairwise.complete.obs", method="pearson")

#For ozone testing
validation4 = list()
index = inla.stack.index(stack,"Ozone.val")$data
oz.val.mean = model3$summary.linear.predictor[index,"mean"]
validation4$res =d$log_oz_max[index_val_oz] - oz.val.mean
validation4$rmse = sqrt(mean(validation4$res^2, na.rm=TRUE))
validation4$cor = cor(d$log_oz_max[index_val_oz], oz.val.mean,
                      use="pairwise.complete.obs", method="pearson")


##Extract the mean of fitted value of full prediction in 2021.
index.mean.pred <- inla.stack.index(stack, "pm_mean.pred")$data
index.max.pred <- inla.stack.index(stack, "Ozone.pred")$data

pred.pm.val <- model3$summary.fitted.values$mean[index.mean.pred]
pred.oz.val <- model3$summary.fitted.values$mean[index.max.pred]

d2021$pred.mean <- pred.pm.val
d2021$pred.max <- pred.oz.val

d_sf2 <- st_as_sf(d2021, coords = c("long", "lat"),crs = 4326)

d_sf2$Mtypr <- NA
d_sf2$Mtypr[d_sf2$month == 1] <- "January"
d_sf2$Mtypr[d_sf2$month == 2] <- "February"
d_sf2$Mtypr[d_sf2$month == 3] <- "March"
d_sf2$Mtypr[d_sf2$month == 4] <- "April"
d_sf2$Mtypr[d_sf2$month == 5] <- "May"
d_sf2$Mtypr[d_sf2$month == 6] <- "June"
d_sf2$Mtypr[d_sf2$month == 7] <- "July"
d_sf2$Mtypr[d_sf2$month == 8] <- "August"
d_sf2$Mtypr[d_sf2$month == 9] <- "September"
d_sf2$Mtypr[d_sf2$month == 10] <- "October"
d_sf2$Mtypr[d_sf2$month == 11] <- "November"
d_sf2$Mtypr[d_sf2$month == 12] <- "December"
d_sf2$Mtypr <- factor(d_sf2$Mtypr,level = c("January","February","March","April","May","June","July","August","September","October","November","December"))

## Caculate the StwCRPS
# parameter settings
index.oz.val <- inla.stack.index(stack, "Ozone.val")$data
pred.oz.val <- model3$summary.fitted.values$mean[index.max.val]
index.oz.train <- inla.stack.index(stack, "Ozone.train")$data
pred.oz.train <- model3$summary.fitted.values$mean[index.oz.train]

obs_train = d$log_oz_max[index_oz]
obs_va = d$log_oz_max[index_val_oz]
mu <- pred.oz.train
mu1 <- pred.oz.val
tau= 212.865#here is the model3 precision for Gumbel observations
tail=0#for Gumbel suitation
sigma=1/sqrt(tau)

# functions
pgev = function(x, μ, σ, ξ) {
  ifelse(ξ == 0,
         exp(-exp(- (x - μ) / σ)),
         exp(-pmax(0, 1 + ξ * (x - μ) / σ) ^ (-1 / ξ)))
}

qgev = function(p, μ, σ, ξ) {
  ifelse(ξ == 0,
         μ - σ * log(-log(p)),
         μ - σ * (1 / ξ) * (1 - (- log(p)) ^ (-ξ)))
}

dgev = function(x, μ, σ, ξ, log = FALSE) {
  res = ifelse(ξ == 0,
               -exp(- (x - μ) / σ),
               -pmax(0, 1 + ξ * (x - μ) / σ) ^ (-1 / ξ))
  res = res - log(σ) +
    ifelse(ξ == 0,
           - (x - μ) / σ,
           ifelse(1 + ξ * (x - μ) / σ > 0,
                  - (1 / ξ + 1) * log(1 + ξ * (x - μ) / σ),
                  -Inf))
  if (!log) res = exp(res)
  res
}


twcrps_gev = function(y, μ, σ, ξ, p) {
  F = function(x) sapply(x, function(z) mean(pgev(z, μ, σ, ξ))) 
  quantiles = qgev(p, μ, σ, ξ)
  if (length(quantiles) == 1) {
    y_min = quantiles
  } else {
    y_min = uniroot(function(x) F(x) - p, lower = min(quantiles), upper = max(quantiles))$root
  }
  p_max = .999
  y_max = max(qgev(p_max, μ, σ, ξ), max(y) + 1)
  res = rep(0, length(y))
  for (i in seq_along(y)) {
    if (y[i] < y_min) {
      res[i] = integrate(function(x) (1 - F(x))^2, lower = y_min, upper = y_max)$value
    } else if (y[i] < y_max) {
      res[i] = integrate(function(x) F(x)^2, lower = y_min, upper = y[i])$value
      res[i] = res[i] + integrate(function(x) (1 - F(x))^2, lower = y[i], upper = y_max)$value
    } else {
      res[i] = integrate(function(x) F(x)^2, lower = y_min, upper = y_max)$value
    }
  }
  res = res + (y_min - y) * (ifelse(y <= y_min, 1, 0) - p)^2
  res
}


expected_twcrps_gev = function(μ, σ, ξ, p, μ_true = μ, σ_true = σ, ξ_true = ξ) {
  p_min = .00001
  y_min = min(qgev(p_min, μ_true, σ_true, ξ_true))
  p_max = .99999
  y_max = max(qgev(p_max, μ_true, σ_true, ξ_true))
  
  if (length(c(μ_true, σ_true, ξ_true)) == 3) {
    density = function(x) dgev(x, μ_true, σ_true, ξ_true)
  } else {
    density = function(x) sapply(x, function(z) mean(dgev(z, μ_true, σ_true, ξ_true)))
  }
  integrate(function(y) density(y) * twcrps_gev(y, μ, σ, ξ, p),
            lower = y_min, upper = y_max)$value
}


stwcrps_gev = function(y, μ, σ, ξ, p) {
  S = abs(expected_twcrps_gev(μ, σ, ξ, p))
  twcrps = twcrps_gev(y, μ, σ, ξ, p)
  twcrps / S + log(S)
}


StwCRPS <- mean(stwcrps_gev(obs_val,mu_val,sigma,tail,0.70))
StwCRPS



