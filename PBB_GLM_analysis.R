
library(data.table)
library(tidyverse)
library(remotePARTS)


# 5. 
####remotePARTS GLS Spatial Error Model Analysis:

############DATASETS###############################################################
plumas_seasonal <- fread("remoteparts_output/PLUMAS_SEASONAL_TREND.csv")
plumas_annual <- fread("remoteparts_output/PLUMAS_ANNUAL_TREND.csv")
sonoma_seasonal <- fread("remoteparts_output/SONOMA_SEASONAL_TREND.csv")
sonoma_annual <- fread("remoteparts_output/SONOMA_ANNUAL_TREND.csv")

##############################SONOMA ANNUAL###############################################
#lat, long, and elevation are jank, so not looking at any drivers
sonoma_annual <- na.omit(sonoma_annual)
annual <- sonoma_annual
fitopt <- fitGLS_opt(formula = AR_coef ~ 1, data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5) # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

##############################SONOMA ANNUAL + veg###############################################
annual <- sonoma_annual
annual <- na.omit(annual)
fitopt <- fitGLS_opt(formula = AR_coef ~ 0 + as.factor(LIFE_FORM), data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

##############################PLUMAS ANNUAL###############################################
#lat, long, and elevation are jank, so not looking at any drivers
plumas_annual <- na.omit(plumas_annual)
annual <- plumas_annual
fitopt <- fitGLS_opt(formula = AR_coef ~ 1, data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5) # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

##############################PLUMAS ANNUAL + veg###############################################

annual <- plumas_annual
annual <- na.omit(annual)

fitopt <- fitGLS_opt(formula = AR_coef ~ 0 + as.factor(LIFE_FORM), data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

#############################################################################################
#############################################################################################
#############################################################################################
##############################SONOMA seasonal###############################################
#lat, long, and elevation are jank, so not looking at any drivers
annual <- sonoma_seasonal %>% filter(season == "fall")
annual <- na.omit(annual)

fitopt <- fitGLS_opt(formula = AR_coef ~ 1+lat+long+elevation, data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5),no.F=FALSE  # lower the convergence tolerance (see ?stats::optim()) 
)


analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

##############################SONOMA seasonal + veg###############################################
#lat, long, and elevation are jank, so not looking at any drivers
annual <- sonoma_seasonal %>% filter(season == "winter")
annual <- na.omit(annual)

fitopt <- fitGLS_opt(formula = AR_coef ~ 0 + as.factor(LIFE_FORM), data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

fall_con <- output_df
win_con <- output_df
spring_con <- output_df
summer_con <- output_df

############################################################################3

##############################PLUMAS seasonal###############################################
#lat, long, and elevation are jank, so not looking at any drivers
annual <- plumas_seasonal %>% filter(season == "summer")
annual <- na.omit(annual)

fitopt <- fitGLS_opt(formula = AR_coef ~ 1+elevation, data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5), no.F = FALSE# lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

##############################PLUMAS seasonal + veg###############################################
#lat, long, and elevation are jank, so not looking at any drivers
annual <- plumas_seasonal %>% filter(season == "fall")
annual <- na.omit(annual)

fitopt <- fitGLS_opt(formula = AR_coef ~ 0 + as.factor(LIFE_FORM), data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5),
                     no.F = FALSE# lower the convergence tolerance (see ?stats::optim()) 
)
fitopt2 <- fitGLS_opt(formula = AR_coef ~ 1 + elevation, data = annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5),
                     no.F = FALSE# lower the convergence tolerance (see ?stats::optim()) 
)



analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)

fall <- output_df
win <- output_df
spring <- output_df
summer <- output_df

############################################################################3








fitopt <- fitGLS_opt(formula = AR_coef ~ 1 , data = sonoma_annual, 
                     coords = annual[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)











#################SEASONAL##############################################################
seasonal_con <- fread("remoteparts_output/sonoma_seasonal_output_conifer_df.csv")
seasonal_fall <- fread("remoteparts_output/sonoma_seasonal_output_fall_df.csv")
seasonal_hw <- fread("remoteparts_output/sonoma_seasonal_output_hardwood_df.csv")
seasonal_herb <- fread("remoteparts_output/sonoma_seasonal_output_herbaceous_df.csv")
seasonal_shrub <- fread("remoteparts_output/sonoma_seasonal_output_shrub_df.csv")
seasonal_spring <- fread("remoteparts_output/sonoma_seasonal_output_spring_df.csv")
seasonal_summer <- fread("remoteparts_output/sonoma_seasonal_output_summer_df.csv")
seasonal_winter <- fread("remoteparts_output/sonoma_seasonal_output_winter_df.csv")

winter <- st_as_sf(seasonal_winter, coords = c("long", "lat"), crs = 4326)
winter <- get_elev_point(winter)
winter <- winter %>% mutate(long = unlist(map(winter$geometry,1)),
                            lat = unlist(map(winter$geometry,2)))
winter <- as.data.frame(winter) %>% mutate(season = "winter")
elevs <- winter %>% select(elevation, elev_units, long, lat)
fall <- left_join(as.data.frame(seasonal_fall), elevs) %>% mutate(season = "fall")
spring <- left_join(as.data.frame(seasonal_spring), elevs) %>% mutate(season = "spring")
summer <- left_join(as.data.frame(seasonal_summer), elevs) %>% mutate(season = "summer")

#do the following like a loop:
annual_veg <-summer
fitopt <- fitGLS_opt(formula = AR_coef ~ 1 + lat + long + elevation, data = annual_veg, 
                     coords = annual_veg[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)

#and then renamed down here:
winter <- as.data.frame(output_df) %>% mutate(season = "winter")
fall <- as.data.frame(output_df) %>% mutate(season = "fall")
spring <- as.data.frame(output_df) %>% mutate(season = "spring")
summer <- as.data.frame(output_df) %>% mutate(season = "summer")

#################SEASONAL + VEG##########################################################
seasonal_con <- fread("remoteparts_output/sonoma_seasonal_output_conifer_df.csv")
seasonal_hw <- fread("remoteparts_output/sonoma_seasonal_output_hardwood_df.csv")
seasonal_herb <- fread("remoteparts_output/sonoma_seasonal_output_herbaceous_df.csv")
seasonal_shrub <- fread("remoteparts_output/sonoma_seasonal_output_shrub_df.csv")
con <- as.data.frame(seasonal_con)
hw <- as.data.frame(seasonal_hw)
herb <- as.data.frame(seasonal_herb)
shrub <- as.data.frame(seasonal_shrub)
con <- left_join(con, elevs)
hw <- left_join(hw, elevs)
herb <- left_join(herb, elevs)
shrub <- left_join(shrub, elevs)

#do the following like a loop:
split <- split(shrub, shrub$season)
annual_veg <- split[[4]]
fitopt <- fitGLS_opt(formula = AR_coef ~ 1+ lat + long + elevation, data = annual_veg, 
                     coords = annual_veg[, c("long", "lat")], 
                     covar_FUN = "covar_exp", 
                     start = c(range = .1, nugget = .2),
                     method = "BFGS", # use BFGS algorightm (see ?stats::optim())
                     control = list(reltol = 1e-5)  # lower the convergence tolerance (see ?stats::optim()) 
)
analysis_coefficents <- fitopt$GLS$coefficients
analysis_pvals <- fitopt$GLS$pval_t
output_df <- data.frame(matrix(nrow=length(analysis_coefficents), ncol = 0))
output_df["trends"] <- analysis_coefficents
output_df["pvalues"] <- analysis_pvals
rownames(output_df) <- names(analysis_coefficents)
print(output_df)
######################################################################################
