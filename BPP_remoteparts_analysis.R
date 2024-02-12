
# 12/12/2023
# remote parts plotting

library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)
library(ggspatial)
################ANNUAL TRENDS########################################################################
#67001f, #b2182b, #d6604d, #f4a582, #fddbc7, #f7f7f7, #d1e5f0, #92c5de, #4393c3, #2166ac, #053061

# plumas Annual trend data
p_annual <- fread("remoteparts_output/plumas_annual_output_df.csv")
pmap <- st_read("ignore/california_counties/Plumas_shp/plumas.shp")
plumas <- p_annual 
plumas <-st_as_sf(plumas, coords = c("long", "lat"), crs = 4326)
plumas <- st_transform(plumas, crs = st_crs(pmap))

p1 <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = plumas, aes(color = AR_coef), size=2) + theme_void() + 
  labs(title = " A. Plumas County", color = " Burn Day Trend ") + 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7", "#fddbc7",  "#f4a582","#d6604d"), 
                        values = scales::rescale(c(-1.616488, -1.29319, -0.9698924, -0.6465948, -0.3232972, 4e-07,0.09210837,
                                                   0.1842167, 0.2763251)))+
  theme(plot.background = element_rect(colour = "black", fill=NA, size=1), legend.box.background = element_rect(colour = "black"), 
        legend.position = c(.85,.85)) + annotation_north_arrow(location = "br")+ annotation_scale()


# sonoma Annual trend data
s_annual <- fread("remoteparts_output/sonoma_annual_output_df.csv")
smap <- st_read("ignore/california_counties/sonoma.shp")
sonoma <- s_annual 
sonoma <-st_as_sf(sonoma, coords = c("long", "lat"), crs = 4326)
sonoma <- st_transform(sonoma, crs = st_crs(smap))

s1 <- ggplot()+  geom_sf(data = smap) +geom_sf(data = sonoma, aes(color = AR_coef), size = 2) + theme_void() + 
  labs(title = " B. Sonoma County", color = " Burn Day Trend ")+ scale_color_gradientn(colours = c("#053061", "#2166ac", 
            "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7", "#fddbc7",  "#f4a582","#d6604d", "#b2182b", "#67001f"), 
        values = scales::rescale(c(-1.969499, -1.575599, -1.181699, -0.7877996, -0.3938998, 0, 0.5386812, 1.077362, 1.616044, 2.154725, 
                                   2.693406)))+ theme(plot.background = element_rect(colour = "black", fill=NA, size=1), 
                                                      legend.box.background = element_rect(colour = "black"), 
                              legend.position = c(.2,.3))+ annotation_north_arrow(location = "tr")+ annotation_scale()
p3 <- ggarrange(p1, s1, nrow=2, align = "hv")

#############SEASONAL TRENDS##############################################################################
#plumas
p_winter <- fread("remoteparts_output/plumas_seasonal_output_winter_df.csv") %>% mutate(Season = "Winter")
p_summer <- fread("remoteparts_output/plumas_seasonal_output_summer_df.csv")%>% mutate(Season = "Summer")
p_fall <- fread("remoteparts_output/plumas_seasonal_output_fall_df.csv")%>% mutate(Season = "Fall")
p_spring <- fread("remoteparts_output/plumas_seasonal_output_spring_df.csv")%>% mutate(Season = "Spring")
p_seasonal <- rbind(rbind(p_winter, p_summer), rbind(p_fall, p_spring))
plumas2 <-st_as_sf(p_seasonal, coords = c("long", "lat"), crs = 4326)
plumas2 <- st_transform(plumas2, crs = st_crs(pmap))

p2 <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = plumas2, aes(color = AR_coef)) +
  scale_color_gradient2(low ="blue", mid = "white" , high ="orange") + facet_wrap(vars(season)) + 
  theme_void() + labs(title = "Plumas - seasonal burn day trend")

p2 <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = plumas2, aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d"), values = scales::rescale(c(-.8068466, 
                    -0.6051348, -0.4034232, -0.2017116, 0, 0.1511797, 0.3023594, 0.453539))) + 
  facet_wrap(vars(Season))+labs(title = " A. Plumas County", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) + annotation_north_arrow(location = "br")+ annotation_scale()

#sonoma
s_winter <- fread("remoteparts_output/sonoma_seasonal_output_winter_df.csv") %>% mutate(Season = "Winter")
s_summer <- fread("remoteparts_output/sonoma_seasonal_output_summer_df.csv")%>% mutate(Season = "Summer")
s_fall <- fread("remoteparts_output/sonoma_seasonal_output_fall_df.csv")%>% mutate(Season = "Fall")
s_spring <- fread("remoteparts_output/sonoma_seasonal_output_spring_df.csv")%>% mutate(Season = "Spring")
s_seasonal <- rbind(rbind(s_winter, s_summer), rbind(s_fall, s_spring))
sonoma2 <-st_as_sf(s_seasonal, coords = c("long", "lat"), crs = 4326)
sonoma2 <- st_transform(sonoma2, crs = st_crs(smap))

s2 <- ggplot()+  geom_sf(data = smap) +geom_sf(data = sonoma2, aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d", "#b2182b", "#67001f"), 
   values = scales::rescale(c(-1.418039, -1.134431, -0.8508234, -0.5672156, -0.2836078, 
                              0, 0.219856, 0.439712, 0.659568,  0.879424, 1.09928))) + 
  facet_wrap(vars(Season))+labs(title = " B. Sonoma County", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) + annotation_north_arrow(location = "tr")+
  annotation_scale()

p4 <- ggarrange(p2, s2, nrow=1, align = "hv")

########################################################################################
#kind of re-formatting plot 4 to add in scale bar/north arrow
p2a <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = subset(plumas2, Season %in% "Fall"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d"), values = scales::rescale(c(-.8068466, 
                                        -0.6051348, -0.4034232, -0.2017116, 0, 0.1511797, 0.3023594, 0.453539))) + 
  labs(title = " Fall", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) + annotation_north_arrow(location = "tr")+ annotation_scale()

p2b <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = subset(plumas2, Season %in% "Winter"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d"), values = scales::rescale(c(-.8068466, 
                                    -0.6051348, -0.4034232, -0.2017116, 0, 0.1511797, 0.3023594, 0.453539))) + 
  labs(title = " Winter", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) 

p2c <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = subset(plumas2, Season %in% "Spring"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d"), values = scales::rescale(c(-.8068466, 
                                  -0.6051348, -0.4034232, -0.2017116, 0, 0.1511797, 0.3023594, 0.453539))) + 
  labs(title = " Spring", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) 

p2d <- ggplot()+  geom_sf(data = pmap) +geom_sf(data = subset(plumas2, Season %in% "Summer"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d"), values = scales::rescale(c(-.8068466, 
                                     -0.6051348, -0.4034232, -0.2017116, 0, 0.1511797, 0.3023594, 0.453539))) + 
  labs(title = " Summer", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) 

p2 <- ggarrange(p2a, p2b, p2c, p2d, align = "hv", common.legend = TRUE, legend="bottom")

p2_final <- annotate_figure(p2, top = text_grob("A. Plumas County", size = 20))

##########

s2a <- ggplot()+  geom_sf(data = smap) +geom_sf(data = subset(sonoma2, Season %in% "Fall"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d", "#b2182b", "#67001f"), 
                        values = scales::rescale(c(-1.418039, -1.134431, -0.8508234, -0.5672156, -0.2836078, 
                                                   0, 0.219856, 0.439712, 0.659568,  0.879424, 1.09928))) + 
  labs(title = " Fall", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) + annotation_north_arrow(location = "tr")+
  annotation_scale()

s2b <- ggplot()+  geom_sf(data = smap) +geom_sf(data = subset(sonoma2, Season %in% "Winter"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d", "#b2182b", "#67001f"), 
                        values = scales::rescale(c(-1.418039, -1.134431, -0.8508234, -0.5672156, -0.2836078, 
                                                   0, 0.219856, 0.439712, 0.659568,  0.879424, 1.09928))) + 
  labs(title = " Winter", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) 

s2c <- ggplot()+  geom_sf(data = smap) +geom_sf(data = subset(sonoma2, Season %in% "Spring"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d", "#b2182b", "#67001f"), 
                        values = scales::rescale(c(-1.418039, -1.134431, -0.8508234, -0.5672156, -0.2836078, 
                                                   0, 0.219856, 0.439712, 0.659568,  0.879424, 1.09928))) + 
  labs(title = " Spring", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) 

s2d <- ggplot()+  geom_sf(data = smap) +geom_sf(data = subset(sonoma2, Season %in% "Summer"), aes(color = AR_coef))+ 
  scale_color_gradientn(colours = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                                    "#fddbc7",  "#f4a582","#d6604d", "#b2182b", "#67001f"), 
                        values = scales::rescale(c(-1.418039, -1.134431, -0.8508234, -0.5672156, -0.2836078, 
                                                   0, 0.219856, 0.439712, 0.659568,  0.879424, 1.09928))) + 
  labs(title = " Summer", color = "Burn Day Trend ") + theme_void() +
  theme(strip.text = element_text(size = 18), plot.background = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=20)) 


s2 <- ggarrange(s2a, s2b, s2c, s2d, common.legend = TRUE, legend="bottom")

s2_final <- annotate_figure(s2, top = text_grob("B. Sonoma County", size = 20))

plot2 <- ggarrange(p2_final, s2_final, align = "hv")

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
# 12/11/2023

# Remote parts analysis

###Contents
# 1. Sonoma Annual trend. FILE: "remoteparts_output/sonoma_annual_output_df.csv"
# 2. Sonoma annual trend + veg: 
# 3. Sonoma Seasonal trends. 
# 4. Sonoma seasonal trends + veg
# 5. sonoma annual spatial analysis
# 6. sonoma seasonal spatial analysis

# 7. Plumas annual trend
# 8. plumas annual + veg
# 9 plumas seasonal
# 10. plumas seasonal + veg
# 11. plumas annual spatial analysis
# 12. plumas seasonal spatial analysis

######################################

library(remotePARTS)
library(raster)
library(tidyverse)
library(data.table)
library(sf)
library(vctrs)
library(elevatr)

shw <- fread("Data_Nov23/prescriptions_daytime/sonoma_hardwood_prescription.csv")
sc <- fread("Data_Nov23/prescriptions_daytime/sonoma_conifer_prescription.csv")
ss <- fread("Data_Nov23/prescriptions_daytime/sonoma_shrub_prescription.csv")
she <- fread("Data_Nov23/prescriptions_daytime/sonoma_herb_prescription.csv")
sonoma <- rbind(rbind(shw, sc), rbind(ss, she))%>% mutate(county = "sonoma")

sonoma <- st_as_sf(sonoma, coords = c("long", "lat"), crs = 26910)
sonoma <- st_transform(sonoma, crs = 4326)
sonoma <- sonoma %>% mutate(long = unlist(map(sonoma$geometry,1)),lat = unlist(map(sonoma$geometry,2)))
sonoma<- as.data.frame(sonoma)

phw <- fread("Data_Nov23/prescriptions_daytime/plumas_hardwood_prescription.csv")
pc1 <- fread("Data_Nov23/prescriptions_daytime/plumas_conifer_prescription1.csv")
pc2 <- fread("Data_Nov23/prescriptions_daytime/plumas_conifer_prescription2.csv")
pc3 <- fread("Data_Nov23/prescriptions_daytime/plumas_conifer_prescription3.csv")
pc <- rbind(rbind(pc1, pc2), pc3)
ps <- fread("Data_Nov23/prescriptions_daytime/plumas_shrub_prescription.csv")
phe <- fread("Data_Nov23/prescriptions_daytime/plumas_herb_prescription.csv")
plumas <- rbind(rbind(phw, pc), rbind(ps, phe)) %>% mutate(county = "plumas")

plumas <- st_as_sf(plumas, coords = c("long", "lat"), crs = 26910)
plumas <- st_transform(plumas, crs = 4326)
plumas <- plumas %>% mutate(long = unlist(map(plumas$geometry,1)),lat = unlist(map(plumas$geometry,2)))
plumas<- as.data.frame(plumas)



# 1. 
#### 1 SONOMA ANNUAL TRENDS ####################################################

########### Datasets ###########################################################
#annual trend for whole county:
input_CSV <- sonoma
################# Data Preprocessing ###########################################
coords <- input_CSV[, c("long", "lat")]
unique_coordinates <- unique(coords)
colnames(unique_coordinates) <- c("long", "lat")
years <- c(unique(input_CSV$year))
output_dataframe <- data.frame(matrix(nrow = 830, ncol = 0))

for (x in years){
  yeardata <- input_CSV[input_CSV$year == x,]
  output <- c()
  for (y in seq(1, nrow(unique_coordinates), by = 1)){
    cord <- unique_coordinates[y,]
    data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
    burndays <- nrow(data_subset)
    output <- c(output, burndays)
  }
  output_dataframe[as.character(x)] <- output
}

output_dataframe["long"] <- unique_coordinates$long
output_dataframe["lat"] <- unique_coordinates$lat



for (x in years){
  #filters single year
  yeardata <- input_CSV[input_CSV$year == x,]
  output <- c()
  for (y in seq(1, nrow(unique_coordinates), by = 1)){
    cord <- unique_coordinates[y,]
    data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
    FFWI <- data_subset$FFWI
    output <- c(output, FFWI)
  }
  output_dataframe[as.character(x)] <- output
}

output_dataframe["long"] <- unique_coordinates$long
output_dataframe["lat"] <- unique_coordinates$lat












########## remotePARTS REML Timeseries Analysis ################################
timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
coords <- as.matrix(output_dataframe[,seq(24:25)])
ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
output_dataframe["AR_coef"] <- ARfit$coefficients[,2]

#12/18: adding back in veg column + elevations
sonomax <- sonoma %>% select(lat, long, LIFE_FORM)
sonomax <- unique(sonomax)
output_dataframe <- left_join(output_dataframe, sonomax)
od <- output_dataframe %>% select(long, lat, AR_coef, LIFE_FORM)
od <- st_as_sf(od, coords = c("long", "lat"), crs = 4326)
od <- get_elev_point(od)
od <- od %>% mutate(long = unlist(map(od$geometry,1)),lat = unlist(map(od$geometry,2)))
od <- as.data.frame(od)


##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(output_dataframe, "remoteparts_output/SONOMA_ANNUAL_TREND.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

# 2. 
#### 2 SONOMA ANNUAL TRENDS + veg ####################################################

########### Datasets ###########################################################
# annual trend by veg type:
input_CSV <- sonoma
input_CSV <- split(input_CSV, list(input_CSV$LIFE_FORM))
input_CSV <- list_drop_empty(input_CSV)
input_CSV <- input_CSV2
################# Data Preprocessing ###########################################


fun <- function(z){
input_CSV <- input_CSV[[z]]
coords <- input_CSV[, c("long", "lat")]
unique_coordinates <- unique(coords)
colnames(unique_coordinates) <- c("long", "lat")
years <- c(unique(input_CSV$year))
time <- nrow(unique_coordinates)
output_dataframe <- data.frame(matrix(nrow = time, ncol = 0))

for (x in years){
  yeardata <- input_CSV[input_CSV$year == x,]
  output <- c()
  for (y in seq(1, nrow(unique_coordinates), by = 1)){
    cord <- unique_coordinates[y,]
    data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
    burndays <- nrow(data_subset)
    output <- c(output, burndays)
  }
  output_dataframe[as.character(x)] <- output
}

output_dataframe["long"] <- unique_coordinates$long
output_dataframe["lat"] <- unique_coordinates$lat
### remotePARTS REML Timeseries Analysis #####
timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
coords <- as.matrix(output_dataframe[,seq(24:25)])
ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
output_dataframe["AR_coef"] <- ARfit$coefficients[,2]

data <- output_dataframe
}

num <- c(1:4)
list <- lapply(num, fun)

conifer <- list[[1]]
hardwood <- list[[2]]
herb <- list[[3]]
shrub <- list[[4]]
##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(conifer, "remoteparts_output/sonoma_annual_output_con_df.csv")
fwrite(hardwood, "remoteparts_output/sonoma_annual_output_hardwood_df.csv")
fwrite(herb, "remoteparts_output/sonoma_annual_output_herb_df.csv")
fwrite(shrub, "remoteparts_output/sonoma_annual_output_shrub_df.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

# 3.
#### 3 SONOMA SEASONAL TRENDS #################################################

########### Datasets #########################################################

# seasonal trends
input_CSV <- sonoma
input_CSV <- split(input_CSV, list(input_CSV$season))
input_CSV <- list_drop_empty(input_CSV)
input_CSV2 <- input_CSV
################# Data Preprocessing ###########################################

fun <- function(z){
  input_CSV <- input_CSV[[z]]
  coords <- input_CSV[, c("long", "lat")]
  unique_coordinates <- unique(coords)
  colnames(unique_coordinates) <- c("long", "lat")
  years <- c(unique(input_CSV$year))
  output_dataframe <- data.frame(matrix(nrow = 830, ncol = 0))
  
  for (x in years){
    yeardata <- input_CSV[input_CSV$year == x,]
    output <- c()
    for (y in seq(1, nrow(unique_coordinates), by = 1)){
      cord <- unique_coordinates[y,]
      data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
      burndays <- nrow(data_subset)
      output <- c(output, burndays)
    }
    output_dataframe[as.character(x)] <- output
  }
  
  output_dataframe["long"] <- unique_coordinates$long
  output_dataframe["lat"] <- unique_coordinates$lat
  ### remotePARTS REML Timeseries Analysis #####
  timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
  coords <- as.matrix(output_dataframe[,seq(24:25)])
  ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
  output_dataframe["AR_coef"] <- ARfit$coefficients[,2]
  
  data <- output_dataframe
}

num <- c(1:4)
list <- lapply(num, fun)

fall <- list[[1]]
spring <- list[[2]]
summer <- list[[3]]
winter <- list[[4]]

fall <- fall %>% mutate(season = "fall") %>% select(long, lat, AR_coef, season)
spring <- spring %>% mutate(season = "spring") %>% select(long, lat, AR_coef, season)
summer <- summer %>% mutate(season = "summer") %>% select(long, lat, AR_coef, season)
winter <- winter %>% mutate(season = "winter") %>% select(long, lat, AR_coef, season)
seasons <- rbind(rbind(fall, spring), rbind(summer, winter))

sonomax <- sonoma %>% select(lat, long, LIFE_FORM)
sonomax <- unique(sonomax)
output_dataframe <- left_join(seasons, sonomax)
od <- st_as_sf(output_dataframe, coords = c("long", "lat"), crs = 4326)
od <- get_elev_point(od)
od <- od %>% mutate(long = unlist(map(od$geometry,1)),lat = unlist(map(od$geometry,2)))
od <- as.data.frame(od)


##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(output_dataframe, "remoteparts_output/PLUMAS_SEASONAL_TREND.csv")


##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(fall, "remoteparts_output/sonoma_seasonal_output_fall_df.csv")
fwrite(spring, "remoteparts_output/sonoma_seasonal_output_spring_df.csv")
fwrite(summer, "remoteparts_output/sonoma_seasonal_output_summer_df.csv")
fwrite(winter, "remoteparts_output/sonoma_seasonal_output_winter_df.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

# 4. 
#### 3 SONOMA SEASONAL TRENDS + veg #################################################

########### Datasets #########################################################

# seasonal trends
input_CSV <- sonoma
input_CSV <- split(input_CSV, list(input_CSV$season, input_CSV$LIFE_FORM))
input_CSV <- list_drop_empty(input_CSV)
input_CSV2 <- input_CSV
################# Data Preprocessing ###########################################

fun <- function(z){
  input_CSV <- input_CSV[[z]]
  coords <- input_CSV[, c("long", "lat")]
  unique_coordinates <- unique(coords)
  colnames(unique_coordinates) <- c("long", "lat")
  years <- c(unique(input_CSV$year))
  time <- nrow(unique_coordinates)
  output_dataframe <- data.frame(matrix(nrow = time, ncol = 0))
  
  for (x in years){
    yeardata <- input_CSV[input_CSV$year == x,]
    output <- c()
    for (y in seq(1, nrow(unique_coordinates), by = 1)){
      cord <- unique_coordinates[y,]
      data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
      burndays <- nrow(data_subset)
      output <- c(output, burndays)
    }
    output_dataframe[as.character(x)] <- output
  }
  
  output_dataframe["long"] <- unique_coordinates$long
  output_dataframe["lat"] <- unique_coordinates$lat
  ### remotePARTS REML Timeseries Analysis #####
  timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
  coords <- as.matrix(output_dataframe[,seq(24:25)])
  ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
  output_dataframe["AR_coef"] <- ARfit$coefficients[,2]
  
  data <- output_dataframe
}

num <- c(1:16)
list <- lapply(num, fun)

conifer.fall <- as.data.frame(list[[1]]) %>% mutate(season = "fall", LIFE_FORM = "conifer")
conifer.spring <- as.data.frame(list[[2]])%>% mutate(season = "spring", LIFE_FORM = "conifer")
conifer.summer <- as.data.frame(list[[3]])%>% mutate(season = "summer", LIFE_FORM = "conifer")
conifer.winter <- as.data.frame(list[[4]])%>% mutate(season = "winter", LIFE_FORM = "conifer")

hw.fall <- as.data.frame(list[[5]]) %>% mutate(season = "fall", LIFE_FORM = "hardwood")
hw.spring <- as.data.frame(list[[6]])%>% mutate(season = "spring", LIFE_FORM = "hardwood")
hw.summer <- as.data.frame(list[[7]])%>% mutate(season = "summer", LIFE_FORM = "hardwood")
hw.winter <- as.data.frame(list[[8]])%>% mutate(season = "winter", LIFE_FORM = "hardwood")
herb.fall <- as.data.frame(list[[9]]) %>% mutate(season = "fall", LIFE_FORM = "herbaceous")

herb.spring <- as.data.frame(list[[10]])%>% mutate(season = "spring", LIFE_FORM = "herbaceous")
herb.summer <- as.data.frame(list[[11]])%>% mutate(season = "summer", LIFE_FORM = "herbaceous")
herb.winter <- as.data.frame(list[[12]])%>% mutate(season = "winter", LIFE_FORM = "herbaceous")

shrub.fall <- as.data.frame(list[[13]]) %>% mutate(season = "fall", LIFE_FORM = "shrub")
shrub.spring <- as.data.frame(list[[14]])%>% mutate(season = "spring", LIFE_FORM = "shrub")
shrub.summer <- as.data.frame(list[[15]])%>% mutate(season = "summer", LIFE_FORM = "shrub")
shrub.winter <- as.data.frame(list[[16]])%>% mutate(season = "winter", LIFE_FORM = "shrub")

con <- rbind(rbind(conifer.fall, conifer.spring), rbind(conifer.summer, conifer.winter))
hw <- rbind(rbind(hw.fall, hw.spring), rbind(hw.summer, hw.winter))
herb <- rbind(rbind(herb.fall, herb.spring), rbind(herb.summer, herb.winter))
shrub <- rbind(rbind(shrub.fall, shrub.spring), rbind(shrub.summer, shrub.winter))

##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(con, "remoteparts_output/sonoma_seasonal_output_conifer_df.csv")
fwrite(hw, "remoteparts_output/sonoma_seasonal_output_hardwood_df.csv")
fwrite(herb, "remoteparts_output/sonoma_seasonal_output_herbaceous_df.csv")
fwrite(shrub, "remoteparts_output/sonoma_seasonal_output_shrub_df.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

# 5. 
####remotePARTS GLS Spatial Error Model Analysis:

############DATASETS###############################################################
annual_con <- fread("remoteparts_output/sonoma_annual_output_con_df.csv")
annual <- fread("remoteparts_output/sonoma_annual_output_df.csv")
annual_hw <- fread("remoteparts_output/sonoma_annual_output_hardwood_df.csv")
annual_herb <- fread("remoteparts_output/sonoma_annual_output_herb_df.csv")
annual_shrub <- fread("remoteparts_output/sonoma_annual_output_shrub_df.csv")

##############################ANNUAL #################################################
annual <- st_as_sf(annual, coords = c("long", "lat"), crs = 4326)
annual <- get_elev_point(annual)
annual <- annual %>% mutate(long = unlist(map(annual$geometry,1)),lat = unlist(map(annual$geometry,2)))
annual <- as.data.frame(annual)
fitopt <- fitGLS_opt(formula = AR_coef ~ 1 , data = annual, 
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

#############ANNUAL + VEG ################################################
annual_con <- annual_con %>% mutate(LIFE_FORM = "conifer")
annual_hw <- annual_hw %>% mutate(LIFE_FORM = "hardwood")
annual_herb <- annual_herb %>% mutate(LIFE_FORM = "herbaceous")
annual_shrub <- annual_shrub %>% mutate(LIFE_FORM = "shrub")
annual_veg <- rbind(rbind(annual_con, annual_hw), rbind(annual_herb, annual_shrub))

elevs <- annual %>% dplyr::select(long, lat, elevation, elev_units)
annual_veg <- left_join(annual_veg, elevs)

annual_veg_split <- split(annual_veg, annual_veg$LIFE_FORM)

#tried to make this a function but just ran 1-4 manually
  annual_veg <- annual_veg_split[[3]]
  annual_veg <- annual_hw
  annual_veg <- as.data.frame(annual_veg)
fitopt <- fitGLS_opt(formula = AR_coef ~ 1 + lat + long, data = annual_veg, 
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

# 6. 
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


# 7. 
### PLUMAS ANNUAL TRENDS ######################
########### Datasets ###########################################################
#annual trend for whole county:
input_CSV <- plumas
################# Data Preprocessing ###########################################
coords <- input_CSV[, c("long", "lat")]
unique_coordinates <- unique(coords)
colnames(unique_coordinates) <- c("long", "lat")
years <- c(unique(input_CSV$year))
output_dataframe <- data.frame(matrix(nrow = 1302, ncol = 0))

for (x in years){
  yeardata <- input_CSV[input_CSV$year == x,]
  output <- c()
  for (y in seq(1, nrow(unique_coordinates), by = 1)){
    cord <- unique_coordinates[y,]
    data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
    burndays <- nrow(data_subset)
    output <- c(output, burndays)
  }
  output_dataframe[as.character(x)] <- output
}

output_dataframe["long"] <- unique_coordinates$long
output_dataframe["lat"] <- unique_coordinates$lat
########## remotePARTS REML Timeseries Analysis ################################
timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
coords <- as.matrix(output_dataframe[,seq(24:25)])
ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
output_dataframe["AR_coef"] <- ARfit$coefficients[,2]

#12/18: adding back in veg column + elevations
plumasx <- plumas %>% select(lat, long, LIFE_FORM)
plumasx <- unique(plumasx)
output_dataframe <- left_join(output_dataframe, plumasx)
od <- output_dataframe %>% select(long, lat, AR_coef, LIFE_FORM)
od <- st_as_sf(od, coords = c("long", "lat"), crs = 4326)
od <- get_elev_point(od)
od <- od %>% mutate(long = unlist(map(od$geometry,1)),lat = unlist(map(od$geometry,2)))
od <- as.data.frame(od)


##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(output_dataframe, "remoteparts_output/PLUMAS_ANNUAL_TREND.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!


# 8. 
### PLUMAS ANNUAL TREND + VEG ###########################################
########### Datasets ###########################################################
# annual trend by veg type:
input_CSV <- plumas
input_CSV <- split(input_CSV, list(input_CSV$LIFE_FORM))
input_CSV <- list_drop_empty(input_CSV)
input_CSV2 <- input_CSV
################# Data Preprocessing ###########################################


fun <- function(z){
  input_CSV <- input_CSV[[z]]
  coords <- input_CSV[, c("long", "lat")]
  unique_coordinates <- unique(coords)
  colnames(unique_coordinates) <- c("long", "lat")
  years <- c(unique(input_CSV$year))
  time <- nrow(unique_coordinates)
  output_dataframe <- data.frame(matrix(nrow = time, ncol = 0))
  
  for (x in years){
    yeardata <- input_CSV[input_CSV$year == x,]
    output <- c()
    for (y in seq(1, nrow(unique_coordinates), by = 1)){
      cord <- unique_coordinates[y,]
      data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
      burndays <- nrow(data_subset)
      output <- c(output, burndays)
    }
    output_dataframe[as.character(x)] <- output
  }
  
  output_dataframe["long"] <- unique_coordinates$long
  output_dataframe["lat"] <- unique_coordinates$lat
  ### remotePARTS REML Timeseries Analysis #####
  timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
  coords <- as.matrix(output_dataframe[,seq(24:25)])
  ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
  output_dataframe["AR_coef"] <- ARfit$coefficients[,2]
  
  data <- output_dataframe
}

num <- c(1:4)
list <- lapply(num, fun)

conifer <- list[[1]]
hardwood <- list[[2]]
herb <- list[[3]]
shrub <- list[[4]]
##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(conifer, "remoteparts_output/plumas_annual_output_con_df.csv")
fwrite(hardwood, "remoteparts_output/plumas_annual_output_hardwood_df.csv")
fwrite(herb, "remoteparts_output/plumas_annual_output_herb_df.csv")
fwrite(shrub, "remoteparts_output/plumas_annual_output_shrub_df.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

# 9.
##### PLUMAS SEASONAL TRENDS ########################################
########### Datasets #########################################################

# seasonal trends
input_CSV <- plumas
input_CSV <- split(input_CSV, list(input_CSV$season))
input_CSV <- list_drop_empty(input_CSV)
input_CSV2 <- input_CSV
input_CSV <- input_CSV2
################# Data Preprocessing ###########################################

fun <- function(z){
  input_CSV <- input_CSV[[4]]
  coords <- input_CSV[, c("long", "lat")]
  unique_coordinates <- unique(coords)
  colnames(unique_coordinates) <- c("long", "lat")
  years <- c(unique(input_CSV$year))
  output_dataframe <- data.frame(matrix(nrow = 1301, ncol = 0))
  
  for (x in years){
    yeardata <- input_CSV[input_CSV$year == x,]
    output <- c()
    for (y in seq(1, nrow(unique_coordinates), by = 1)){
      cord <- unique_coordinates[y,]
      data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
      burndays <- nrow(data_subset)
      output <- c(output, burndays)
    }
    output_dataframe[as.character(x)] <- output
  }
  
  output_dataframe["long"] <- unique_coordinates$long
  output_dataframe["lat"] <- unique_coordinates$lat
  ### remotePARTS REML Timeseries Analysis #####
  timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
  coords <- as.matrix(output_dataframe[,seq(24:25)])
  ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
  output_dataframe["AR_coef"] <- ARfit$coefficients[,2]
  
  data <- output_dataframe
}

num <- c(1:4)
list <- lapply(num, fun)

fall <- data
spring <- data
summer <- data
winter <- data

#12/18: adding back in veg column + elevations
fall <- fall %>% mutate(season = "fall") %>% select(long, lat, AR_coef, season)
spring <- spring %>% mutate(season = "spring") %>% select(long, lat, AR_coef, season)
summer <- summer %>% mutate(season = "summer") %>% select(long, lat, AR_coef, season)
winter <- winter %>% mutate(season = "winter") %>% select(long, lat, AR_coef, season)
seasons <- rbind(rbind(fall, spring), rbind(summer, winter))

plumasx <- plumas %>% select(lat, long, LIFE_FORM)
plumasx <- unique(plumasx)
output_dataframe <- left_join(seasons, plumasx)
od <- st_as_sf(output_dataframe, coords = c("long", "lat"), crs = 4326)
od <- get_elev_point(od)
od <- od %>% mutate(long = unlist(map(od$geometry,1)),lat = unlist(map(od$geometry,2)))
od <- as.data.frame(od)


##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(output_dataframe, "remoteparts_output/PLUMAS_SEASONAL_TREND.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!



##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(fall, "remoteparts_output/plumas_seasonal_output_fall_df.csv")
fwrite(spring, "remoteparts_output/plumas_seasonal_output_spring_df.csv")
fwrite(summer, "remoteparts_output/plumas_seasonal_output_summer_df.csv")
fwrite(winter, "remoteparts_output/plumas_seasonal_output_winter_df.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!


# 10.
#### PLUMAS SEASONAL + VEG ###############################################
########### Datasets #########################################################

# seasonal trends
input_CSV <- plumas
input_CSV <- split(input_CSV, list(input_CSV$season, input_CSV$LIFE_FORM))
input_CSV <- list_drop_empty(input_CSV)
input_CSV2 <- input_CSV
################# Data Preprocessing ###########################################

fun <- function(z){
  input_CSV <- input_CSV[[z]]
  coords <- input_CSV[, c("long", "lat")]
  unique_coordinates <- unique(coords)
  colnames(unique_coordinates) <- c("long", "lat")
  years <- c(unique(input_CSV$year))
  time <- nrow(unique_coordinates)
  output_dataframe <- data.frame(matrix(nrow = time, ncol = 0))
  
  for (x in years){
    yeardata <- input_CSV[input_CSV$year == x,]
    output <- c()
    for (y in seq(1, nrow(unique_coordinates), by = 1)){
      cord <- unique_coordinates[y,]
      data_subset <- yeardata[yeardata$long == unname(cord[[1]]) & yeardata$lat == unname(cord[[2]]),]
      burndays <- nrow(data_subset)
      output <- c(output, burndays)
    }
    output_dataframe[as.character(x)] <- output
  }
  
  output_dataframe["long"] <- unique_coordinates$long
  output_dataframe["lat"] <- unique_coordinates$lat
  ### remotePARTS REML Timeseries Analysis #####
  timeseries_data <- as.matrix(output_dataframe[,seq(1:23)])
  coords <- as.matrix(output_dataframe[,seq(24:25)])
  ARfit <- fitAR_map(Y = timeseries_data, coords = coords)
  output_dataframe["AR_coef"] <- ARfit$coefficients[,2]
  
  data <- output_dataframe
}

num <- c(1:16)
list <- lapply(num, fun)

conifer.fall <- as.data.frame(list[[1]]) %>% mutate(season = "fall", LIFE_FORM = "conifer")
conifer.spring <- as.data.frame(list[[2]])%>% mutate(season = "spring", LIFE_FORM = "conifer")
conifer.summer <- as.data.frame(list[[3]])%>% mutate(season = "summer", LIFE_FORM = "conifer")
conifer.winter <- as.data.frame(list[[4]])%>% mutate(season = "winter", LIFE_FORM = "conifer")

hw.fall <- as.data.frame(list[[5]]) %>% mutate(season = "fall", LIFE_FORM = "hardwood")
hw.spring <- as.data.frame(list[[6]])%>% mutate(season = "spring", LIFE_FORM = "hardwood")
hw.summer <- as.data.frame(list[[7]])%>% mutate(season = "summer", LIFE_FORM = "hardwood")
hw.winter <- as.data.frame(list[[8]])%>% mutate(season = "winter", LIFE_FORM = "hardwood")
herb.fall <- as.data.frame(list[[9]]) %>% mutate(season = "fall", LIFE_FORM = "herbaceous")

herb.spring <- as.data.frame(list[[10]])%>% mutate(season = "spring", LIFE_FORM = "herbaceous")
herb.summer <- as.data.frame(list[[11]])%>% mutate(season = "summer", LIFE_FORM = "herbaceous")
herb.winter <- as.data.frame(list[[12]])%>% mutate(season = "winter", LIFE_FORM = "herbaceous")

shrub.fall <- as.data.frame(list[[13]]) %>% mutate(season = "fall", LIFE_FORM = "shrub")
shrub.spring <- as.data.frame(list[[14]])%>% mutate(season = "spring", LIFE_FORM = "shrub")
shrub.summer <- as.data.frame(list[[15]])%>% mutate(season = "summer", LIFE_FORM = "shrub")
shrub.winter <- as.data.frame(list[[16]])%>% mutate(season = "winter", LIFE_FORM = "shrub")

con <- rbind(rbind(conifer.fall, conifer.spring), rbind(conifer.summer, conifer.winter))
hw <- rbind(rbind(hw.fall, hw.spring), rbind(hw.summer, hw.winter))
herb <- rbind(rbind(herb.fall, herb.spring), rbind(herb.summer, herb.winter))
shrub <- rbind(rbind(shrub.fall, shrub.spring), rbind(shrub.summer, shrub.winter))

##############!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
fwrite(con, "remoteparts_output/plumas_seasonal_output_conifer_df.csv")
fwrite(hw, "remoteparts_output/plumas_seasonal_output_hardwood_df.csv")
fwrite(herb, "remoteparts_output/plumas_seasonal_output_herbaceous_df.csv")
fwrite(shrub, "remoteparts_output/plumas_seasonal_output_shrub_df.csv")
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

# 11. 
####remotePARTS GLS Spatial Error Model Analysis:

############DATASETS###############################################################
annual_con <- fread("remoteparts_output/plumas_annual_output_con_df.csv") %>% mutate(veg = "con")
annual <- fread("remoteparts_output/plumas_annual_output_df.csv")%>% 
annual_hw <- fread("remoteparts_output/plumas_annual_output_hardwood_df.csv")%>% mutate(veg = "hw")
annual_herb <- fread("remoteparts_output/plumas_annual_output_herb_df.csv")%>% mutate(veg = "herb")
annual_shrub <- fread("remoteparts_output/plumas_annual_output_shrub_df.csv")%>% mutate(veg = "shrub")

annual_con <- annual_con %>%mutate(veg = "con")
annual_hw <- annual_hw %>% mutate(veg = "hw")
annual_herb <- annual_herb %>% mutate(veg = "herb")
annual_shrub <- annual_shrub %>% mutate(veg = "shrub")
annual <- rbind(rbind(annual_con, annual_hw), rbind(annual_herb, annual_shrub))

##############################ANNUAL #################################################
annual <- st_as_sf(annual, coords = c("long", "lat"), crs = 4326)
annual <- get_elev_point(annual)
annual <- annual %>% mutate(long = unlist(map(annual$geometry,1)),lat = unlist(map(annual$geometry,2)))
annual <- as.data.frame(annual)
fitopt <- fitGLS_opt(formula = AR_coef ~ 0 + as.factor(veg), data = annual, 
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

#############ANNUAL + VEG ################################################
annual_con <- annual_con %>% mutate(LIFE_FORM = "conifer")
annual_hw <- annual_hw %>% mutate(LIFE_FORM = "hardwood")
annual_herb <- annual_herb %>% mutate(LIFE_FORM = "herbaceous")
annual_shrub <- annual_shrub %>% mutate(LIFE_FORM = "shrub")
annual_veg <- rbind(rbind(annual_con, annual_hw), rbind(annual_herb, annual_shrub))

elevs <- annual %>% select(long, lat, elevation, elev_units)
annual_veg <- left_join(annual_veg, elevs)

annual_veg_split <- split(annual_veg, annual_veg$LIFE_FORM)

#tried to make this a function but just ran 1-4 manually
annual_veg <- annual_veg_split[[1]]
annual_veg <- as.data.frame(annual_veg)
fitopt <- fitGLS_opt(formula = AR_coef ~ 1+lat+long+elevation, data = annual_veg, 
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

# 12. 
#################SEASONAL##############################################################
seasonal_con <- fread("remoteparts_output/plumas_seasonal_output_conifer_df.csv")
seasonal_fall <- fread("remoteparts_output/plumas_seasonal_output_fall_df.csv")
seasonal_hw <- fread("remoteparts_output/plumas_seasonal_output_hardwood_df.csv")
seasonal_herb <- fread("remoteparts_output/plumas_seasonal_output_herbaceous_df.csv")
seasonal_shrub <- fread("remoteparts_output/plumas_seasonal_output_shrub_df.csv")
seasonal_spring <- fread("remoteparts_output/plumas_seasonal_output_spring_df.csv")
seasonal_summer <- fread("remoteparts_output/plumas_seasonal_output_summer_df.csv")
seasonal_winter <- fread("remoteparts_output/plumas_seasonal_output_winter_df.csv")

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
annual_veg <-spring
annual_veg <- na.omit(annual_veg)
fitopt <- fitGLS_opt(formula = AR_coef ~ 1+lat+long+elevation, data = annual_veg, 
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
split <- split(con, con$season)
annual_veg <- split[[1]]
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
print(output_df)
######################################################################################

#13. re-formatting CSVs for new script

#Sonoma Annual                                     
annual_con <- fread("remoteparts_output/sonoma_annual_output_con_df.csv")
annual_hw <- fread("remoteparts_output/sonoma_annual_output_hardwood_df.csv")
annual_herb <- fread("remoteparts_output/sonoma_annual_output_herb_df.csv")
annual_shrub <- fread("remoteparts_output/sonoma_annual_output_shrub_df.csv")
annual_con <- annual_con %>% mutate(veg = "con")
annual_hw <- annual_hw %>% mutate(veg = "hw")
annual_herb <- annual_herb %>% mutate(veg = "herb")
annual_shrub <- annual_shrub %>% mutate(veg = "shrub")

annual <- rbind(rbind(annual_con, annual_con), rbind(annual_herb, annual_shrub))

annual <- st_as_sf(annual, coords = c("long", "lat"), crs = 4326)
annual <- get_elev_point(annual)
annual <- annual %>% mutate(long = unlist(map(annual$geometry,1)),lat = unlist(map(annual$geometry,2)))
annual <- as.data.frame(annual)
annual <- annual %>% dplyr::select(-geometry)

fwrite(annual, "remoteparts_output/SONOMA_ANNUAL.csv")

#Plumas Annual                                     
annual_con <- fread("remoteparts_output/plumas_annual_output_con_df.csv")
annual_hw <- fread("remoteparts_output/plumas_annual_output_hardwood_df.csv")
annual_herb <- fread("remoteparts_output/plumas_annual_output_herb_df.csv")
annual_shrub <- fread("remoteparts_output/plumas_annual_output_shrub_df.csv")
annual_con <- annual_con %>% mutate(veg = "con")
annual_hw <- annual_hw %>% mutate(veg = "hw")
annual_herb <- annual_herb %>% mutate(veg = "herb")
annual_shrub <- annual_shrub %>% mutate(veg = "shrub")

annual <- rbind(rbind(annual_con, annual_con), rbind(annual_herb, annual_shrub))

annual <- st_as_sf(annual, coords = c("long", "lat"), crs = 4326)
annual <- get_elev_point(annual)
annual <- annual %>% mutate(long = unlist(map(annual$geometry,1)),lat = unlist(map(annual$geometry,2)))
annual <- as.data.frame(annual)
annual <- annual %>% dplyr::select(-geometry)

fwrite(annual, "remoteparts_output/PLUMAS_ANNUAL.csv")

#Sonoma seasonal                                     
annual_con <- fread("remoteparts_output/sonoma_seasonal_output_conifer_df.csv")
annual_hw <- fread("remoteparts_output/sonoma_seasonal_output_hardwood_df.csv")
annual_herb <- fread("remoteparts_output/sonoma_seasonal_output_herbaceous_df.csv")
annual_shrub <- fread("remoteparts_output/sonoma_seasonal_output_shrub_df.csv")
annual_con <- annual_con %>% mutate(veg = "con")
annual_hw <- annual_hw %>% mutate(veg = "hw")
annual_herb <- annual_herb %>% mutate(veg = "herb")
annual_shrub <- annual_shrub %>% mutate(veg = "shrub")

annual <- rbind(rbind(annual_con, annual_con), rbind(annual_herb, annual_shrub))

annual <- st_as_sf(annual, coords = c("long", "lat"), crs = 4326)
annual <- get_elev_point(annual)
annual <- annual %>% mutate(long = unlist(map(annual$geometry,1)),lat = unlist(map(annual$geometry,2)))
annual <- as.data.frame(annual)
annual <- annual %>% dplyr::select(-geometry)

fwrite(annual, "remoteparts_output/SONOMA_SEASONAL.csv")

#Plumas seasonal                                     
annual_con <- fread("remoteparts_output/plumas_seasonal_output_conifer_df.csv")
annual_hw <- fread("remoteparts_output/plumas_seasonal_output_hardwood_df.csv")
annual_herb <- fread("remoteparts_output/plumas_seasonal_output_herbaceous_df.csv")
annual_shrub <- fread("remoteparts_output/plumas_seasonal_output_shrub_df.csv")
annual_con <- annual_con %>% mutate(veg = "con")
annual_hw <- annual_hw %>% mutate(veg = "hw")
annual_herb <- annual_herb %>% mutate(veg = "herb")
annual_shrub <- annual_shrub %>% mutate(veg = "shrub")

annual <- rbind(rbind(annual_con, annual_con), rbind(annual_herb, annual_shrub))

annual <- st_as_sf(annual, coords = c("long", "lat"), crs = 4326)
annual <- get_elev_point(annual)
annual <- annual %>% mutate(long = unlist(map(annual$geometry,1)),lat = unlist(map(annual$geometry,2)))
annual <- as.data.frame(annual)
annual <- annual %>% dplyr::select(-geometry)

fwrite(annual, "remoteparts_output/SONOMA_SEASONAL.csv")




