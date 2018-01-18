#Plot and site means
#

require(plyr)
require(dplyr)
require(tidyr)

# Importing the pcl transect data
pcl <- read.csv("./data/laserquest_pcl_transects_2018.csv")


# # pcl means
pcl %>%  select(-c(filename, transectID, transect.length)) %>%
  group_by(plotID, siteID) %>% 
  summarise_all(funs(mean, sd, var, min, max)) -> plot.means

plot.means <- data.frame(plot.means)

#write to file
write.csv(plot.means, "./data/laserquest_pcl_plot_means.csv")


### SITE MEANS
pcl %>% select(-c(filename, transectID, plotID, transect.length)) %>% 
  group_by(siteID) %>% 
  summarise_all(funs(mean, sd, var, min, max)) -> site.means

site.means <- data.frame(site.means)

str(site.means)

#write to file
write.csv(site.means, "./data/laserquest_pcl_site_means.csv")



### LIGHT DATA

light <- read.csv("./data/light_data.csv")

# Renaming some variables
light <- plyr::rename(light, c( "Average.Above.PAR" = "aPAR", "Average.Below.PAR" = "bPAR",  "Leaf.Area.Index..LAI." = "LAI"))

# calculate fPAR - the fraction of photosynthetically availble radiation absorbed by the canopy
light$fPAR <- 1 - (light$bPAR / light$aPAR)

# plot means
light %>% select(plotID, siteID, fPAR, aPAR, bPAR, LAI) %>%
  group_by(plotID, siteID) %>%
  summarise_all(funs(mean, sd, var, min, max)) -> plot.light.means
  
plot.light.means <- data.frame(plot.light.means)

# site means
light %>% select(siteID, fPAR) %>%
  group_by(siteID) %>%
  summarise_all(funs(mean)) -> site.light.means
  
site.light.means <- data.frame(site.light.means)

site.w.light <- merge(site.light.means, site.means, by = c("siteID"))
plot.w.light <- merge(plot.light.means, plot.means, by = c("plotID", "siteID"))

write.csv(plot.w.light, "./data/laserquest_plot_means_fpar_csc.csv")
write.csv(site.w.light, "./data/laserquest_site_means_fpar_csc.csv")
