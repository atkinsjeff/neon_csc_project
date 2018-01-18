## Box plots of variables in Appendix

require(ggplot2)


# custom plot theme
theme_new <- function(base_size = 12){
  theme_bw(base_size = base_size) %+replace%
    theme(
      #line = element_line(colour="black"),
      #text = element_text(colour="black"),
      #axis.text = element_text(colour="black", size=8),
      #strip.text = element_text(size=12),
      legend.key=element_rect(colour=NA, fill =NA),
      panel.grid = element_blank(),   
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", colour = "white"), 
      strip.background = element_rect(fill = NA),
      axis.text = element_text( size = 14),
      axis.title  = element_text( size = 16, margin = margin(12, unit = "cm")),
      legend.title=element_blank()
    )
  
}

# custom plot label

vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))



# read in data  (change to figshare later)
site.data <- read.csv("./data/laserquest_pcl_site_means.csv")
plot.means


# mean leaf height
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = mean.height_mean)) +
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("Mean Leaf Height (m)")

# mean outer canopy height
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = mean.max.ht_mean)) +
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("Mean Outer Canopy Height (m)")

# canopy porosity
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = porosity_mean)) +
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("Canopy Porosity")

# clumping index
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = clumping.index_mean)) +
  geom_boxplot()+
  theme_classic()+
  ylim(0.7, 1)+
  xlab("")+
  ylab("Clumping Index")

# canopy rugosity
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = rugosity_mean)) +
  geom_boxplot()+
  theme_classic()+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("Canopy Rugosity")

# Top rugosity
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = top.rugosity_mean)) +
  geom_boxplot()+
  theme_classic()+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("Top Rugosity")

# Rumple
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = rumple_mean)) +
  geom_boxplot()+
  theme_classic()+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("Rumple")

# Deep Gap Fraction
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = deep.gap.fraction_mean)) +
  geom_boxplot()+
  theme_classic()+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("Deep Gap Fraction")

# Gap Fraction
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = sky.fraction_mean)) +
  geom_boxplot()+
  theme_classic()+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("Gap Fraction")

# Mean VAI
x11(width = 6, height = 3)
ggplot(plot.means, aes(x = siteID, y = mean.vai_mean)) +
  geom_boxplot()+
  theme_classic()+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("VAI")

  
  ### fpar
  plot.w.light <- read.csv("./data/laserquest_plot_means_fpar_csc.csv")
  
  x11(width = 6, height = 3)
  ggplot(plot.w.light, aes(x = siteID, y = mean)) +
    geom_boxplot()+
    theme_classic()+
    # ylim(0.7, 1)+
    xlab("")+
    ylab("fPAR")

  
