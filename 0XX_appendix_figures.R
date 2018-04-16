## Box plots of variables in Appendix

require(ggplot2)


# custom plot theme
theme_new2 <- function(base_size = 12){
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
      axis.text = element_text( size = 12),
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
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = mean.height_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
  axis.title  = element_text( size = 16))+
  xlab("")+
  ylab("H (m)")

# mean outer canopy height
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = mean.max.ht_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
    xlab("")+
  ylab("MOCH (m)")

# canopy porosity
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = porosity_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
    xlab("")+
  ylab(expression("P"[C]))

# clumping index
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = clumping.index_mean)) +
  geom_boxplot()+
  theme_classic()+
  ylim(0.7, 1)+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  xlab("")+
  ylab(expression(Omega))

# canopy rugosity
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = rugosity_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  xlab("")+
  ylab(expression("R"[C]))

# Top rugosity
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = top.rugosity_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  xlab("")+
  ylab(expression("R"[T]))

# Rumple
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = rumple_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  xlab("")+
  ylab("Rumple")

# Deep Gap Fraction
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = deep.gap.fraction_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  xlab("")+
  ylab("DGF")

# Gap Fraction
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = sky.fraction_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  xlab("")+
  ylab(expression(Theta))

# Mean VAI
x11(width = 7, height = 3)
ggplot(plot.means, aes(x = siteID, y = mean.vai_mean)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text = element_text( size = 12),
        axis.title  = element_text( size = 16))+
  # ylim(0.7, 1)+
  xlab("")+
  ylab("VAI")

  
  ### fpar
  plot.w.light <- read.csv("./data/laserquest_plot_means_fpar_csc.csv")
  
  x11(width = 7, height = 3)
  ggplot(plot.w.light, aes(x = siteID, y = fPAR_mean)) +
    geom_boxplot()+
    theme_classic()+
    theme(axis.text = element_text( size = 12),
    axis.title  = element_text( size = 16))+
    xlab("")+
    ylab("fPAR")

  
