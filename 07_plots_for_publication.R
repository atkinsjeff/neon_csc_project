# Plots for publication

#
require(plyr)
require(ggplot2)
require(dplyr)
require(vegan)
require(ggrepel)
require(magrittr)

#adding in palette
library(RColorBrewer)
darkcols <- brewer.pal(10, "Dark2")

#Data import
plot.fpar <- read.csv("./data/laserquest_plot_means_fpar_csc.csv")

# designate light regime based on aPAR or above canopy PAR
plot.fpar$regime <- ifelse(plot.fpar$aPAR_mean >= 1000, "high",
                             "low")

plot.fpar$regime <- as.factor(plot.fpar$regime)

### Modeling section
#Subset data for modelling
high <- subset(plot.fpar, aPAR_mean >= 1000)
low <- subset(plot.fpar, aPAR_mean < 1000)


###########

#new.master <- read.csv("laserquest_plot_means_with_fPAR.CSV")
#custom plot theme for all
theme_new <- function(base_size = 12){
     theme_bw(base_size = base_size) %+replace%
          theme(
               #line = element_line(colour="black"),
               #text = element_text(colour="black"),
               #axis.text = element_text(colour="black", size=8),
               #strip.text = element_text(size=12),
               #legend.key=element_rect(colour=NA, fill =NA),
               panel.grid = element_blank(),   
               # panel.border = element_blank(),
               panel.background = element_rect(fill = "white", colour = "white"), 
               strip.background = element_rect(fill = NA),
               axis.text = element_text( size = 14),
               axis.ticks.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title  = element_text( size = 16, margin = margin(12, unit = "cm"))
          )
     
}
# custom plot label

vai.label =  expression(paste(VAI~(m^2 ~m^-2)))
lai.label =  expression(paste(LAI~(m^2 ~m^-2)))
log.lai.label = expression(paste(log~( LAI~(m^2 ~m^-2))))
r.label = expression("R"[C])
fpar.label = expression(paste(italic("f"), "PAR"))



# break up the light regimes
# 
# high <- subset(master, master$regime == "high")
# low <- subset(master, master$regime == "low")

#Imports high and low from fPAR modeling script

# Canopy Rugosity Model under high light
y <- high$fPAR_mean
x <- high$rugosity_mean
rc.model <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 0.1, b = 1, k = 0.1))
AIC(rc.model)
overview(rc.model)

#coefficients
a <- coef(rc.model)[1] 
b <- coef(rc.model)[2]
k <- coef(rc.model)[3]

rc.fit <- function(x) ifelse(x >= 0.05 & x <= 35, (a / (1 + b * exp(-k * x))), NA)

# Canopy Rugosity under low light
y <- low$fPAR_mean
x <- low$rugosity_mean
rc.low.model <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
AIC(rc.low.model)
overview(rc.low.model)
#coefficients
a <- coef(rc.low.model)[1] 
b <- coef(rc.low.model)[2]

rc.low.fit <- function(x) ifelse(x >= 3 & x<= 30, (a * x) / (b + x), NA)
     { (a * x) / (b + x)}

x11(width = 4, height = 4)
rc.plot <- ggplot(plot.fpar, aes(x = rugosity_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     # geom_point( data = low, aes( x = rugosity_mean, y = fPAR_mean), size = 3, shape = 21)+
     theme_new()+
     ylim(c(0, 1))+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     xlim(c(0,45))+
     xlab(r.label)+
     ylab(fpar.label)+
     stat_function(fun = rc.fit, col = "blue", size = 1)+
     stat_function(fun = rc.low.fit, col = "blue", size = 1, linetype = "dashed")+
     theme(legend.position="none")


#####################################

# TOP RUGOSITY

#Top Rugosity
#Top Rugosity under high light
y <- high$fPAR_mean
x <- high$top.rugosity_mean
rt.model <- lm( y ~ x)

AIC(rt.model)
RSS <- sum(residuals(rt.model)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)
summary(rt.model)

rt.fit <- function(x) 

x11(width = 4, height = 4)
rt.plot <- ggplot(plot.fpar, aes(x = top.rugosity_mean, y = fPAR_mean, fill = regime))+
  geom_point(size = 3, shape = 21)+
  scale_fill_manual(values = c("black", "white"))+
  theme_new()+
  xlim(c(0,15))+
  ylim(c(0, 1))+
  xlab(expression("R"[T]))+
  ylab(fpar.label)+
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  geom_smooth(data = subset(plot.fpar, plot.fpar$regime == "high"), method = "lm", size = 1, se = FALSE, color = "blue")+
  theme(legend.position="none")



# Canopy Porosity

# Canopy Porosity under high light
y <- high$fPAR_mean
x <- high$porosity_mean

pc.model <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1.62, b = 1, c = -1.25))

#coefficients
a <- coef(pc.model)[1] 
b <- coef(pc.model)[2]
c <- coef(pc.model)[3]

#pc.high.fit <- function(x) ifelse(x >= 0.5 & x <= 0.9, a + ((b * x) / (c + x)), NA )
pc.high.fit <- function(x) ifelse(x >= 0.5 & x <= 0.9, 1.622 + ((0.5214 * x) / (-1.2482 + x)), NA )

AIC(pc.model)
overview(pc.model)

# Canopy Porosity under  low light
y <- low$fPAR_mean
x <- low$porosity_mean
pc.low.model <-nls(y ~ a * (x^b), start = list(a = 1, b = 1))

AIC(pc.low.model)
overview(pc.low.model)

#coefficients
a <- coef(pc.low.model)[1] 
b <- coef(pc.low.model)[2]
c <- coef(pc.low.model)[3]


pc.low.fit <- function(x) ifelse(x >= 0.5 & x<= 0.9,  a * (x^b), NA)

x11(width = 4, height = 4)
pc.plot <- ggplot(plot.fpar, aes(x = porosity_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     xlim(0.4, 1)+
     ylim(c(0, 1))+
     xlab(expression("P"[C]))+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     stat_function(fun = pc.high.fit, col = "blue", size = 1, linetype = "solid")+   
     stat_function(fun = pc.low.fit, col = "blue", size = 1, linetype = "dashed")+
     theme(legend.position="none")


#########################
# RUMPLE

# Rumple

y <- high$fPAR_mean
x <- high$rumple_mean

rump.model <-nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
AIC(rump.model)
overview(rump.model)

#coefficients
a <- coef(rump.model)[1] 
b <- coef(rump.model)[2]

rump.fit <- function(x) ifelse(x >= 2 & x <= 10, (1 / (1 + exp(-a * (x - b)))), NA)

x11(width = 4, height = 4)
rump.plot <- ggplot(plot.fpar, aes(x = rumple_mean, y = fPAR_mean, fill = regime))+
  geom_point(size = 3, shape = 21)+
  scale_fill_manual(values = c("black", "white"))+
  theme_new()+
  xlim(c(0,15))+
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  xlab("Rumple")+
  ylab(fpar.label)+
  stat_function(fun = rump.fit, col = "blue", size = 1)+
  theme(legend.position="none")



#################################
# VAI

# Mean VAI
# sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))

y <- high$fPAR_mean
x <- high$mean.vai_mean

vai.model <-nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
AIC(vai.model)
overview(vai.model)

#coefficients
a <- coef(vai.model)[1] 
b <- coef(vai.model)[2]

vai.fit <- function(x) {1 / (1 + exp(-a * (x - b)))}

#VAI under low light

y <- low$fPAR_mean
x <- low$mean.vai_mean

vai.low.model <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
AIC(vai.low.model)
overview(vai.low.model)

#coefficients
c <- coef(vai.low.model)[1] 
d <- coef(vai.low.model)[2]

vai.low.fit <- function(x) { 1 / (1 + exp(-c * (x - d)))}

x11(width = 4, height = 4)
vai.plot <- ggplot(plot.fpar, aes(x = mean.vai_mean, y = fPAR_mean, fill = regime))+
  geom_point(size = 3, shape = 21)+
  scale_fill_manual(values = c("black", "white"))+
  theme_new()+
  xlim(c(0,8))+
  ylab(fpar.label)+
  xlab("VAI")+
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  stat_function(fun = vai.fit, col = "blue", size = 1)+
  stat_function(fun = vai.low.fit, col = "blue", size = 1, linetype = "dashed")+
  theme(legend.position="none")

##################################
# MOCH

# MOCH

y <- high$fPAR_mean
x <- high$mean.max.ht_mean

#moch.model <-nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b = 10, k = 0.1))
moch.model <-nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b = 10, k = 0.1))

AIC(moch.model)
overview(moch.model)
#coefficients
a <- coef(moch.model)[1] 
b <- coef(moch.model)[2]
k <- coef(moch.model)[3]

moch.fit <- function(x) ifelse(x >=2 & x <=25, a / (1 + b * exp(-k * x)), NA)

x11(width = 4, height = 4)
moch.plot <- ggplot(plot.fpar, aes(x = mean.max.ht_mean , y = fPAR_mean, fill = regime))+
  geom_point(size = 3, shape = 21)+
  scale_fill_manual(name = "Light Regime",
                    breaks = c("high", "low"),
                    labels = c("High", "Low"),values = c("black", "white"))+
  theme_new()+
  xlim(c(0,25))+
  ylim(c(0, 1))+
  xlab("MOCH")+
  ylab("fPAR")+
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  stat_function(fun = moch.fit, col = "blue", size = 1)+
  theme(legend.position="none")

#########################################
# deep gap fraction
# Deep Gaps (ADJUSTED)

y <- high$fPAR_mean
x <- high$deep.gap.fraction_mean

#dg.model <-nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -6, b = 0.1, k = -1))
dg.model <- nls(y ~ a + ((b * x) / (c + x)), start = list( a = 0.1, b = 1, c =  1) )
AIC(dg.model)
overview(dg.model)

#coefficients
a <- coef(dg.model)[1] 
b <- coef(dg.model)[2]
k <- coef(dg.model)[3]

#dg.fit <- function(x) {a / (1 + b * exp(-k * x))}
#dg.fit <- function(x) ifelse(x >= 0 & x <=0.8, 0.9379 + ((-1.5107 * x) / (0.6624 + x)))

dg.fit <- function(x) {0.9379 + ((-1.50454 * x) / (0.66598 + x))}

x11(width = 4, height = 4)
dg.plot <- ggplot(plot.fpar, aes(x = deep.gap.fraction_mean, y = fPAR_mean, fill = regime))+
  geom_point(size = 3, shape = 21)+
  scale_fill_manual(values = c("black", "white"))+
  theme_new()+
  xlim(c(0,1))+
  ylim(c(0, 1))+
  xlab("Deep Gap Fraction")+
  ylab(fpar.label)+
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  stat_function(fun = dg.fit, col = "blue", size = 1)+
  theme(legend.position="none")

##################################
# Gap Fraction

# Sky/ Gap Fraction
x <- high$sky.fraction_mean / 100
y <- high$fPAR_mean

gf.model <- lm( y ~ x)
summary(gf.model)

# gap fraction under low light
x <- low$sky.fraction_mean / 100
y <- low$fPAR_mean

gf.low.model <- lm( y ~ x)
summary(gf.low.model)

summary(dg.model)
x11(width = 4, height = 4)
gf.plot <- ggplot(plot.fpar, aes(x = sky.fraction_mean / 100, y = fPAR_mean, fill = regime))+
  geom_point(size = 3, shape = 21)+
  scale_fill_manual(values = c("black", "white"))+
  theme_new()+
  xlim(c(0, 1))+
  ylim(c(0, 1))+
  xlab(expression(Theta))+
  ylab(fpar.label)+
  scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
  geom_smooth(data = subset(plot.fpar, plot.fpar$regime == "high"), method = "lm", size = 1, se = FALSE, color = "blue")+
  geom_smooth(data = subset(plot.fpar, plot.fpar$regime == "low"), method = "lm", size = 1, se = FALSE, color = "blue", linetype = "dashed")+
  theme(legend.position="none")

####################
# Clumping Index
y <- high$fPAR_mean
x <- high$clumping.index_mean

ci.model <- nls(y ~ (a + ((b * x) / (c + x))), start = list( a = -1, b = 0.1, c =  -1))
AIC(ci.model)
overview(ci.model)

#coefficients
a <- coef(ci.model)[1] 
b <- coef(ci.model)[2]
c <- coef(ci.model)[3]
ci.fit <- function(x) ifelse(x >= 0.72 & x<= 0.99,  a + ((b * x) / (c + x)), NA)



# Clumping index under low light
y <- low$fPAR_mean
x <- low$clumping.index_mean
ci.low.model <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -1, b = 1))

AIC(ci.low.model)
overview(ci.low.model)

#coefficients
d <- coef(ci.low.model)[1] 
e <- coef(ci.low.model)[2]


ci.low.fit <- function(x) ifelse(x >= 0.72 & x<= 0.99, (1 / (1 + exp(-d * (x - e)))), NA)


x11(width = 4, height = 4)
ci.plot <- ggplot(plot.fpar, aes(x = clumping.index_mean, y = fPAR_mean, fill = regime))+
     geom_point(size = 3, shape = 21)+
     scale_fill_manual(values = c("black", "white"))+
     theme_new()+
     ylim(c(0, 1))+
     xlim(c(0.7, 1))+
     xlab(expression(Omega))+
     ylab(fpar.label)+
     scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0, 1))+
     stat_function(fun = ci.fit, col = "blue", size = 1)+
     stat_function(fun = ci.low.fit, col = "blue", size = 1, linetype = "dashed")+
     theme(legend.position="none")


################################################################################
#######SITE FACET PLOTS

master.site <- read.csv("./data/laserquest_site_means_fpar_csc.csv")
colnames(master.site)[3] <- "fPAR_mean"

####### SITE LEVEL
####### SITE LEVEL
# Light!

require(plyr)
require(ggplot2)
require(dplyr)
require(vegan)
require(magrittr)
require(ggrepel)

#adding in palette
library(RColorBrewer)
darkcols <- brewer.pal(10, "Dark2")


# SITE MODELLING
#RUGOSITY
#HIGH
y <- master.site$fPAR_mean
x <- master.site$rugosity_mean


m.rc.site<- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))

#model stats
overview(m.rc.site)
AIC(m.rc.site)

fit.rc.site <- function(x) {(1.08604 * x) / (3.43236 + x)}

x11(width = 4, height = 4)
site.rc <- ggplot(master.site, aes(x = rugosity_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(rugosity_mean, fPAR_mean, label = siteID))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0, 45))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(expression("R"[C]))+
     ylab(fpar.label)+
     stat_function(fun = fit.rc.site, col = "blue", size = 1)
# stat_function(fun = function(x) (0.97624 * x) / (1.74132 + x), colour = "dark grey", size = 1.25)

# Top Rugosity
x11(width = 4, height = 4)
site.rt <- ggplot(master.site, aes(x = top.rugosity_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0, 15))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(expression("R"[T]))+
     ylab(fpar.label)

# Porosity
summary(lm(master.site$fPAR ~ master.site$porosity_mean))

x11(width = 4, height = 4)
site.pc <- ggplot(master.site, aes(x = porosity_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(porosity_mean, fPAR_mean, label = siteID), force = 10)+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0.6, 1))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab(expression("P"[C]))+
     ylab(fpar.label)+
     geom_smooth(method = lm, se = FALSE, col = "blue", size = 1)

# Rumple

y <- master.site$fPAR_mean
x <- master.site$rumple_mean


m.rump.site<- nls(y ~ 1 / (1 + exp(a * (x - b))), start = list(a = 0.1, b = 1))
overview(m.rump.site)
AIC(m.rump.site)

x11(width = 4, height = 4)
site.rump <- ggplot(master.site, aes(x = rumple_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     xlim(c(0, 15))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     theme_new()+
     xlab("Rumple")+
     ylab("fPAR")+
     geom_smooth(method = "nls",
                 formula = y ~ 1 / (1 + exp(a * (x - b))),
                 method.args = list(start = list(a = 0.1, b = 1)),
                 se = FALSE,
                 color = "blue",
                 size = 1)

# VAI
# sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))

y <- master.site$fPAR_mean
x <- master.site$mean.vai_mean

#m.vai.site <-nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.1, b = 1))
m.vai.site <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 0.1) )
overview(m.vai.site)
AIC(m.vai.site)

#coefficients
a <- coef(vai.model)[1] 
b <- coef(vai.model)[2]

vai.fit <- function(x) {a / (1 + b * exp(-k * x))}

x11(width = 4, height = 4)
site.vai <- ggplot(master.site, aes(x = mean.vai_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(2, 8))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab("VAI")+
     ylab(fpar.label)+
     geom_smooth(method = "nls",
                 formula = y ~ (1 / (1 + exp(-a * (x - b)))),
                 method.args = list(start = list(a = 0.1, b = 1)),
                 se = FALSE,
                 color = "blue",
                 size = 1)
####################################
#MOCH
y <- master.site$fPAR_mean
x <- master.site$mean.max.ht_mean
# 
site.moch.model <-nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))

overview(site.moch.model)
AIC(site.moch.model)

#coefficients
a <- coef(site.moch.model)[1] 
b <- coef(site.moch.model)[2]

site.moch.fit <- function(x) { a + (b * log(x))}

# AIC(site.moch.model)

x11(width = 4, height = 4)
site.moch <- ggplot(master.site, aes(x = mean.max.ht_mean , y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     theme_new()+
     xlim(c(5,25))+
     ylim(c(0, 1))+
     xlab("MOCH")+
     ylab("fPAR")+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
    stat_function(fun = site.moch.fit, col = "blue", size = 1)


##################################
# Deep Gap Fraction. 
summary(lm(master.site$fPAR_mean ~ master.site$deep.gap.fraction_mean))

x11(width = 4, height = 4)

site.dg <- ggplot(master.site, aes(x = deep.gap.fraction_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     theme_new()+
     xlim(c(0, 0.45))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     xlab("Deep Gap Fraction")+
     ylab(fpar.label)+
     geom_smooth(method = "lm", size = 1, color = "blue", se = FALSE)

# Gap Fraction/Sky Fraction
x11(width = 4, height = 4)
summary(lm(master.site$fPAR_mean ~ master.site$sky.fraction_mean))

site.gf <- ggplot(master.site, aes(x = sky.fraction_mean / 100, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     theme_new()+
     xlim(c(0, 0.6))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     scale_x_continuous(breaks = seq(0, 0.6, 0.3), limits = c(0, 0.6))+
     xlab(expression(Theta))+
     ylab(fpar.label)+
     geom_smooth(method = "lm", size = 1, color = "blue", se = FALSE)

# Clumping Index!
# x <- master.site$clumping.index_mean
# y <- master.site$fPAR_mean
# m.site.ci <- nls(y ~ a + (b * log(x)), start = list(a = -0.1, b = 1))
# AIC(m.site.ci)

x11(width = 4, height = 4)
site.ci <- ggplot(master.site, aes(x = clumping.index_mean, y = fPAR_mean))+
     geom_point(size = 2)+
     geom_text_repel(data = master.site, aes(label = siteID))+
     #scale_color_manual(values=cbPalette)+
     #scale_color_brewer(palette="Dark2")+
     #geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
     #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
     theme_new()+
     xlim(c(0.8, 1))+
     scale_y_continuous(breaks = seq(0.2, 1, 0.2), limits = c(0.2, 1))+
     scale_x_continuous(breaks = seq(0.8, 1, 0.1), limits = c(0.8, 1))+
     xlab(expression(Omega))+
     ylab(fpar.label)
