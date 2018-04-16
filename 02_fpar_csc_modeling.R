# 03_fpar_modelling
require(plyr)
require(dplyr)
require(ggplot2)

## renaming dfs to coincide with analysis scripts

master.plot <- plot.w.light
master.site <- site.w.light

## coding light regime classification
master.plot$regime <- ifelse(master.plot$aPAR_mean >= 1000, "high",
                             "low")

master.plot$regime <- as.factor(master.plot$regime)
high2 <- subset(master.plot, aPAR_mean >= 1000)
low <- subset(master.plot, aPAR_mean < 1000)

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




### Making functions

##### RUGOSITY

# Direct Light modeling for HIGH light
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- high$fPAR_mean
x <- high$rugosity_mean

# 1
m.rugosity <- nls(y ~ a / (1 + (b * exp(-k * x))), start = list(a = 1, b = 1, k = 1))
m.rugosity <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 0.08, b = -0.9, k = 0.01) )
summary(m.rugosity)


# RSS <- sum(residuals(m.rugosity)^2)
# TSS <- sum((y - mean(y))^2)
# R.square <- 1 - (RSS/TSS)
# print(R.square)
AIC(m.rugosity)
overview(m.rugosity)



##### Top Rugosity
# 2
x <- high$top.rugosity_mean
# m.top <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
m.lm.top <- lm(y ~ x)

# summary(m.top)
summary(m.lm.top)


# plot w/ coefficents
x11()# plot w/ coefficents
x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.rugosity)[1]
b <- coef(m.rugosity)[2]
k <- coef(m.rugosity)[3]
curve( (a / (1 + (b * exp(-k * x)))), col = "blue", add = TRUE)
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve(a + (b * x) / (c + x), col = "blue", add = TRUE)
### LOW LIGHT
y <- low$fPAR_mean
x <- low$rugosity_mean
m.low <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))

x11()
plot(x, y, xlab = "Canopy Rugosity (m)", ylab = "fPAR")
a <- coef(m.direct)[1]
b <- coef(m.direct)[2]
curve((a * x) / (b + x), col = "blue", add = TRUE)


RSS <- sum(residuals(m.low)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

fpar.high <- function(x) {(1.075147 * x) / ( 3.085004+ x)}
fpar.low <- function(x) {(0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean)}

x11(width = 5, height = 5)
ggplot(high, aes(x = rugosity_mean, y = fPAR_mean))+
  geom_point(size = 4, shape = 21)+
  theme_new()+
  xlab(r.label)+
  ylab("fPAR")+
  stat_function(fun = fpar.high, col = "blue", size = 1)+
  geom_point(data = low, aes(x = rugosity_mean, y = fPAR_mean), size = 4, color = "black")+
  stat_function(fun = function(x) (0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean), col = "red")

#### VAI
y <- high$fPAR_mean
x <- high$mean.vai_mean


m.vai <- lm(y ~ x)
summary(m.vai)


RSS <- sum(residuals(m.direct)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)

x11()
ggplot(aes(x = x, y = y))+
  geom_point(size = 2, shape = 21)+
  theme_classic()+
  xlab("Rugosity (m)")+
  ylab("fPAR")+
  geom_smooth(method = "lm")


##### MEAN VAI

# Direct Light modeling for HIGH light
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- high$fPAR_mean
x <- high$mean.vai_mean


m.vai.high <- lm(y ~ x)
summary(m.vai.high)


x11(width = 5, height = 5)
ggplot(high, aes(x = mean.vai_mean, y = fPAR_mean))+
  geom_point(size = 4, shape = 21)+
  theme_new()+
  xlab(vai.label)+
  ylab("fPAR")+
  geom_smooth(method = "lm", col = "red", size = 1, se = FALSE)

x11()
plot(log(high$rugosity_mean), resid(m.vai.high))

summary(lm(resid(m.vai.high) ~ log(high$rugosity_mean)))
# x11()
# ggplot(master.site, aes(x = rugosity_mean, y = fPAR,  color = site))+
#      geom_point(size = 5)+
#      #geom_text()+
#      #scale_color_manual(values=cbPalette)+
#      #scale_color_brewer(palette="Dark2")+
#      geom_errorbarh(aes(xmin =rugosity_mean - rugosity_sd, xmax = rugosity_mean + rugosity_sd)) + 
#      #geom_errorbar(aes(ymin = fPAR_mean - fPAR_sd, ymax = fPAR_mean + fPAR_sd))+
#      theme_new()+
#      ylab("fPAR ")+
#      xlab("Rugosity (m)")+
#      stat_function(fun = function(x) (0.97624 * x) / (1.74132 + x), colour = "dark grey", size = 1.25)


### RUMPLE
##### RUGOSITY

# Direct Light modeling for HIGH light
# model should be fPAR = a * x / (b + x) ...Michaelis-Menten
y <- low$fPAR_mean
x <- low$rugosity_mean


m.light <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
summary(m.light)


RSS <- sum(residuals(m.light)^2)
TSS <- sum((y - mean(y))^2)
R.square <- 1 - (RSS/TSS)
print(R.square)


fpar.rump <- function(x) {(2.3478 * x) / ( 10.4647 + x)}
fpar.low <- function(x) {(0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean)}

x11(width = 5, height = 5)
ggplot(high, aes(x = rugosity_mean, y = fPAR_mean))+
  geom_point(size = 4, shape = 21)+
  theme_new()+
  xlab(r.label)+
  ylab("fPAR")+
  stat_function(fun = fpar.high, col = "blue", size = 1)+
  geom_point(data = low, aes(x = rugosity_mean, y = fPAR_mean), size = 4, color = "black")+
  stat_function(fun = function(x) (0.89777 * low$rugosity_mean) / ( 1.16947 + low$rugosity_mean), col = "red")


m.multi <- lm(fPAR_mean ~ mean.vai_mean * rugosity_mean * rumple_mean, data = master.site)


####
####
m.vai <- lm(rugosity_mean ~ mean.vai_mean, data = high)
x11(width = 5, height = 5)
ggplot(master.plot, aes(x = rugosity_mean, y = fPAR_mean, color = regime ))+
  geom_point(size = 4, shape = 21)+
  theme_new()+
  xlab(r.label)+
  ylab("fPAR")
