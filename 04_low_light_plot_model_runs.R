#### 04_low light models
#### 
#### 
# Required packages
require(broom)
require(ggplot2)
require(nls2)
require(MuMIn)
####### Data import section for plot level analysis
#Data import
# plot.fpar <- read.csv("laserquest_master_fpar_plot.csv")
# 
# # designate light regime based on aPAR or above canopy PAR
# plot.fpar$regime <- ifelse(plot.fpar$aPAR_mean >= 1000, "high",
#                            "low")
# 
# plot.fpar$regime <- as.factor(plot.fpar$regime)
# 
# ### Modeling section
# #Subset data for modelling

# high <- subset(plot.fpar, aPAR_mean >= 1000)
# low <- subset(plot.fpar, aPAR_mean < 1000)
# 
# # Define x and y for script

#########################################
##### canopy rugosity

y <- low$fPAR_mean
x <- low$rugosity_mean

# y <- low$fPAR_mean
# x <- low$mean.vai_mean
# ###### Data import section for site level analysis
# x <- master.site$deep.gaps_mean
# y <- master.site$fPAR_mean
# 
# #w/o OSBS
# x <- subset(master.site$mean.max.ht_mean, master.site$site != "OSBS")
# y <- subset(master.site$fPAR_mean, master.site$site != "OSBS")
# 
plot(x, y)

x.label <- "Rc"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rc <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = -1, b = -1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 1, c = 1))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))


## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2, sigmoidal.curve,  logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rectangular hyperbola 2", "sigmoidal curve",  "logarithmic", "power", "log")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}

#####################################################
##### TOP RUGOSITY
##### 


y <- low$fPAR_mean
x <- low$top.rugosity_mean

plot(x, y)

x.label <- "Rt"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = -1, b = -1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = -1, c = 3))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
## Collates model list
model.list <- list(linear.model, rec.hyper, sigmoidal.curve, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "sigmoidal curve", "logarithmic", "power", "log")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)
#create model output date frame first

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}


####################################################################
##### Canopy Porosity
y <- low$fPAR_mean
x <- low$porosity_mean

plot(x, y)

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = -0.1, b = 0.1, c = 0.1))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -1, b = -0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

## Collates model list
model.list <- list(linear.model, rec.hyper,  logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "logarithmic", "power", "log")

# reorganizes

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}



###############################

##### Rumple
y <- low$fPAR_mean
x <- low$rumple_mean

plot(x, y)

x.label <- "Rt"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 0.1, c = 1))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 1, b = 1))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =0.1, k = -1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2, sigmoidal.curve, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rec hyper 2", "s curve", "logarithmic", "power", "log")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #model Name
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}



###############################

##### VAI
y <- low$fPAR_mean
x <- low$mean.vai_mean

plot(x, y)

x.label <- "VAI"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0, b = 10, c = 100))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.4, b = 4.4))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =-1, k = 0.1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

## Collates model list
model.list <- list(linear.model, rec.hyper, sigmoidal.curve,logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "sigmoidal.curve", "logarithmic", "power", "log")


#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #model Name
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}


#####################################################

##### MOCH
y <- low$fPAR_mean
x <- low$mean.max.ht_mean

plot(x, y)

x.label <- "VAI"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 0.1, c = 0.01))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -0.5, b = -0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b =1, k = 1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "logarithmic", "power", "log")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #model Name
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}

#####################################################

##### deep gap fraction
y <- low$fPAR_mean
x <- low$deep.gap.fraction_mean

plot(x, y)

x.label <- "VAI"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = -0.01, b = 0.1, c = 1))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = 10.5, k = 0.01) )
#logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = 1, b = 1))
#power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
#log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2,  sigmoidal.curve)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rec hyper 2","s curve")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #model Name
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}

#####################################################

##### gap fraction
y <- low$fPAR_mean
x <- low$sky.fraction_mean

plot(x, y)

x.label <- "VAI"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = -0.01, b = 0.1, c = 1))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -0.1, b = 0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = 10.5, k = 0.01) )
#logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = 1, b = 1))
#power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
#log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2,  sigmoidal.curve)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rec hyper 2","s curve")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #model Name
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}

#####################################################

#####  CLUMPING INDEX
y <- low$fPAR_mean
x <- low$clumping.index_mean

plot(x, y)

x.label <- "VAI"
y.label <- "fPAR"

# 
df <- data.frame(x, y)
# 
# #################
#model list
log.rt <- lm(log(y) ~ log(x))

plot(log(x), log(y))

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0.001, b = 0.01, c = -0.01))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -5, b = 1))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b = 1, k = 0.01) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = 1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, sigmoidal.curve,  logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "s curve",  "logarithmic", "power", "log")

#### AIC and summary stats
#AIC
lapply(model.list, AIC)
lapply(model.list, AICc)
lapply(model.list, tidy)

# List of R-squared values. Unadjusted.
for (name in names(model.list)) {
  
  #model Name
  
  #R squared
  RSS <- sum(residuals(model.list[[name]])^2)
  TSS <- sum((y - mean(y))^2)
  r2 = 1 - (RSS/TSS)
  print(name)
  print(r2)
}
