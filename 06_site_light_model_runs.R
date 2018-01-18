#06_site_light_model_runs

# Required packages
require(broom)
require(ggplot2)
require(nls2)
require(MuMIn)
site.means <- read.csv("./data/laserquest_site_means_fpar_csc.csv")


#########################################
##### canopy rugosity

y <- site.means$fPAR_mean
x <- site.means$rugosity_mean


plot(x, y)

df <- data.frame(x, y)

#model list
#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = -1, b = -1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 1, c = 1))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))


## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2, sigmoidal.curve, log.growth,  logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rectangular hyperbola 2", "sigmoidal curve", "log growth", "logarithmic", "power", "log")

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


y <- site.means$fPAR_mean
x <- site.means$top.rugosity_mean

plot(x, y)

df <- data.frame(x, y)

#model list
#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = -1, b = -1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0, b = 0.1, c = 2))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))


## Collates model list
model.list <- list(linear.model, rec.hyper, sigmoidal.curve,  logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola",  "sigmoidal curve",  "logarithmic", "power", "log")

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

####################################################################
##### Canopy Porosity
y <- site.means$fPAR_mean
x <- site.means$porosity_mean

plot(x, y)

# 
df <- data.frame(x, y)
# 
#model list
#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0, b = 0.38, c = -0.8))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 1, b = 1))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = 0.38, k = 0.38) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))


## Collates model list
model.list <- list(linear.model, rec.hyper, rec.hyper2,   logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "rectangular hyperbola 2",  "logarithmic", "power", "log")

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
y <- site.means$fPAR_mean
x <- site.means$rumple_mean

plot(x, y)

# 
df <- data.frame(x, y)

#model list

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 1.5, c = 5))
sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 1, b = 1))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = 1, k = -1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, sigmoidal.curve, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola",  "s curve", "logarithmic", "power", "log")

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
y <- site.means$fPAR_mean
x <- site.means$mean.vai_mean

plot(x, y)

df <- data.frame(x, y)
#model list
#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0, b = 10, c = 100))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = -1, b = -1))
log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b =1, k = 0.1) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = -1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

## Collates model list
model.list <- list(linear.model, rec.hyper, log.growth, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "log grwoth", "logarithmic", "power", "log")


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
y <- site.means$fPAR_mean
x <- site.means$mean.max.ht_mean

plot(x, y)

df <- data.frame(x, y)

#model list

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 1, c = 10))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = -0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b = 6, k = 0.1) )
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
y <- site.means$fPAR_mean
x <- site.means$deep.gap.fraction_mean

plot(x, y)
df <- data.frame(x, y)
#model list
#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 1, c = -11))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.5, b = 0.5))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = 10.5, k = 0.01) )
#logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = 1, b = 1))
#power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
#log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola")

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
y <- site.means$fPAR_mean
x <- site.means$sky.fraction_mean

plot(x, y)
df <- data.frame(x, y)
#model list

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 1, b = 0.5, c = -0.5))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 1, b = 1))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = -1, b = 10.5, k = 0.01) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = 1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper, logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola", "logarithmic","power", "log")

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
y <- site.means$fPAR_mean
x <- site.means$clumping.index_mean

plot(x, y)
# 
df <- data.frame(x, y)

#### Michaelis Menten equation ### generating some data
# building a Self Start object, finding the optimal values
print(getInitial(y ~ SSmicmen(x, max(y), 1), data = df), digits = 3)  

linear.model        <- lm(y ~ x)
rec.hyper           <- nls(y ~ (a * x) / (b + x), start = list(a = 1, b = 1))
#rec.hyper2          <- nls(y ~ a + ((b * x) / (c + x)), start = list(a = 0.001, b = 0.01, c = -0.01))
#sigmoidal.curve     <- nls(y ~ (1 / (1 + exp(-a * (x - b)))), start = list(a = 0.05, b = 0.05))
#log.growth          <- nls(y ~ a / (1 + b * exp(-k * x)), start = list(a = 1, b = 1, k = 0.01) )
logarithmic         <- nls(y ~ a + (b * log(x)), start = list(a = 1, b = 1))
power               <- nls(y ~ a * (x^b), start = list(a = 1, b = 1))
log                 <- lm(y ~ log(x))

# 
# 
## Collates model list
model.list <- list(linear.model, rec.hyper,  logarithmic, power, log)

#names the model items
names(model.list) <- c("linear.model","rectangular hyperbola",  "logarithmic", "power", "log")

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

