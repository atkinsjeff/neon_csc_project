require(MASS)
require(leaps)

str(site.means)

#chris.site <- read.csv("./data/step_wise_site.csv")
chris.site <- read.csv("./data/laserquest_pcl_site_means_chris.csv")
chris.plot <- read.csv("./data/laserquest_pcl_plot_means_chris.csv")

chris.site$log.mean.ht <- log(chris.site$mean.height_mean)
chris.site$log.mode.el <- log(chris.site$mode.el_mean)
chris.site$log.ht.2 <- log(chris.site$height.2_mean)
chris.site$log.max.el <- log(chris.site$max.el_mean)
chris.site$log.max.can <- log(chris.site$max.can.ht_mean)
chris.site$log.vai <- log(chris.site$mean.vai_mean)
# chris.site$log.dg <- log(chris.site$deep.gaps_mean)
# chris.site$log.dgf <- log(chris.site$deep.gap.fraction_mean)
chris.site$log.pore <- log(chris.site$porosity_mean)
chris.site$log.stdstd <- log(chris.site$std.std_mean)
chris.site$log.meanstd <- log(chris.site$mean.std_mean)
chris.site$log.rugosity <- log(chris.site$rugosity_mean)
chris.site$log.toprug <- log(chris.site$top.rugosity_mean)
chris.site$log.rtnht <- log(chris.site$mean.return.ht_mean)
chris.site$log.sdrtht <- log(chris.site$sd.return.ht_mean)
chris.site$log.sf <- log(chris.site$sky.fraction_mean)
chris.site$log.cf <- log(chris.site$cover.fraction_mean)
chris.site$log.max.ht <- log(chris.site$max.ht_mean)
chris.site$log.rumple <- log(chris.site$rumple_mean)
chris.site$log.ci <- log(chris.site$clumping.index_mean)
########
chris.site %>% select(-siteID) -> chris.site.df
leaps.df <- regsubsets(fpar ~. , data = chris.site.df, nbest = 2)



# cut  + log.dg + log.dgf from model to -Inf values
fit <- step(lm(fpar ~ mean.height_mean + mode.el_mean + height.2_mean + max.el_mean + mode.2_mean + max.can.ht_mean + mean.max.ht_mean +
            mean.vai_mean + deep.gaps_mean + deep.gap.fraction_mean + porosity_mean + std.std_mean + mean.std_mean + rugosity_mean +
            top.rugosity_mean + mean.return.ht_mean + sd.return.ht_mean + sky.fraction_mean + cover.fraction_mean + max.ht_mean +
            rumple_mean + clumping.index_mean +  log.mean.ht + log.mode.el + log.pore + log.rumple + log.ht.2 + log.max.el +
            log.max.can + log.vai + log.stdstd + log.meanstd + log.rugosity + log.toprug +
            log.rtnht + log.sdrtht + log.sf + log.cf + log.max.ht + log.ci, data = chris.site), direction = "backward")

site.fit <-lm(fpar ~., data = chris.site.df)
          
step <- step(site.fit, direction = "forward")


fit <- lm(fpar ~  log.vai + log.ci + sky.fraction_mean  + log.sf +
            mean.max.ht_mean  + deep.gap.fraction_mean  + rugosity_mean +
                 top.rugosity_mean + sky.fraction_mean  + clumping.index_mean , data = chris.site)

step <- stepAIC(fit, direction = "both")

big.site.model <- lm(fpar ~ mean.max.ht_mean + deep.gap.fraction_mean + top.rugosity_mean + 
  clumping.index_mean, data = chris.site)
og.step.model <- lm(fpar ~ log.vai + log.ci + sky.fraction_mean  + log.sf, data = chris.site)

rump <- lm(fpar ~ log.rumple, data = chris.site)


test.model <- big.site.model <- lm(fpar ~ log.vai + log.ci + sky.fraction_mean  + log.sf +
                                     mean.max.ht_mean  + deep.gap.fraction_mean  + rugosity_mean +
                                     top.rugosity_mean + sky.fraction_mean  + clumping.index_mean , data = chris.site)
vai.only <- lm(fpar ~ mean.vai_mean, data = chris.site)

#####################################3
chris.plot <- read.csv("./data/laserquest_pcl_plot_means_chris.csv")


chris.plot$log.mean.ht <- log(chris.plot$mean.height_mean)
chris.plot$log.mode.el <- log(chris.plot$mode.el_mean)
chris.plot$log.ht.2 <- log(chris.plot$height.2_mean)
chris.plot$log.max.el <- log(chris.plot$max.el_mean)
chris.plot$log.max.can <- log(chris.plot$max.can.ht_mean)
chris.plot$log.vai <- log(chris.plot$mean.vai_mean)
chris.plot$log.dg <- log(chris.plot$deep.gaps_mean)
chris.plot$log.dgf <- log(chris.plot$deep.gap.fraction_mean)
chris.plot$log.pore <- log(chris.plot$porosity_mean)
chris.plot$log.stdstd <- log(chris.plot$std.std_mean)
chris.plot$log.meanstd <- log(chris.plot$mean.std_mean)
chris.plot$log.rugosity <- log(chris.plot$rugosity_mean)
chris.plot$log.toprug <- log(chris.plot$top.rugosity_mean)
chris.plot$log.rtnht <- log(chris.plot$mean.return.ht_mean)
chris.plot$log.sdrtht <- log(chris.plot$sd.return.ht_mean)
chris.plot$log.sf <- log(chris.plot$sky.fraction_mean)
chris.plot$log.cf <- log(chris.plot$cover.fraction_mean)
chris.plot$log.max.ht <- log(chris.plot$max.ht_mean)
chris.plot$log.rumple <- log(chris.plot$rumple_mean)
chris.plot$log.ci <- log(chris.plot$clumping.index_mean)


# cut  + log.dg + log.dgf from model to -Inf values
plot.sets <- regsubsets(fPAR_mean ~., data = chris.plot, nbest = 2, method = "forward")


plot.fit <- lm(fPAR_mean ~ mean.height_mean + mode.el_mean + height.2_mean  + mode.2_mean + max.can.ht_mean + mean.max.ht_mean +
                 mean.vai_mean + deep.gaps_mean + deep.gap.fraction_mean + porosity_mean + std.std_mean + mean.std_mean + rugosity_mean +
                 top.rugosity_mean + mean.return.ht_mean + sd.return.ht_mean + sky.fraction_mean + cover.fraction_mean + max.ht_mean + 
                 rumple_mean + clumping.index_mean +  log.mean.ht + log.mode.el + log.pore + log.rumple + log.ht.2  +          
                 log.max.can + log.vai + log.stdstd + log.meanstd + log.rugosity + log.toprug +
                 log.rtnht + log.sdrtht + log.sf + log.cf + log.max.ht + log.ci, data = chris.plot)

plot.step <- stepAIC(plot.fit, direction = "both")

big.model <- lm(fPAR_mean ~ mean.height_mean + mode.2_mean + mean.vai_mean + 
                  deep.gaps_mean + mean.std_mean + sky.fraction_mean + max.ht_mean + 
                  rumple_mean + log.ht.2 + log.vai + log.rtnht + log.cf + log.max.ht, data = chris.plot)

plot.vai.only <- lm(fPAR_mean ~ mean.vai_mean, data = chris.plot)
plot.chris <- lm(fPAR_mean ~ mean.vai_mean + log.rugosity, data = chris.plot)

plot(chris.plot$mean.vai, chris.plot$fpar)
# plot.fit <- step(lm(fpar ~ mean.height + mode.el + height.2 + max.el + mode.2 + max.can.ht + mean.max.ht +
#                       mean.vai + deep.gaps + deep.gap.fraction + porosity + std.std + mean.std + rugosity +
#                       top.rugosity + mean.return.ht + sd.return.ht + sky.fraction + cover.fraction + max.ht + 
#                       rumple + clumping.index +  log.mean.ht + log.mode.el + log.pore + log.rumple + log.ht.2 + log.max.el +            
#                       log.max.can + log.vai + log.stdstd + log.meanstd + log.rugosity + log.toprug +
#                       log.rtnht + log.sdrtht + log.sf + log.cf + log.max.ht + log.ci, data = chris.site), direction = "backward")



####### Messing with the plot model to eliminate some colinearites
plot.fit.exp <- lm(fPAR_mean ~ mean.height_mean + mode.el_mean + height.2_mean  + mode.2_mean + max.can.ht_mean + mean.max.ht_mean +
                 mean.vai_mean +  deep.gap.fraction_mean + porosity_mean  + rugosity_mean +
                 top.rugosity_mean +  sky.fraction_mean + cover.fraction_mean + 
                 rumple_mean + clumping.index_mean +  log.mean.ht + log.mode.el + log.pore + log.rumple + log.ht.2  +          
                 log.max.can + log.vai + log.rugosity + log.toprug +
                 log.sf + log.cf + log.ci, data = chris.plot)

plot.step.exp <- stepAIC(plot.fit.exp, direction = "both")


exp.plot.model <- lm(fPAR_mean ~ mean.max.ht_mean + mean.vai_mean + deep.gap.fraction_mean + 
                       sky.fraction_mean + rumple_mean + log.mean.ht + log.ht.2 + 
                       log.vai + log.cf, data = chris.plot)