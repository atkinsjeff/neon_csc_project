#### 05 script to look at covariance for appendix
####
require(tidyverse)
require(Hmisc)
require(corrplot)

# filter to only variables used in manuscript
master.plot %>% dplyr::select(rugosity_mean, top.rugosity_mean, porosity_mean, mean.vai_mean, rumple_mean, mean.max.ht_mean, deep.gap.fraction_mean, sky.fraction_mean, clumping.index_mean, mean.height_mean) -> new.df

# make matrix
res <-cor(new.df)

round(res, 2)


# correlation plot
corrplot(res, method = "number", type = "upper")


###
###broom [Robinson 2017], dplyr [Wickham et al. 2017], forestr [Atkins et al. 2017], ggplot [Wickham 2009], ggrepel [Slowikowski 2017], MuMIn [Barton 2017], plyr [Wickham 2011], and tidyr [Wickham & Henry 2017]. 