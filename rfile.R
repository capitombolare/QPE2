library(rdlocrand)
library(tidyverse)
library(dplyr)
library(rdrobust)
library(haven)

data <- read_dta("D:/teaching/data/us_c_50_75.dta")

Yphy = data$sf12pcs_dv

# We could plot the data direcly, but that does not result in a very informative graph.
# Instead, we can compute the average outcome per age group, storing those data in an object
# Then we can plot the aggregated data.

# This aggregates the data by age group.

agg<- data %>% group_by(age_Sd) %>% summarize(propret = mean(retired), meanElig = mean(elig), meanPH=mean(sf12pcs_dv), meanMH=mean(sf12mcs_dv))

# The following plots the data. We are going to include a vertical line at the state pension age to 
# aid with the interpretation.

ggplot(agg , aes(y=propret,x=age_Sd)) + geom_smooth()+geom_point(data=agg, aes(y=propret,x=age_Sd))+geom_vline(xintercept = 0, linetype="dashed", color = "red", size=.5)

ggplot(agg , aes(y=meanPH,x=age_Sd)) + geom_smooth()+geom_point(data=agg, aes(y=meanPH,x=age_Sd))+geom_vline(xintercept = 0, linetype="dashed", color = "red", size=.5)

ggplot(data, aes(x=age_Sd)) + geom_histogram(size = 1)+geom_vline(xintercept = 0, linetype="dashed", color = "red", size=.5)


# To calculate the difference in means, you can use regression. Remember that if you run a regression of a variable Y
# on a dummy (binary), then the estimated coefficient of the dummy variable equals the difference in mean outcome
# by group.

diffMean <- lm(Yphy ~ data$retired)
summary(diffMean)

#What's wrong with this?  We saw in the graphs, that health declines with age; the difference 
# in means misses this trend. As a result your estimates will likely be misleading. A better
# strategy from a modelling point of view is to add age in the above regression in order to capture trends in 
# age. The coefficient of the dummy variable is not the difference in means any longer, but it still provides
# and estimate of the treatment effect IF the conditions of a randomized experiment had been met.
linear <- lm(Yphy ~ data$retired+data$age_Sd)
summary(diffMean)


# The instrumental variables approach is an improvement with respect to the difference in means. It takes
# into account the fact that treatment is confounded. 
# We implement the local randomization method, however, we fist use rdrobust to plot an optimised graph of the relationship
# between treatment (retirement) and age, and health and age,

rdplot(Yphy, data$age_Sd)
rdplot(data$retired, data$age_Sd)

# To implement the Fisherian' RD design, we need to take a series of steps. First we compute the bandwith. 
# To this end, we need to find a number of pre-treatment covariates. Among the variables in the dataset
# we find british born, white, school leaving age and area of residence. These seem all fine pre-treatment
# variables. 

preTreatment <- cbind(data$britishBorn, data$white, data$scend, data$gor_dv)

# Next, we need to estimate the bandwidth, for which we can use `rdwinselect()`,

tmp = rdwinselect(data$age_Sd,preTreatment, plot= TRUE, wstep = 1)

# In the above command, we have included two options. `wstep` tells `R` to compute a range of bandwidths 
# from smallest to largest, increasing the window width by 1 on each side of the cutoff at each calculation
# If didn't specify this, the program would move just a few decimals away from the smallest window and would
# fail to find a bandwith (try the command without `wstep=` to see what this means). 
# The option plot creates a scatter plot of the p-values vs the bandwidth. 
# Another thing to bear in mind is that `rdwinselect` has a default significance level of 15%. That is why the
# suggested bandwith is 4, and not 6. If you wanted to modify this, you can include the option `level=0.05` to
# use the 5% significance level 
# Finally, we estimate the RD parameter. Note, that we have a Fuzzy Design: here eligibility is the 
# unconfounded assignment, with compliance being imperfect. However, this does not create any problems
# We can use the `rdrandinf` method, but specifying the option `fuzzy=data$elig`, which tells `R`, first, that
# it shoudl estimate a fuzzy design and, second, that the instrument is `elig`. We must also either provide the
# bandwith or ask `R` to compute the bandwidth (using rdwinselect). Since we have computed the 
# bandwidht above, we can input the suggested values ourselves,

rdrandinf(data$sf12pcs_dv, data$age_Sd, p=0, fuzzy=data$elig, bernoulli = data$elig, wl=-4, wr=4 ,  covariates = preTreatment, wstep = 1,)

# THe only shortcoming of this command is that it does not give us an estimate of the treatment effect. 

rdrandinf(agg$meanPH, agg$age_Sd, p=0, fuzzy=agg$meanElig, wl=-4, wr=4 ,  covariates = preTreatment, wstep = 1,)
