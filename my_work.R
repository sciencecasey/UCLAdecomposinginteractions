#from https://stats.idre.ucla.edu/r/seminars/interactions-r/
library(emmeans)
library(ggplot2)
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2019/03/exercise.csv")
names(dat)
#change to factor
dat$prog=factor(dat$prog)
dat$gender=factor(dat$gender)
summary(dat)

cont=lm(loss~hours, data = dat)
summary(cont)
#intercept is where the hours is 0, value of y (weight loss)
#coefficient hours is the slope of hours: for every 1 unit change in hours, the predicted change in weight loss

#calculate predicted post-estimation functions
mylist=list(hours=2)
emmeans(cont, ~ hours, at=mylist)
emtrends(cont, ~ 1, var="hours")

#calculate for hours=10 and hours=20
mylist=list(hours=10)
emmeans(cont, ~ hours, at=mylist)
emtrends(cont, ~ 1, var="hours")
mylist=list(hours=20)
emmeans(cont, ~ hours, at=mylist)
emtrends(cont, ~ 1, var="hours")

#plot with emmeans
(mylist=list(hours=seq(0,4, by=.4))) #this sets the x axis intervals
emmip(cont, ~hours, at=mylist, CIs=TRUE) #ip is interaction plot; keep confidence intervals

#do same but by effort
summary(dat$effort)
(mylist=list(effort=seq(12,max(dat$effort), by=2)))
cont=lm(loss~effort, data=dat)
emmip(cont, ~effort, at=mylist, CIs=TRUE)

#which is moderating and which independent?
#the model same either way, we choose which we treat as which
#Hours is IV and effort is moderating

#
contcont=lm(loss~hours*effort, data=dat)
#this is same as lm(loss~hours+effort+hours:effort, data=dat)
summary(contcont)
#here the intercept is where hours and effort both =0; 
#hours is the pred. change in y (loss) for every one hour change in hours if effort=0; (b1)
#effort is pred. change in y (loss) for every one hour change in effort if hours=0 (b2)
#interaction is change in slope of hours for every one unit increase in effort (and vice versa) (b3)
##(not in output) simple slope is b1+b3W;
#only interaction is significant

#now must look at the minimum value of effort because the data suggests we gain weight for every 1 unit change in hours if effort is zero
#the minimum value of effort is 12.95
min(dat$effort)

#this means that the extrapolation doesn't work outside the bounds of the data
(mylist=list(hours=2, effort=30))
emmeans(contcont, ~hours*effort, at=mylist) #predictive value
(mylist=list(hours=2, effort=0))
emmeans(contcont, ~hours*effort, at=mylist) #predictive value

#we may want to recenter the data at the minimum value of effort
#then can interpret the coefficients at a new value of effort (instead of =0)
(dat$effort_c=dat$effort-mean(dat$effort)) #recenter by the mean of effort
mean(dat$effort_c) #make sure this worked
ccontcont=lm(loss~hours*effort_c, data=dat)
summary(ccontcont)
#now the intercept is the weight loss at hours=0 and effort=mean effort
#now the hours coefficient is the predicted weight loss for a one unit change in hours at effort is the mean of effort
#now the effort coefficient is the predicted weight loss for a one unit change in effort from the mean if hours =0
#now the intercept is the change in slope of hours for every one unit increase in effort from the mean
ccontcont
(mylst=list(hours=2, effort_c=0))
emmeans(ccontcont, ~hours*effort_c, at=mylst)

#the recommended high value: one st dev above mean; med: mean effort; low: one st dev below mean
effa=mean(dat$effort) +sd(dat$effort)
eff=mean(dat$effort)
effb=mean(dat$effort)-sd(dat$effort)

#reduce the decimals 
effar=round(effa, 1)
effr=round(eff, 1)
effbr=round(effb, 1)

mylist=list(effort=c(effbr, effr, effar))
emtrends(contcont, ~effort, var="hours", at=mylist)
#this tells us the simple slope of hours at different (low, med, high) levels of effort
#the trend tells us the simple slope of hours, the effort is the effort level indicated
#this means that the trend is the change predicted weight loss for every one unit increase in hours at the set effort
#if the confidence value does contain zero, it's not significant
#if CI doesn't contain zero, it's not sig

#Use emtrends to obtain the hours slope at ceffort=0. Confirm that the slope matches the hours coefficient from summary(ccontcemt
ceffort=dat$effort_c
emtrends(contcont, ~effort, var="hours", ceffort=0)

#plot the weight loss by cont. interactions
mylist=list(hours=seq(0, 4, by=.4), effort=c(effbr, effr, effar))
emmip(contcont, effort~hours, at=mylist, CIs=TRUE) #the y axis is moderator, on the left of x axis
#emmip(contcont, hours~effort, at=mylist, CIs=TRUE) #this changes the hours to moderator
emtrends(contcont, pairwise~effort, var="hours", at=mylist, adjust="none")  #no adjustment asks to not Tukey correct
#give the difference in simple slope of hours for the pairwise differences in effort
#this means the change in predicted weight loss varies by the simple slope of hours between the values of effort change 
#the same p value for the summary of the coefficient of interaction

#at a specific value of effort, what is the change in weight loss by hours?
#this isn't slope, it's a specifc predicted value
emmeans(contcont, pairwise~hours*effort, at=list(hours=4, effort=c(effbr, effar)))

#what is the differnce in predicted values at hours=0 for effort is high v low
emmeans(contcont, pairwise~hours*effort, at=list(hours=0, effort=c(effar, effbr)))

#now do it in ggplot
#want to keep x axis sequence
contcontdat=emmip(contcont, effort~hours, at=mylist, CIs=TRUE, plotit = FALSE) #save values as df instead of plotting
contcontdat=factor(contcontdat$effort) 
levels(contcontdat)=c("low", "med", "high")
p=ggplot(data=contcontdat, aes(x=hours, y=var, color=feffort)) +geom_line() +
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=feffort, alpha=.4)) +
  labs(x="Hours", y="Weight Loss", color="Effort", fill="Effort")
#the alpha is transparence; UCL and LCL come out of the df made by eemip above

