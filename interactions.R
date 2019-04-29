##A) DATA PRE-PROCESSING##

#a1) import data#
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2019/03/exercise.csv")
names(dat)

#a2) load packages#
#install.packages("emmeans")
#install.packages("ggplot2")
library(emmeans)
library(ggplot2)

#a3) make factor variables#
dat$prog <- factor(dat$prog,labels=c("jog","swim","read"))
dat$gender <- factor(dat$gender,labels=c("male","female"))

#a4) get descriptives#
summary(dat)

##B) PREDICTED VALUE VS SIMPLE SLOPE## 

#b1) linear model of loss on hours#
cont <- lm(loss~hours,data=dat)
summary(cont)

#b2) predicted weight loss at Hours = 2#
(mylist <- list(hours=2))
emmeans(cont, ~ hours, at=mylist)

#b3) overall slope of Hours#
emtrends(cont, ~ 1, var="hours")

#b4) exercise#
(mylist <- list(hours=10))
emmeans(cont, ~ hours, at=mylist)
(mylist <- list(hours=20))
emmeans(cont, ~ hours, at=mylist)
(54.5-29.8)/(20-10)

#b5) plotting a regression slope#
(mylist <- list(hours=seq(0,4,by=0.4)))
emmip(cont,~hours,at=mylist, CIs=TRUE)

##C) CONTINUOUS-BY-CONTINUOUS INTERACTION##

#c1) linear model of loss on hours by effort#
contcont <- lm(loss~hours*effort,data=dat)
summary(contcont)
#c2) be wary of extrapolation# 
#create list hours=2, effort = 30
(mylist <- list(hours=2,effort=30))
#reasonable: predicted value at hours=2, effort = 30
emmeans(contcont, ~ hours*effort, at=mylist)
#extrapolation: predicted value at hours=2, effort = 0
mylist <- list(hours=2,effort=0)
emmeans(contcont, ~ hours*effort, at=mylist)
#c3) simple slopes for continuous by continuous model#  

#high, medium, low levels of effort#
effa <- mean(dat$effort) + sd(dat$effort)
eff <- mean(dat$effort)
effb <- mean(dat$effort) - sd(dat$effort)

#round effort values to one decimal place
(effar <- round(effa,1))
(effr <- round(eff,1))
(effbr <- round(effb,1))

#create sequence of values at "low", "med" and "high" effort
mylist <- list(effort=c(effbr,effr,effar))
#simple slopes at three effort levels
emtrends(contcont, ~effort, var="hours",at=mylist)

#plot continuous by continuous interaction using emmip
(mylist <- list(hours=seq(0,4,by=0.4),effort=c(effbr,effr,effar)))
emmip(contcont,effort~hours,at=mylist, CIs=TRUE)

#c4) test differences in slopes#
emtrends(contcont, pairwise ~effort, var="hours",at=mylist, adjust="none")

#c5) test the difference in predicted value hour = 4#
(mylist <- list(hours=4,effort=c(effbr,effar)))
emmeans(contcont, pairwise ~ hours*effort, at=mylist)

#c6) plot continuous by continuous interaction using ggplot#

#save simple slopes as dataset from emmip
(mylist <- list(hours=seq(0,4,by=0.4),effort=c(effbr,effr,effar)))
contcontdat <- emmip(contcont,effort~hours,at=mylist, CIs=TRUE, plotit=FALSE)

#convert effort into a factor variable 
contcontdat$feffort <- factor(contcontdat$effort)
levels(contcontdat$feffort) <- c("low","med","high")

#using ggplot, add geom_line()
(p <- ggplot(data=contcontdat, aes(x=hours,y=yvar, color=feffort)) + geom_line())
#add confidence ribbons
(p1 <- p + geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=feffort), alpha=0.4))
#relabel axes
(p2 <- p1  + labs(x="Hours", y="Weight Loss", color="Effort", fill="Effort"))

#c7) exercise: create a plot of hours on the x-axis with effort = 0
(mylist <- list(hours=seq(0,4,by=0.4),effort=0))
contcontdat <- emmip(contcont,effort~hours,at=mylist, CIs=TRUE, plotit=FALSE)
(p <- ggplot(data=contcontdat, aes(x=hours,y=yvar)) + geom_line())
(p1 <- p + geom_ribbon(aes(ymax=UCL, ymin=LCL), alpha=0.4))
(p2 <- p1  + labs(x="Hours", y="Weight Loss", color="Effort", fill="Effort"))

#c8) for each of the following, use emmeans and store as an object k1, k2, k3, k4
#get the difference of a unit increase in hours at effort = 0
#predicted value of hours = 0, effort= 0 
mylist <- list(hours=0,effort=0)
p00 <- emmeans(contcont, ~ hours*effort, at=mylist)
(y00 <- summary(p00)$emmean)

#predicted value of hours = 1, effort= 0
mylist <- list(hours=1,effort=0)
p10 <- emmeans(contcont, ~ hours*effort, at=mylist)
(y10 <- summary(p10)$emmean)

# get the difference of  unit increase in hours at effort = 1
#predicted value of hours = 0, effort= 1 
mylist <- list(hours=0,effort=1)
p01 <- emmeans(contcont, ~ hours*effort, at=mylist)
(y01 <- summary(p01)$emmean)

#predicted value of hours = 1, effort= 1
mylist <- list(hours=1,effort=1)
p11 <- emmeans(contcont, ~ hours*effort, at=mylist)
(y11 <- summary(p11)$emmean)

#simple slopes and difference of simple slopes
(y10 - y00)
(y11 - y01)
(y11 - y01)-(y10 - y00)
summary(contcont)

##D) CATEGORICAL-BY-CONTINUOUS INTERACTION##
class(dat$gender)
levels(dat$gender)
#d1)show that male =1 and female =2, so male is the reference group (R takes lowest value)
catm <- lm(loss~gender,data=dat)
summary(catm)
#d2)relevel prog to make reading the reference group
dat$gender <- relevel(dat$gender, ref="female")
levels(dat$gender)

#d3) predict linear model
contcat <- lm(loss~hours*gender,data=dat)
summary(contcat)

#d4) simple slopes at hours = 0 
emtrends(contcat, ~ gender, var="hours")
#d5) comparing slopes 
emtrends(contcat, pairwise ~ gender, var="hours")

#d6) flipping the MV and the IV 
(mylist <- list(hours=c(0,2,4),gender=c("female","male")))
emcontcat <- emmeans(contcat, ~ hours*gender, at=mylist)
contrast(emcontcat, "revpairwise",by="hours")

#d7) plot using emmip
(mylist <- list(hours=seq(0,4,by=0.4),gender=c("female","male")))
emmip(contcat, gender ~hours, at=mylist,CIs=TRUE)

#exercise: use ggplot to recreate the interaction plot above
#d8) save simple slopes as dataset 
(mylist <- list(hours=seq(0,4,by=0.4),gender=c("female","male")))
contcatdat <- emmip(contcat,gender~hours,at=mylist, CIs=TRUE, plotit=FALSE)
#d9) place hours on the x-axis and program as separate lines using color
(p <- ggplot(data=contcatdat, aes(x=hours,y=yvar, color=gender)) + geom_line())
#d10) add a confidence band with alpha = 0.4
(p1 <- p + geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=gender), alpha=0.4))
#d11) label the legend Program, Hours on the x-axis, Weight Loss on the y-axis
(p2 <- p1  + labs(x="Hours", y="Weight Loss", color="Gender", fill="Gender"))

##E) CATEGORICAL-BY-CATEGORICAL INTERACTION##
#e1)
#relevel prog to make reading the reference group
dat$prog <- relevel(dat$prog, ref="read")
#relevel gender to make female the reference group
dat$gender <- relevel(dat$gender, ref="female")

#e2) fit linear model
catcat <- lm(loss~gender*prog,data=dat)
summary(catcat)

#e3) emmeans
emcatcat <- emmeans(catcat, ~ gender*prog)
##pairwise contrasts by gender (automatically adjusts for tukey)
contrast(emcatcat, "revpairwise",by="prog",adjust="none")
#optional: TRY glht in the multcomp package

#e4) interaction plot
emmip(catcat, prog ~ gender,CIs=TRUE)

#e5) try it with ggplot by storing as data frame
catcatdat <- emmip(catcat, gender ~ prog, CIs=TRUE, plotit=FALSE)
head(catcatdat)

#e6) create template
(p <- ggplot(data=catcatdat, aes(x=prog,y=yvar, fill=gender)) + geom_bar(stat="identity",position="dodge"))
#add error bars
(p1 <- p + geom_errorbar(position=position_dodge(.9),width=.25, aes(ymax=UCL, ymin=LCL),alpha=0.3))
#change labels
(p2 <- p1  + labs(x="Program", y="Weight Loss", fill="Gender"))
#optional theme change
#(p3 <- p2 + scale_fill_manual(name="gender",values=c(female="lightpink",male="skyblue")))