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

#export tsv
write.csv(dat, file = "data.csv")

#a4) get descriptives#
summary(dat)

##B) PREDICTED VALUE VS SIMPLE SLOPE## 

#Ex2) linear model of loss on hours#
conteffort <- lm(loss~effort,data=dat)
summary(conteffort)
(mylist <- list(effort=30))
emmeans(conteffort, ~ effort, at=mylist)
##using the same model as above, if we set effort to 30 (instead of zero), what is the estimated effect on weight loss (emmean= estimated marginal means for a factor)
#optionally known as comparisons or constrasts/least-squares meand

#Ex3) overall slope of Hours#
cont <- lm(loss~hours,data=dat)
summary(cont)
#b4) exercise#
(mylist <- list(hours=10))
emmeans(cont, ~ hours, at=mylist)
(mylist <- list(hours=20))
emmeans(cont, ~ hours, at=mylist)
(54.5-29.8)/(20-10)

#Ex4) plotting a regression slope#
(mylist <- list(effort=seq(12,44,by=2)))
emmip(conteffort,~effort,at=mylist, CIs=TRUE)

##C) CONTINUOUS-BY-CONTINUOUS INTERACTION##

#Ex6) create a new centered variable
dat$ceffort <- dat$effort - mean(dat$effort)
ccontcont <- lm(loss~hours*ceffort,data=dat)
summary(ccontcont)

#Ex7) be wary of extrapolation# 
(mylist <- list(ceffort=0))
emtrends(ccontcont, ~ceffort, var="hours",at=mylist)

#Ex8) plot continuous by continuous interaction using emmip
mylist <- list(hours=seq(0,4,by=0.4),ceffort=c(-5.14, 0, 5.14))
emmip(ccontcont,ceffort~hours,at=mylist, CIs=TRUE)

#Ex9) ceffort = 1 vs. ceffort =0#
mylist <- list(ceffort=c(0, 1))
emtrends(ccontcont, revpairwise ~ceffort, var="hours",at=mylist, adjust="none")
summary(ccontcont)

#Ex10) test the difference in predicted value high vs. low effort#
contcont <- lm(loss~hours*effort,data=dat)
summary(contcont)
#high, medium, low levels of effort#
effa <- mean(dat$effort) + sd(dat$effort)
eff <- mean(dat$effort)
effb <- mean(dat$effort) - sd(dat$effort)
#round effort values to one decimal place
(effar <- round(effa,1))
(effr <- round(eff,1))
(effbr <- round(effb,1))
#create sequence of values at "low", "med" and "high" effort
mylist <- list(hours=0, effort=c(effbr,effar))
emmeans(contcont, pairwise ~ hours*effort, at=mylist)
