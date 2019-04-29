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
emmeans(cont, ~hours, at=mylist)
install.packages("emmeans", dependencies = TRUE)
n