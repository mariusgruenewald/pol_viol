rm(list=ls())
setwd("~/Dropbox/KommPolViolence/survey experiment/replication_survey")
library(ggplot2)
library(lmtest)
library(sandwich)

# laod data set
dat = read.csv("data_survey_allrounds.csv")

# restrict data to those interviewed in third round
dat = dat[dat$third_round==1,]

# create indicator for high political interest
dat$high_polint = dat$polint >=5

# models
ests_df <- rbind.data.frame(
  coeftest(lm(wtr_cand~treatment, dat[dat$female==0 & dat$high_polint==0,]),
           vcov = vcovHC(lm(wtr_cand~treatment, dat[dat$female==0 & dat$high_polint==0,]), type = "HC0"))[2,1:2],
  coeftest(lm(wtr_cand~treatment, dat[dat$female==0 & dat$high_polint==1,]),
           vcov = vcovHC(lm(wtr_cand~treatment, dat[dat$female==0 & dat$high_polint==1,]), type = "HC0"))[2,1:2],
  coeftest(lm(wtr_cand~treatment*high_polint, dat[dat$female==0,]),
           vcov = vcovHC(lm(wtr_cand~treatment*high_polint, dat[dat$female==0,]), type = "HC0"))[4, 1:2],
  coeftest(lm(wtr_cand~treatment, dat[dat$female==1 & dat$high_polint==0,]),
           vcov = vcovHC(lm(wtr_cand~treatment, dat[dat$female==1 & dat$high_polint==0,]), type = "HC0"))[2, 1:2],
  coeftest(lm(wtr_cand~treatment, dat[dat$female==1 & dat$high_polint==1,]),
           vcov = vcovHC(lm(wtr_cand~treatment, dat[dat$female==1 & dat$high_polint==1,]), type = "HC0"))[2, 1:2],
  coeftest(lm(wtr_cand~treatment*high_polint, dat[dat$female==1,]),
           vcov = vcovHC(lm(wtr_cand~treatment*high_polint, dat[dat$female==1,]), type = "HC0"))[4, 1:2],
  coeftest(lm(wtr_engage~treatment, dat[dat$female==0 & dat$high_polint==0,]),
           vcov = vcovHC(lm(wtr_engage~treatment, dat[dat$female==0 & dat$high_polint==0,]), type = "HC0"))[2,1:2],
  coeftest(lm(wtr_engage~treatment, dat[dat$female==0 & dat$high_polint==1,]),
           vcov = vcovHC(lm(wtr_engage~treatment, dat[dat$female==0 & dat$high_polint==1,]), type = "HC0"))[2,1:2],
  coeftest(lm(wtr_engage~treatment*high_polint, dat[dat$female==0,]),
           vcov = vcovHC(lm(wtr_engage~treatment*high_polint, dat[dat$female==0,]), type = "HC0"))[4,1:2],
  coeftest(lm(wtr_engage~treatment, dat[dat$female==1 & dat$high_polint==0,]),
           vcov = vcovHC(lm(wtr_engage~treatment, dat[dat$female==1 & dat$high_polint==0,]), type = "HC0"))[2,1:2],
  coeftest(lm(wtr_engage~treatment, dat[dat$female==1 & dat$high_polint==1,]),
           vcov = vcovHC(lm(wtr_engage~treatment, dat[dat$female==1 & dat$high_polint==1,]), type = "HC0"))[2,1:2],
  coeftest(lm(wtr_engage~treatment*high_polint, dat[dat$female==1,]),
           vcov = vcovHC(lm(wtr_engage~treatment*high_polint, dat[dat$female==1,]), type = "HC0"))[4,1:2])

# create figure
names(ests_df) <- c("coef", "se")
ests_df$outcome <- rep(c("WTR", "WTE"), each=6)
ests_df$outcome <- factor(ests_df$outcome, levels=c("WTR", "WTE"))
ests_df$polint <- rep(c("Low Pol. Interest", "High Pol. Interest", "Treatment*High Pol. Interest"), 4)
ests_df$polint <- factor(ests_df$polint, levels=c("Low Pol. Interest", "High Pol. Interest", "Treatment*High Pol. Interest"))
ests_df$gender <- rep(c("Male Subsample", "Male Subsample", "Male Subsample", "Female Subsample", "Female Subsample", "Female Subsample"), 2)
ests_df$gender <- factor(ests_df$gender, levels=c("Male Subsample", "Female Subsample"))

ggplot(data=ests_df, aes(x=outcome, y=coef, group=polint, shape=polint)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
  facet_wrap(~gender) +
  scale_shape_manual(values=c(16, 17, 15), name="") +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("Outcome") + ylab("Coefficient")
