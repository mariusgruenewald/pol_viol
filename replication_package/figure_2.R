rm(list=ls())
setwd("~/Dropbox/KommPolViolence/survey experiment/replication_survey")
library(ggplot2)
library(lmtest)
library(sandwich)

# laod data set
dat = read.csv("data_survey_allrounds.csv")

# restrict data to those with high political interest
dat = dat[dat$polint>=5,]

# models
risk_csp_eff <- rbind.data.frame(coeftest(lm(wtr_cand~treatment*risk_aversion_ind, dat), vcov = vcovHC(lm(wtr_cand~treatment*risk_aversion_ind, dat), type = "HC0"))[4,1:2],
                                 coeftest(lm(wtr_engage~treatment*risk_aversion_ind, dat), vcov = vcovHC(lm(wtr_engage~treatment*risk_aversion_ind, dat), type = "HC0"))[4,1:2],
                                 coeftest(lm(information~treatment*risk_aversion_ind, dat), vcov = vcovHC(lm(information~treatment*risk_aversion_ind, dat), type = "HC0"))[4,1:2],
                                 coeftest(lm(wtr_cand~treatment*csp+third_round, dat), vcov = vcovHC(lm(wtr_cand~treatment*csp+third_round, dat), type = "HC0"))[5,1:2],
                                 coeftest(lm(wtr_engage~treatment*csp+third_round, dat), vcov = vcovHC(lm(wtr_engage~treatment*csp+third_round, dat), type = "HC0"))[5,1:2],
                                 coeftest(lm(information~treatment*csp, dat), vcov = vcovHC(lm(information~treatment*csp, dat), type = "HC0"))[4,1:2]
)

# create figure
names(risk_csp_eff) <- c("coef", "se")
risk_csp_eff$moderator <- rep(c("Treatment * Risk aversion", "Treatment * CSP"), each=3)
risk_csp_eff$moderator <- factor(risk_csp_eff$moderator, levels=c("Treatment * Risk aversion", "Treatment * CSP"))
risk_csp_eff$outcome <- c("WTR\n(Surveys 1 & 2)", "WTE\n(Surveys 1 & 2)", "Information\n(Surveys 1 & 2)", 
                          "WTR\n(Surveys 1-3)", "WTE\n(Surveys 1-3)", "Information\n(Surveys 1 & 2)")
risk_csp_eff$outcome <- factor(risk_csp_eff$outcome, levels=c("WTR\n(Surveys 1 & 2)", "WTE\n(Surveys 1 & 2)", "WTR\n(Surveys 1-3)", "WTE\n(Surveys 1-3)", "Information\n(Surveys 1 & 2)"))

ggplot(data=risk_csp_eff, aes(x=outcome, y=coef)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0) +
  facet_wrap(~moderator, scales="free_x") +
  theme_bw() +
  xlab("Outcome") + ylab("Coefficient") +
  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=.9, hjust=1))
