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
m1 = coeftest(lm(wtr_cand~treatment*risk_aversion_ind, dat), vcov = vcovHC(lm(wtr_cand~treatment*risk_aversion_ind, dat), type = "HC0"))
m2 = coeftest(lm(wtr_engage~treatment*risk_aversion_ind, dat), vcov = vcovHC(lm(wtr_engage~treatment*risk_aversion_ind, dat), type = "HC0"))
m3 = coeftest(lm(information~treatment*risk_aversion_ind, dat), vcov = vcovHC(lm(information~treatment*risk_aversion_ind, dat), type = "HC0"))
m4 = coeftest(lm(wtr_cand~treatment*csp+third_round, dat), vcov = vcovHC(lm(wtr_cand~treatment*csp+third_round, dat), type = "HC0"))
m5 = coeftest(lm(wtr_engage~treatment*csp+third_round, dat), vcov = vcovHC(lm(wtr_engage~treatment*csp+third_round, dat), type = "HC0"))
m6 = coeftest(lm(information~treatment*csp, dat), vcov = vcovHC(lm(information~treatment*csp, dat), type = "HC0"))

