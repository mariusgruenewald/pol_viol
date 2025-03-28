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
m1 = coeftest(lm(perception_risk_1~treatment, dat[dat$female==0,]), vcov = vcovHC(lm(perception_risk_1~treatment, dat[dat$female==0,]), type = "HC0"))
m2 = coeftest(lm(perception_risk_1~treatment, dat[dat$female==1,]), vcov = vcovHC(lm(perception_risk_1~treatment, dat[dat$female==1,]), type = "HC0"))
m3 = coeftest(lm(perception_risk_1~treatment*female+third_round, dat), vcov = vcovHC(lm(perception_risk_1~treatment*female, dat), type = "HC0"))
m4 = coeftest(lm(perception_risk_2~treatment+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(perception_risk_2~treatment+third_round, dat[dat$female==0,]), type = "HC0"))
m5 = coeftest(lm(perception_risk_2~treatment+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(perception_risk_2~treatment+third_round, dat[dat$female==1,]), type = "HC0"))
m6 = coeftest(lm(perception_risk_2~treatment*female+third_round, dat), vcov = vcovHC(lm(perception_risk_2~treatment*female+third_round, dat), type = "HC0"))
