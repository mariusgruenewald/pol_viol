rm(list=ls())
setwd("~/Dropbox/KommPolViolence/survey experiment/replication_survey")
library(ggplot2)
library(lmtest)
library(sandwich)

# laod data set
dat = read.csv("data_survey_allrounds.csv")

# restrict data to those with high political interest
dat = dat[dat$polint>=5,]

# create table
tab = rbind.data.frame(cbind(mean(dat$wtr_cand[dat$female==0], na.rm=T), sd(dat$wtr_cand[dat$female==0], na.rm=T), mean(dat$wtr_engage[dat$female==0], na.rm=T), sd(dat$wtr_engage[dat$female==0], na.rm=T), mean(dat$information[dat$female==0], na.rm=T), sd(dat$information[dat$female==0], na.rm=T)),
                       cbind(mean(dat$wtr_cand[dat$female==1], na.rm=T), sd(dat$wtr_cand[dat$female==1], na.rm=T), mean(dat$wtr_engage[dat$female==1], na.rm=T), sd(dat$wtr_engage[dat$female==1], na.rm=T), mean(dat$information[dat$female==1], na.rm=T), sd(dat$information[dat$female==1], na.rm=T)))
tab = round(tab, 2)
row.names(tab) = c("Male", "Female")
names(tab) = c("WTR_mean", "WTR_sd", "WTE_mean", "WTE_sd", "Information_mean", "Information_sd")

