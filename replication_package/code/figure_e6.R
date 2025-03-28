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
wtr_ind_reduc_eff <-
  rbind.data.frame(coeftest(lm(wtr_ind_reduc~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(wtr_ind_reduc~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_ind_reduc~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(wtr_ind_reduc~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_ind_reduc~treatment*female+age+factor(education)+polint+third_round, dat), vcov = vcovHC(lm(wtr_ind_reduc~treatment*female+age+factor(education)+polint+third_round, dat), type = "HC0"))[9,1:2])
wtr_ind_friend_eff <-
  rbind.data.frame(coeftest(lm(wtr_ind_friend~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(wtr_ind_friend~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_ind_friend~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(wtr_ind_friend~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_ind_friend~treatment*female+age+factor(education)+polint+third_round, dat), vcov = vcovHC(lm(wtr_ind_friend~treatment*female+age+factor(education)+polint+third_round, dat), type = "HC0"))[9,1:2])
wtr_ind_full_eff <-
  rbind.data.frame(coeftest(lm(wtr_ind_full~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(wtr_ind_full~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_ind_full~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(wtr_ind_full~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_ind_full~treatment*female+age+factor(education)+polint+third_round, dat), vcov = vcovHC(lm(wtr_ind_full~treatment*female+age+factor(education)+polint+third_round, dat), type = "HC0"))[9,1:2])
wtr_work_bt_eff <-
  rbind.data.frame(coeftest(lm(wtr_work_bt~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(wtr_work_bt~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_work_bt~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(wtr_work_bt~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_work_bt~treatment*female+age+factor(education)+polint+third_round, dat), vcov = vcovHC(lm(wtr_work_bt~treatment*female+age+factor(education)+polint+third_round, dat), type = "HC0"))[9,1:2])

# create figure
names(wtr_ind_reduc_eff) <- c("coef", "se")
names(wtr_ind_friend_eff) <- c("coef", "se")
names(wtr_ind_full_eff) <- c("coef", "se")
names(wtr_work_bt_eff) <- c("coef", "se")
wtr_oth_ests <- rbind.data.frame(wtr_ind_reduc_eff, wtr_ind_friend_eff, wtr_ind_full_eff, wtr_work_bt_eff)

wtr_oth_ests$category <- rep(c("Male Subsample", "Female Subsample", "Female*Treatment"), 4)
wtr_oth_ests$category <- factor(wtr_oth_ests$category, levels=c("Male Subsample", "Female Subsample", "Female*Treatment"))
wtr_oth_ests$outcome <- rep(c("WTR/WTE index\n(Surveys 1-3)", "WTR/WTE friend index\n(Surveys 1-3)", "WTR/WTE+WTR/WTE friend index\n(Surveys 1-3)", "Work for MP\n(Surveys 1-3)"), each=3)
wtr_oth_ests$outcome <- factor(wtr_oth_ests$outcome, levels=c("WTR/WTE index\n(Surveys 1-3)", "WTR/WTE friend index\n(Surveys 1-3)", "WTR/WTE+WTR/WTE friend index\n(Surveys 1-3)", "Work for MP\n(Surveys 1-3)"))

ggplot(data=wtr_oth_ests, aes(x=outcome, y=coef, color=category, shape=category)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
  theme_bw() +
  scale_color_manual(values=c("blue", "red", "darkgreen", "blue", "red", "darkgreen"), name="") +
  scale_shape_manual(values=c(16, 17, 15), name="") +
  xlab("Outcome") + ylab("Coefficient") +
  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=.9, hjust=1),
        legend.position = "bottom")
