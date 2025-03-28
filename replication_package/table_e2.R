rm(list=ls())
setwd("~/Dropbox/KommPolViolence/survey experiment/replication_survey")

# laod data set
dat = read.csv("data_survey_allrounds.csv")

# restrict data to those with high political interest
dat = dat[dat$polint>=5,]


dat$employed <- ifelse(dat$employment=="Angestellt oder selbstständig", 1, 0)
dat$afd_vote <- dat$party_pref=="AfD"
dat$cdu_vote <- dat$party_pref=="CDU/CSU"
dat$fdp_vote <- dat$party_pref=="FDP"
dat$gruene_vote <- dat$party_pref=="Gruene"
dat$linke_vote <- dat$party_pref=="Linke"
dat$spd_vote <- dat$party_pref=="SPD"
dat$other_vote <- dat$party_pref=="Other"

vars <- c(dat$risk_aversion_ind, dat$csp, dat$education, dat$polint, dat$prof_training, 
          dat$employed, dat$east, dat$internal_effic_ind, dat$afd_vote,
          dat$afd_vote, dat$cdu_vote, dat$fdp_vote, dat$gruene_vote, dat$linke_vote, dat$spd_vote, dat$other_vote)

sum_tab <- rbind.data.frame(
  cbind(mean(dat$risk_aversion_ind, na.rm=T), sd(dat$risk_aversion_ind, na.rm=T), min(dat$risk_aversion_ind, na.rm=T), max(dat$risk_aversion_ind, na.rm=T)),
  cbind(mean(dat$csp, na.rm=T), sd(dat$csp, na.rm=T), min(dat$csp, na.rm=T), max(dat$csp, na.rm=T)),
  cbind(mean(dat$age, na.rm=T), sd(dat$age, na.rm=T), min(dat$age, na.rm=T), max(dat$age, na.rm=T)),
  cbind(mean(dat$education, na.rm=T), sd(dat$education, na.rm=T), min(dat$education, na.rm=T), max(dat$education, na.rm=T)),
  cbind(mean(dat$polint, na.rm=T), sd(dat$polint, na.rm=T), min(dat$polint, na.rm=T), max(dat$polint, na.rm=T)),
  cbind(mean(dat$prof_training, na.rm=T), sd(dat$prof_training, na.rm=T), min(dat$prof_training, na.rm=T), max(dat$prof_training, na.rm=T)),
  cbind(mean(dat$employed, na.rm=T), sd(dat$employed, na.rm=T), min(dat$employed, na.rm=T), max(dat$employed, na.rm=T)),
  cbind(mean(dat$east, na.rm=T), sd(dat$east, na.rm=T), min(dat$east, na.rm=T), max(dat$east, na.rm=T)),
  cbind(mean(dat$internal_effic_ind, na.rm=T), sd(dat$internal_effic_ind, na.rm=T), min(dat$internal_effic_ind, na.rm=T), max(dat$internal_effic_ind, na.rm=T)),
  cbind(mean(dat$external_effic_ind, na.rm=T), sd(dat$external_effic_ind, na.rm=T), min(dat$external_effic_ind, na.rm=T), max(dat$external_effic_ind, na.rm=T)),
  cbind(mean(dat$afd_vote, na.rm=T), sd(dat$afd_vote, na.rm=T), min(dat$afd_vote, na.rm=T), max(dat$afd_vote, na.rm=T)),
  cbind(mean(dat$cdu_vote, na.rm=T), sd(dat$cdu_vote, na.rm=T), min(dat$cdu_vote, na.rm=T), max(dat$cdu_vote, na.rm=T)),
  cbind(mean(dat$fdp_vote, na.rm=T), sd(dat$fdp_vote, na.rm=T), min(dat$fdp_vote, na.rm=T), max(dat$fdp_vote, na.rm=T)),
  cbind(mean(dat$gruene_vote, na.rm=T), sd(dat$gruene_vote, na.rm=T), min(dat$gruene_vote, na.rm=T), max(dat$gruene_vote, na.rm=T)),
  cbind(mean(dat$linke_vote, na.rm=T), sd(dat$linke_vote, na.rm=T), min(dat$linke_vote, na.rm=T), max(dat$linke_vote, na.rm=T)),
  cbind(mean(dat$spd_vote, na.rm=T), sd(dat$spd_vote, na.rm=T), min(dat$spd_vote, na.rm=T), max(dat$spd_vote, na.rm=T)),
  cbind(mean(dat$other_vote, na.rm=T), sd(dat$other_vote, na.rm=T), min(dat$other_vote, na.rm=T), max(dat$other_vote, na.rm=T))
)
sum_tab <- round(sum_tab, 2)
names(sum_tab) <- c("Mean", "SD", "Min.", "Max.")
sum_tab$variable <- c("Risk Aversion Index", "CSP", "Age", "Education", "Political Interest", "Professional Training", "Employed", "East Germany", 
                      "Internal Efficacy Index", "External Efficacy Index", "AfD Vote", "CDU/CSU Vote", "FDP Vote", "Greens Vote", "Linke Vote", "SPD Vote", "Other Vote")
sum_tab <- cbind.data.frame(sum_tab[,5], sum_tab[,1:4])
names(sum_tab)[1] <- "Variable"

print(sum_tab)