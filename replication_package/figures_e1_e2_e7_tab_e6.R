rm(list=ls())

#install.packages("sandwich")
library(tidyverse)
library(MatchIt)
library(ivreg)
library(ivmodel)
library(lmtest)
library(sandwich)
library(readr)
library(fastDummies)

setwd(path)

resp <- read_delim("data_survey_allrounds.csv", delim = ";")
# Define the columns to clean and convert
cols_to_fix <- c("internal_effic_ind", "external_effic_ind", 
                 "risk_aversion_ind", "perception_envir_ind", 
                 "wtr_ind_full", "wtr_ind_reduc", "wtr_ind_friend")

# Apply the transformations
resp <- resp %>%
  mutate(across(all_of(cols_to_fix), ~ gsub("[^0-9]", "", .))) %>%  # Remove non-numeric characters
  mutate(across(all_of(cols_to_fix), as.numeric)) %>%  # Convert to numeric
  mutate(across(all_of(cols_to_fix), ~ if_else(. > 10, . / 10, .)))  # Divide by 10 if > 10
         

################################################################################
############################ Balancing checks ##################################
################################################################################

dat_4 <- resp %>% drop_na()

dat_4$round <- dat_4$round - 1

dat_5 <- subset(dat_4, select = -c(wtr_work_bt, risk_aversion_ind, wtr_ind_reduc, wtr_engage, wtr_cand, 
                                   wtr_ind_full, information, wtr_ind_friend, 
                                   perception_envir_ind, 
                                   # affected by treatment therefore not part of balancing
                                   perception_risk_2, perception_risk_1))

dat_5 <- dummy_cols(dat_5, select_columns = "education", remove_first_dummy = FALSE)
dat_5 <- dummy_cols(dat_5, select_columns = "prof_training", remove_first_dummy = FALSE)
dat_5 <- dummy_cols(dat_5, select_columns = "party_pref", remove_first_dummy = FALSE)
dat_5 <- dummy_cols(dat_5, select_columns = "employment", remove_first_dummy = FALSE)

############### Crude difference between treat and control #####################

mean_t <- dat_5 %>% group_by(treatment)  %>% summarise(across(everything(), mean))
# Treatment - Control means
mean_t_diff <- mean_t %>% select(-1) %>% mutate(across(everything(), ~lead(.x) - (.x))) #%>% na.omit()
mean_t_diff <- abs(mean_t_diff)
mean_t_diff <- mean_t_diff %>% pivot_longer(everything(), names_to="variable", values_to="diff") %>% na.omit()

var_t <- dat_5 %>% group_by(treatment)  %>% summarise(across(everything(), var)) %>% mutate(across(everything(), sum)) %>% unique()
var_t <- var_t[,2:39] %>% pivot_longer(everything(), names_to="variable", values_to="var") %>% na.omit()

asd <- bind_cols(mean_t_diff[2:39,], var_t[2:39,]) %>% na.omit()
asd$asd_ <- asd$diff / sqrt(asd$var)


ggplot(asd,aes(x=asd_,y=variable...1))+
  geom_rect(xmin=-0.1,xmax=0.1,
            ymin=-Inf,ymax=Inf,
            fill="steelblue",alpha=0.01)+ # green rectangle below threshold
  geom_segment(aes(y=variable...1,x=0,xend=asd_, yend=variable...1), # connect segment from point to name
               data=. %>% filter(asd_>0.1), # only for unbalanced covariates
               linetype="dotted")+
  geom_point()+
  scale_x_continuous(expand=c(0,0.01))+
  theme_light()+
  labs(y="",
       x="Absolute standardized difference", title="Absolute Standardized Difference - Treatment & Control")

################################################################################
##################### Propensity Score Matching ################################

# Use full set of controls but no outcome/treatment variables

model_ps <- glm(treatment ~ female + migrant +factor(party_pref) + age + factor(education) + factor(prof_training) 
                + polint + internal_effic_ind + external_effic_ind + east + active_sport + active_music + active_union +
                  active_religion + active_social + active_political + active_other
                #+ city + small_town + suburb + I(state)
                + factor(employment) + csp + round + third_round
                , family="binomial", data=dat_4)

summary(model_ps)

dat_4$p_score <- predict(model_ps, dat_4, type="response")

dat_4 %>%
  mutate(t=if_else(treatment==1,"Treated","Control")) %>% # label for treatment
  ggplot(aes(x=p_score,y=stat(width*density),fill=t))+
  geom_histogram(bins=100,position = "identity",alpha=0.7)+
  theme_light()+
  labs(y="density",
       x="Propensity score",
       fill="",
       title="Overlap of Propensity Scores - All Identified Covariates")


############################## Trimming ########################################

min_p_score <- min(dat_4$p_score[dat_4$treatment == 1])
max_p_score <- max(dat_4$p_score[dat_4$treatment == 0]) 
dat_6 <- subset(dat_4, p_score >= min_p_score)
dat_6 <- subset(dat_6,  max_p_score >= p_score)

###############################################################################
########################### Matching ##########################################

# With all data
match_set <- matchit(treatment ~ female + migrant +factor(party_pref) + age + factor(education) + factor(prof_training) 
                     + polint + internal_effic_ind + external_effic_ind + east + active_sport + active_music + active_union +
                       active_religion + active_social + active_political + active_other
                     #+ city + small_town + suburb + I(state)
                     + factor(employment) + csp + round + third_round,
                     family="binomial", # PS formula
                     data=dat_4, # data
                     ratio=1, # 1:1 matching
                     replace=F) # No replacement


match_data <- match.data(match_set)

##### IV Regression - Treatment Effect "on the treated" #######################

# General Treatment Effect for those where treatment "worked"
summary(ivreg(wtr_cand~ treatment|perception_risk_2, data=match_data))
summary(lm(treatment~ perception_risk_2, data=match_data))

# Construct First Stage
match_data$iv_no_controls <- predict(lm(treatment~ perception_risk_2, data=match_data)) # without female coefficient
summary(lm(treatment~perception_risk_2,data=match_data))

match_data$iv_fem_no_controls <- predict(lm(treatment~female + perception_risk_2, data=match_data)) # with female coefficient
summary(lm(treatment~female + perception_risk_2,data=match_data))

match_data$iv_fem_controls <- predict(lm(treatment~female + perception_risk_2 + age + education + prof_training + factor(employment)
                                         + east + factor(party_pref) + polint + internal_effic_ind + external_effic_ind + round + third_round, data=match_data)) # with female coefficient


# LATE with risk perception only
summary(lm(wtr_cand~iv_no_controls, match_data))
summary(lm(wtr_engage~iv_no_controls, match_data))
summary(lm(information~iv_no_controls, match_data))

# LATE - females
summary(lm(wtr_cand~female*iv_fem_no_controls, match_data))
summary(lm(wtr_engage~female*iv_fem_no_controls, match_data))
summary(lm(information~female*iv_fem_no_controls, match_data))


# LATE - f w/ controls
summary(lm(wtr_cand~female*iv_fem_controls + age + education + prof_training + factor(employment)
           + east + factor(party_pref) + polint + internal_effic_ind + external_effic_ind + round + third_round, data=match_data))
summary(lm(wtr_engage~female*iv_fem_controls + age + education + prof_training + factor(employment)
           + east + factor(party_pref) + polint + internal_effic_ind + external_effic_ind + round + third_round, data=match_data))
summary(lm(information~female*iv_fem_controls + age + education + prof_training + factor(employment)
           + east + factor(party_pref) + polint + internal_effic_ind + external_effic_ind + round + third_round, data=match_data))

# Plot CACE results - overall population
#iv_eff <-
#  rbind.data.frame(coeftest(lm(wtr_cand~iv_no_controls, match_data), vcov = vcovHC(lm(wtr_cand~iv_no_controls, match_data), type = "HC0"))[2,1:2],
#                   coeftest(lm(wtr_engage~iv_no_controls, match_data), vcov = vcovHC(lm(wtr_engage~iv_no_controls, match_data), type = "HC0"))[2,1:2],
#                   coeftest(lm(information~iv_no_controls, match_data), vcov = vcovHC(lm(information~iv_no_controls, match_data), type = "HC0"))[2,1:2]
#  )

#names(iv_eff) <- c("coef", "se")
#iv_eff$moderator <- rep(c("Complier Average Causal Effect"))
#iv_eff$outcome <- rep(c("WTR", "WTE", "Information"))
#iv_eff$outcome <- factor(iv_eff$outcome, levels=c("WTR", "WTE", "Information"))

#ggplot(data=iv_eff, aes(x=outcome, y=coef, color=outcome)) + 
#  geom_hline(yintercept=0, linetype="dashed") +
#  geom_point(size=2.5, position=position_dodge(width=.5)) + 
#  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
#  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
#  facet_wrap(~moderator, scales="free_x") +
#  theme_bw() +
#  scale_color_manual(values=c("blue", "red", "darkgreen"), name="") +
#  xlab("Outcome") + ylab("Coefficient") +
#  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=.9, hjust=1),
#        legend.position = "bottom")
#ggsave("respondi_cace.pdf", width=7, height=5)


# Create Interaction Plot
fem_mig_eff <-
  rbind.data.frame(
    coeftest(lm(wtr_cand~female*iv_fem_no_controls, match_data), vcov = vcovHC(lm(wtr_cand~female*iv_fem_no_controls, match_data), type = "HC0"))[4,1:2],
    coeftest(lm(wtr_engage~female*iv_fem_no_controls, match_data), vcov = vcovHC(lm(wtr_engage~female*iv_fem_no_controls, match_data), type = "HC0"))[4,1:2],
    coeftest(lm(information~female*iv_fem_no_controls, match_data), vcov = vcovHC(lm(information~female*iv_fem_no_controls, match_data), type = "HC0"))[4,1:2])

names(fem_mig_eff) <- c("coef", "se")
fem_mig_eff$moderator <- rep(c("Female"), each=3)
fem_mig_eff$category <- rep(c("Treatment interaction"))
fem_mig_eff$outcome <- rep(rep(c("WTR", "WTE", "Information")), 1)
fem_mig_eff$outcome <- factor(fem_mig_eff$outcome, levels=c("WTR", "WTE", "Information"))

ggplot(data=fem_mig_eff, aes(x=outcome, y=coef, color=outcome)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
  facet_wrap(~moderator, scales="free_x") +
  theme_bw() +
  scale_color_manual(values=c("blue", "red", "darkgreen"), name="") +
  labs(y="Coefficient",
       x="Outcome",
       fill="",
       title="Complier Average Causal Effect") +
  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=.9, hjust=1),
        legend.position = "bottom")
ggsave("respondi_female_migrant_cace.pdf", width=7, height=5)