library(glue)
library(stringr)
library(WriteXLS)
library(readtext)
library(readxl)
library(ngram)
library(tidyverse)
library(gdata)
library(lubridate)
library(haven)
library(stringi)
library(xtable)
library(lfe)
library(stargazer)
library(gridExtra)  
library(sf)

rm(list=ls())
getwd()

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
##### Replication script for observational analyses:
rm(list=ls())


## specify correct replication directory: 
setwd("/Users/fabio/Library/CloudStorage/Dropbox/HU_dropbox/projects/KommPolViolence/submission/ejpr/final_submission/replication_materials")

dat_observational <- read_rds("data/ejpr_observational_data.rds")
names(dat_observational)


## main outcome: p_female_city_diff
  # A differenced measure of the overall share of female candidates on city's party lists

m0 <- felm(p_female_city_diff ~ crime_dum   + pop_ln + density | state  + election_year | 0 | city_id,
            data = dat_observational) 
m1 <- felm(p_female_city_diff ~ crime_sum   + pop_ln + density | state + election_year | 0 | city_id,
           data = dat_observational)  

m2 <- felm(p_female_city_diff ~ politician_dum   + pop_ln + density | state  + election_year| 0 | city_id,
           data = dat_observational)  
m3 <- felm(p_female_city_diff ~ politician_sum   + pop_ln + density | state  + election_year| 0 | city_id,
            data = dat_observational)  

m4 <- felm(p_female_city_diff ~ severe_politician_dum   + pop_ln + density | state  + election_year| 0 | city_id,
      data = dat_observational)  
m5 <- felm(p_female_city_diff ~ severe_politician_sum   + pop_ln + density | state  + election_year| 0 | city_id,
          data = dat_observational)  

## create regression table 1 (main manuscript)
stargazer(m0, m1,  m2, m3, m4, m5,  
          type='text',
          add.lines = list(c("Municipality Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes"),
                           c("Year Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes") ),
          out =  c("results/table_1.tex"))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#### Create appendix tables for observational data:
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## C.2. Summary statistics table:
dat_descr <- dat_observational %>% select(  
                                  p_female_city, p_female_city_diff, 
                                 crime_dum, crime_sum,
                                 politician_dum, politician_sum,
                                 severe_politician_dum, severe_politician_sum,
                                 pop_ln, density )

dat_descr_tab <- psych::describe(dat_descr) 
dat_descr_tab <- dat_descr_tab  %>% 
  select(-vars, -trimmed, -skew, -kurtosis, -se, - mad)

dat_descr_tab
print(xtable(dat_descr_tab), type="latex",
      file = "results/table_c2.tex" ) 

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## Table C.4: Does Outcome Predict IV?

m0 <- felm( crime_dum ~ p_female_city_diff   + pop_ln+ density | state  + election_year | 0 | city_id,
            data = dat_observational)
m1 <- felm( crime_sum  ~ p_female_city_diff  + pop_ln+ density | state + election_year | 0 | city_id,
            data = dat_observational)

m2 <- felm( severe_politician_dum ~ p_female_city_diff   + pop_ln+ density | state  + election_year| 0 | city_id,
            data = dat_observational)
m3 <- felm(severe_politician_sum ~ p_female_city_diff   + pop_ln+ density | state  + election_year| 0 | city_id,
           data = dat_observational)

m4 <- felm( politician_dum ~ p_female_city_diff   + pop_ln+ density | state  + election_year| 0 | city_id,
            data = dat_observational)
m5 <- felm(politician_sum ~ p_female_city_diff   + pop_ln+ density | state  + election_year| 0 | city_id,
           data = dat_observational)

stargazer(m0, m1,  m2, m3,  m4, m5,    type='text',
          add.lines = list(c("State + Year Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes")),
          omit.stat=c("f", "ser"),
          out =  c("results/table_c4.tex"))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## Table C.5: Do IVs predict Level Outcome? 
m0 <- felm(p_female_city ~ crime_dum   + pop_ln + density | state  + election_year | 0 | city_id,
           data = dat_observational)
m1 <- felm(p_female_city ~ crime_sum   + pop_ln+ density | state + election_year | 0 | city_id,
           data = dat_observational)
m2 <- felm(p_female_city ~ severe_politician_dum   + pop_ln+ density | state  + election_year| 0 | city_id,
           data = dat_observational)
m3 <- felm(p_female_city ~ severe_politician_sum   + pop_ln+ density | state  + election_year| 0 | city_id,
           data = dat_observational)
m4 <- felm(p_female_city ~ politician_dum   + pop_ln+ density | state  + election_year| 0 | city_id,
           data = dat_observational)
m5 <- felm(p_female_city ~ politician_sum   + pop_ln+ density | state  + election_year| 0 | city_id,
           data = dat_observational)

stargazer(m0, m1,  m4, m5,   m2, m3,   type='text',
          add.lines = list(c("State + Year Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes")),
          omit.stat=c("f", "ser"),
          out =  c("results/table_c5.tex"))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## Figure C.3: Party-based jackknifing:

names(dat_observational)

omit <- c("afd", 'cdu', 'fdp', "spd", 'greens', 'linke')
res_loop <- tibble()

for (i in 1:length(omit)) {
  print(paste0("crime_dum_",omit[i]))
  
  ## crime
  dv = paste0('fem_share_diff_wo_',omit[i])
  # dv stays constant
  
  iv = paste0("crime_dum_",omit[i])
  # iv changes across models:
  m0 <- felm(eval(parse(text = dv ))  ~ eval(parse(text = iv ))   + pop_ln + density | state  + election_year | 0 | city_id,
             data = dat_observational) %>% broom::tidy() %>% 
    filter(!str_detect(term, "pop_ln|density")    ) %>% 
    mutate(      term = paste0("crime_dum_",omit[i]),
                 wo_party = omit[i],
                 outcome = dv )
  res_loop <- bind_rows(res_loop, m0)  
  
  iv = paste0("crime_sum_",omit[i])
  m1 <- felm(eval(parse(text = dv )) ~ eval(parse(text = iv ))   + pop_ln + density | state  + election_year | 0 | city_id,
             data = dat_observational) %>% broom::tidy() %>% 
    filter(!str_detect(term, "pop_ln|density")    ) %>% 
    mutate(      term = paste0("crime_sum_",omit[i]),
                 wo_party = omit[i],
                 outcome = dv  )
  res_loop <- bind_rows(res_loop, m1)  
  ## politician
  iv = paste0("politician_dum_",omit[i])
  m3 <- felm(eval(parse(text = dv )) ~ eval(parse(text = iv ))   + pop_ln + density | state  + election_year | 0 | city_id,
             data = dat_observational) %>% broom::tidy() %>% 
    filter(!str_detect(term, "pop_ln|density")    ) %>% 
    mutate(      term = paste0("politician_dum_",omit[i]),
                 wo_party = omit[i],
                 outcome = dv  )
  res_loop <- bind_rows(res_loop, m3)  
  
  iv = paste0("politician_sum_",omit[i])
  m4 <- felm(eval(parse(text = dv )) ~ eval(parse(text = iv ))   + pop_ln + density | state  + election_year | 0 | city_id,
             data = dat_observational) %>% broom::tidy() %>% 
    filter(!str_detect(term, "pop_ln|density")    ) %>% 
    mutate(      term = paste0("politician_sum_",omit[i]),
                 wo_party = omit[i] ,
                 outcome = dv )
  res_loop <- bind_rows(res_loop, m4)  
  ## severe politician
  iv = paste0("severe_politician_dum_",omit[i])
  m6 <- felm(eval(parse(text = dv )) ~ eval(parse(text = iv ))   + pop_ln + density | state  + election_year | 0 | city_id,
             data = dat_observational) %>% broom::tidy() %>% 
    filter(!str_detect(term, "pop_ln|density")    ) %>% 
    mutate(      term = paste0("severe_politician_dum_",omit[i]),
                 wo_party = omit[i] ,
                 outcome = dv )
  res_loop <- bind_rows(res_loop, m6)  
  
  iv = paste0("severe_politician_sum_",omit[i])
  m7 <- felm(eval(parse(text = dv )) ~ eval(parse(text = iv ))   + pop_ln + density | state  + election_year | 0 | city_id,
             data = dat_observational) %>% broom::tidy() %>% 
    filter(!str_detect(term, "pop_ln|density")    ) %>% 
    mutate(      term = paste0("severe_politician_sum_",omit[i]),
                 wo_party = omit[i],
                 outcome = dv  )
  res_loop <- bind_rows(res_loop, m7)  
}  

res_plot <- res_loop %>%   mutate(  conf.low90 = estimate - qnorm(0.95)*std.error,
                                    conf.high90 = estimate + qnorm(0.95)*std.error,
                                    conf.low95 = estimate - qnorm(0.975) * std.error,
                                    conf.high95 = estimate + qnorm(0.975) * std.error,
                                    term2 = sub("_[^_]*$", "", term)
) %>% 
  mutate(term2 = case_when(
    term2 == "crime_dum" ~ "Any Political Crime",
    term2 == "crime_sum" ~ "Any Political Crime Count",
    term2 == "politician_dum" ~ "Attack ag. Politician",
    term2 == "politician_sum" ~ "Attack ag. Politician Count",
    term2 == "severe_politician_dum" ~ "Severe Attack ag. Politician",
    term2 == "severe_politician_sum" ~ "Severe Attack ag. Politician Count",
    TRUE ~ term2
  ) ) %>% 
  mutate(wo_party = case_when(
    wo_party == "afd" ~ "Excluding AfD",
    wo_party == "cdu" ~ "Excluding CDU/CSU",
    wo_party == "fdp" ~ "Excluding FDP",
    wo_party == "greens" ~ "Excluding Greens",
    wo_party == "linke" ~ "Excluding Linke",
    wo_party == "spd" ~ "Excluding SPD",
    TRUE ~ wo_party
  ) ) %>% 
  mutate(term2 = fct_relevel(term2, 
                             "Any Political Crime",
                             "Attack ag. Politician",
                             "Severe Attack ag. Politician",
                             "Any Political Crime Count",
                             "Attack ag. Politician Count",
                             "Severe Attack ag. Politician Count"))

## Plot leave one out:

pd <- position_dodge(1)
p1 <- ggplot(res_plot, aes(term2, estimate)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90),
                position = pd,
                width = 0, size = 1.2) +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95),
                position = pd,
                width = 0, size = 0.5) +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  facet_wrap(~wo_party, scales = 'free_y', ncol = 2) +
  theme_bw() +
  coord_flip()+
  scale_color_manual(values=c("blue", "red", "darkgreen"), name="") +
  scale_shape_manual(values=c(16, 17, 15), name="") +
  xlab("") + ylab("Coefficient") +
  theme(text=element_text(size=16), 
        legend.position = "bottom")

p1
## Save
ggsave("results/figure_c3.png",
       width = 16, height = 12)






## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## C.6 and C.7:  Split sample analysis for high- and low-representation municipalities

## C.6: High representation municipalities
dat_high <- dat_observational  %>% 
  filter(fem_representation_pre == 1)

m0 <- felm(p_female_city_diff ~ crime_dum   + pop_ln + density | state  + election_year | 0 | city_id,
            data = dat_high) 
m1 <- felm(p_female_city_diff ~ crime_sum   + pop_ln + density | state + election_year | 0 | city_id,
           data = dat_high)  
m2 <- felm(p_female_city_diff ~ politician_dum   + pop_ln + density | state  + election_year| 0 | city_id,
           data = dat_high)  
m3 <- felm(p_female_city_diff ~ politician_sum   + pop_ln + density | state  + election_year| 0 | city_id,
            data = dat_high)  
m4 <- felm(p_female_city_diff ~ severe_politician_dum   + pop_ln + density | state  + election_year| 0 | city_id,
      data = dat_high)  
m5 <- felm(p_female_city_diff ~ severe_politician_sum   + pop_ln + density | state  + election_year| 0 | city_id,
          data = dat_high)  

stargazer(m0, m1,  m2, m3, m4, m5,
          type='text',
          add.lines = list(c("Municipality Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes"),
                           c("Year Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes") ),
          out =  c("results/table_c6.tex"))

## C.7: Low representation municipalities
dat_low <- dat_observational  %>% 
  filter(fem_representation_pre ==  0)

m0 <- felm(p_female_city_diff ~ crime_dum   + pop_ln + density | state  + election_year | 0 | city_id,
            data = dat_low) 
m1 <- felm(p_female_city_diff ~ crime_sum   + pop_ln + density | state + election_year | 0 | city_id,
           data = dat_low)  
m2 <- felm(p_female_city_diff ~ politician_dum   + pop_ln + density | state  + election_year| 0 | city_id,
           data = dat_low)  
m3 <- felm(p_female_city_diff ~ politician_sum   + pop_ln + density | state  + election_year| 0 | city_id,
            data = dat_low)  
m4 <- felm(p_female_city_diff ~ severe_politician_dum   + pop_ln + density | state  + election_year| 0 | city_id,
      data = dat_low)  
m5 <- felm(p_female_city_diff ~ severe_politician_sum   + pop_ln + density | state  + election_year| 0 | city_id,
          data = dat_low)  

stargazer(m0, m1,  m2, m3, m4, m5,
          type='text',
          add.lines = list(c("Municipality Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes"),
                           c("Year Fixed Effects","Yes","Yes","Yes", "Yes","Yes","Yes","Yes", "Yes", "Yes") ),
          out =  c("results/table_c7.tex"))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## R-Adaptation of Python Figures:
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
### Figure C.1.: Replicate Crime Count Figure

# Filter the data to include only rows where crime_sum >= 1
filtered_data <- dat_observational[dat_observational$crime_sum >= 1, ]

# Create the histogram plot
ggplot(filtered_data, aes(x = crime_sum)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "steelblue") +
  labs(
    x = "Crime Distribution",  # X-axis label
    y = "Count",               # Y-axis label
    title = "Crime Count Conditional on Occurrence"  # Plot title
  ) +
  theme_minimal(base_size = 14) +   # Minimal theme with readable font size
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Centered bold title
    axis.title = element_text(size = 12)                             # Axis title size
  ) +
  scale_x_continuous(limits = c(NA, 85)) 

# Save the plot as a PNG file (optional)
ggsave("results/figure_c1.png", width = 10, height = 6, dpi = 300)

 ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
### Figure C.4.: Replicate Crime Count Figure

ggplot(dat_observational, aes(x = p_female_city_diff)) +
  geom_histogram(binwidth = 2.5, fill = "steelblue", color = "steelblue", boundary = 0) +  # Adjust binwidth for similar bins
  labs(
    x = expression(Delta ~ "Share of Female Candidates (in %)"),  # X-axis label with Delta symbol
    y = "Count",                                                 # Y-axis label
    title = "Changes in Share of Female Candidates"              # Plot title
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme for clean gridlines
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center and bold the title
    axis.title = element_text(size = 12),                              # Adjust axis label size
    panel.grid.major = element_line(color = "grey80"),                 # Add gridlines
    panel.grid.minor = element_line(color = "grey90")                  # Fainter minor gridlines
  ) +
  scale_x_continuous(breaks = seq(-100, 100, by = 25))  # Match x-axis ticks to Python plot

# Save the plot (optional)
ggsave("results/figure_c4.png", width = 10, height = 6, dpi = 300)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
### Figure C.5.: Replicate Changes in Female Candidates by Party Ideology Figure
# Load necessary party-representation data
dat_party <- readRDS("data/party_femshare_data.rds")

# Define bin range and width
bin_width <- (100 - (-100)) / 100  # Matches 81 bins in the range -70 to 70

# Create histogram with grouping by 'quota'
ggplot(dat_party, aes(x = p_female_diff, fill = quota)) +
  geom_histogram(binwidth = bin_width, boundary = -70, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("No Quota" = "blue", "Quota" = "green"),
                    name = "Female Candidate Quota") +
  labs(
    title = "Changes in Share of Female Candidates",
    x = expression(Delta ~ "Share of Female Candidates (in %)"),
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12)
  )

# Save the plot (optional)
ggsave("results/figure_c5.png", width = 10, height = 6)



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
### Replicate state plots in R:
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

plz_shape_df <- st_read("/Users/fabio/Library/CloudStorage/Dropbox/HU_dropbox/projects/KommPolViolence/submission/ejpr/final_submission/replication_materials/data/plz-5stellig.geojson")


# Convert "plz" column to numeric
plz_shape_df$plz <- as.numeric(plz_shape_df$plz)

names(dat_observational)
dat_observational %>%
  select(plz, state, crime_sum) %>%   # Select relevant columns
  group_by(plz) %>%                           # Group by 'plz'
  summarise(across(everything(), ~sum(!is.na(.)))) %>%  # Count non-NA values in each column
  arrange(state)                               # Sort by 'state'


# Drop the row where plz == 69434
dat_observational <- dat_observational %>%
  filter(plz != 69434)

# Group by 'plz' and 'state' and compute the sum
dat_observational_geo <- dat_observational %>%
  group_by(plz, state) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>% 
  ungroup()  # Equivalent to reset_index(drop=False) in Pandas

# Group by 'plz' and 'state' and compute the mean
dat_observational_geo_2 <- dat_observational %>%
  group_by(plz, state) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% 
  ungroup()


# Merge plz_shape_df with dat_observational_geo (crime_sum)
dat_observational_geo <- plz_shape_df %>%
  left_join(dat_observational_geo %>% select(plz, state, crime_sum), by = "plz")

# Display first few rows
head(dat_observational_geo)


dat_observational_geo <- dat_observational_geo %>%
  mutate(city = str_split(note, "[0-9]\\s", simplify = TRUE)[,2]) 


# Replace crime_count_party with crime_sum
dat_observational_geo <- dat_observational_geo %>%
  group_by(city) %>%
  mutate(
    state = ifelse(is.na(state), first(na.omit(state)), state),
    crime_sum = ifelse(is.na(crime_sum), first(na.omit(crime_sum)), crime_sum)
  ) %>%
  ungroup()

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
### create plots: Thuringia


# Filter for Thuringia
th_fem <- dat_observational_geo %>%
  filter(state == 16) %>%
  st_as_sf()

# Get bounding box dimensions
bbox <- st_bbox(th_fem)
aspect_ratio <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)

th_plot <- ggplot() +
  geom_sf(data = th_fem, aes(fill = crime_sum), color = "black") +
  scale_fill_gradientn(colors = rev(c("black", "darkred", "orange", "yellow")), name = "Number of Crimes") +
  ggtitle("Thuringia: Politically Motivated Crimes") +
  theme_minimal() +
  theme(
  axis.text = element_blank(),  # Remove axis labels
  axis.ticks = element_blank(), # Remove axis ticks
  panel.grid = element_blank(), # Remove grid lines
  legend.position = "right",
  plot.title = element_text(size = 14, face = "bold")
)

# Save
ggsave("results/th_cc.png", width = 8, height = 8 )

## ## ## ## ## ##
## Saxony plot: 

sn_crime <- dat_observational_geo %>%
  filter(state == 14) %>%
  st_as_sf()

sn_crime_2 <- sn_crime %>%
  filter(!plz %in% c(63936, 21258, 71737, 94259, 84434, 74638, 
                     94530, 92449, 29468, 83346, 55608))


bbox <- st_bbox(sn_crime_2)
aspect_ratio <- (bbox$ymax - bbox$ymin) / (bbox$xmax - bbox$xmin)

sn_plot <- ggplot() +
  geom_sf(data = sn_crime_2, aes(fill = crime_sum), color = "black") +
  scale_fill_gradientn(colors = rev(c("black", "darkred", "orange", "yellow")), name = "Number of Crimes") +
  ggtitle("Saxony: Politically Motivated Crimes") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(), # Remove grid lines
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

# Save the plot
ggsave("results/sn_cc.png", width = 8, height = 8 , dpi = 300)


## ## ## ## ## ##
## Brandenburg plot:

bb_crime <- dat_observational_geo %>%
  filter(state == 12) %>%
  st_as_sf()

bb_crime_2 <- bb_crime %>%
  filter(!plz %in% c(29497, 37699
                     #, 17337, 1990
                     ))

bbox <- st_bbox(bb_crime)

bb_plot <- ggplot() +
  geom_sf(data = bb_crime_2, aes(fill = crime_sum), color = "black") +
  scale_fill_gradientn(colors = rev(c("black", "darkred", "orange", "yellow")), name = "Number of Crimes") +
  ggtitle("Brandenburg: Politically Motivated Crimes") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(), # Remove grid lines
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

# Save the plot
ggsave("results/bb_cc.png", width = 8, height = 8 , dpi = 300)

## ## ## ## ## ##
## Hessen plot:

he_crime <- dat_observational_geo %>%
  filter(state == 6) %>%
  st_as_sf()

he_crime_2 <- he_crime %>%
  filter(!plz %in% c(23730, 85456, 72531, 41366, 87727, 6193, 90765, 1734, 9669, 
                     55490, 97490, 90763, 90762, 90766, 90768, 93485, 84367, 95496, 
                     96215, 95496, 96486, 89281, 86972, 6618, 67688))  

bbox <- st_bbox(he_crime_2)

he_plot <- ggplot() +
  geom_sf(data = he_crime_2, aes(fill = crime_sum), color = "black") +
  scale_fill_gradientn(colors = rev(c("black", "darkred", "orange", "yellow")), name = "Number of Crimes") +
  ggtitle("Hesse: Politically Motivated Crimes") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(), # Remove grid lines
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

# Save the plot
ggsave("results/he_cc.png", width = 8, height = 8 , dpi = 300)


## ## ## ## ## ##
## BaWue plot:

bw_crime <- dat_observational_geo %>%
  filter(state == 8) %>%
  st_as_sf()

bw_crime_2 <- bw_crime %>%
  filter(!plz %in% c(8107, 8396, 9244, 9350, 21224, 30966, 33165, 36341, 50321,
                     56317, 56653, 57290, 63683,   65391, 66538,63925 , 63930,
                     66539, 66540, 66557, 91097, 91301, 91359, 91489, 91586, 91626,
                     92360, 92366, 92702, 93192, 93489, 94259, 94374, 94377, 94481,
                     94554, 95180, 96163, 96172, 96523, 97249, 97252, 97633, 97834,
                     97877, 97896, 82061, 82285, 82335, 83368, 84337, 84434, 85095,
                     86519, 86871, 87778, 87784, 89269, 89367, 89368, 99846))

bbox <- st_bbox(bw_crime_2)

bw_plot <- ggplot() +
  geom_sf(data = bw_crime_2, aes(fill = crime_sum), color = "black") +
  scale_fill_gradientn(colors = rev(c("black", "darkred", "orange", "yellow")), name = "Number of Crimes") +
  ggtitle("Baden-Württemberg: Politically Motivated Crimes") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Remove axis labels
    axis.ticks = element_blank(), # Remove axis ticks
    panel.grid = element_blank(), # Remove grid lines
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

# Save the plot
ggsave("results/bw_cc.png", width = 8, height = 8, dpi = 300)

######
# combine plots:

## **Combine All Plots Using Your Layout**
grid.arrange(
  bb_plot, bw_plot,  # First row
  he_plot, sn_plot,  # Second row
  th_plot,           # Third row (centered)
  ncol = 2
)

# Save the combined plot
ggsave("results/figure_c2.png", grid.arrange(bb_plot, bw_plot, he_plot, sn_plot, th_plot, ncol = 2),
       width = 12, height = 18, dpi = 300)

