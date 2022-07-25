#********************************************************************************************
#---- POPULATION PROJECTIONS to 2025----
#********************************************************************************************
# Projecting the ERP to 2025 consists of 3 main steps:

# 1 Arranging available ERP (to June 2021) into a cohort table which will form the base of a cohort projection
# 2 Assume rates of population growth (change) within cohorts (accounting for net migration)
# 3 Project age cohorts from 2021 out to 2025 by advancing the cohorts while applying the specific age and state rate of migration. 

# The majority of the projection is based on advancing the existing cohorts of children who were aged 0 to 4 years in 2021. 
# The only assumptions that are needed are around the rate of 'year-on-year' growth which means net migration effcts. 


#----Load libraries----
library(tidyverse)
library(lubridate)
library(zoo)
library(openxlsx)
library(writexl)
library(readxl)
library(yaml)
library(plotly)
library(ggthemes)
library(broom)
library(labeling)
library(scales)
library(crosstalk)
library(farver)
library(reasdmx)
library(DT)
library(utf8)
library(fpp3)
library(rematch)
library(htmlwidgets)     
library(trelliscopejs)  


#---- Colours for plotting----
abscol <- c("#4FADE7", 	"#1A4472", 	"#F29000", 	"#993366", 	"#669966", 	"#99CC66",
            "#CC9966", 	"#666666", 	"#8DD3C7", 	"#BEBADA", 	"#FB8072", 	"#80B1D3",
            "#FDB462", 	"#B3DE69", 	"#FCCDE5", 	"#D9D9D9", 	"#BC80BD", 	"#CCEBC5", 	"#ffcc99")


#----Factors for state ----
State_lvl <- c("NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust")


#------------------------------------------------
# #---Load and prepare ERP from ABS.stat data explorer ---
#-----------------------------------------------

# ---From: Q2-2001 to Q2-2021; Ages: 0,1,2,3,4,5,6; States/terr + Aust ----
ERP_sdmx <- "https://api.data.abs.gov.au/data/ABS,ERP_Q,1.0.0/1.3.7+6+5+4+3+2+1+0..Q?startPeriod=2001-Q2&endPeriod=2021-Q2&dimensionAtObservation=AllDimensions"

download.file(ERP_sdmx,'erp.xml')

ERP <- read_sdmx('./erp.xml')

#--- Clean / format ERP data.  
ERP2 <- ERP %>% 
  mutate(Age = as.numeric(AGE), ERP = as.numeric(ObsValue),
         Q = substr(TIME_PERIOD,6,7), 
         yr = as.numeric(substr(TIME_PERIOD,1,4))) %>% 
  filter(Age <=9, SEX == "3") %>% 
  mutate(Year = make_date(yr), month = 6, day = 30) %>% 
  mutate(Year = format(Year, format = "%Y"))%>% 
  mutate(STATE = case_when(REGION == "AUS" ~ "Aust",
                           REGION == "1" ~ "NSW", 
                           REGION == "2" ~ "Vic", 
                           REGION == "3" ~ "Qld", 
                           REGION == "4" ~ "SA",  
                           REGION == "5" ~ "WA",  
                           REGION == "6" ~ "Tas", 
                           REGION == "7" ~ "NT",  
                           REGION == "8" ~ "ACT")) %>% 
  mutate(state = factor(STATE, levels = State_lvl)) %>% 
  mutate(Cohort = yr - Age) %>%                    # Derives the `Cohort` column which represents the year in which they were aged 0 in the ERP.  
  filter(Q == "Q2", Cohort >2000) %>%               # Start with 2006 cohort 
  select(Year, yr, state, Age, ERP, Cohort) %>% 
  arrange(Cohort, state, yr)

Latest <- max(ERP2$yr)  # Assign latest avail ERP year


#--- Arrange 2006-2021 ERP 4 / 5 in separate cols ---
ERP3 <- ERP2 %>% 
  select(-Cohort) %>% 
  pivot_wider(1:3, names_from = Age, names_prefix = "Aged_", values_from = ERP) %>% 
  select(Year, state, Aged_4, Aged_5) %>% 
  drop_na() %>% 
  arrange(state, Year)



#---  Arranging available ERP (to June 2021) into a cohort table ---
ERP_0 <- ERP2 %>% 
  filter(Age == 0) %>% 
  mutate(Cohort = as.numeric(Year),
         ERP0 = ERP,
         Yr_in_ERP4 = Cohort + 4, 
         Yr_in_ERP5 = Cohort + 5) %>% 
  select(Cohort, state, Yr_in_ERP4, Yr_in_ERP5, ERP0) 

ERP_1 <- ERP2 %>% 
  filter(Age == 1) %>% 
  mutate(Cohort = as.numeric(Year)-Age,
         ERP1 = ERP,
         Yr_in_ERP4 = Cohort + 4) %>% 
  select(Cohort, state, Yr_in_ERP4, ERP1) 

ERP_2 <- ERP2 %>% 
  filter(Age == 2) %>% 
  mutate(Cohort = as.numeric(Year)-Age,
         ERP2 = ERP,
         Yr_in_ERP4 = Cohort + 4) %>% 
  select(Cohort, state, Yr_in_ERP4, ERP2) 

ERP_3 <- ERP2 %>% 
  filter(Age == 3) %>% 
  mutate(Cohort = as.numeric(Year)-Age,
         ERP3 = ERP,
         Yr_in_ERP4 = Cohort + 4) %>% 
  select(Cohort, state, Yr_in_ERP4, ERP3) 

ERP_4 <- ERP2 %>% 
  filter(Age == 4) %>% 
  mutate(Cohort = as.numeric(Year)-Age,
         ERP4 = ERP,
         Yr_in_ERP4 = Cohort + 4) %>% 
  select(Cohort, state, Yr_in_ERP4, ERP4) 

ERP_5 <- ERP2 %>% 
  filter(Age == 5) %>% 
  mutate(Cohort = as.numeric(Year)-Age,
         ERP5 = ERP,
         Yr_in_ERP4 = Cohort + 4) %>% 
  select(Cohort, state, Yr_in_ERP4, ERP5) 

Cohort_ERP21 <- ERP_0 %>% 
  left_join(ERP_1, by = c("Cohort", "Yr_in_ERP4", "state")) %>% 
  left_join(ERP_2, by = c("Cohort", "Yr_in_ERP4", "state")) %>% 
  left_join(ERP_3, by = c("Cohort", "Yr_in_ERP4", "state")) %>% 
  left_join(ERP_4, by = c("Cohort", "Yr_in_ERP4", "state")) %>% 
  left_join(ERP_5, by = c("Cohort", "Yr_in_ERP4", "state")) %>% 
  select(Cohort, state, everything(), -Yr_in_ERP5)

Table_Cohort_ERP21 <- Cohort_ERP21 %>% 
  select(-Yr_in_ERP4) %>% 
  rename(`Birth cohort` = Cohort, State = state) %>% 
  arrange(State)

Table_Cohort_ERP21 %>% 
  DT::datatable(extensions = 'Buttons', filter = 'top', 
                options = list(dom = 'lBfrtip',
                               buttons = c('excel'),
                               lengthMenu = list(c(10,25,50,-1),
                                                 c(10,25,50,"All"))))

# The 'Table_Cohort_ERP21' table above provides the basis for projecting of ERP from 2021 thru to age 4 years by 2025. 
# All we need now is the cohort adjustment factors to apply to account for migration. 

#***************************************************************************************************
#---- Assume rates of population growth (change) within cohorts (accounting for net migration)----
#****************************************************************************************************

# In this scenario we have incomplete birth cohorts from 2017 to 2021. Our job is to fill in the NAs by advancing the cohort with a migration adjustment.
# 
# Migration adjustment takes a historic look at trends and patterns to come up with plausible assumptions - similar to what ABS population projections does.  

# Take a look at the state migration picture over time

# Pivot long for creating new variables
Cohort_YBFSL <- Cohort_ERP21 %>%
  arrange(state, Cohort) %>% 
  pivot_longer(4:9, names_to = "Cohort_age", values_to = "ERP_at_age") %>%
  mutate(Age = case_when(Cohort_age == "ERP0" ~ 0,
                         Cohort_age == "ERP1" ~ 1,
                         Cohort_age == "ERP2" ~ 2,
                         Cohort_age == "ERP3" ~ 3,
                         Cohort_age == "ERP4" ~ 4,
                         Cohort_age == "ERP5" ~ 5)) %>%
  mutate(Year = Cohort + Age) %>%
  select(Cohort, state, Yr_in_ERP4, Age, Year, ERP_at_age)


# Calculate the annual within-cohort growth between ages by state (restrict cohort from 2010 on)
chg <- ERP2 %>% 
  filter(Age<7) %>% 
  arrange(Cohort, state, yr) %>% 
  group_by(state, Cohort) %>%  
  mutate(chg = ERP - lag(ERP)) %>% 
  mutate(p_chg = round(chg/(ERP - chg),5)) %>% 
  filter(Cohort >2009) %>% 
  drop_na()

#--- Visualise annual within-cohort ERP growth for children (0-6 years)---

# Plot annual population growth within cohorts (0->1yrs, 1->2yrs etc aggregate those into single % (for transitions 0->1 thru to 5->6 yrs by year by state)
agg_chg_yr <- chg %>% 
  mutate(Year = factor(Year)) %>% 
  group_by(state, Year, yr) %>% 
  summarise(erp = sum(ERP), 
            chg = sum(chg)) %>% 
  ungroup() %>% 
  mutate(chg_p = round(chg/erp*100,1)) %>% 
  select(-erp)

p1 <- agg_chg_yr %>% 
  filter(yr>2011) %>% 
  ggplot(aes(x = yr, y = chg_p))+
  geom_col(fill= "#4FADE7")+
  scale_x_continuous(breaks = seq(from = 2012, to = 2021, by = 2))+
  facet_wrap(~ state, scales = "fixed", nrow = 3, ncol = 3)+
  xlab("Year")+
  ylab("Annual average growth (%)")+
  theme(legend.position = "none")

# Cohort annual growth (avg 1-6 years) Column plot
p1

# Shows us pattern from 2012 to 2021.  Key points are:

# 1) Prior to 2020, all states were generally positive. 
# 2) NT was very different with consistent cohort declines from 2013-14 through to 2020-21
# 3) 2020-21 year (full pandemic year) was lowest overall growth across period (-0.2%) with negative change in:
#  NSW (-0.4%), Vic (-0.9%), NT (-1.2%), ACT(-0.5%),
#  Contrast that with Tas, WA, SA & Qld 

# Come up with plausible assumptions about jurisdiction's cohort change (migration) for:2022, 2023, 2024, 2025

# 1) Average annual cohort growth (single year ages 1 to 6 years) from 2014-2019. This is 'pre-pandemic' growth.  
# 2) Cohort growth from June 2020 to June 2021  - 'mid-pandemic'

#------------
# Scenario A:
#------------
#1. A_2022 == historical % change between June 2020 and June 2021.  (Quarterly migration indicates continuation at least up to Q3 2021)
#2. A_2023 == 0.5 x  historical % change between June 2020 and June 2021 (easing)
#3. A_2024:2025 = 1.0 x historical %    (back to average pre-pandemic growth in 2015-19) 


#--- growth from June 2020 to June 2021  one year to June 2021 
A_2022_23 <- chg %>% 
  filter(yr> 2020) %>%    # 1 yr to June 2021 
  group_by(state, Age) %>% 
  summarise(A_2022 = round(mean(p_chg),5), 
            A_2023 = round(mean(p_chg*0.5),5)) %>%
  ungroup() 


# Calculate the mean annual cohort growth for each transition (0->1, 1->2 etc) over 5 years (2015-19) (i.e. pre-covid high growth)  
# Five years to 2019 (pre-pandemic  = 2015 to 2019)
A_2024_25 <- chg %>% 
  filter(yr> 2015, yr<2020) %>%  # # n of YoY chg = 'over n years'. Therefore incl base yr is n+1
  group_by(state, Age) %>% 
  summarise(A_2024 = round(mean(p_chg),5)) %>%  
  ungroup() %>% 
  select(-Age, -state)

# Combine scenariosA + repeat 2024 rate as A_2025
scenariosA <- A_2022_23 %>% 
  bind_cols(A_2024_25) %>% 
  mutate(A_2025 = A_2024)

#------------
# Scenario B:
#------------
#1. B_2022 == historical % change between June 2020 and June 2021 (for all states) 
#2. B_2023n == 0.5 x  historical % change between June 2020 and June 2021 (for states which were negative in 2020-21)
#2. B_2023p == 0.5 x historical % change in pre-pandemic 2015-19 period (for states which were positive in 2020-21)  
#3. B_2024:2025 = 1.0 x historical % (back to pre-pandemic growth) 

# Make an indicator of +ve / -ve in 2021
ind <- agg_chg_yr %>% 
  filter(Year == "2021") %>% 
  mutate(chg21 = case_when(chg_p>0 ~ "pos", TRUE ~ "neg")) %>% 
  select(state, chg21)


B_2022 <- chg %>% 
  filter(yr> 2020) %>%    # 1 yr to June 2021 
  group_by(state, Age) %>% 
  summarise(B_2022 = round(mean(p_chg),5)) %>% 
  ungroup() 

B_2023n <- chg %>% 
  left_join(ind, by = "state") %>% 
  filter(chg21== "neg") %>% 
  filter(yr> 2020) %>%    
  group_by(state, Age) %>% 
  summarise(B_2023 = round(mean(p_chg*0.5),5)) %>% 
  ungroup() 

B_2023p <- chg %>% 
  left_join(ind, by = "state") %>% 
  filter(chg21== "pos") %>% 
  filter(yr> 2015, yr<2020) %>% 
  group_by(state, Age) %>% 
  summarise(B_2023 = round(mean(p_chg*0.5),5)) %>% 
  ungroup() 

B_2023 <- B_2023n %>% 
  bind_rows(B_2023p) %>% 
  arrange(state, Age) %>% 
  select(B_2023)

B_2024_25 <- A_2024_25 %>% 
  rename(B_2024 = A_2024)

#---------------------------
#----- Combine scenariosB----
#--------------------------
scenariosB <- B_2022 %>% 
  bind_cols(B_2023, B_2024_25) %>% 
  mutate(B_2025 = B_2024)


#---Transpose scenarios to long, append
scenariosAL <- scenariosA %>% 
  pivot_longer(3:6, names_to = "scenario", values_to = "inv_chg_p") %>% 
  mutate(Year = as.integer(substr(scenario,3,6))) %>% 
  mutate(scenario = substr(scenario,1,1))

scenariosBL <- scenariosB %>% 
  pivot_longer(3:6, names_to = "scenario", values_to = "inv_chg_p") %>% 
  mutate(Year = as.integer(substr(scenario,3,6))) %>% 
  mutate(scenario = substr(scenario,1,1))

scenariosL <- scenariosAL %>% 
  bind_rows(scenariosBL) %>% 
  mutate(scenario = factor(scenario))

# Plot
scenarios_p <-  scenariosL %>% 
  mutate(percent = round(inv_chg_p * 100,2)) %>% 
  mutate(scenario = factor(scenario)) %>% 
  mutate(yr = factor(Year)) %>% 
  mutate(Age_turn = case_when(Age == 6 ~ "5 to 6 yr", 
                              Age == 5 ~ "4 to 5 yr", 
                              Age == 4 ~ "3 to 4 yr",
                              Age == 3 ~ "2 to 3 yr", 
                              Age == 2 ~ "1 to 2 yr", 
                              Age == 1 ~ "0 to 1 yr") )

# Series A&B Growth 
p2 <- scenarios_p %>% 
  ggplot(aes(x = Age_turn, y = percent, fill = paste0(Age_turn, ", ", scenario)))+
  geom_col()+
  coord_flip()+
  scale_colour_manual(values = abscol, aesthetics = "fill")+
  ggtitle("Avg annual growth" )+
  facet_trelliscope(~ state + yr + scenario, scales = "same", nrow = 2, ncol = 4, width = 400, as_plotly = T )+
  # facet_wrap(~ state + yr, scales = "fixed", ncol = 4)+
  guides(colour=FALSE)+
  theme(legend.position = "none")

p2


#  The remaining `NA` cells in the table above be calculated from the value 
# of the left hand cell x that state's _within-cohort growth rate_.  

# **Note: Still need to implement this change so it can be seen/selected alongside the current assumptions.** 

### A ten year view of cohort change
#To see the 2015-19 average in a wider time context, the plot below shows the annual change at each age transition from 2009-10 to 2019-20.  

# Calculate the annual within-cohort growth between ages by state
chg0 <- ERP2 %>% 
  filter(Age<7) %>% 
  arrange(Cohort, state, yr) %>% 
  group_by(state, Cohort) %>%  
  mutate(chg = ERP - lag(ERP)) %>% 
  mutate(p_chg = round(chg/(ERP - chg),5)) %>% 
  drop_na()


# Cohort annual growth 1-5 years line  plot
p3 <- chg0 %>% 
  mutate(Age_turn = case_when(Age == 5 ~ "4 to 5 yr", 
                              Age == 4 ~ "3 to 4 yr",
                              Age == 3 ~ "2 to 3 yr", 
                              Age == 2 ~ "1 to 2 yr", 
                              Age == 1 ~ "0 to 1 yr") ) %>% 
  mutate(Year = make_date(yr)) %>% 
  filter(yr != Cohort, yr> 2010) %>% 
  group_by(state) %>% 
  ggplot(aes(x = Year, y = round(p_chg*100,2), colour = Age_turn))+
  geom_point()+
  geom_line(size = 0.75)+
  geom_hline(yintercept=0, size = 0.75, alpha = 0.75, linetype = 'dotted', color = "black")+
  facet_wrap(~ state, scales = "fixed")+
  scale_colour_manual(values = abscol, aesthetics = "colour")+
  ggtitle("2010-11 to 2019-20 annual rate change within cohorts")+
  labs(y = "% change", x = "Year")+
  facet_wrap(~ state, scales = "fixed", nrow = 3)

p3


## Apply the cohort growth scenarios to the ERP cohort table 

# Join growth rates to Cohort_YBFSL
Cohort_YBFSL2a <- Cohort_YBFSL %>% 
  left_join(filter(scenariosL, scenario == "A"), by = c("state", "Year", "Age"))%>% 
  rename(inv_chg_p_A = inv_chg_p, scenario_A = scenario) %>% 
  left_join(filter(scenariosL, scenario == "B"), by = c("state", "Year", "Age"))%>% 
  rename(inv_chg_p_B = inv_chg_p, scenario_B = scenario) %>% 
  group_by(state, Cohort) %>% 
  mutate(ERP_A = case_when(is.na(ERP_at_age) ~ lag(ERP_at_age) +  (lag(ERP_at_age) * inv_chg_p_A), TRUE ~ ERP_at_age)) %>% 
  mutate(ERP_B = case_when(is.na(ERP_at_age) ~ lag(ERP_at_age) +  (lag(ERP_at_age) * inv_chg_p_B), TRUE ~ ERP_at_age)) 

# Iterate to complete cohorts
Cohort_YBFSL2b <- Cohort_YBFSL2a %>%  
  group_by(state, Cohort) %>% 
  mutate(ERP_A = case_when(is.na(ERP_A) ~ lag(ERP_A) +  lag(ERP_A) * inv_chg_p_A, TRUE ~ ERP_A)) %>% 
  mutate(ERP_B = case_when(is.na(ERP_B) ~ lag(ERP_B) +  lag(ERP_B) * inv_chg_p_B, TRUE ~ ERP_B)) 

Cohort_YBFSL2c <- Cohort_YBFSL2b %>%  
  group_by(state, Cohort) %>% 
  mutate(ERP_A = case_when(is.na(ERP_A) ~ lag(ERP_A) +  lag(ERP_A) * inv_chg_p_A, TRUE ~ ERP_A)) %>% 
  mutate(ERP_B = case_when(is.na(ERP_B) ~ lag(ERP_B) +  lag(ERP_B) * inv_chg_p_B, TRUE ~ ERP_B)) 

Cohort_YBFSL2d <- Cohort_YBFSL2c %>%  
  group_by(state, Cohort) %>% 
  mutate(ERP_A = case_when(is.na(ERP_A) ~ lag(ERP_A) +  lag(ERP_A) * inv_chg_p_A, TRUE ~ ERP_A)) %>% 
  mutate(ERP_B = case_when(is.na(ERP_B) ~ lag(ERP_B) +  lag(ERP_B) * inv_chg_p_B, TRUE ~ ERP_B)) %>%  
  filter(Year<2026) %>% 
  ungroup()


Proj_2025 <- Cohort_YBFSL2d %>% 
  select(1,2,4,5,6,11,12) %>% 
  mutate(ERP_A = as.integer(ERP_A), 
         ERP_B = as.integer(ERP_B))


#-- Subset 'Cohort_scenario' (Zero, Med, High) to 3, 4, 5 year olds 
# Aged 3
age_3 <- Proj_2025 %>% 
  filter(Year>2005, Age == 3)%>%
  select(Cohort, state, Year, ERP_A, ERP_B)%>% 
  pivot_longer(4:5, names_to = "Assumption", values_to = "ERP_3yr")

# Aged 4
age_4 <- Proj_2025 %>% 
  filter(Year>2005, Age == 4)%>%
  select(Cohort, state, Year, ERP_A, ERP_B)%>% 
  pivot_longer(4:5, names_to = "Assumption", values_to = "ERP_4yr")

# Aged 5
age_5 <-  Proj_2025 %>% 
  filter(Year>2005, Age == 5)%>%
  select(Cohort, state, Year, ERP_A, ERP_B)%>% 
  pivot_longer(4:5, names_to = "Assumption", values_to = "ERP_5yr")

# Join the age groups by year & Assumption 
Cohort_ages_scen <- age_5 %>% 
  left_join(age_4, by = c("state", "Year", "Assumption")) %>% 
  left_join(age_3, by = c("state",  "Year", "Assumption"))%>% 
  select(Year, Cohort, state, everything()) %>% 
  rename(Cohort_4yrs = Cohort) %>% 
  select(-2,-4,-7,-9 )  # Drop ERP_3yr


#------------------
# Plot projection 
#------------------

Proj_2025_4_5 <- Cohort_ages_scen %>% 
  mutate(Assumption = case_when(Year <2022 ~ "NA", TRUE ~ Assumption)) %>% 
  distinct()

# Make dummy row for 2022
dumm <- Proj_2025_4_5 %>% 
  filter(Year==  "2022") %>% 
  mutate(Assumption = "NA")

Proj_2025_4_5 <- Proj_2025_4_5 %>% 
  bind_rows(dumm)


# Save for YBFS model
write.xlsx(Proj_2025_4_5, file = "./Proj2025_4yr_5yrs.xlsx")

# PLot  ERP 4
Proj_2025_4_5 %>% 
  mutate(Assumption = factor(Assumption)) %>% 
  ggplot(aes(Year, ERP_4yr, colour = Assumption))+
  geom_line(size = 0.75)+
  scale_colour_manual(values = abscol)+
  facet_wrap(~ state, nrow = 3, scales = "free")


# PLot  ERP 5
Proj_2025_4_5 %>% 
  mutate(Assumption = factor(Assumption)) %>% 
  ggplot(aes(Year, ERP_5yr, colour = Assumption))+
  geom_line(size = 0.75)+
  scale_colour_manual(values = abscol)+
  facet_wrap(~ state, nrow = 3, scales = "free")

#--- Write key data to Excel---
dflist <- list("Cohorts to project" = Table_Cohort_ERP21,  "Avg cohort chg 0-6" = agg_chg_yr,
               "Cohort Annual rate chg"  = chg0, 
               "Cohort chg assumption" = scenarios_p, 
               "ERP and Projection" = Proj_2025, 
               "Projection 4 5 yrs" = Proj_2025_4_5)

write.xlsx(dflist, "ERP_projection.xlsx")

