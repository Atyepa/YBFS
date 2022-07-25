#=======================
# YBFS data preparation
#=======================

## OBJECTIVES
#
# The objectives of this program are to use single year ERP to:
#    
# 1) Calculate the annual change in ERP cohort numbers from age 4-5 years and 5-6 years. From this, we derive an adjustment factor  
#   equivalent to the inverse of the cohort's migration growth rate. 
# 
# 2) Derive the YBFS by applying the adjustment factor `inv_chg_p` to the school enrolment numbers from the following year. 
#    E.g. to derive the YBFS for 2020, we take the 2021 first year of school enrolment counts (labelled '1st year of school (lagged)' in this analysis) and use `inv_chg_p` to reverse the effect of the migration occurring between 2020 and 2021. 
# 
# 3) Determine historic age-specific growth rates by each age transition (e.g. for 0 to 1 year olds, measure cohort growth from the
#   ERP of 0 year-olds in year n to ERP of 1 year olds in year n+1).  Use the historic patterns seen pre-pandemic (2014-2019) and 
#   more latterly within-pandemic (2020-2021) to derive migration assumptions for next 4 years (2022 to 2025).  
# 
# 4) Project ERP of children from 2022 to 2025 by taking the June 2021 cohorts as the base and advancing those cohorts while applying 
#   age-specific and state-specific assumptions on migration. 

#----Load libraries----
library(tidyverse)
library(lubridate)
library(zoo)
library(openxlsx)
library(writexl)
library(readxl)
library(yaml))
library(plotly)
library(ggthemes)
library(broom)
library(labeling)
library(scales)
library(crosstalk)
library(farver)
library(readsdmx)
library(DT)
library(utf8)
library(fpp3)
library(rematch)
library(htmlwidgets)      
library(trelliscopejs)
library(ggpubr)


# ---- Colours for plotting----
abscol <- c("#4FADE7", 	"#1A4472", 	"#F29000", 	"#993366", 	"#669966", 	"#99CC66",
            "#CC9966", 	"#666666", 	"#8DD3C7", 	"#BEBADA", 	"#FB8072", 	"#80B1D3",
            "#FDB462", 	"#B3DE69", 	"#FCCDE5", 	"#D9D9D9", 	"#BC80BD", 	"#CCEBC5", 	"#ffcc99")

#---- functions -----
rnd4 <- function(x) round(x,4)
rnd3 <- function(x) round(x,3)
rnd2 <- function(x) round(x,2)
rnd1 <- function(x) round(x,1)

#----Factors for state ----
State_lvl <- c("NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust")

#----------------------------------------
#---Enrolments to lagged enrolments  ---
#---------------------------------------

#----  Load  datacube Schools, Table 42b from  'Pre-Year 1 (Foundation Year)' by state.----

Table42b <- "https://www.abs.gov.au/statistics/people/education/schools/2021/Table%2042b%20Number%20of%20Full-time%20and%20Part-time%20Students%2C%202006-2021.xlsx"

download.file(Table42b,"datacube.xlsx", mode = "wb" )

Table2 <- read_excel(sheet = 3, range = "A5:M72866")

Fdn_yr <- Table2 %>% 
  select(1,2,9,10,13) %>% 
  rename(state = `State/Territory`, 
         grade = `Year (Grade)`,
         All_ages = `All Full-time and Part-time Student count` ) %>% 
        filter(grade == "a Pre-Year 1 (Foundation Year)") %>% 
  mutate(Age = case_when(Age == "a 4 years and under" ~ "Aged_4", 
                         Age == "b 5 years" ~ "Aged_5",
                         Age == "c 6 years" ~ "Aged_6",
                         Age == "d 7 years" ~ "Aged_7",
                         Age == "e 8 years" ~ "Aged_8", TRUE ~ Age )) %>% 
  
  mutate(state = case_when(state == "a NSW" ~ "NSW", 
                           state == "b Vic." ~ "Vic",
                           state == "c Qld" ~ "Qld",
                           state == "d SA" ~ "SA",
                           state == "e WA" ~ "WA",
                           state == "f Tas." ~ "Tas",
                           state == "g NT" ~ "NT",
                           state == "h ACT" ~ "ACT", TRUE ~ state)) 

# Sum All_ages for unique series (Year, state, Age)
sum_fdn <- Fdn_yr %>% 
  group_by(Year, state, grade, Age) %>% 
  summarise(n = sum(All_ages)) %>% 
  ungroup()


# Make Total Aust
Aust <- sum_fdn %>% 
  mutate(state = "Aust") %>% 
  select(Year, state, grade, Age, n) %>% 
  group_by(Year, state, grade, Age) %>% 
  summarise(n = sum(n))


# Append total, spread ages, fix NAs
Fdn_yr2 <- sum_fdn %>% 
  bind_rows(Aust) %>% 
  pivot_wider(1:3, names_from = Age, values_from = n) %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%  # Replace NA with 0
  mutate(All_ages = Aged_4 + Aged_5 + Aged_6 + Aged_7 + Aged_8) %>%   # sum cols  
  mutate(Total_5_6 = Aged_5 + Aged_6) %>%    # Denominator for fraction
  select(-Aged_4, -Aged_7, -Aged_8)  # Drop ages 4, 7, 8 (have values in All_ages, Aged_5 + Aged_6 required for ratio)

# Drop 'grade', apply factors
Fdn_yr3 <- Fdn_yr2 %>% 
  select(-grade) %>% 
  mutate(state = factor(state, levels = State_lvl)) %>% 
  mutate(orig_yr = Year,
         Year = make_date(year = Year-1, month = 6, day = 30)) %>%   
  mutate(Year = format(Year, format = "%Y"))


# Calculate proportions of 5yrs / 6yrs (of just 5-6 yrs)
sch_age_fraction <-Fdn_yr3 %>%  
  mutate(P5 = round(Aged_5 / Total_5_6,2), 
         P6 = round(Aged_6 / Total_5_6,2)) %>% 
  select(-Aged_5, -Aged_6, -Total_5_6, -All_ages)

# Assign fractions to total foundation year
FndnYr_lag <- Fdn_yr3 %>% 
  select(Year, state, All_ages) %>% 
  rename(val  = All_ages) %>% 
  left_join(sch_age_fraction, by = c("Year", "state")) %>% 
  mutate(Year = make_date(year = Year, month = 6, day = 30)) %>%   # Lag Year, format as date, display as year--
  mutate(Year = format(Year, format = "%Y"))


#----------------------
#---ERP to cohorts ---- 
#----------------------

#---Load and prepare ERP from ABS.Stat ---
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


# Arrange 2006-2021 ERP in sep cols 
ERP3 <- ERP2 %>% 
  select(-Cohort) %>% 
  pivot_wider(1:3, names_from = Age, names_prefix = "Aged_", values_from = ERP) %>% 
  select(Year, state, Aged_4, Aged_5) %>% 
  drop_na() %>% 
  arrange(state, Year)


#********************************************************************************************
#----  Migration adjustment to align first year school with YBFS ----
#********************************************************************************************

# Make cohort change table 
# Interested in inverse of 4->5yrs & 5->6yrs, within cohort & state.
cohort_chg <- ERP2 %>% 
  select(-1,-2, 3,6, everything()) %>% 
  pivot_wider(id_cols = 1:4, names_prefix = "Age_", names_from = Age, values_from = ERP) %>% 
  mutate(Yr_turn_5 = Cohort+5, 
         Yr_turn_6 = Cohort+6) %>% 
  mutate(inv_chg_4_5 = round(Age_4/Age_5,4),
         inv_chg_5_6 = round(Age_5/Age_6,4))


# cohort_chg5
cohort_chg5 <- ERP2 %>% 
  filter(Age == 5) %>% 
  left_join(select(cohort_chg, state, Yr_turn_5, inv_chg_4_5),
            by = c("state", "yr" = "Yr_turn_5")) %>% 
  mutate(Year = make_date(yr-1, 6, 30))

# cohort_chg6 
cohort_chg6 <- ERP2 %>% 
  filter(Age == 6) %>% 
  left_join(select(cohort_chg, state, Yr_turn_6, inv_chg_5_6),
            by = c("state", "yr" = "Yr_turn_6")) %>% 
  mutate(Year = make_date(yr-1, 6, 30)) 

# Make table of adj
Chg_4_6<- cohort_chg %>% 
  filter(Cohort %in% c(2015, 2016)) %>% 
  arrange(state) %>% 
  select(-3:-6, -Age_7) %>% 
  mutate(chg_4_5 = case_when(Cohort == 2016 ~ Age_5-Age_4, TRUE ~ 0 )) %>% 
  mutate(chg_5_6 = case_when(Cohort == 2015 ~ Age_6-Age_5, TRUE ~ 0 )) 
  
# Adjust vals on FndnYr_lag
FndnYr_adj <- FndnYr_lag %>% 
  left_join(select(cohort_chg5, yr, state, Age, inv_chg_4_5), 
            by = c("state", "orig_yr" = "yr")) %>% 
  rename(Age5 = Age) %>% 
  left_join(select(cohort_chg6, yr, state, Age, inv_chg_5_6), 
            by = c("state", "orig_yr" = "yr")) %>% 
  rename(Age6 = Age) %>% 
  drop_na() %>% 
  mutate(YBFS = round(val * ((P5*inv_chg_4_5)+(P6*inv_chg_5_6))))

YBFS <- FndnYr_adj %>% 
  select(Year, state, YBFS)

#-- Join to ERP 4 & 5 
# Subset ERP to Age 4,5
ERP4_5 <- ERP2 %>% 
  filter(Age %in% c(4:5)) %>% 
  pivot_wider(1:3, names_from = Age, values_from = ERP, names_prefix = "ERP_")


#  Join ERP & lagged FndtnYr_lag counts
YBFS01 <- YBFS %>% 
  full_join(select(ERP4_5, -yr), by = c("Year", "state")) %>% 
  filter(Year != "2005") %>% 
  left_join(select(FndnYr_lag, Year, state, val), by = c("Year", "state")) %>% 
  rename(`1st yr sch enrol lagged` = val) 

# Define levels for Series as a factor
series_level <- c("1st yr sch enrol lagged", "YBFS", "ERP_4", "ERP_5")

# Transpose & plot
YBFS01L <- YBFS01 %>% 
  filter(Year >2006) %>% 
  pivot_longer(3:6, names_to = "Series", values_to = "Number") %>% 
  mutate(Year = make_date(Year, 6, 30)) %>%  # Keep as unformatted (otherwise ggplot thinks it's a char - apply format to date in ggplot!!)
  mutate(Series = factor(Series, levels = series_level))


#---Plot1: YBFS Vs 1st yr school enrolment --
p1 <-  YBFS01L %>% 
  filter(Series %in% c("1st yr sch enrol lagged", "YBFS")) %>% 
  ggplot(aes(x = Year, y = Number, colour = Series))+
  geom_line(size = 0.8)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  scale_colour_manual(values = abscol)+
  facet_wrap(~ state, nrow = 3, scales = "free")

p1


#---Plot2: YBFS Vs ERP---
p2 <-  YBFS01L %>% 
  filter(Series != "1st yr sch enrol lagged") %>% 
  ggplot(aes(x = Year, y = Number, colour = Series))+
  geom_line(size = 0.8)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  scale_colour_manual(values = abscol)+
  facet_wrap(~ state, nrow = 3, scales = "free")

p2


#--------------------------
# Investigate SA results  
#--------------------------

# Calculate the mean 4 & 5 years ERP from 2013-2020 then difference between the ERP 4yrs & 5yrs and YBFS.
SA <- YBFS01 %>% 
  select(1:5) %>% 
  filter(state == "SA", Year <2021) %>% 
  mutate(Avg_4_5_yrs = (ERP_4 + ERP_5)/2) %>% 
  mutate(diff_pc = (YBFS - Avg_4_5_yrs)/Avg_4_5_yrs*100)

# Plot SA YBFS v ERP 
SA_L <- SA %>% 
  pivot_longer(3:7, names_to = "Series", values_to = "val") %>% 
  mutate(Year = make_date(Year, 6, 30)) 

p3 <- SA_L %>%
  filter(Series != "diff_pc") %>% 
  ggplot(aes(x = Year, y = val, colour = Series))+
  geom_line(size = 0.8)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  scale_colour_manual(values = abscol)

p3


# Plot SA % difference between ERP & YBFS 
p4 <- SA_L %>%
  filter(Series == "diff_pc")%>% 
ggplot(aes(x = Year, y = val, colour = Series))+
           geom_line(size = 0.8)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  scale_colour_manual(values = abscol)
  
p4  # Notice the step function occurs between 2012 and 2013


# Assume the step function is some artefact of changes in policy or scope of collection?
# Either way assuming it is artificial we can remove that difference - a kind of rebasing of the 2006-12 period.

# If the mean of the 2006-2012 diff represents overall 2006-12 difference, we can simply apply the inverse of diff(%)
# to YBFS to get a rebased series. I believe this won't be too biased because the adjustment is a constant %, so it's 
# same % change to all 2006-12 points  - that adjustment itself does will not make the data correlated  - the correlation is still 
# reliant on the linear relationship  - and the slope for 2006-12 doesn't change - just the intercept. 


# Calculate average difference between 4 & 5 ERP and YBFS from 2013-2020

avg_diff2006_12 <- SA %>% 
  group_by(state) %>% 
  filter(Year < 2013) %>% 
  select(state, diff_pc) %>% 
  summarise(avg_diff2006_12 = mean(diff_pc))


# Make YBFS adjustment by subtracting a constant % (avg_diff2006_12) from YBFS for 2006-2012 
SAadj <- SA %>% 
    left_join(avg_diff2006_12, by = "state") %>% 
  mutate(YBFS_adj = YBFS-(avg_diff2006_12/100*YBFS)) %>%  # ATYEPA Fixed formula here (by removing -1*)
  #mutate(YBFS_adj = YBFS-(avg_diff2006_12/100*-1*YBFS)) %>%  
  filter(Year<2013) %>% 
  select(Year,state, YBFS_adj) %>% 
  right_join(SA, by = c("Year", "state")) %>% 
  select(Year, state, YBFS, YBFS_adj, ERP_4, ERP_5, Avg_4_5_yrs) %>% 
  mutate(YBFS_adj = case_when(Year == 2013 ~ YBFS, TRUE ~ YBFS_adj)) %>% 
  mutate(YBFS_new = case_when(Year %in% 2006:2012 ~ YBFS_adj, TRUE ~ YBFS))
           

SAOrig <- SAadj %>% 
  pivot_longer(3:8, names_to = "series", values_to = "val") %>% 
  filter(series %in% c("YBFS", "ERP_4", "ERP_5")) %>% 
  mutate(Year = make_date(Year, 6, 30)) %>% 
  mutate(Group = "Original")

SAadjL <- SAadj %>% 
  mutate(YBFS = case_when(Year<2013 ~ NA_real_, TRUE ~ YBFS)) %>% 
  pivot_longer(3:8, names_to = "series", values_to = "val") %>% 
  filter(series %in% c("YBFS_adj", "YBFS", "ERP_4", "ERP_5")) %>% 
  mutate(Year = make_date(Year, 6, 30)) %>% 
  mutate(Group = "Adjust 2006-12")

# Save an adjusted version to append to YBFS01L - rename SA as 'SA (adj)'
SA_adj <- SAadjL %>% 
  rename(Number = val, Series = series) %>% 
  select(-Group) %>% 
  mutate(state = "SA (adj)") %>% 
  mutate(Year = substr(Year,1,4)) %>% 
  pivot_wider(1:2,names_from = Series, values_from = Number) %>% 
  mutate(YBFS = as.integer(coalesce(YBFS, YBFS_adj))) %>% 
  select(-YBFS_adj) %>% 
  left_join(filter(YBFS01, state =="SA") %>% select(`1st yr sch enrol lagged`, Year), by = c("Year"))


# Define levels for Group
grp_lvl <- c("Original", "Adjust 2006-12")

# Join Original + Adjusted
SA_Orig_adj <- SAOrig %>% 
  bind_rows(SAadjL) %>% 
  mutate(Group = factor(Group, levels = grp_lvl))
  

p5 <- SA_Orig_adj %>%  
  ggplot(aes(x = Year, y = val, colour = series))+
  geom_point()+
  geom_line(size = 0.9)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  scale_colour_manual(values = abscol)+
  facet_wrap(~ Group)

p5


#  Make a new series 
p6 <- SAadj %>% 
  mutate(YBFS = case_when(Year<2013 ~ NA_real_, TRUE ~ YBFS)) %>% 
  pivot_longer(3:8, names_to = "series", values_to = "val") %>% 
  filter(series %in% c("YBFS_adj", "YBFS", "ERP_4", "ERP_5")) %>% 
  mutate(Year = make_date(Year, 6, 30)) %>% 

  ggplot(aes(x = Year, y = val, colour = series))+
  geom_point()+
  geom_line(size = 0.8)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  scale_colour_manual(values = abscol)

p6


#------------------------
#--- Correlation----
#-------------------------

#--- Correlation period 2006-20

# Append SA_adj to YBFS01 - so `SA (adj)` appears as if it were another state
YBFS02 <-  YBFS01 %>% 
  bind_rows(SA_adj)


# Update state factor levels
State_lvl2 <- c("NSW", "Vic", "Qld", "SA", "SA (adj)", "WA", "Tas", "NT", "ACT", "Aust")

YBFS02_SAadj <- YBFS02 %>% 
  mutate(state = factor(state, levels = State_lvl2))


### Correlation of 4 and 5 years ERP with `New_YBFS` 
cordf <- YBFS02 %>%
  filter(Year != 2021) %>% 
  select(Year, ERP_4, ERP_5, YBFS, state) %>%
  drop_na() %>%
  group_by(state) 

Corr_erp4 <- cordf %>% 
  summarize(`ERP 4yrs`= round(cor(ERP_4, YBFS),2))

Corr_erp4p <- cordf %>% 
  summarize(`p-value (ERP 4yrs)` = round(cor.test(ERP_4, YBFS)$p.value,4)) %>% 
  mutate(`Sig  (ERP 4yrs)` = case_when(`p-value (ERP 4yrs)` >= 0.05 ~ "Not sig", 
                                       `p-value (ERP 4yrs)` < 0.001 ~ "***",
                                       `p-value (ERP 4yrs)` < 0.01 ~ "**",
                                       `p-value (ERP 4yrs)` < 0.05 ~ "*", TRUE ~ ""))

Corr_erp5 <- cordf %>% 
  summarize(`ERP 5yrs`= round(cor(ERP_5, YBFS),2))

Corr_erp5p <- cordf %>% 
  summarize(`p-value (ERP 5yrs)`= round(cor.test(ERP_5, YBFS)$p.value,4)) %>% 
  mutate(`Sig  (ERP 5yrs)`= case_when(`p-value (ERP 5yrs)` >= 0.05 ~ "Not sig", 
                                      `p-value (ERP 5yrs)` < 0.001 ~ "***",
                                      `p-value (ERP 5yrs)` < 0.01 ~ "**",
                                      `p-value (ERP 5yrs)` < 0.05 ~ "*", TRUE ~ ""))

Corr1 <- Corr_erp4 %>% 
  left_join(Corr_erp4p, by = "state") %>% 
  left_join(Corr_erp5, by = "state") %>% 
  left_join(Corr_erp5p, by = "state") %>% 
  select(-`p-value (ERP 4yrs)`, -`p-value (ERP 5yrs)`)

# Corr coefficients (2006 to 2020)
Corr1 %>% 
  DT::datatable()


#--- Correlation period 2013-20
cordf13_20 <- YBFS02 %>%
  filter(Year >"2012-06-30", Year < "2020-06-30") %>%
  select(Year, ERP_4, ERP_5, YBFS, state) %>%
  drop_na() %>%
  group_by(state) 

Corr_erp4 <- cordf13_20 %>% 
  summarize(`ERP 4yrs`= round(cor(ERP_4, YBFS),2))

Corr_erp4p <- cordf13_20 %>% 
  summarize(`p-value (ERP 4yrs)` = round(cor.test(ERP_4, YBFS)$p.value,4)) %>% 
  mutate(`Sig  (ERP 4yrs)` = case_when(`p-value (ERP 4yrs)` >= 0.05 ~ "Not sig", 
                                       `p-value (ERP 4yrs)` < 0.001 ~ "***",
                                       `p-value (ERP 4yrs)` < 0.01 ~ "**",
                                       `p-value (ERP 4yrs)` < 0.05 ~ "*", TRUE ~ ""))

Corr_erp5 <- cordf13_20 %>% 
  summarize(`ERP 5yrs`= round(cor(ERP_5, YBFS),2))

Corr_erp5p <- cordf13_20 %>% 
  summarize(`p-value (ERP 5yrs)`= round(cor.test(ERP_5, YBFS)$p.value,4)) %>% 
  mutate(`Sig  (ERP 5yrs)`= case_when(`p-value (ERP 5yrs)` >= 0.05 ~ "Not sig", 
                                      `p-value (ERP 5yrs)` < 0.001 ~ "***",
                                      `p-value (ERP 5yrs)` < 0.01 ~ "**",
                                      `p-value (ERP 5yrs)` < 0.05 ~ "*", TRUE ~ ""))

Corr13_20 <- Corr_erp4 %>% 
  left_join(Corr_erp4p, by = "state") %>% 
  left_join(Corr_erp5, by = "state") %>% 
  left_join(Corr_erp5p, by = "state") %>% 
  select(-`p-value (ERP 4yrs)`, -`p-value (ERP 5yrs)`)

# Corr coefficients (2013 to 2020) 
Corr13_20 %>% 
  DT::datatable()

# Save all jurisdiction's YBFS x ERP (+ SA adj. )
write_xlsx(YBFS02_SAadj,"YBFS02_SAadj.xlsx" ) 

#=====================
#---- Modelling ----
#=====================

#----Factor levels ----
State_lvl <- c("NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust")
State_lvl2 <- c("NSW", "Vic", "Qld", "SA", "SA (adj)",  "WA", "Tas", "NT", "ACT", "Aust")
series_level <- c("1st yr sch enrol lagged", "YBFS", "ERP_4", "ERP_5")
ass_lvl <- c("N.A.", "ERP_A", "ERP_B")

#--- Load population projections -- 
# Projections updated with 2021 rebased ERP :

Proj_2025_4_5 <- read.xlsx("Proj2025_4yr_5yrs.xlsx") 


# Make df for `SA (adj)`
SA_proj <- Proj_2025_4_5 %>% 
  filter(state == "SA") %>% 
  mutate(state = "SA (adj)")
  
# Join 'SA (adj)' to  Proj_2025_4_5 
Proj_2025_4_5 <-  Proj_2025_4_5 %>% 
  bind_rows(SA_proj)

# 'Target' dataset where predictions are made for values of ERP
newdat <- Proj_2025_4_5 %>% 
  rename(ERP_4 = ERP_4yr, ERP_5 = ERP_5yr) %>% 
  mutate(Assumption = case_when(is.na(Assumption)~ "N.A.", TRUE ~ Assumption)) %>% 
  mutate(Assumption = factor(Assumption, levels = ass_lvl))

#--- Load (actual 2006-2020) YBFS -- 
YBFS02 <- read_xlsx("//Path/YBFS02_SAadj.xlsx") 

YBFS02 <- YBFS02 %>% 
  mutate(state = factor(state, levels = State_lvl2))

YBFS02L <- YBFS02 %>% 
  filter(Year >2006) %>% 
  pivot_longer(3:6, names_to = "Series", values_to = "Number") %>% 
  mutate(Year = make_date(Year, 6, 30)) %>%  # Keep as unformatted (otherwise ggplot thinks it's a char - apply format to date in ggplot!!)
  mutate(Series = factor(Series, levels = series_level)) 


# Add YBFS as a separate y-var, Series as x-var
YBFS02 <- YBFS02 %>% 
  mutate(year = make_date(Year, 6, 30))


YBFS03 <- YBFS02L %>%
  left_join(select(YBFS02, year, state, YBFS), by = c("Year" = "year", "state")) %>% 
  rename(ERP = Number)


# 1) covariate = ERP_4yrs 
state_mod4 <- YBFS02 %>%
  filter(Year != "2021") %>% 
  nest(data = -state) %>%
  mutate(
    fit = map(data, ~ lm(YBFS ~ ERP_4, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment, newdata = newdat, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov. = "ERP 4yrs", model_id = 4)


# 2)  covariate =  ERP_5yrs 
state_mod5 <- YBFS02 %>%
  filter(Year != "2021") %>% 
  nest(data = -state) %>%
  mutate(
    fit = map(data, ~ lm(YBFS ~ ERP_5, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment, newdata = newdat, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov. = "ERP 5yrs", model_id = 5)


# 3) 2 x covariates ERP_4yrs + ERP_5yrs 
state_mod45 <- YBFS02 %>%
  filter(Year != "2021") %>%   
  nest(data = -state) %>%
  mutate(
    fit = map(data, ~ lm(YBFS ~ ERP_4 + ERP_5, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment, newdata = newdat, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov. = "ERP 4yrs + 5yrs", model_id = 45)

# Combine into a single table of state models
state_mod <- state_mod4 %>% 
  bind_rows(state_mod5, state_mod45) 

# Distil 27 models down to 'best' per state  
state_summ <- state_mod %>%
  unnest(glanced) %>% 
  group_by(state) %>% 
  mutate(best = case_when(Cov. == "ERP 4yrs" ~ r.squared,
                          Cov. == "ERP 5yrs" ~ r.squared,
                          Cov. ==  "ERP 4yrs + 5yrs" ~ adj.r.squared)) %>% 
  filter(best == max(best)) %>% 
  select(-best)

# Format for display
state_fit_id <-  state_summ %>% 
  mutate(Sig.= case_when(p.value >= 0.05 ~ "Not sig", 
                         p.value < 0.001 ~ "***",
                         p.value < 0.01 ~ "**",
                         p.value < 0.05 ~ "*", TRUE ~ "")) %>% 
  rename(State = state, `R-squared` = `r.squared`, `adj. R-squared` = adj.r.squared,
         `p-val.` = p.value, `df res.` = df.residual ) %>% 
  mutate_at(c("R-squared", "adj. R-squared"),rnd2) %>% 
  mutate_at("p-val.", rnd4)

state_fit <- state_fit_id %>% 
  select(State, Cov., `R-squared`, `adj. R-squared`, `p-val.`, Sig., `df res.`) 

#  Fit for 2006-2020
state_fit %>% 
  DT::datatable()

# Coeff
state_all_coeff <- state_mod %>% 
  unnest(tidied) %>% 
  filter(term != "(Intercept)") %>% 
  select(1,4:8,11) %>% 
  mutate_at(c("estimate", "std.error", "statistic", "p.value"), rnd3) 

state_coeff <- state_fit %>% 
  left_join(state_all_coeff, by = c("State" = "state", "Cov.")) %>% 
  mutate(`Model call` = paste0("YBFS ~ ", Cov.)) %>% 
  arrange(State) %>% 
  select(State, `Model call`, term, estimate, std.error, statistic, p.value) %>% 
  mutate(Sig.= case_when(p.value >= 0.05 ~ "Not sig",
                         p.value < 0.001 ~ "***",
                         p.value < 0.01 ~ "**",
                         p.value < 0.05 ~ "*", TRUE ~ "")) %>% 
  rename(`p-val.` = p.value)

#  Coeff for 2013-2020 
state_coeff %>% 
  DT::datatable()

#--------------------------
#--Modelling continued
#--------------------------
## Fitted/predicted YBFS by state                                                                                           

### Linear fit of YBFS with ERP 
# Scatterplots of ERP with `New YBFS`. 

# Plots below show the relationship between the available values of YBFS with ERP (4 and 5 yrs respectively). 
#The diagonal line is the line generated by the simple linear regression y ~ x and the shaded area represents the 95% confidence interval. 

## Fitted/predicted YBFS by state                                                                                           
finmod <- state_summ %>%                                                                         
  rename(state_ = state) %>%                                                                  
  unnest(augmented) %>%
  mutate(state = factor(state, levels = State_lvl2)) %>% 
  group_by(state) %>%                                                                                                     
  filter(state_ == state) %>%
  select(state, Year, Assumption, ERP_4, ERP_5, `.fitted`, `.se.fit`, Cov.) %>% 
  rename( fitted = `.fitted`, SE_fit = `.se.fit`) %>% 
  mutate(Year = make_date(Year, 6, 30)) %>% 
  mutate(Year = format(Year, format = "%Y")) 

# Join YBFS for plotting
YBFS_mod1 <- finmod %>% 
  left_join(select(YBFS02, Year, year, state, YBFS), by = c("Year", "state")) %>% 
  mutate(year = make_date(Year,06,30))


#----------------------------------
# Fit/prediction Plot for 2006-2020    
#----------------------------------
# Data frame of fit for output - selecting Series A / B separately (for simplicity)
YBFS_fit_A <- YBFS_mod1 %>% 
  filter(Assumption != "ERP_B") %>% 
  mutate(`fitted YBFS` = round(fitted,0),
         lower = round(fitted - (1.96* SE_fit),0), 
         upper = round(fitted + (1.96* SE_fit),0), 
         CI_pc = round((upper - lower) /fitted *100,1)) %>%  
  mutate(residual = round(YBFS - fitted,0), 
         `residual %` = round(residual/YBFS*100,1)) %>% 
  select(state, Year, ERP_4, ERP_5, YBFS, `fitted YBFS`, SE_fit, lower, upper, residual, `residual %` ) %>% 
  arrange(state, Year) %>% 
  distinct()



YBFS_fit_B <- YBFS_mod1 %>% 
  filter(Assumption != "ERP_A") %>% 
  mutate(`fitted YBFS` = round(fitted,0),
         lower = round(fitted - (1.96* SE_fit),0), 
         upper = round(fitted + (1.96* SE_fit),0), 
         CI_pc = round((upper - lower) /fitted *100,1)) %>%  
  mutate(residual = round(YBFS - fitted,0), 
         `residual %` = round(residual/YBFS*100,1)) %>% 
  select(state, Year, ERP_4, ERP_5, YBFS, `fitted YBFS`, SE_fit, lower, upper, residual, `residual %` ) %>% 
  arrange(state, Year) %>% 
  distinct()


predict <- YBFS_mod1 %>% 
  select(state, Year, Assumption, SE_fit, YBFS, fitted) %>% 
  rename(`Measured YBFS` = YBFS, `Predicted YBFS` = fitted) %>%
  pivot_longer(5:6, names_to = "Series", values_to = "value") %>% 
  mutate(value = round(value, 0)) %>%
  mutate(lower = case_when(Series == "Predicted YBFS" ~ round(value - (1.96* SE_fit),2)),
         upper = case_when(Series == "Predicted YBFS" ~ round(value + (1.96* SE_fit),2)),
         CI_pc = case_when(Series == "Predicted YBFS" ~ round((upper - lower) /value *100,1))) 

# Model fit (prediction) up to 2020
p7 <-  predict %>% 
  mutate(Series = case_when(Series== "Measured YBFS" ~ "Actual YBFS", TRUE ~ Series)) %>% 
  mutate(Year = make_date(Year, 6, 30)) %>%  
  filter(state != "Aust") %>% 
  filter(Year < "2021-06-30") %>% 
  ggplot(mapping = aes(x = Year, y = value, colour = Series, group = Series))+
  geom_point(size = 1)+
  geom_line(size = 0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey60", alpha = 0.25)+ 
  scale_colour_manual(values = abscol, aesthetics = "colour")+
  scale_x_date(breaks = "2 years", labels = date_format("%Y"))+
  facet_wrap(~ state, scales = "free", ncol = 3)+
  xlab("\n \n Year ending June")+
  ylab("Number")

p7


#--------------------------------------------------------
#----Predict YBFS from 2021 to 2025 (on projections)----
#--------------------------------------------------------

p8 <- predict %>% 
  filter(Assumption != "ERP_A", state != "Aust") %>%   # Plot Series B & drop Aust
  
  mutate(Year = make_date(Year, 6, 30)) %>%  
  
  ggplot(mapping = aes(x = Year, y = value, colour = Series, group = Series))+
  geom_point(size = 1)+
  geom_line(size = 0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey60", alpha = 0.25)+ 
  scale_colour_manual(values = abscol, aesthetics = "colour")+
  scale_x_date(breaks = "3 years", labels = date_format("%Y"))+
  facet_wrap(~ state, scales = "free", ncol = 3)+
  xlab("\n \n Year ending June")+
  ylab("Number")


p8  

#-----------------------------
#---- Measures of model fit ----
#-----------------------------

#--- Plot of linear fit: x= fitted, y = YBFS ---
p9 <-   YBFS_mod1 %>% 
  filter(year < "2021-06-30", state != "Aust") %>% 
  group_by(state) %>% 
  ggplot(aes(x= fitted, y = YBFS))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_colour_manual(values = abscol)+
  stat_cor(aes(label = ..rr.label..), color = "#336699", geom = "label")+
  facet_wrap(~ state, nrow = 3, scales = "free")

p9


#--- Plot normality of residuals---

# Fit 3 models for 2006-2020
rs_mod4 <- YBFS_mod1 %>%
  filter(year < "2021-06-30") %>% 
  nest(data = -state) %>%
  mutate(fit = map(data, ~ lm(YBFS ~ ERP_4, data = .x)),augmented = map(fit, augment, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov. = "ERP 4yrs")

rs_mod5 <- YBFS_mod1 %>%
  filter(year < "2021-06-30") %>% 
  nest(data = -state) %>%
  mutate(fit = map(data, ~ lm(YBFS ~ ERP_5, data = .x)), augmented = map(fit, augment, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov. = "ERP 5yrs")

rs_mod45 <- YBFS_mod1 %>%
  filter(year < "2021-06-30") %>% 
  nest(data = -state) %>%
  mutate(fit = map(data, ~ lm(YBFS ~ ERP_4 + ERP_5, data = .x)),
         augmented = map(fit, augment, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov. = "ERP 4yrs + 5yrs")

# Stack 3 model results, filter to state-specific model,
rs <- rs_mod4 %>%
  bind_rows(rs_mod5, rs_mod45) %>%
  right_join(state_fit, by = c("state" = "State", "Cov.")) %>%   # To filter to state-specifi model
  select(1:5) %>%
  unnest(augmented) %>%    # Unnesting augmented dfs gives variables  `.resid` +`.std.resid`
  group_by(state) %>% 
  filter(state != "Aust")

p10 <- ggplot(rs, aes(qqnorm(.std.resid)[[1]], .std.resid, colour = Cov.))+geom_point(na.rm = TRUE, size = 3)+
  geom_abline(color = "blue", alpha = 0.4)+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+
  ggtitle("Normal Q-Q")+
  scale_colour_manual(values = abscol, aesthetics = "colour")+
  facet_wrap(~ state, ncol = 3)

p10

# --Arrange for Excel output ---
Table1<- YBFS02 %>% 
  arrange(state) %>% 
  select(Year, state, `1st yr sch enrol lagged`, YBFS, ERP_4, ERP_5)


dflist <- list("Table42b"=Table2, "Derived YBFS" = FndnYr_adj,
               "YBFS ERP" = Table1, "Corr 2006-20" = Corr1, "Corr 2013-20" = Corr13_20, 
               "lm summary" = state_fit,  "Coefficients" = state_coeff,
               "YBFS v fitted (A)" = YBFS_fit_A, "YBFS v fitted (B)" = YBFS_fit_B )

write.xlsx(dflist, "YBFS_data_updated DEC2021_ERP_raw.xlsx")
