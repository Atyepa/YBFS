#-- This app is primarily for visualising denominators representing Year Before First Year of School (YBFS)
#-- Includes linear models by state for YBFS based on ERP of 4 yr and 5 yr
# Deployed at: https://atyepa.shinyapps.io/YBFS_new/

library(tidyverse)
library(lubridate)
library(openxlsx)
library(writexl)
library(plotly)
library(ggthemes)
library(labeling)
library(scales)
library(crosstalk)
library(farver)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(readsdmx)
library(zoo)
library(data.table)
library(broom)

options(scipen=999)
#==============================

#--- Read in YBFS_data05w.xlsx---

YBFS_05w <- read.xlsx("https://github.com/Atyepa/YBFS/raw/main/YBFS_05w.xlsx")

#--- Fix colnames (Excel adds .)---
colnames(YBFS_05w)

YBFS_05w <- YBFS_05w %>%
  rename(`Preschool (SS YBFS)` = `Preschool.(SS.YBFS)`,
         `Preschool (4-5 yrs)` = `Preschool.(4-5.yrs)`,
         `State-specific YBFS` = `State-specific.YBFS`,
         New_YBFS = First_yr_sch_adj,
         `ERP (4yrs)` = `ERP.(4yrs)`, `ERP (4yrs, r)` = `ERP.(4yrs,.r)`)


#---Fix factors---
# state
State_lbl <- c("NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT", "Aust")

YBFS_05w <- YBFS_05w %>%
mutate(state = factor(state, levels = State_lbl))

#=========================================================
# Visualisation
#=========================================================

#--- Transpose long for ease of plotting multiple series
YBFS_05L <- YBFS_05w %>%
  mutate(ERP4yr = ERP_4yr) %>%
  select(Year, Cohort_4yrs, state, Assumption, Numerator, Numerator45, ERP4yr, everything(), -`ERP (4yrs, r)`) %>%
  pivot_longer(8:16, names_to = "Series", values_to = "value")

#------------------------------------------------------

#-------------------------------------------------------------------
#  Amend / drop outliers
#-------------------------------------------------------------------
##scenario1
##  For Qld: drop the years prior to 2016 + smooth 2018 (with moving avg)
##  For SA: apply the 3 term moving avg over available years (and drop 2018 & 2019)

YBFS_06w <- YBFS_05w %>%
  mutate(yr = as.numeric(Year)) %>%
  mutate(`New YBFS (adj)` = `New_YBFS`) %>%
  mutate(`New YBFS (adj)` = replace(`New YBFS (adj)`, state == "Qld" & yr <2016, NA)) %>%
  group_by(state) %>%
  mutate(adj = rollmean(`New YBFS (adj)`, k = 3, align = "center", na.pad=TRUE)) %>%
  ungroup() %>%
  mutate(`New YBFS (adj)` = case_when(state == "Qld" & Year == "2018" ~ round(adj,0), TRUE ~ round(`New YBFS (adj)`,0)))%>%
  mutate(`New YBFS (adj)` = case_when(state == "SA" ~ round(adj,0), TRUE ~ round(`New YBFS (adj)`,0)))%>%
  mutate(`New YBFS (adj)` = replace(`New YBFS (adj)`, state == "SA" & Year == "2018", NA)) %>%
  select(yr, everything(),-adj)

##scenario2
##  Qld: smooth 2014, 2016, 2018 with a centered 3 point mean
##  SA: smooth 2015

# YBFS_06w <- YBFS_05w %>%
#   mutate(`New YBFS (adj)` = `New_YBFS`) %>%  
#   group_by(state) %>%
#   # make a 3 term moving avg (centered)
#   mutate(adj_c = rollmean(`New YBFS (adj)`, k = 3, align = "center", na.pad=TRUE)) %>%
#   # make a 3 term moving avg (right aligned)
#   mutate(adj_r = rollmean(`New YBFS (adj)`, k = 3, align = "right", na.pad=TRUE)) %>%
#   ungroup() %>%
#   mutate(`New YBFS (adj)` = case_when(state == "Qld" & yr %in% c(2014, 2016, 2018) ~ round(adj_c,0), TRUE ~ round(`New YBFS (adj)`,0)))%>%
#   mutate(`New YBFS (adj)` = case_when(state == "Qld" & yr == 2019 ~ round(adj_r,0), TRUE ~ round(`New YBFS (adj)`,0)))%>%
#   mutate(`New YBFS (adj)` = case_when(state == "SA" & yr == 2015 ~ round(adj_c,0), TRUE ~ round(`New YBFS (adj)`,0)))%>%
#   select(Year, everything(),-adj_c, -adj_r)


# Updated correlation for ERP_4yr
corr_4<- YBFS_06w %>%
  mutate(yr = as.numeric(as.character(Year))) %>%
  filter(yr >2012, yr < 2020) %>%
  select(ERP_4yr, `New YBFS (adj)`, state) %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(COR=cor(ERP_4yr, `New YBFS (adj)`))

# corr for ERP_5yr
corr_5<- YBFS_06w %>%
  mutate(yr = as.numeric(as.character(Year))) %>%
  filter(yr >2012, yr < 2020) %>%
  select(ERP_5yr, `New YBFS (adj)`, state) %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(COR=cor(ERP_5yr, `New YBFS (adj)`))


#corr_4
#corr_5

#--------------------------------------------------------------------
# Fit lm for: `New YBFS (adj)`
# by state using 3 lm models: 
# 1) ERP_4yr
# 2) ERP_5yr
# 3) ERP_4yr + ERP_5yr
# Larger states appear to have better fit better with just ERP_4yr
# Smaller states may do better to have both ERP_4yr + ERP_5yr
#--------------------------------------------------------------------

# nest() by-groups
state_mod45 <- YBFS_06w %>%
  nest(data = -state) %>%
  mutate(
    fit = map(data, ~ lm(`New YBFS (adj)` ~ ERP_4yr + ERP_5yr, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment, newdata = YBFS_06w, se_fit=TRUE, interval = "prediction")) %>% 
    mutate(Cov = "ERP 4yrs + 5 yrs")

state_mod4 <- YBFS_06w %>%
  nest(data = -state) %>%
  mutate(
    fit = map(data, ~ lm(`New YBFS (adj)` ~ ERP_4yr, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment, newdata = YBFS_06w, se_fit=TRUE, interval = "prediction")) %>% 
    mutate(Cov = "ERP 4yrs")

state_mod5 <- YBFS_06w %>%
  nest(data = -state) %>%
  mutate(
    fit = map(data, ~ lm(`New YBFS (adj)` ~ ERP_5yr, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment, newdata = YBFS_06w, se_fit=TRUE, interval = "prediction")) %>% 
  mutate(Cov = "ERP 5yrs")

state_mod <- state_mod4 %>% 
  bind_rows(state_mod5, state_mod45)

# Distil 27 down to 'best' per state
state_summ <- state_mod %>% 
  unnest(glanced) %>% 
  group_by(state) %>% 
  mutate(best = case_when(Cov %in% c("ERP 4yrs", "ERP 5yrs") ~ r.squared, 
                          Cov == "ERP 4yrs + 5 yrs" ~ adj.r.squared)) %>% 
  filter(best == max(best)) %>% 
  select(-best)

# Rounding functions 
rnd3 <- function(x) round(x,3)
rnd2 <- function(x) round(x,2)
rnd1 <- function(x) round(x,1)

# Format for display
state_fit <-  state_summ %>%
  mutate(Sig.= case_when(p.value >= 0.05 ~ "Not sig",
                         p.value < 0.001 ~ "***",
                         p.value < 0.01 ~ "**",
                         p.value < 0.05 ~ "*", TRUE ~ "")) %>%
  rename(State = state, Cov. = Cov, `R-squared` = `r.squared`, `adj. R-squared` = adj.r.squared,
         `p-val.` = p.value, `df res.` = df.residual ) %>%
  mutate_at(c("R-squared", "adj. R-squared", "statistic"),rnd2) %>%
  mutate_at(c("sigma", "logLik", "AIC", "BIC" ),rnd1) %>%
  mutate_at("p-val.", rnd3) %>%
  select(State, Cov., `R-squared`, `adj. R-squared`, sigma, statistic, 
         `p-val.`, Sig., `df res.`, logLik, AIC, BIC,  nobs) %>% 
  arrange(State)
  

#head(state_fit,10)                          

#Coeff
state_all_coeff <- state_mod %>% 
  unnest(tidied) %>% 
  filter(term != "(Intercept)") %>% 
  select(1,4:8,11) %>% 
  mutate_at(c("estimate", "std.error", "statistic", "p.value"), rnd3) 


state_coeff <- state_fit %>% 
  left_join(state_all_coeff, by = c("State" = "state", "Cov." = "Cov")) %>% 
  mutate(Call = paste0("YBFS ~ ", Cov.)) %>% 
  select(1,19,14:18) %>% 
  arrange(State) %>% 
  mutate(Sig.= case_when(p.value >= 0.05 ~ "Not sig",
                         p.value < 0.001 ~ "***",
                         p.value < 0.01 ~ "**",
                         p.value < 0.05 ~ "*", TRUE ~ "")) %>% 
  rename(`p-val.` = p.value)

# state model
state_model <- state_coeff %>% 
  distinct() %>% 
  mutate(Cov = substr(Call,8,23)) %>% 
  select(1,9) 

# Prediction state_mod
state_pred1 <- state_mod %>% 
  rename(state_ = state) %>% 
  unnest(augmented)%>% 
  filter(state_ == state) %>% 
  select(-1:-5)  
  # mutate(lower = .fitted - (1.96*.se.fit), 
  #        upper = .fitted + (1.96*.se.fit),
  #        RSE = round((upper - lower) /.fitted*100,1)) 
# Join 
state_pred <- state_model %>% 
  left_join(state_pred1, by = c("State" = "state", "Cov")) %>% 
  distinct()

YBFS_07w <- state_pred %>%
  rename(`First year school (lagged)` = First_yr_sch) %>% 
  mutate(`New YBFS (fitted)` = round(.fitted,0),
         upper = round(.fitted + 1.96*.se.fit,0),
         lower = round(.fitted - 1.96*.se.fit,0),
         CI = round(.se.fit*1.96,0)) %>% 
  select(-`.se.fit`, -`.fitted`, -.lower, -.upper, -.resid)


# Select columns for the 'Fitted' display/download table in the app
YBFS_07w2 <- YBFS_07w %>% 
  mutate(`New YBFS (fitted)` = round(`New YBFS (fitted)`,0)) %>% 
  rename(`ERP 4yr` = ERP_4yr, `ERP 5yr` = ERP_5yr, Covariates = Cov) %>% 
  select(1,4,2,6, 8,9, 19:22) 


YBFS_07L1 <- YBFS_07w %>%
  rename(`ERP (4yrs p)` = `ERP (4yrs)`) %>%
  mutate(`New_YBFS_(adj)` = `New YBFS (adj)`, ERP4yr = ERP_4yr, Date = ymd(make_date(Year, 6, 30)), Year = as.character(yr) ) %>%
  select(Year, yr, Date, State, Assumption, Cov, ERP4yr, Numerator, Numerator45, `New YBFS (adj)`, `New YBFS (fitted)`, everything(), lower, upper,
         -CI,  -Cohort_4yrs, -`ERP (4yrs, r)`, -New_YBFS) %>%
  pivot_longer(10:19, names_to = "Series", values_to = "value") %>%
  mutate(value = round(value,0)) %>%
  rename(`New YBFS (adj)` = `New_YBFS_(adj)`, ERP_4yr = ERP4yr)

# Make `Numerator / Denominator` + rename `New YBFS (adj)` to `New YBFS (actual)`
# to better distinguish from `New YBFS (fitted)`
YBFS_07L2 <- YBFS_07L1 %>%
  rename(`New YBFS (actual)` = `New YBFS (adj)`) %>%
  mutate(Series = case_when(Series == "New YBFS (adj)" ~ "New YBFS (actual)", TRUE ~ Series)) %>%
  mutate(`Numerator / Denominator` = paste0("Preschool (SS YBFS) / ", Series)) %>%
  mutate(Percent = round(Numerator/value*100,1)) %>%
  mutate(`Numerator / Denominator45` = paste0("Preschool (4-5 yrs) / ", Series)) %>%
  mutate(Percent45 = round(Numerator45/value*100,1)) 

YBFS_07La <-  YBFS_07L2 %>% 
  select(1:16)

YBFS_07L <- YBFS_07L2 %>% 
  select(1:14,17:18) %>% 
  rename(`Numerator / Denominator` = `Numerator / Denominator45`, Percent = Percent45) %>% 
  bind_rows(YBFS_07La) %>%
  arrange(State) %>% 
  ungroup()


#*********************************************************************
# Make a Table for YBFS 2020 - 2025 for States fitted zero, medium high 
#*********************************************************************

YBFS_fL <- YBFS_07L %>% 
  filter(Series == "New YBFS (fitted)", yr >= 2013) %>%  
  select(1,4,5,11,10,7,14) %>% 
  rename(YBFS_fitted = value) %>% 
  distinct()

# Make 3 rows (Assumptions) per state for the N.A. year 
zero <- YBFS_fL %>% 
  filter(Assumption == "N.A.") %>% 
  mutate(Assumption = "Zero")
  
med <- YBFS_fL %>% 
  filter(Assumption == "N.A.") %>% 
  mutate(Assumption = "Med")

high <- YBFS_fL %>% 
  filter(Assumption == "N.A.") %>% 
  mutate(Assumption = "High")

# Bind extra rows 
YBFS_fL2 <- YBFS_fL %>% 
  bind_rows(zero, med, high) %>% 
  filter(Assumption != "N.A.", State != "Aust") %>% 
  arrange(State, Year) 

# Make Aust equal to the sum of the states or skip this step to let Aust just be the values from its model fit
Aust_sum <- YBFS_fL2 %>%   
group_by(Year, Assumption) %>% 
summarise(lower = sum(lower), upper = sum(upper), ERP_4yr = sum(ERP_4yr), YBFS_fitted = sum(YBFS_fitted)) %>% 
  mutate(State = "Aust") %>% 
  select(Year, State, Assumption, lower, upper, ERP_4yr, YBFS_fitted)
  
# Bind
YBFS_fL2 <- YBFS_fL2 %>% 
      bind_rows(Aust_sum)
  

  #Pivot wide for output
  YBFS_sw2 <- YBFS_fL2 %>% 
  pivot_wider(names_from = Assumption, values_from = c(ERP_4yr, YBFS_fitted, lower, upper)) 


nnames <- c("Year", "state", 
            "ERP 4yrs (zero)", "ERP 4yrs (med)", "ERP 4yrs (high)",
            "YBFS (zero)", "YBFS (med)", "YBFS (high)",
            "YBFS (zero, lower CI)", "YBFS (med, lower CI)", "YBFS (high, lower CI)",
            "YBFS (zero, upper CI)", "YBFS (med, upper CI)", "YBFS (high, upper CI)")

colnames(YBFS_sw2) <- nnames 

YBFS_ranges <- YBFS_sw2 %>% 
  select(1:5,6,9,12,7,10,13, 8, 11, 14 ) %>% 
  mutate(yr = as.numeric(Year))
  #mutate(Date = make_date(Year, 6, 30))


#*********************************
#  UI prep for Shiny
#*********************************

#---------------------------------------------------------------
#  Make list objects for 'state', 'Year', 'series', 'Num_Denom'
#---------------------------------------------------------------

State <- YBFS_07L %>%
  mutate(State = as.character(State)) %>%
  select(State) %>%
  distinct()

State <- as.list(State$State)

dmin <- min(YBFS_07L$yr)
dmax <- max(YBFS_07L$yr)


Year. <- YBFS_07L %>%
  select(Year) %>%
  distinct()

Year <- as.list(Year.$Year)


Series. <- YBFS_07L %>%
  select(Series) %>%
  mutate(Series = as.character(Series)) %>%
  distinct()

Series <-as.list(Series.$Series)

Num_Denom. <- YBFS_07L %>%
  select(`Numerator / Denominator`) %>%
  distinct()
Num_Denom <- as.list(Num_Denom.$`Numerator / Denominator`)

# Colour scale

abscol <- c("#336699", "#669966", "#7ad2f6", "#993366", "#CC9966", "#99CC66", "#666666", "#FF9900", "#0099C6", "#EA4C46")

#==================
# UI
#==================

ui <- shinyUI(fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
  ),
  
  headerPanel("Proposed YBFS measure - exploration of preliminary results"),
  
  
  sidebarPanel( id="sidebar",
                
                radioButtons('Plot', 'Select plot axis:', choices = c( "Series x year", "Preschool enrolment (%) x year", 
                                                                       "Linear relationship of new YBFS x ERP"),
                             selected = "Series x year"),
                
                conditionalPanel("input.Plot == `Series x year`",
                                 
                                 checkboxGroupInput('Series', 'Compare measures of preschool enrolment and population denominators:',
                                                    choices = c(
                                                      "Preschool (SS YBFS)",
                                                      "Preschool (4-5 yrs)", 
                                                      "First year school (lagged)", 
                                                      "New YBFS (actual)",
                                                      "New YBFS (fitted)",
                                                      "ERP_3yr",
                                                      "ERP_4yr",
                                                      "ERP_5yr",
                                                      "ERP (4yrs p)",
                                                      "State-specific YBFS"),
                                                    
                                                    selected = c("New YBFS (fitted)", "ERP_4yr")),
                                 
                                 radioButtons('CI', 'Show CI on modelled YBFS:', 
                                              choices = c("On", "Off"), 
                                              selected = "Off",  inline = TRUE)),
                
                conditionalPanel("input.Plot == `Linear relationship of new YBFS x ERP`",
                                 
                                 pickerInput('Seriesx', 'Select covariate of new YBFS:',
                                             choices = c("Preschool (SS YBFS)",
                                                         "State-specific YBFS",
                                                         "First_yr_sch",
                                                         "New YBFS (actual)",
                                                         "New YBFS (fitted)",
                                                         "ERP_3yr",
                                                         "ERP_4yr",
                                                         "ERP_5yr"),
                                             selected = c("ERP_4yr"), multiple = FALSE)),
                
                conditionalPanel("input.Plot == `Preschool enrolment (%) x year`",
                                 
                                 pickerInput('num_denom', 'Select rates of preschool with different denominators:',
                                             choices = c(
                                               "Preschool (SS YBFS) / New YBFS (actual)",
                                               "Preschool (SS YBFS) / New YBFS (fitted)",
                                               "Preschool (SS YBFS) / ERP (4yrs p)",
                                               "Preschool (SS YBFS) / ERP_4yr",
                                               "Preschool (SS YBFS) / ERP_5yr",
                                               "Preschool (SS YBFS) / Preschool (SS YBFS)",
                                               "Preschool (SS YBFS) / State-specific YBFS",
                                               "Preschool (SS YBFS) / First_yr_sch", 
                                               
                                               "Preschool (4-5 yrs) / New YBFS (actual)",
                                               "Preschool (4-5 yrs) / New YBFS (fitted)",
                                               "Preschool (4-5 yrs) / ERP (4yrs p)",
                                               "Preschool (4-5 yrs) / ERP_4yr",
                                               "Preschool (4-5 yrs) / ERP_5yr",
                                               "Preschool (4-5 yrs) / Preschool (SS YBFS)",
                                               "Preschool (4-5 yrs) / State-specific YBFS",
                                               "Preschool (4-5 yrs) / First_yr_sch"),
                                             
                                             selected = c("Preschool (4-5 yrs) / ERP (4yrs p)", "Preschool (SS YBFS) / New YBFS (fitted)"), multiple = TRUE)),
                
                
                pickerInput("State", 'Select states', choices = c(State),
                            selected = c(State), multiple = TRUE),
                
        
         
        sliderInput("dateRange","Year range:",
                    min = (dmin),
                    max = (dmax),
                    value= (c(dmin, dmax)),
                    sep = "",
                    step = 1),
        
        
               
                radioButtons("growth", 'Cohort growth assumption:', choices = c("Zero", "Med", "High"),
                             selected = "Med", inline = TRUE),
                
                # submitButton("Update view", icon("refresh")),
                
                
                width = 3),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotlyOutput('plot')),
                tabPanel(style = "overflow-y:scroll; max-height: 700px","Data table", DT::dataTableOutput('Table'),
                         downloadButton("downloadData", "Download Data table")),
                
                tabPanel(style = "overflow-y:scroll; max-height: 700px","YBFS fitted values", DT::dataTableOutput('modrange'),
                         downloadButton("downloadProj", "Download YBFS projections")),
                
                tabPanel(style = "overflow-y:scroll; max-height: 700px","Model summary", DT::dataTableOutput('modsum'),
                         downloadButton("downloadDatasum", "Download model summary")),
                
                tabPanel(style = "overflow-y:scroll; max-height: 700px","Coefficients", DT::dataTableOutput('modfit'),
                         downloadButton("downloadDatafit", "Download coefficients ")),
                
                tabPanel("About",
                         br(),
                         hr(),
                         tags$div( tags$h2(tags$b("Explanatory notes")),
                                   tags$h3(tags$b("Preschool enrolments and selected denominators")),
                                   p("This app is designed to facilitate an assessment and comparison of existing and proposed data for the purpose of measuring progress towards targets of universal access to early childhood education."),
                                   p("The major challenge for all methods which use separate aggregate counts for the numerator (i.e. Preschool enrolments) and denominators (such as ERP) is their alignment - i.e. the degree to which enrolments are a subset of the denominator population."),
                                   p("While the ultimate goal for performance measurement of universal access to early childhood education is an integrated data approach based on the school enrolment (in one year) and preschool enolment (from previous year) of individuals, this information is not yet available."),
                                   br(),
                                   tags$h4(tags$b("Proposed YBFS methodology")), 
                                   p("This app provides information and results from a possible interim solution (called the New YBFS)."), 
                                   
                                   p("The proposed method for the new YBFS is based on the published figure of children in their first year of school, but adjusted so the cohort in the first year of school (year n) aligns with that same cohort the preschool year (year n-1)."), 
                                   
                                   p("For the historical series (2013 to 2019), this adjustment was done by applying a factor to the first year of school count which is the inverse of the proportional change in ERP between 4 year olds in year n-1 and 5 year olds in year n."), 
                                   
                                   p("This derived measure from 2013-19 [called 'New YBFS (actual)' in this app] can then provide the basis for establishing relationship between the ERP and new YBFS through a linear regression."), 
                                   
                                   
                                   p("Regression models to predict YBFS were fitted for each state and territory based on the ERP of 4 year olds and 5 year olds (as separate predictor variables). Based on the optimal fit of these two covariates, jurisdictions were either given the simple model (just ERP of 4 year olds) or the model accounting for both ERP of 4 year olds and the ERP of 5 year olds."), 
                                   
                                   p("Once the linear models are fit, it is possible to predict the YBFS value for 2020 from the 2020 ERP.  Because it is also of interest for planners and policy-makers to project demand and program costs up to 2025, this method is extended to incorporate an ERP projection."), 
                                   
                                   p("Most of this projection (up to 2024) is based on the progression of cohorts who were  already aged 0-4 and accounted for in the Dec 2020 ERP.  The only assumption required to project these cohorts is the impact of each state's annual migration. Based the average annual cohort change (by state and age) over 2015-19, we have applied three assumptions of cohort change:"), 
                                   
                                   tags$ul(
                                     tags$li(tags$b("High"), " - equivalent to the average from 2015-19. For Australia overall this growth ranged from 0.7% between age 0 and 1 year, to 1.2% for the cohort growth between age 4  and 5 years."),
                                     
                                     tags$li(tags$b("Medium"), " - half the rate of change used in the 'high' assumption"),
                                     
                                     tags$li(tags$b("Low"), " - zero change, so the size of the cohort size remains unchanged as it ages")),
                                   
                                   p("In addition to projecting the existing cohorts (born before 2021), it was necessary to assume the starting size (0 year olds) of the 2021 cohort. This was simply done as the average of the previous three years (2018-20)."),
                                   
                                   br(),
                                   tags$h4(tags$b("Data definitions")), 
                                   
                                   
                                   tags$ul(
                                     
                                     tags$li(tags$b("Preschool (SS YBFS)"), " -  the number of preschool enrolments taking account of state-specific age eligibility for starting preschool and school of the state in which the child usually resides and the child's date of birth. For New South Wales and Victoria adjustment factors have been applied to account for the rates at which children proceed from preschool to school education. Accounting for the state-specific entry provisions is also known as the state- specific Year Before Full-time Schooling (state-specific YBFS). Data source:",
                                             tags$a(href="https://www.abs.gov.au/statistics/people/education/preschool-education-australia/latest-release", target="_blank", "Preschool Education Australia, Table 28.")),
                                     br(),
                                     
                                     
                                     tags$li(tags$b("Preschool (4-5 yrs)"), " -  the number of preschool enrolments among all 4 and 5 year olds.  Data source:",
                                             tags$a(href="https://www.abs.gov.au/statistics/people/education/preschool-education-australia/latest-release", target="_blank", "Preschool Education Australia, Table 2.")),
                                     br(),
                                     
                                     
                                     tags$li(tags$b("First year school (lagged)"), " - the number of enrolments of children in their first year of school. Note that the Schools reference year for this count is the year after the preschool year. Data source:", 
                                             tags$a(href= "https://www.abs.gov.au/statistics/people/education/schools/latest-release#data-download", target="_blank", "Schools, Australia Table 42b")),
                                     br(),
                                     
                                     
                                     tags$li(tags$b("New YBFS (actual)"), " - the adjusted estimate of 'First year of school' to account for within-cohort ERP growth between ages 4 and 5 years (described further above in 'Proposed YBFS methodology').  "), 
                                     
                                     br(),
                                     
                                     tags$li(tags$b("New YBFS (fitted)"), " - the modelled estimate of YBFS based on the linear relationship between 'New YBFS (actual)' and the ERP of 4 and 5 year olds as described above in 'Proposed YBFS methodology'."), 
                                     
                                     br(),
                                     
                                     tags$li(tags$b("ERP_3yr, ERP_4yr, ERP_5yr"), " - up to 2020, these are the latest estimates of the Estimated Resident Population (ERP) for 3, 4 and 5 year olds. From 2021, these are projections as described above in 'Proposed YBFS methodology'."), 
                                     
                                     br(),
                                     
                                     
                                     tags$li(tags$b("ERP (4yrs p)"), " - the ", tags$em("preliminary"), "ERP of four year olds. Because the components of population in any quarter are updated based on more complete information following first publication they are denoted 'preliminary' for the 12 months following their intial publication, before becoming 'revised'.  The 'preliminary' estimate is presented here because this reflects the actual data provided in each year to measure the UANP indicator. While the timeliness imperative for national reporting means these revised figures were not used, but their inclusion here can be used to assess the data quality impact of using preliminary ERP. To access the original historical data, please see the time-series spreadsheets available for download from the ",
                                             tags$a(href="https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population", target="_blank",
                                                    "'Previous releases' section of National, state and territory population.")),
                                     br(),
                                     
                                     
                                     tags$li(tags$b("State-specific YBFS"), " -  an adjustment made to the ERP to align the estimated population count with the state-specific age requirements. The method uses the monthly pattern of births to distribute the annual aggregate ERP by month of birth. The aim of deriving the month of birth estimate of ERP by state in which the child usually resides is to allow the ERP denominators to be tailored by month of birth to be conceptually equivalent to the state-specific preschool age requirements.  For New South Wales and Victoria, this method also takes account of the proportion of younger children in preschool who are likely to proceed to their first year of school in the following year.  A deduction is also made for the number of children aged 4 and 5 years of age in the state-specific YBFS cohorts who were attending school in each state/territory. For more information, please refer 'Preschool program' and 'Measurement concepts' in the",
                                             tags$a(href="https://www.abs.gov.au/methodologies/preschool-education-australia-methodology/2020", target="_blank",
                                                    "Methodology"), "section of Preschool Education, Australia.")),
                                   br(),      
                                   
                                   tags$h4(tags$b("Further information")),
                                   
                                   tags$p("For more information, please contact ABS Education and Training Statistics Section:",
                                          a("ABS Education and Training Statistics", href="mailto:NCETS_2015_Plus_WDB@abs.gov.au"))
                                   
                         ))))))



## Server
server <- function(input, output) {
  
  S <- reactive({
    list(State=input$State)
  })
  
  Y <- reactive({
    list(Year=input$Year)
  })
  
  D <- reactive({
    list(Series=input$Series)
  })
  
  Dx <- reactive({
    list(Seriesx=input$Seriesx)
  })
  
  
  A <- reactive({
    list(growth=input$growth)
  })
  
  Nm<- reactive({
    list(num_denom=input$num_denom)
  })
  
#@#@#@ filtering with  input$dateRange[1] + input$dateRange[2])
  
    dfLc <- reactive({  YBFS_07L %>%
      filter(Assumption %in% c("N.A.", A()$growth)) %>%
      filter(State %in% S()$State) %>%
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%  
      filter(Series %in% D()$Series)
  })
  
  
  dfLcx <- reactive({ YBFS_07L %>%
      filter(Assumption %in% c("N.A.", A()$growth)) %>%
      filter(State %in% S()$State) %>%
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%
      filter(Series == Dx()$Seriesx)
  })
  
  dfLr <- reactive({  YBFS_07L %>%
      filter(Assumption %in% c("N.A.", A()$growth)) %>%
      filter(State %in% S()$State) %>%
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%
      filter(`Numerator / Denominator` %in% Nm()$num_denom)
  })
  
  Tablec <-  reactive({ YBFS_07L %>%
      filter(Assumption %in% c("N.A.", A()$growth)) %>%
      filter(State %in% S()$State) %>%
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%
      filter(Series %in% D()$Series) %>%
      select(Year, State, Series, value, ERP_4yr) %>%
      distinct() %>%
      drop_na()
  })
  
  
  Tabler <-  reactive({  YBFS_07L %>%
      filter(Assumption %in% c("N.A.", A()$growth)) %>%
      filter(State %in% S()$State) %>%
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%
      filter(`Numerator / Denominator` %in% Nm()$num_denom) %>%
      select(Year, State, `Numerator / Denominator`, Numerator, Series, value, Percent) %>%
      distinct() %>%
      drop_na()
  })
  
  
  Tablex <-  reactive({  YBFS_07L %>%
      filter(Assumption %in% c("N.A.", A()$growth)) %>%
      filter(State %in% S()$State) %>%
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%
      filter(Series == Dx()$Seriesx) %>%
      select(Year, State, `New YBFS (actual)`, Series, value) %>%
      distinct() %>%
      drop_na()
  })
  
  
  Sumtab <- reactive({ dfLc %>%
      filter(State %in% S()$State) %>%
      filter(Date >= input$dateRange[1]) %>%
      filter(Date <= input$dateRange[2]+1) 
  })
  
  
  # model tables
  
  modfittbl <- reactive({ state_coeff %>%
      filter(State %in% S()$State)
  })
  
  modsummtbl <- reactive({ state_fit %>%
      filter(State %in% S()$State)
  })
  
  
  
  Proj_ranges <- reactive({ YBFS_ranges %>%
      filter(state %in% S()$State) %>% 
      filter(state != "Aust") %>% 
      filter(yr >= input$dateRange[1]) %>%
      filter(yr <= input$dateRange[2]) %>%
      select(-yr)
  })
  
  
  output$plot <- renderPlotly({
    
    if(input$Plot == "Series x year" & input$CI == "On"){
      p <- ggplot(dfLc(), aes(x= Date, y = value, colour = Series, group = Series)) +
        geom_line(size = .8)+
        geom_point(size = 2) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3)+
        scale_y_continuous(labels = comma)+
        theme_few()+
        scale_colour_manual(values = abscol)+
        xlab("Year")+
        ylab("n")
      
      
      p <- p + facet_wrap(~ State, scales = "free")
    }
    
    if(input$Plot == "Series x year" & input$CI == "Off"){
      p <- ggplot(dfLc(), aes(x= Date, y = value, colour = Series, group = Series)) +
        geom_line(size = .8)+
        geom_point(size = 2) +
        scale_y_continuous(labels = comma)+
        theme_few()+
        scale_colour_manual(values = abscol)+
        xlab("Year")+
        ylab("n")
      
      
      p <- p + facet_wrap(~ State, scales = "free")
    }
    
    
    if(input$Plot == "Linear relationship of new YBFS x ERP"){
      
      p <- ggplot(dfLcx(), aes(x = value, y = `New YBFS (actual)`))+
        geom_point(size = 2) +
        geom_smooth(method = lm)+
        scale_x_continuous(labels = comma)+
        scale_y_continuous(labels = comma)+
        scale_colour_manual(values = abscol)+
        scale_color_few()+
        xlab(input$Seriesx)+
        ylab("New YBFS")
      
      p <- p + facet_wrap(~ State, scales = "free")
    }
    
    if(input$Plot == "Preschool enrolment (%) x year"){
      p <- ggplot(dfLr(), aes(x= Date, y = Percent, colour = `Numerator / Denominator`, group = `Numerator / Denominator`)) +
        geom_line(size = .8)+
        geom_point(size = 2) +
        geom_hline(yintercept=100, size = 0.5, alpha = 0.5, linetype = 'dotted', color = "black")+
        scale_y_continuous(labels = comma)+
        theme_few()+
        scale_colour_manual(values = abscol)+
        xlab("Year")+
        ylab("Percent")
      
      p <- p + facet_wrap(~ State)
    }
    
    
    ggplotly(p, height = 780, width = 1350, autosize= TRUE, tooltip = c("x", "y", "colour", "ymin", "ymax") )
    
  })
  
  
  output$Table <- DT::renderDataTable({
    
    if(input$Plot == "Series x year"){
      
      T <- Tablec()
    }
    
    if(input$Plot == "Preschool enrolment (%) x year"){
      
      T <- Tabler()
    }
    
    if(input$Plot == "Linear relationship of new YBFS x ERP"){
      
      T <- Tablex()
    }
    
    T
  })
  
  
  output$modfit <- DT::renderDataTable({
    
    fit <- modfittbl()
    
    fit
    
  })
  
  output$modsum <- DT::renderDataTable({
    
    sum <- modsummtbl()
    sum
  })
  
  output$modpred <- DT::renderDataTable({
    
    pred <- modpredtbl()
    pred
  })
  
  
  output$modrange <- DT::renderDataTable({
    
    ranges <- Proj_ranges()
    ranges
  })
  
  
  
  # Downloadable xlsx --
  
  # DataTable 1
  
  
  TableDL <- reactive({
    
    if(input$Plot == "Series x year"){
      
      T <- Tablec()
    }
    
    if(input$Plot == "Preschool enrolment (%) x year"){
      
      T <- Tabler()
    }
    
    if(input$Plot == "Linear relationship of new YBFS x ERP"){
      
      T <- Tablex()
    }
    
    T
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Selected YBFS data", ".xlsx")},
    content = function(file) { write_xlsx(TableDL(), path = file) }
  )
  
  # Modelfit
  TableDLF <- reactive({
    
    modfittbl()
    
  })
  
  output$downloadDatafit <- downloadHandler(
    filename = function() {
      paste("Model fit", ".xlsx")},
    content = function(file) { write_xlsx(TableDLF(), path = file) }
  )
  
  # Modelpred
  TableDLP <- reactive({
    
    modpredtbl()
    
  })
  
  output$downloadDatapred <- downloadHandler(
    filename = function() {
      paste("Model predict", ".xlsx")},
    content = function(file) { write_xlsx(TableDLP(), path = file) }
  )
  
  # Modelsum
  
  TableDLS <- reactive({
    
    modsummtbl()
    
  })
  
  output$downloadDatasum <- downloadHandler(
    filename = function() {
      paste("Model summary", ".xlsx")},
    content = function(file) { write_xlsx(TableDLS(), path = file) }
  )
  
  
  # Ranges 
  TableRange <- reactive({
    
    Proj_ranges()
    
  })
  
  output$downloadProj <- downloadHandler(
    filename = function() {
      paste("YBFS ranges", ".xlsx")},
    content = function(file) { write_xlsx(TableRange(), path = file) }
  )
  
  
}

#========================================
shinyApp(ui, server)
#========================================
