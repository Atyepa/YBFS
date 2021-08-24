withRepo(library(tidyverse))
withRepo(library(openxlsx))
withRepo(library(writexl))
withRepo(library(yaml))
withRepo(library(plotly))
withRepo(library(ggthemes))
withRepo(library(labeling))
withRepo(library(scales))
withRepo(library(crosstalk))
withRepo(library(farver))
withRepo(library(shiny))
withRepo(library(shinyWidgets))
withRepo(library(shinythemes))
withRepo(library(readsdmx))
withRepo(library(DT))
#==============================

# This version of the app to do regression x state + adjust ACT 
# 
# Subset the Series to:

# "Preschool (SS YBFS)", "ERP (4yrs)",  "1st year of school (lagged 1 yr)" 
# + Predicted "1st year of school (lagged 1 yr)"

#--Working directory

T <- "//...Reassembled data"

setwd(T)

#---------------------
#--- Not in operator---
`%!in%` = negate(`%in%`)


#======================================================
# Read in & wrangle assembled data (from ABS ETS)
#======================================================

# --- Download Preschool + ERP + YBFS data from GitHub repo---
daturl <- "https://github.com/Atyepa/YBFS/raw/main/Assembled_YBFS_28072021.xlsx"
download.file(daturl,"Assembled_YBFS_28072021.xlsx", mode = "wb" )

#--- Read in Excel --
Inp_YBFS_data <- read.xlsx("Assembled_YBFS_28072021.xlsx")
YBFS_dataw <- Inp_YBFS_data %>%
  select(-1)

# Wrangle YBFS_data -pivot states to long, pivot Data_item wide---
    # Make factors for state, Year as char
    State_lbl <- c("Aust", "NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT")
    State_lvl <- c("0", "1", "2", "3", "4", "5", "6", "7", "8")

YBFS_data <- YBFS_dataw %>% 
  rename(`0` = Aust, `1` = NSW, `2` = Vic, `3` = Qld, `4` = SA, `5` = WA, `6`= Tas, `7` = NT, `8` = ACT) %>%
  pivot_longer(3:11, names_to = "state", values_to = "value") %>% 
  mutate(state = factor(state, levels = State_lvl, labels = State_lbl)) %>%
  mutate(value = as.integer(value), Year = as.character(Year)) %>% 
  pivot_wider(names_from = Data_item, values_from = value) %>% 
  select(Year, state, `Preschool (SS YBFS)`, `ERP (4yrs)`, `ERP (4yrs, r)`, `State-specific YBFS`,
         `1st year of school (lagged 1 yr)`) %>% 
  rename(First_yr_sch_orig = `1st year of school (lagged 1 yr)`)

# Note have dropped  the non-required measures of preschool & , 
# renamed 1st year of school (lagged 1 yr) to First_yr_sch_orig


#=====================================================================
# Read in & wrangle ERP and migration numbers via SDMX from ABS.Stat
#====================================================================

# Download ERP & NOM + internal migration 
Imig_sdmx <- "https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/INTERSTATEMIGRATION_CY/3.3.A04+A59.0+1+2+3+4+5+6+7+8.A/all?startTime=2007&endTime=2020"
NOM_sdmx <- "https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/NETOVERSEASMIGRATION_CY/3.3.A04+A59.0+1+2+3+4+5+6+7+8.A/all?startTime=2007&endTime=2020"
ERP_sdmx <-  "https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0+1+2+3+4+5+6+7+8.3+1+2.0+1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100.Q/all?startTime=2000-Q1&endTime=2020-Q4"
#births_sdmx <- "https://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/BIRTHS_MONTH_OCCURRENCE/1.1+2+3+4+5+6+7+8+9+10+11+12.0+1+2+3+4+5+6+7+8.A/all?startTime=2000&endTime=2019"

download.file(ERP_sdmx,'erp.xml')
download.file(Imig_sdmx,'Imig.xml')
download.file(NOM_sdmx,'NOM.xml')
# download.file(births_sdmx,'births.xml')

#---Read in ERP & Migration data---
ERP <- read_sdmx('erp.xml')
Imig <- read_sdmx('Imig.xml')
NOM <- read_sdmx('NOM.xml')
#Births <- read_sdmx('births.xml')
#-----------------------------------------


#--Join & tidy  internal + NOM migration

Imig04_59 <- Imig %>% 
  rename(state_code  = ASGS_2011, Year = Time) %>% 
  mutate(Age_grp = case_when(AGE == "A04" ~ "0-4", 
                         AGE == "A59" ~ "5-9"), 
         Imig = as.numeric(ObsValue)) %>% 
  filter(MEASURE == 3) %>% 
  select(Year, Age_grp, state_code, Imig) 
  

NOM04_59 <- NOM %>% 
  rename(state_code  = ASGS_2011, Year = Time) %>% 
  mutate(Age_grp = case_when(AGE == "A04" ~ "0-4", 
                         AGE == "A59" ~ "5-9"), 
                         NOM = as.numeric(ObsValue)) %>% 
                                filter(MEASURE == 3) %>% 
                                select(Year, Age_grp, state_code, NOM)


Mig04_59 <- NOM04_59 %>% 
  left_join(Imig04_59, by = c("Age_grp", "state_code", "Year")) 


# Impute 0 for Aust Imig.  Sum NOM + Imig for NetMIG
Mig04_59 <- Mig04_59 %>% 
  replace_na(list(Imig = 0)) %>% 
  mutate(NetMig_grp = NOM + Imig) 

#--- Tidy ERP.  Subset to June, psn, 0-4yrs  & 5-9
ERP04_59 <- ERP %>% 
  mutate(Age = as.numeric(AGE), ERP_sya = as.numeric(ObsValue),   # <- sya ~ "single year of age"
         Q = substr(Time, 6,7), Year = substr(Time, 1,4)) %>% 
  filter(Age <=9, SEX_ABS == 3) %>% 
   mutate(Age_grp = case_when(Age %in% c(0:4) ~ "0-4", 
                              Age %in% c(5:9) ~ "5-9"),
          yr = as.numeric(Year)) %>% 
  rename(state_code  = STATE) %>% 
  filter(Q == "Q2", yr > 2006) %>% 
  select(Year, state_code, Age, Age_grp, ERP_sya)
  
#  Calculate ERP fractions of single year (0,1,2..9yr olds) within the five-year age groups 
fraction04_59 <- ERP04_59 %>% 
  group_by(state_code, Age_grp, Year) %>% 
  mutate(ERP_grp = sum(ERP_sya), Mig_year = Year) %>% 
  mutate(p = round(ERP_sya/ERP_grp,4)) 
  

# Use the fraction as a spine for joining Mig (fractions will split Migration)  --
MigFr <- fraction04_59 %>% 
  left_join(Mig04_59, by = c("Year", "state_code", "Age_grp")) %>% 
   mutate(NetMig_sya = round(NetMig_grp * p,0)) %>% 
   select(Year, state_code, Age, Age_grp, NetMig_sya, NetMig_grp, ERP_sya, ERP_grp, p) %>% 
  ungroup()

# Make state a factor & a numeric Year (+1) 
MigFr5 <- MigFr %>% 
  mutate(state = factor(state_code, levels = State_lvl, labels = State_lbl)) %>% 
  mutate(Year_next = as.character(as.integer(Year)+1)) %>% 
  filter(Age == 5) %>% 
  select(Year_next, state, NetMig_sya) %>% 
  rename(NetMig = NetMig_sya)
  

# MigFr5 is being used to gives us NetMig, but advanced by 1yr (i.e. yearn - which will join on )
YBFS_mig <- YBFS_data %>% 
    left_join(MigFr5, by = c("Year" = "Year_next", "state"))


# Adjust for the migration (NetMig)
YBFS_data2  <- YBFS_mig %>% 
  mutate(First_yr_sch_revised = First_yr_sch_orig - NetMig) %>% 
  select(-NetMig) %>% 
  mutate(Numerator = `Preschool (SS YBFS)`) %>% 
  pivot_longer(3:8, names_to = "Series", values_to = "value") %>% 
  mutate(Percent = round(Numerator/value*100,1), 
         Year = factor(Year)) %>% 
  mutate(`Numerator / Denominator` = paste0("Preschool (SS YBFS) / ",Series))
  



#*********************************
#  UI prep
#*********************************
YBFS_data2w <- YBFS_data2
#----------------------------------------------------
#  list for selecting state, year, series, Num_Denom
#----------------------------------------------------

State <- YBFS_data2 %>%
  mutate(State = as.character(state)) %>%
  select(State) %>%
  distinct()

State <- as.list(State$State)

Year <- YBFS_data2 %>%
  mutate(Year = as.character(Year)) %>%
  select(Year) %>%
  distinct()

Year <- as.list(Year$Year)


Series <- YBFS_data2 %>%
  select(Series) %>%
  mutate(Series = as.character(Series)) %>%
  distinct()

Series <-as.list(Series$Series)

#@#@## `Numerator / Denominator` is a Feature 
Num_Denom_ <- YBFS_data2 %>% 
  select(`Numerator / Denominator`) %>%
  distinct()
Num_Denom <- as.list(Num_Denom_$`Numerator / Denominator`)


#==================
# UI
#==================  

ui <- shinyUI(fluidPage(
  
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
  ),
  
  headerPanel("Preschool enrolments and selected denominators"),
  
  
  sidebarPanel( id="sidebar",
                radioButtons('Plot', 'Select unit of measure:', choices = c("Preschool enrolment (%)", "Enrolment & population numbers"),
                             selected = "Preschool enrolment (%)"),
                
                
                conditionalPanel("input.Plot == `Enrolment & population numbers`",
                                 checkboxGroupInput('Series', 'Compare measures of preschool enrolment and population denominators:',
                                                    choices = c("Preschool (SS YBFS)", 
                                                                "ERP (4yrs)", "ERP (4yrs, r)", "State-specific YBFS", 
                                                                "First_yr_sch_orig", "First_yr_sch_revised"),
                                                    selected = c("ERP (4yrs)", "1st year of school (lagged 1 yr)"))
                                 
                ),
                
                
                conditionalPanel("input.Plot == `Preschool enrolment (%)`",
                                 
                                 checkboxGroupInput('Num_Denom', 'Compare proportions from different Numerator / denominator combinations:',
                                                    choices = c(
                                                      "Preschool (SS YBFS) / ERP (4yrs)",              
                                                      "Preschool (SS YBFS) / ERP (4yrs, r)",           
                                                      "Preschool (SS YBFS) / State-specific YBFS",     
                                                      "Preschool (SS YBFS) / First_yr_sch_orig",       
                                                      "Preschool (SS YBFS) / First_yr_sch_revised"),     
                                                      
                                                    selected = c("Preschool (4-5 yrs) / ERP (4yrs)", "Preschool (SS YBFS) / ERP (4yrs)"))
                ),
                
                pickerInput('State', 'Select states', choices = c(State),
                            selected = c(State), multiple = TRUE),
                
                pickerInput('Year', 'Select year', choices = c(Year),
                            selected = c("2016", "2017", "2018", "2019", "2020" ), multiple = TRUE),
                #selected = c(Year), multiple = TRUE),
                
                
                # submitButton("Update view", icon("refresh")),
                
                downloadButton("downloadData", "Download current selection"),
                
                
                downloadButton("downloadDataAll", "Download all underlying data"),
                
                # options = list(`actions-box` = TRUE),   # build buttons for collective selection
                width = 3),
  
  mainPanel( 
    tabsetPanel(type = "tabs", 
                tabPanel("Plot", plotlyOutput('plot')),
                tabPanel(style = "overflow-y:scroll; max-height: 700px","Data table", DT::dataTableOutput('Table')),
                tabPanel("About",
                         br(),
                         hr(),
                         tags$div( tags$h2(tags$b("Explanatory notes")), 
                                   tags$h3(tags$b("Preschool enrolments and selected denominators")),
                                   
                                   p("This app is designed to assist evaluation of the performance indicators of access to early childhood education. The side-by-side comparison of the three existing measures of preschool enrolment along with three population denominators provides important context for decisions on future measurement and reporting requirements."),
                                   p(tags$b("Preschool "), "refers to enrolments in preschools and centre based day care providing preschool programs and where an ", tags$b("enrolment "), "is counted if a child attended the preschool program for at least one hour during the reference period, or were absent due to illness or extended holiday leave and expected to return. For more information, please refer to 'Preschool program' in the ",
                                     tags$a(href="https://www.abs.gov.au/methodologies/preschool-education-australia-methodology/2020", target="_blank",
                                            "Methodology"), "section of Preschool Education, Australia."),  
                                   br(),
                                   
                                   tags$h4(tags$b("Data definitions")),
                                   tags$ul(
                                     
                                     tags$li(tags$b("Preschool (SS YBFS)"), " -  the number of preschool enrolments taking account of state-specific age eligibility for starting preschool and school of the state in which the child usually resides and the child's date of birth. For New South Wales and Victoria adjustment factors have been applied to account for the rates at which children proceed from preschool to school education. Accounting for the state-specific entry provisions is also known as the state- specific Year Before Full-time Schooling (state-specific YBFS). Data source:",
                                             tags$a(href="https://www.abs.gov.au/statistics/people/education/preschool-education-australia/latest-release", target="_blank", "Preschool Education Australia, Table 28.")),
                                     br(),
                                     
                                     tags$li(tags$b("Preschool (4-5 yrs)"), " - the number of enrolments among all 4 and 5 year olds. See", 
                                             tags$a(href="https://www.abs.gov.au/statistics/people/education/preschool-education-australia/latest-release", target="_blank" ,
                                                    paste0("Preschool Education, Australia."))),
                                     br(),
                                     
                                     tags$li(tags$b("Preschool (original YBFS)"), " - the number of children enrolled in a preschool program aged 4 years and only children aged 5 years who have not previously attended a preschool program as a 4 year old."),
                                     
                                     br(),
                                     
                                     tags$li(tags$b("ERP (4yrs)"), " - the ", tags$em("preliminary"), "Estimated Resident Population of four year olds. The ERP is Australia's official measure of the population of Australia and is based on the concept of usual residence. It refers to all people, regardless of nationality, citizenship or legal status, who usually live in Australia, with the exception of foreign diplomatic personnel and their families. It includes usual residents who are overseas for fewer than 12 months. It excludes overseas visitors who are in Australia for fewer than 12 months. Upon the initial publication and for the 12 months following, the ERP status is termed 'preliminary'.  After this time, ABS updates the ERP to 'revised' based on more complete information on the components (births, deaths, migration).  The 'preliminary' ERP is presented here because this reflects the actual data provided in each year to measure the UANP indicator.  To access the original historical data, please see the time-series spreadsheets available for download from the ",
                                             tags$a(href="https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population", target="_blank",
                                                    "'Previous releases' section of National, state and territory population.")),
                                     br(),
                                     
                                     tags$li(tags$b("ERP (4yrs, r)"), " - the ", tags$em("revised"), "ERP of four year olds. The ERP figures for a given period remain 'preliminary' for 12 months following their intial publication, before becoming 'revised' and become 'revised' as population components are updated. While the timeliness imperative for national reporting means these revised figures are not used, their inclusion here can be used to assess the data quality impact of using preliminary ERP. Data source: ",
                                             tags$a(href="https://stat.data.abs.gov.au/", target="_blank",
                                                    "ABS.Stat "), "Quarterly Population Estimates (ERP), by State/Territory, Sex and Age."),
                                     br(),
                                     
                                     tags$li(tags$b("1st year of school (lagged 1 yr)"), " - the number of children who were enrolled in their first year of school in the year following the preschool reference year. Also known as Transition to primary school Foundation year (Year prior to year 1). The rationale for including this as a denominator is that it effectively measures the population who, 12 months earlier, were in their year before school. However, the numbers are not expected to perfectly agree with the target number of children enrolled in a preschool program the previous year because of interstate and overseas migration and the presence of repeating students in their first year of school.  Data source: ",
                                             tags$a(href= "https://www.abs.gov.au/statistics/people/education/schools/latest-release#data-download", target="_blank", "Schools, Australia Table 42b")),
                                     
                                     br(),
                                     
                                     tags$li(tags$b("State-specific YBFS"), " -  an adjustment made to the ERP to align the estimated population count with the state-specific age requirements. The method uses the monthly pattern of births to distribute the annual aggregate ERP by month of birth. The aim of deriving the month of birth estimate of ERP by state in which the child usually resides is to allow the ERP denominators to be tailored by month of birth to be conceptually equivalent to the state-specific preschool age requirements.  For New South Wales and Victoria, this method also takes account of the proportion of younger children in preschool who are likely to proceed to their first year of school in the following year.  A deduction is also made for the number of children aged 4 and 5 years of age in the state-specific YBFS cohorts who were attending school in each state/territory. For more information, please refer 'Preschool program' and 'Measurement concepts' in the",
                                             tags$a(href="https://www.abs.gov.au/methodologies/preschool-education-australia-methodology/2020", target="_blank",
                                                    "Methodology"), "section of Preschool Education, Australia.")),
                                   br(),      
                                   
                                   tags$h4(tags$b("Further information")),
                                   
                                   tags$p("For more information, please contact ABS Education and Training Statistics Section:",
                                          a("NCETS_2015_Plus_WDB@abs.gov.au", href="mailto:NCETS_2015_Plus_WDB@abs.gov.au"))
                                   
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
  
  N<- reactive({
    list(Num_Denom=input$Num_Denom)
  })
  
  
  YBFS_data2c <- reactive({ YBFS_data2 %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(Series %in% D()$Series)
  })
  
  
  YBFS_data2r <- reactive({ YBFS_data2 %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(`Numerator / Denominator` %in% N()$Num_Denom)
  })
  
  
  
  Tablec <-  reactive({ YBFS_data2w %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(Series %in% D()$Series) %>% 
      select(Year, state, Series, value) %>% 
      rename(State = state) %>% 
      distinct() %>% 
      drop_na() 
  })
  
  
  Tabler <-  reactive({ YBFS_data2w %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(`Numerator / Denominator` %in% N()$Num_Denom) %>% 
      rename(Denominator = Series, State = state, `Num. value` = n, `Denom. value` = value) %>% 
      select(Year, State, Numerator, `Num. value`, Denominator, `Denom. value`, Percent) %>% 
      drop_na() 
  })
  
  
  TableA <-  reactive({ YBFS_data2w %>%
      drop_na() 
  })
  
  
  
  Sumtab <- reactive({ YBFS_data2c %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year)
  })
  
  
  
  output$plot <- renderPlotly({
    
    if(input$Plot == "Preschool enrolment (%)"){
      p <- ggplot(YBFS_data2r(), aes(x= Year, y= Percent, 
                                colour = `Numerator / Denominator`, 
                                group = `Numerator / Denominator`)) +
        geom_line(size = .8)+
        geom_point(size = 2)+
        geom_hline(yintercept=100, size = 0.5, alpha = 0.5, color = "black")+
        theme_few()+
        scale_color_few()+
        xlab("Year")+
        ylab("%")
      
      p <- p + facet_wrap(~ state)
    }
    
    if(input$Plot == "Enrolment & population numbers"){
      p <- ggplot(YBFS_data2c(), aes(x= Year, y = value, colour = Series, group = Series)) +
        geom_line(size = .8)+
        geom_point(size = 2) +
        scale_y_continuous(labels = comma)+
        theme_few()+
        scale_color_few()+
        ylab("n")
      
      p <- p + facet_wrap(~ state, scales = "free")
    }
    
    ggplotly(p, height = 780, width = 1350, autosize= TRUE, tooltip = c("x", "y", "colour") )
    
  })
  
  
  output$Table <- DT::renderDataTable({
    
    
    if(input$Plot == "Enrolment & population numbers"){
      T <- Tablec()
      
    }
    
    if(input$Plot == "Preschool enrolment (%)"){
      
      T <-Tabler()  
      
    }
    
    T
    
  })  
  
  
  # Downloadable xlsx --
  
  TableDL <- reactive({
    if(input$Plot == "Enrolment & population numbers"){
      Tablec()
    } else {
      Tabler()
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Selected YBFS data", ".xlsx")},
    content = function(file) { write_xlsx(TableDL(), path = file) }
  )
  
  
  output$downloadDataAll <- downloadHandler(
    filename = function() {
      paste("All YBFS data", ".xlsx")},
    content = function(file) { write_xlsx(TableA(), path = file) }
  )
  
  
}

#========================================  
shinyApp(ui, server)
#========================================






#@##====================================================================
#@##====================================================================
#@##====================================================================
#@##====================================================================


#---Adjust for state of child at school---

#---Read in state of student adjustment for 1st yr school--
adj_schl <- read.xlsx("first_yr_sch_adj.xlsx.")

# prep for join
adj_schl <- adj_schl %>% 
  select(-state) %>% 
  mutate(state = factor(state_code, levels = State_lvl, labels = State_lbl)) %>% 
  select(-state_code) 
  

# Subset to "1st year of school (lagged 1 yr)" then join adj_schl 
school0 <- data2 %>% 
  filter(Series == "1st year of school (lagged 1 yr)") %>% 
  left_join(adj_schl, by = c("state", "Year")) %>% 
  rename(valueO = value)
  
school0 <- replace_na(school0, list(child_oth_state = 0))

school0 <- school0 %>% 
  mutate(value = valueO - child_oth_state) %>% 
  select(-valueO, -child_oth_state)

# replace 1st year of school (lagged 1 yr) with school0
data2 <- data2 %>% 
  filter(Series != "1st year of school (lagged 1 yr)") %>% 
  bind_rows(school0)

# data 2 is now adjusted

#---subset SS preschool for Left-joining as numerator ---
prescSS <- data2 %>%
  filter(Series == "Preschool (SS YBFS)") %>%
  mutate(Numerator = "Preschool (SS YBFS)" ) %>% 
  rename(`Preschool (SS YBFS)` = value) %>%
  select(-2)


# Left Join preschool numerators then stack
presc <- prescSS %>%
    rename(n = `Preschool (SS YBFS)`) %>% 
    mutate(Numerator= "Preschool (SS YBFS)") %>% 
  select(Year, state, Numerator, n)

# Join to num to denom, calc %
YBFS_data2 <- data2 %>%
  left_join(presc, by = c("Year", "state")) %>%
  mutate(Prop = round(n/value*100,1))

# Make a Numerator-Denominator combination variable
YBFS_data2 <- YBFS_data2 %>%
  mutate(`Numerator / Denominator` = paste0(Numerator," / ",Series)) %>% 
  mutate(Year = factor(Year))

tempw <- YBFS_data2 %>% 
  select(-5:-8) %>% 
  pivot_wider(names_from = "Series", values_from = "value") %>% 
  arrange(state) 

withRepo(library(caret))

NSW <-  tempw %>% 
  filter(state == "NSW", Year %!in% c(2012:2015), Year != 2020)

mod_fit1 <- train(`1st year of school (lagged 1 yr)` ~ `ERP (4yrs)` + `Preschool (SS YBFS)` + `State-specific YBFS`,
                  data = NSW,
                  trControl = trainControl(method = "cv", number = 5),
                  method = "glm")


DI_lvl <- c("ERP (4yrs)", "State-specific YBFS", "1st year of school (lagged 1 yr)","Preschool (SS YBFS)")
