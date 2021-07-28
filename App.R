#---Libraries--- 
library(tidyverse)
library(lubridate)
library(openxlsx)
library(writexl)
library(plotly)
library(scales)
library(crosstalk)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
#---------------------
#--- Not in operator---
`%!in%` = negate(`%in%`)

#--- Download data from GitHub repo---
daturl <- "https://github.com/Atyepa/YBFS/raw/main/Assembled_YBFS_28072021.xlsx"

download.file(daturl,"Assembled_YBFS_28072021.xlsx", mode = "wb" )

#--- Read in assembled data --
data1 <- read.xlsx("Assembled_YBFS_28072021.xlsx")

data1 <- data1 %>%
  select(-1)

DI <- data1 %>%
  select(Data_item) %>%
  group_by(Data_item) %>%
  distinct()

#---Tidy long---
DI_lvl <- c("ERP (4yrs)", ("ERP (4yrs, r)"),  "State-specific YBFS", "1st year of school (lagged 1 yr)",
            "Preschool (SS YBFS)", "Preschool (original YBFS)", "Preschool (4-5 yrs)")

State_lbl <- c("Aust", "NSW", "Vic", "Qld", "SA", "WA", "Tas", "NT", "ACT")
State_lvl <- c("0", "1", "2", "3", "4", "5", "6", "7", "8")

data2 <- data1 %>%
  rename(`0` = Aust, `1` = NSW, `2` = Vic, `3` = Qld, `4` = SA, `5` = WA, `6`= Tas, `7` = NT, `8` = ACT) %>%
  pivot_longer(3:11, names_to = "state", values_to = "value")%>%
  mutate(state = factor(state, levels = State_lvl, labels = State_lbl)) %>%
  rename(Series = Data_item) %>%
  mutate(Series = factor(Series, levels = DI_lvl)) %>%
  mutate(value = round(value,0))

#---subset SS preschool for Left-joining as numerator ---
prescSS <- data2 %>%
  filter(Series == "Preschool (SS YBFS)") %>%
  rename(`Preschool (SS YBFS)` = value) %>%
  select(-2)

#---subset Preschool (original YBFS)  for Left-joining as numerator ---
prescYBFS <- data2 %>%
  filter(Series == "Preschool (original YBFS)") %>%
  rename(`Preschool (original YBFS)` = value) %>%
  select(-2)

#---subset 4&5 preschool for Left-joining as numerator ---
presc45 <- data2 %>%
  filter(Series == "Preschool (4-5 yrs)") %>%
  rename(`Preschool (4-5 yrs)` = value) %>%
  select(-2)

# Left Join 3 preschool numerators then stack
presc <- prescSS %>%
  left_join(prescYBFS, by = c("state", "Year")) %>%
  left_join(presc45, by = c("state", "Year")) %>%
  pivot_longer(3:5, names_to = "Numerator", values_to = "n")

# Join to num to denom, calc %
data4 <- data2 %>%
  left_join(presc, by = c("Year", "state")) %>%
  mutate(Prop = round(n/value*100,1))

# Make a Numerator-Denominator combination variable
data4 <- data4 %>%
  mutate(`Numerator / Denominator` = paste0(Numerator," / ",Series)) %>% 
  mutate(Year = factor(Year))


#*********************************
#  UI prep
#*********************************

#----------------------------------------------------
#  list for selecting state, year, series, Num_Denom
#----------------------------------------------------

State <- data4 %>%
  mutate(State = as.character(state)) %>%
  select(State) %>%
  distinct()

State <- as.list(State$State)

Year <- data4 %>%
  mutate(Year = as.character(Year)) %>%
  select(Year) %>%
  distinct()

Year <- as.list(Year$Year)


Series <- data4 %>%
  select(Series) %>%
  mutate(Series = as.character(Series)) %>%
  distinct()

Series <-as.list(Series$Series)


Num_Denom_ <- data4 %>%
  select(`Numerator / Denominator`) %>%
  distinct() %>%
  filter(`Numerator / Denominator` %in% c("Preschool (SS YBFS) / ERP (4yrs)",
                                          "Preschool (SS YBFS) / ERP (4yrs, r)",
                                          "Preschool (SS YBFS) / 1st year of school (lagged 1 yr)",
                                          "Preschool (SS YBFS) / State-specific YBFS",
                                          "Preschool (4-5 yrs) / ERP (4yrs)",
                                          "Preschool (4-5 yrs) / 1st year of school (lagged 1 yr)",
                                          "Preschool (4-5 yrs) / State-specific YBFS",
                                          "Preschool (original YBFS) / ERP (4yrs)",
                                          "Preschool (original YBFS) / 1st year of school (lagged 1 yr)",
                                          "Preschool (original YBFS) / State-specific YBFS"))

Num_Denom <- as.list(Num_Denom_$`Numerator / Denominator`)


#==================
# UI
#==================  

ui <- shinyUI(fluidPage(

  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #EBEEF9;
        }

         # main {
            background-color: #EBEEF9;
        }
         
        body, label, input, button, select { 
          font-family: "Arial";
        }
         
         .nav-tabs {
           background-color: #EBEEF9;
         } '
         
         )
  )),
  
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  headerPanel("Preschool enrolments and selected denominators"),
  
  
  sidebarPanel( id="sidebar",
    radioButtons('Plot', 'Select unit of measure:', choices = c("Preschool enrolment (%)", "Enrolment & population numbers"),
                 selected = "Preschool enrolment (%)"),
  
    
    conditionalPanel("input.Plot == `Enrolment & population numbers`",
                     checkboxGroupInput('Series', 'Compare measures of preschool enrolment and population denominators:',
                                        choices = c("Preschool (SS YBFS)", "Preschool (4-5 yrs)", "Preschool (original YBFS)",
                                                    "ERP (4yrs)", "ERP (4yrs, r)", "1st year of school (lagged 1 yr)", "State-specific YBFS"),
                                        selected = c("ERP (4yrs)", "1st year of school (lagged 1 yr)"))
                     
    ),
    
    
    conditionalPanel("input.Plot == `Preschool enrolment (%)`",
                     
                     checkboxGroupInput('Num_Denom', 'Compare proportions from different Numerator / denominator combinations:',
                                        choices = c(
                                          "Preschool (4-5 yrs) / ERP (4yrs) - Current UANP measure" = "Preschool (4-5 yrs) / ERP (4yrs)",
                                          "Preschool (4-5 yrs) / ERP (4yrs, r)",
                                          "Preschool (4-5 yrs) / 1st year of school (lagged 1 yr)",
                                          "Preschool (4-5 yrs) / State-specific YBFS",
                                          
                                          "Preschool (SS YBFS) / ERP (4yrs)",
                                          "Preschool (SS YBFS) / 1st year of school (lagged 1 yr)",
                                          "Preschool (SS YBFS) / State-specific YBFS",
                                          
                                          "Preschool (original YBFS) / ERP (4yrs)",
                                          "Preschool (original YBFS) / 1st year of school (lagged 1 yr)",
                                          "Preschool (original YBFS) / State-specific YBFS"),
                                          
                                        selected = c("Preschool (4-5 yrs) / ERP (4yrs)"))
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
                tabPanel("Data table", DT::dataTableOutput('Table')),
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
  
  
  data4c <- reactive({ data4 %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(Series %in% D()$Series)
  })
  
  
  data4r <- reactive({ data4 %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(`Numerator / Denominator` %in% N()$Num_Denom)
  })
  
  
  
  Tablec <-  reactive({ data4w %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(Series %in% D()$Series) %>% 
      drop_na() %>% 
      rename(Percent = Prop) 
  })
  
  
  Tabler <-  reactive({ data4w %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year) %>%
      filter(`Numerator / Denominator` %in% N()$Num_Denom) %>% 
      drop_na() %>% 
      rename(Percent = Prop) 
  })
  
  
  TableA <-  reactive({ data4w %>%
              drop_na() %>% 
              rename(Percent = Prop) 
  })
  
  
  
  Sumtab <- reactive({ data4c %>%
      filter(state %in% S()$State) %>%
      filter(Year %in% Y()$Year)
  })
  
  
  
  output$plot <- renderPlotly({
    
    if(input$Plot == "Preschool enrolment (%)"){
      p <- ggplot(data4r(), aes(x= Year, y= Prop, 
                                colour = `Numerator / Denominator`, 
                                group = `Numerator / Denominator`)) +
        geom_line(size = .8)+
        geom_point(size = 2)+
        geom_hline(yintercept=100, size = 0.5, alpha = 0.5, color = "black")+
        xlab("Year")+
        ylab("%")
      
      p <- p + facet_wrap(~ state)
    }
    
    if(input$Plot == "Enrolment & population numbers"){
      p <- ggplot(data4c(), aes(x= Year, y = value, colour = Series, group = Series)) +
        geom_line(size = .8)+
        geom_point(size = 2) +
        scale_y_continuous(labels = comma)+
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
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Selected YBFS data", ".xlsx")},
    content = function(file) { write_xlsx(Tabler(), path = file) }
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
