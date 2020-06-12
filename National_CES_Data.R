## national_ces_data.R ##
#  ******** This program creates a table of National CES Data ******************************
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(markdown)
library(tidyverse)
library(DT)
library(tools)
library(shinyjs)
library(sodium)
library(shinyalert)
library(RSQLite)
library(RODBC)
library(shinyjqui)
library(shinymanager)
library(gmailr)
library(shinyBS)
library(readxl)
library(rpivotTable)

series <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.series", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
industry <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.industry", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
datatype <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.datatype", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
period <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.period", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
supersector <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.supersector", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
seasonal <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.seasonal", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
data <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.data.0.AllCESSeries", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)
empl0 <- right_join(data, series, by = "series_id", copy = FALSE)
empl1 <- filter(empl0, seasonal == "S", data_type_code == "01")
empl1$month <- paste(empl1$year, empl1$period, sep = "", collapse = NULL)
empl2 <- right_join(empl1, supersector, by = "supersector_code", copy = FALSE)
empl3 <- right_join(empl2, datatype, by = "data_type_code", copy = FALSE)
empl4 <- right_join(empl3, industry, by = "industry_code", copy = FALSE)

empl <- empl4[,c ("supersector_name", "industry_name", "industry_code", "series_title", "series_id", "data_type_text", "display_level", "year","month", "period", "value", "data_type_code")]  # select fields to keep

rpivotTable(empl)
write.csv(empl, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/empl.csv")
saveRDS(empl,"empl.rds")

wages0 <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.data.02b.AllRealEarningsAE", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
wages1 <- right_join(wages0, series, by="series_id", copy = FALSE)
wages2 <- filter(wages1, data_type_code == "12" & seasonal == "S")
wages2$month <- paste(wages2$year, wages2$period, sep = "", collapse = NULL)
wages3 <- right_join(wages2, supersector, by = "supersector_code", copy = FALSE)
wages4 <- right_join(wages3, datatype, by = "data_type_code", copy = FALSE)
wages5 <- right_join(wages4, industry, by = "industry_code", copy = FALSE)

wages <- wages5[,c ("supersector_name", "industry_name", "series_title", "data_type_text", 
                    "display_level", "year", "period", "month", "value","seasonal", 
                    "data_type_code")]  # select fields to keep

rpivotTable(wages)
write.csv(wages, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/Wages.csv")

#Minimum list of columns to merge with employment date
wages_min <- wages[,c("industry_name", "month", "value")]

#combine employment and wage data into a single data table.
empl_wages <- merge(empl, wages_min, by=c("industry_name","month"), all=TRUE)

rpivotTable(empl_wages)
#write.csv(empl_wages, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/empl_wages.csv")

#Open the excel files below and inset a pivot table that can then be used to create scatter plots of
# of wages and employment
write_excel_csv(empl_wages, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/empl_wages.xls", 
                na = "0", append = FALSE, delim = ",", quote_escape = "double")

# ************************** County High Level QCEW Data *************************************
cnty <- read_delim("https://data.bls.gov/cew/data/files/2019/xls/2019_all_county_high_level.zip", 
                   "\t", escape_double = FALSE, trim_ws = TRUE)
data <- read.csv(unz("https://data.bls.gov/cew/data/files/2019/xls/2019_all_county_high_level.zip", "file1.csv"), header = TRUE,
                 sep = ",") 


#col_names = !append, 
header <- dashboardHeader(title = "BLS")

sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",
                                        menuItem(text = "BLS Data", tabName = "bls_data", selected = TRUE)
)) 

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "bls_data", useShinyjs(), useShinyalert(),#extendShinyjs(text = jscode),
            boxPlus(id = "main_window", width = 12,
                    div(align = 'center', style = "font-size: 25px; padding-top: 0px; margin-top:1em",
                        h1(strong("Welcome to the BLS Data"))),
                    fluidRow(
                      column(
                        width = 4,
                        selectInput(
                          inputId = "supersector",
                          label = "Super Sector Name",
                          choices =  c(Any = '', sort(unique(national$supersector_name))),
                          selected = '',
                          multiple = TRUE,
                          width = '100%'
                        )
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "industry",
                          label = "Industry Name",
                          choices =  c(Any = '',sort(unique(national$industry_name))),
                          selected = '',
                          multiple = TRUE,
                          width = '100%'
                        )
                      ),
                      
                    ),
                    
                    fluidRow(
                      column(
                        width = 4,
                        selectInput(
                          inputId = "display",
                          label = "Display Level",
                          choices =  c(Any = '',sort(unique(national$display_level))),
                          selected = '',
                          multiple = TRUE,
                          width = '100%'
                        )
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "year",
                          label = "Year",
                          choices =  c(Any = '',sort(unique(national$year))),
                          selected = '',
                          multiple = TRUE,
                          width = '100%'
                        )
                      ),
                      column(
                        width = 4,
                        selectInput(
                          inputId = "period",
                          label = "Period",
                          choices =  c(Any = '',sort(unique(national$period))),
                          selected = '',
                          multiple = TRUE,
                          width = '100%'
                        )
                      )
                    ),
                    div(align = 'left', style = "font-size: 24px; padding-top: 0px; margin-top:1em; ", 
                        boxPlus(
                          id = "table1",
                          title = "BLS DATA",
                          width = '100%',
                          solidHeader = TRUE,
                          #                         background = 'light-blue',
                          collapsible = TRUE,
                          collapsed = FALSE,
                          closable = FALSE,
                          div(style = 'overflow-x: scroll',
                              DT::dataTableOutput(
                                outputId = "bls",
                                width = "100%",
                                height = "auto"
                              ))),
                    rpivotTableOutput(outputId = "pivot")
            )#boxPlus
    )))
)

ui <- dashboardPagePlus(header, sidebar, body)

server <- function(input, output, session) {
  
  bls_var <- reactive({
    national_temp <- national
    #    print(input$supersector)
    if(!is.null(input$supersector)){
      national_temp <- national_temp %>% filter(supersector_name %in% input$supersector)
    }
    if(!is.null(input$industry)){
      national_temp <- filter(national_temp, industry_name %in% input$industry)
    }
    #    if(!is.null(input$series)){
    #      national_temp <- filter(national_temp, series_title %in% input$series)
    #    }
    #    if(!is.null(input$data)){
    #      national_temp <- filter(national_temp, data_type_text %in% input$data)
    #    }
    if(!is.null(input$display)){
      national_temp <- filter(national_temp, display_level %in% input$display)
    }
    if(!is.null(input$year)){
      national_temp <- filter(national_temp, year %in% input$year)
    }
    if(!is.null(input$period)){
      national_temp <- filter(national_temp, period %in% input$period)
    }
    national_temp <- national_temp %>% select("supersector_name", "industry_name", 
                                              "display_level", "year", "period", "value")
  })
  observe({
    if(is.null(input$supersector) || input$supersector == ''){
      updateSelectInput(session, inputId = "supersector", label = "Supersector Name",
                        choices = c(Any = '', sort(unique(bls_var()$supersector_name))),
                        selected = '')
    }
    if(is.null(input$industry) || input$industry == ''){
      updateSelectInput(session, inputId = "industry", label = "Industry Name",
                        choices = c(Any = '', sort(unique(bls_var()$industry_name))),
                        selected = '')
    }
    #    if(is.null(input$series) || input$series == ''){
    #    updateSelectInput(session, inputId = "series", label = "Series Title",
    #                      choices = c(Any = '', sort(unique(bls_var()$series_title))),
    #                      selected = '')
    #    }
    
    if(is.null(input$display) || input$display == ''){
      updateSelectInput(session, inputId = "display", label = "Display Level",
                        choices = c(Any = '', sort(unique(bls_var()$display_level))),
                        selected = '')
    }
    if(is.null(input$year) || input$year == ''){
      updateSelectInput(session, inputId = "year", label = "Year",
                        choices = c(Any = '', sort(unique(bls_var()$year))),
                        selected = '')
    }
    if(is.null(input$period) || input$period == ''){
      updateSelectInput(session, inputId = "period", label = "Period",
                        choices = c(Any = '', sort(unique(bls_var()$period))),
                        selected = '')
    }
  })
  observe( {  
    #    req(national_temp)
    output$bls <- renderDataTable({
      DT::datatable(data = bls_var(),
                    selection = list(mode = 'single')
      )
    })
  })
  observe({
    output$pivot <- renderRpivotTable({ rpivotTable(data = bls_var(), rows = c("supersector_name", "industry_name"), cols = c("year","period"),
                                                    aggregatorName = "Treemap", vals = "value",subtotals=TRUE,
                                                    options = list(scrollX = TRUE))
    })
  })
}

shinyApp(ui, server)   

# ********************************* Tools for use as needed *********************
#unused <- as.integer(c(1:55))
#rename(iris, petal_length = Petal.Length)
#UnitCIP <- distinct(dataSetName2, .keep_all = FALSE)
#saveRDS(CIP_Data, "CIPS.rds")  
#OCCFcst <- OCCFcst[, 4:9] <- round(OCCFcst[, 4:9], digits = 1)
#OCCFcst <- OCCFcst %>% rename(new_name = old_name)
#OCCFcst <- rename(OCCFcst, new_name = old_name)
#cipcode_all <- cipcode_all %>% filter(CIPCODE %notin% (unused))
#schools_columns <- readRDS("c_n_school.rds")
#OCCQint4 <- OCCQint3 %>% filter(o_group %in% "detailed")
#OCCQint5 <- OCCQint4[ -c(1,2,3,4,5,6,7,12,13,14,15,18)]  # removed unused columns
#OCCQint <- rename(OCCQint5, OCCCODE = occ_code)  # rename occ_code to OCCCODE in order to merge with OCCFcst
#OCC_Detail1 <- merge(x=OCCQint, y=OCCFcst, by="OCCCODE", all = TRUE)  #Merge OCC forecast & OCC salary data
#OCC_Detail2 <- OCC_Detail1 %>% mutate_if(is.double, ~replace(., is.na(.), 0)) # change "na" to "0"
#Backbone3 <- Backbone2[,c ("UNITID", "CIPCODE", "AWLEVEL", "CTOTALT", "OCCCODE", "Entry_Code")]  # select fields to keep

