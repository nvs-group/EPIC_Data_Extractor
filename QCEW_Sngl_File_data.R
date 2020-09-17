## QCEW_Sngl_File_data.R ##
#  ******** This program creates a table of State MSA and County QCEW Data ******************************
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

#Load QCEW single file data and Other files from https://www.bls.gov/cew/downloadable-data-files.htm
qcewsgl <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/County/2020.q1-q1.singlefile.csv")
qcew0 <- filter(qcewsgl, qtr == "1" & agglvl_code > "69" & agglvl_code < "75")  #qtr == "1" & aggregation levels
#qcewsgl2019 <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/County/2019.q1-q4.singlefile.csv")
#qcew2019 <- filter(qcewsgl2019, qtr == "4" & agglvl_code > "69" & agglvl_code < "75")  #qtr == "4" & aggregation levels
#qcewsgl2018 <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/County/2018.q1-q4.singlefile.csv")
#qcew2018 <- filter(qcewsgl2018, qtr == "3" & agglvl_code > "69" & agglvl_code < "80")

industry <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/industry_titles.csv")
agglevel <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/agglevel_titles.csv")
area <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/area_titles.csv")
owner <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/ownership_titles.csv")
size <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/size_titles.csv")
chg_pc <- read.csv("C:/Users/lccha/OneDrive/Database/QCEW Data/chg_pc.csv")

qcew1 <- merge(qcew0, industry, by="industry_code")
qcew2 <- merge(qcew1, area, by="area_fips")
qcew3 <- merge(qcew2, agglevel, by="agglvl_code")
qcew4 <- merge(qcew3, size, by="size_code")
qcew5 <- merge(qcew4, chg_pc, by="industry_code")
qcew5$decline_amt <- qcew5$total_qtrly_wages * qcew5$ChgPC
qcew5$decline_pc <- qcew5$decline_amt / qcew5$total_qtrly_wages

# Data for 1 quarter only saved as csv for loading into an excel pivot table
write.csv(qcew5, "C:/Users/lccha/OneDrive/Database/QCEW Data/County/qcew2020q1.csv")

rpivotTable(qcew5)

# ************************* End of QCEW by county data extraction *****************************

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
national1 <- right_join(supersector, series, by = "supersector_code", copy = FALSE)
national2 <- right_join(national1, datatype, by = "data_type_code", copy = FALSE)
national3 <- right_join(national2, industry, by = "industry_code", copy = FALSE)
national4 <- right_join(national3, data, by = "series_id", copy = FALSE)
national5 <- filter(national4, seasonal == "S", data_type_code == "01")
national5$month <- paste(national5$year, national5$period, sep = "", collapse = NULL)
national <- national5[,c ("supersector_name", "industry_name", "series_title", "data_type_text", "display_level", "year","month", "period", "value","seasonal", "data_type_code")]  # select fields to keep

rpivotTable(national)
write.csv(national, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/National.csv")
saveRDS(national,"national.rds")

wages1 <- read_delim("https://download.bls.gov/pub/time.series/ce/ce.data.02b.AllRealEarningsAE", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
wages2 <- right_join(wages1, national3, by = "series_id", copy = FALSE)
wages2$month <- paste(wages2$year, wages2$period, sep = "", collapse = NULL)
wages <- wages2[,c ("supersector_name", "industry_name", "series_title", "data_type_text", "display_level", "year", "period", "month", "value","seasonal", "data_type_code")]  # select fields to keep

rpivotTable(wages)
write.csv(wages, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/Wages.csv")

Combined <- left_join(national,wages, by=c("industry_name","month"), copy = FALSE)
Combined <- filter(Combined, data_type_code.y == "12" & seasonal.y == "S")
rpivotTable(Combined)
write.csv(Combined, "C:/Users/lccha/OneDrive/Database/CE Data/National Data/Current_Employment_Statistics/Combined.csv")

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

