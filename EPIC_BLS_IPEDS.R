## app.R ##
#  ******** This program creates the RDS files for the EPIC app ******************************
##Self Directed EPIC 1/07/2020
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
library(pKSEA)
#library(write.xlsx)

# Tools ********************************* Tools for use as needed ********************* ----
#percentile rank
#for(i in 1:6587) {
  # i-th element of `u1` squared into `i`-th position of `usq`
#  x[i] <- perc.rank(vector, x[i])
#}

#unused <- as.integer(c(1:55))
#rename(iris, petal_length = Petal.Length)  # Rename column headings
#UnitCIP <- distinct(dataSetName2, .keep_all = FALSE)
#saveRDS(CIP_Data, "CIPS.rds")  
#OCCFcst <- OCCFcst[, 4:9] <- round(OCCFcst[, 4:9], digits = 1)
#OCCFcst <- OCCFcst %>% rename(new_name = old_name)
#OCCFcst <- rename(OCCFcst, new_name = old_name)
#cipcode_all <- cipcode_all %>% filter(CIPCODE %notin% (unused))
#schools_columns <- readRDS("c_n_school.rds")
#filter(starwars, hair_color == "none" & eye_color == "black")
#wages <- filter(wages, data_type_code.y == "12" & seasonal.y == "S")
#CIP_Data <- CIP_Data3[,c("CIPCODE", "UNITID", "AWLEVEL", "CTOTALT", "Years")]
#School5 <- School5 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
#merge(x, y, by = c("k1","k2")) # NA's match
#rownames(df) #Rownames will return rownumbers present in Dataset,df=DataFrame name.
#row.names(myDataFrame) <- 1:nrow(myDataFrame)
#dat$x = as.numeric(as.character(dat$x)) #changes character column to numberic
#newdata <- mtcars[order(mpg, -cyl),]  #sort by mpg (ascending) and cyl (descending)
#AltTitle1<- AltTitle[order(AltTitle$AltName),c(1,2)] #sort by Alt Name
#OCC_CIP_CW$pre <- substr(OCC_CIP_CW$OCCCODE_OLD, 1, 2)  #Insert a dash "-" into a text string
#   OCC_CIP_CW$post <- substr(OCC_CIP_CW$OCCCODE_OLD, 3, 6)
#   OCC_CIP_CW$OCCCODE <- paste0(OCC_CIP_CW$pre, "-", OCC_CIP_CW$post)
#df$Country <- sub(" - USA","",df$Country) #replace " - USA" with ""
#data_price$nbrSims[is.na(data_price$nbrSims)] <- "Single" #replace "NA" with the text "Single"
#OCC_Detail1$OCCNAME <- ifelse(is.na(OCC_Detail1$OCCNAME), OCC_Detail1$occ_title, OCC_Detail1$OCCNAME)     # combine OCCNAME and occ_title
#for(i in 1:NROW(SchoolData)) {
#        SchoolData$GTotCstOutHi[i] <- ifelse(SchoolData$TUITION7[i] == 0,0,SchoolData$TUITION7[i] + SchoolData$FEE7[i] + 
#        SchoolData$CHG4AY3[i] + SchoolData$ROOMAMT[i] + SchoolData$BOARDAMT[i] + SchoolData$RMBRDAMT[i])}
#SchoolData <- merge(x=School1, y=School2, by="UNITID", all = TRUE)
#OCCQint5 <- OCCQint4[ -c(1,2,3,4,5,6,7,12,13,14,15,18)]  # removed unused columns
#write.xlsx(x, file, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
#write.xlsx2(x, file, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE) #faster on big files
#data <- read.csv(unz("master.zip", "file1.csv"), header = TRUE, sep = ",") 
#aggregate(x = testDF, by = list(by1, by2), FUN = "mean")
#full_join(data1, data2, by = c("id", "spp"))

# Load Occupation Description File and save as an RDS file ***************************** ----

# from: https://www.onetcenter.org/dictionary/25.1/excel/occupation_data.html
OCCDescriptions <- read_excel(path = "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Occupation Data.xlsx",
                              col_names = c("SOCCODE","OCCNAME","Description"))
#OCCDescriptions$OCCCode <- substr(OCCDescriptions$ONET_OCC_Code, 1, 7)
#OCCDescriptions$SubCode <- substr(OCCDescriptions$ONET_OCC_Code, 9, 10)
#OCCDescriptions <- filter(OCCDescriptions, SubCode == "00")
saveRDS(OCCDescriptions, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/occupation_desc.rds")


# IPEDS Access***** Create Channel to IPEDS Access file ************* ----
#Need latest Microsoft Access Database Engine 2010 Redistributable 64bit in order to use the channel function
#The IPEDS data can be found at https://nces.ed.gov/ipeds/use-the-data/download-access-database
#channela is the most recent "Final" IPEDS date. channelb is the most recent "Preliminary" IPEDS data
channelb <- odbcConnectAccess2007("C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/IPEDS201920.accdb")
channela <- odbcConnectAccess2007("C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/IPEDS201819.accdb")


# CIP List ***************Create full list of CIP codes and names *********** ----
#pull full CIP code list from the IPEDS data
CIP_List0 <- sqlQuery(channelb, "SELECT varTitle, varName, Codevalue, valueLabel FROM valuesets19 WHERE Codevalue Like '__.____'", as.is = TRUE) 
CIP_List0 <- filter(CIP_List0, varTitle == "CIPCODE")
CIP_List1 <- CIP_List0[-c(1,2)]
CIP_List2 <- rename(CIP_List1, CIPCODE = Codevalue)
CIP_List3 <- rename(CIP_List2, CIPNAME = valueLabel)

# remove cip names that end with a "." and save unique records only. Save the dataframe as an RDS file.
#CIP_List1$valueLabel <- gsub(glob2rx("*."), "*", CIP_List1$valueLabel, ignore.case = TRUE) #replace " - USA" with ""
#CIP_List1 <- filter(CIP_List1, valueLabel != "*")
CIP_List <- unique(CIP_List3)

saveRDS(CIP_List, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/CIP_List.rds")                                                      #Need to add TEXT for codes


# CIP Data ************************* CREATE CIP DATA FILE **************************************** ----
# import data table c2019_a (which includes completion information by institution) from IPEDS access database
CIP_Data0 <- sqlQuery(channelb, "SELECT CIPCODE, UNITID, AWLEVEL, CTOTALT, MAJORNUM FROM C2019_A WHERE CIPCODE Like '__.____'", as.is = TRUE ) 

#combine total awards to include double majors as separate awards
Major1 <- CIP_Data0 %>% filter(MAJORNUM ==1)  #select degrees awarded as first or primary major
Major2 <- CIP_Data0 %>% filter(MAJORNUM ==2)  #select degrees awarded as second or dual major
MajorT <- merge(Major1, Major2, by = c("CIPCODE", "UNITID", "AWLEVEL"), all = TRUE)
MajorT <- MajorT %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
MajorT$CTOTALT <- MajorT$CTOTALT.x + MajorT$CTOTALT.y

CIP_Data1 <- MajorT[,c("CIPCODE", "UNITID", "AWLEVEL", "CTOTALT")]
CIP_Data1$UNITID <- as.character(CIP_Data1$UNITID)
CIP_Data1$AWLEVEL <- as.character(CIP_Data1$AWLEVEL)    # Change AWLEVEL from integer to character

# Add AWLEVEL codes "01" and "05" to the CIP_Data file - these map to "1" and "2" Entry_Codes.
#AWLADD <- c("", "", "05","")
#CIP_Data1 <- rbind(CIP_Data1, AWLADD)
#AWLADD <- c("", "", "01","")
#CIP_Data1 <- rbind(CIP_Data1, AWLADD)

#Read degree crosswalk table between school degress and occupational entry degrees
DegreeCrosswalk <- read_excel("C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/DegreeCrosswalk.xlsx")

#create updated list of valid school degree codes
DegreeCodes <- DegreeCrosswalk$AWLEVEL

#delete summary school degree codes (duplicates) from main school completion dataset
CIP_Data2 <- CIP_Data1 %>% filter(AWLEVEL %in% DegreeCodes)

#Add Years to dataSetName3 file by AWLEVEL
CIP_Data3 <- merge(x=CIP_Data2, y=DegreeCrosswalk, by="AWLEVEL", all = FALSE)
CIP_Data <- CIP_Data3[,c("CIPCODE", "UNITID", "AWLEVEL", "CTOTALT", "Years")]       #Designate columns to keep

#Find total degrees awarded by CIP

CIP_Tot <- aggregate(x = (CIP_Data$CTOTALT), by=list(CIPCODE = CIP_Data$CIPCODE), FUN = sum)
CIP_Tot <- rename(CIP_Tot, "CTOTALT" = "x")  # Rename column headings
CIP_Tot <- CIP_Tot[order(-CIP_Tot$CTOTALT),]
CIP_Tot <- filter(CIP_Tot, CTOTALT > 0)
CIP_Tot$PC_CIP <- CIP_Tot$x    #Initialize new variable
NumRow <- nrow(CIP_Tot)
for(i in 1:NumRow) {
  CIP_Tot$PC_CIP[i] <- perc.rank(CIP_Tot$CTOTALT, CIP_Tot$CTOTALT[i])
}
#Save CIP percentile rank file as .RDS
saveRDS(CIP_Tot, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/CIP_PC.rds")                                                      #Need to add TEXT for codes

#Save complete CIP_Data file as .RDS
saveRDS(CIP_Data, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/CIPS.rds")                                                      #Need to add TEXT for codes


# Schools.rds ******************************** CREATE SCHOOL FILE ********************************** ----
#Load data from latest "Preliminary" IPEDS files using "channelb"
#When a new preliminary data file is released, all of the Access Tables year codes need to be updated below e.g. HD2019 >> HD2020, etc
School1 <- sqlQuery(channelb, "SELECT UNITID, INSTNM, CITY, STABBR, ZIP, WEBADDR FROM HD2019", as.is = TRUE ) 
School2 <- sqlQuery(channelb, "SELECT UNITID, APPLCN, ADMSSN, ENRLT FROM ADM2019", as.is = TRUE ) 
School3 <- sqlQuery(channelb, "SELECT UNITID, TUITION2, TUITION3, FEE2, FEE3, TUITION6, TUITION7, FEE6, FEE7, CHG4AY3 FROM IC2019_AY", as.is = TRUE )
School4 <- sqlQuery(channelb, "SELECT UNITID, ROOMAMT, BOARDAMT, RMBRDAMT FROM IC2019", as.is = TRUE ) 
School5 <- sqlQuery(channelb, "SELECT UNITID, BAGR100, BAGR150, BAGR200, L4GR100, L4GR150, L4GR200 FROM GR200_19", as.is = TRUE ) 
School6 <- sqlQuery(channelb, "SELECT UNITID, IGRNT_P, IGRNT_A FROM SFA1819_P1", as.is = TRUE )
School7 <- sqlQuery(channelb, "SELECT UNITID, FTEUG, FTEGD FROM EFIA2019", as.is = TRUE )

#Load data from latest "Final" IPEDS files using "channela" 
School1a <- sqlQuery(channela, "SELECT UNITID, INSTNM, CITY, STABBR, ZIP, WEBADDR FROM HD2018", as.is = TRUE ) 
School2a <- sqlQuery(channela, "SELECT UNITID, APPLCN, ADMSSN, ENRLT FROM ADM2018", as.is = TRUE ) 
School3a <- sqlQuery(channela, "SELECT UNITID, TUITION2, TUITION3, FEE2, FEE3, TUITION6, TUITION7, FEE6, FEE7, CHG4AY3 FROM IC2018_AY", as.is = TRUE )
School4a <- sqlQuery(channela, "SELECT UNITID, ROOMAMT, BOARDAMT, RMBRDAMT FROM IC2018", as.is = TRUE ) 
School5a <- sqlQuery(channela, "SELECT UNITID, BAGR100, BAGR150, BAGR200, L4GR100, L4GR150, L4GR200 FROM GR200_18", as.is = TRUE ) 
School6a <- sqlQuery(channela, "SELECT UNITID, IGRNT_P, IGRNT_A FROM SFA1718_P1", as.is = TRUE )
School7a <- sqlQuery(channela, "SELECT UNITID, FTEUG, FTEGD FROM EFIA2018", as.is = TRUE )

#Merge earlier "Final" data with later "Preliminary" then use final data if no preliminary data exists
School1 <- merge(x=School1, y=School1a, by="UNITID", all = TRUE)
School1 <- School1 %>% mutate_if(is.character, ~replace(., is.na(.), "")) # change "na" to "0"
School2 <- merge(x=School2, y=School2a, by="UNITID", all = TRUE)
School2 <- School2 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
School3 <- merge(x=School3, y=School3a, by="UNITID", all = TRUE)
School3 <- School3 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
School4 <- merge(x=School4, y=School4a, by="UNITID", all = TRUE)
School4 <- School4 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
School5 <- merge(x=School5, y=School5a, by="UNITID", all = TRUE)
School5 <- School5 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
School6 <- merge(x=School6, y=School6a, by="UNITID", all = TRUE)
School6 <- School6 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
School7 <- merge(x=School7, y=School7a, by="UNITID", all = TRUE)
School7 <- School7 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"

School1$INSTNM <- ifelse(School1$INSTNM.x == "",School1$INSTNM.y,School1$INSTNM.x)
School1$CITY <- ifelse(School1$CITY.x == "",School1$CITY.y,School1$CITY.x)
School1$STABBR <- ifelse(School1$STABBR.x == "",School1$STABBR.y,School1$STABBR.x)
School1$ZIP <- ifelse(School1$ZIP.x == "",School1$ZIP.y,School1$ZIP.x)
School1$WEBADDR <- ifelse(School1$WEBADDR.x == "",School1$WEBADDR.y,School1$WEBADDR.x)

School2$APPLCN <- ifelse(School2$APPLCN.x == 0,School2$APPLCN.y,School2$APPLCN.x)
School2$ADMSSN <- ifelse(School2$ADMSSN.x == 0,School2$ADMSSN.y,School2$ADMSSN.x)
School2$ADMRT <- ifelse(School2$ADMSSN.x == 0,School2$ADMSSN.y/School2$APPLCN.y,School2$ADMSSN.x/School2$APPLCN.x)
School2$ENRLT <- ifelse(School2$ENRLT.x == 0,School2$ENRLT.y,School2$ENRLT.x)

School3$TUITION2 <- ifelse(School3$TUITION2.x == 0,School3$TUITION2.y,School3$TUITION2.x)
School3$TUITION3 <- ifelse(School3$TUITION3.x == 0,School3$TUITION3.y,School3$TUITION3.x)
School3$TUITION6 <- ifelse(School3$TUITION6.x == 0,School3$TUITION6.y,School3$TUITION6.x)
School3$TUITION7 <- ifelse(School3$TUITION7.x == 0,School3$TUITION7.y,School3$TUITION7.x)

School3$FEE2 <- ifelse(School3$FEE2.x == 0,School3$FEE2.y,School3$FEE2.x)
School3$FEE3 <- ifelse(School3$FEE3.x == 0,School3$FEE3.y,School3$FEE3.x)

School3$FEE6 <- ifelse(School3$FEE6.x == 0,School3$FEE6.y,School3$FEE6.x)
School3$FEE7 <- ifelse(School3$FEE7.x == 0,School3$FEE7.y,School3$FEE7.x)

School3$CHG4AY3 <- ifelse(School3$CHG4AY3.x == 0,School3$CHG4AY3.y,School3$CHG4AY3.x)

School4$ROOMAMT <- ifelse(School4$ROOMAMT.x == 0,School4$ROOMAMT.y,School4$ROOMAMT.x)
School4$BOARDAMT <- ifelse(School4$BOARDAMT.x == 0,School4$BOARDAMT.y,School4$BOARDAMT.x)
School4$RMBRDAMT <- ifelse(School4$RMBRDAMT.x == 0,School4$RMBRDAMT.y,School4$RMBRDAMT.x)

School5$BAGR100 <- ifelse(School5$BAGR100.x == 0,School5$BAGR100.y,School5$BAGR100.x)
School5$BAGR150 <- ifelse(School5$BAGR150.x == 0,School5$BAGR150.y,School5$BAGR150.x)
School5$BAGR200 <- ifelse(School5$BAGR200.x == 0,School5$BAGR200.y,School5$BAGR200.x)
School5$L4GR100 <- ifelse(School5$L4GR100.x == 0,School5$L4GR100.y,School5$L4GR100.x)
School5$L4GR150 <- ifelse(School5$L4GR150.x == 0,School5$L4GR150.y,School5$L4GR150.x)
School5$L4GR200 <- ifelse(School5$L4GR200.x == 0,School5$L4GR200.y,School5$L4GR200.x)

School6$IGRNT_P <- ifelse(School6$IGRNT_P.x == 0,School6$IGRNT_P.y,School6$IGRNT_P.x)
School6$IGRNT_A <- ifelse(School6$IGRNT_A.x == 0,School6$IGRNT_A.y,School6$IGRNT_A.x)

School7$FTEUG <- ifelse(School7$FTEUG.x == 0,School7$FTEUG.y,School7$FTEUG.x)
School7$FTEGD <- ifelse(School7$FTEGD.x == 0,School7$FTEGD.y,School7$FTEGD.x)


#Total Enrollment Percentile Rankings ----

School7$FTETOT <- School7$FTEUG + School7$FTEGD
School7 <- filter(School7, FTETOT > 0)
School7 <- School7[order(-School7$FTETOT),]
School7$PCFTETOT <- School7$FTETOT    #Initialize new variable
NumRow <- nrow(School7)
for(i in 1:NumRow) {
  School7$PCFTETOT[i] <- perc.rank(School7$FTETOT, School7$FTETOT[i])
}


#Total Admission Percentile Rankings ----

School2 <- filter(School2, ADMRT > 0)
School2 <- School2[order(-School2$ADMRT),]
NumRow <- nrow(School2)
for(i in 1:NumRow) {
  School2$PCADMRT[i] <- perc.rank(School2$ADMRT, School2$ADMRT[i])
}


#write_csv(School7b, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/PCFTEUG.csv")

# Create Graduation Rate Factor for each school

# Normalize grad rates for 4 (LA) and 2 (L4) year programs
School5$pc75 <- 0
School5$pc100 <- (School5$BAGR100+School5$L4GR100)/(School5$BAGR200+School5$L4GR200)
School5$pc150 <- (School5$BAGR150+School5$L4GR150)/(School5$BAGR200+School5$L4GR200)
School5$pc150 <- ifelse(School5$pc150 > 1, 0,School5$pc150)  #Rate over 100% set to 0: error
School5$pc200 <- (School5$BAGR200+School5$L4GR200)/(School5$BAGR200+School5$L4GR200)
School5$Factor  <- if_else(School5$pc100 >= .5, ((.5-School5$pc75)/(School5$pc100-School5$pc75)*(1)+3)/4,
                           if_else(School5$pc150 >= .5, ((.5-School5$pc100)/(School5$pc150-School5$pc100)*(2)+4)/4,
                                   if_else(School5$pc200 >= .5, ((.5-School5$pc150)/(School5$pc200-School5$pc150)*(2)+4)/4,0)))

#combine school data into a single file and save as an RDS file
SchoolData <- merge(x=School1, y=School2, by="UNITID", all = TRUE)
SchoolData <- merge(x=SchoolData, y=School3, by="UNITID", all = TRUE)
SchoolData <- merge(x=SchoolData, y=School4, by="UNITID", all = TRUE)
SchoolData <- merge(x=SchoolData, y=School5, by="UNITID", all = TRUE)
SchoolData <- merge(x=SchoolData, y=School6, by="UNITID", all = TRUE)
SchoolData <- merge(x=SchoolData, y=School7, by="UNITID", all = TRUE)
SchoolData <- SchoolData %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) # change "na" to "0"

# Calculate total cost for in state and out of state undergraduate students
SchoolData$TotCstInHi <- ifelse(SchoolData$TUITION2 == 0,0,SchoolData$TUITION2 + SchoolData$FEE2 + SchoolData$CHG4AY3 + SchoolData$ROOMAMT + 
  SchoolData$BOARDAMT + SchoolData$RMBRDAMT)
SchoolData$TotCstOutHi <- ifelse(SchoolData$TUITION3 == 0,0,SchoolData$TUITION3 + SchoolData$FEE3 + SchoolData$CHG4AY3 + SchoolData$ROOMAMT + 
  SchoolData$BOARDAMT + SchoolData$RMBRDAMT)

SchoolData$TotCstInLo <- ifelse(SchoolData$TUITION2 == 0,0,SchoolData$TotCstInHi - SchoolData$IGRNT_A)
SchoolData$TotCstOutLo <- ifelse(SchoolData$TUITION3 == 0,0,SchoolData$TotCstOutHi - SchoolData$IGRNT_A)
SchoolData$UNITID <- as.character(SchoolData$UNITID)  # Make UNITID a character string
SchoolData$GTotCstInHi <- ifelse(SchoolData$TUITION6 == 0,0,SchoolData$TUITION6 + SchoolData$FEE6 + 
            SchoolData$CHG4AY3 + SchoolData$ROOMAMT + SchoolData$BOARDAMT + SchoolData$RMBRDAMT)
SchoolData$GTotCstOutHi <- ifelse(SchoolData$TUITION7 == 0,0,SchoolData$TUITION7 + SchoolData$FEE7 + 
            SchoolData$CHG4AY3 + SchoolData$ROOMAMT + SchoolData$BOARDAMT + SchoolData$RMBRDAMT)

SchoolData <- SchoolData[ c("UNITID","INSTNM","CITY","STABBR","ZIP","WEBADDR","APPLCN","ADMSSN","ENRLT","FTEUG","FTEGD",
                             "TUITION2","TUITION3","TUITION6","TUITION7","FEE2","FEE3","FEE6","FEE7","CHG4AY3",
                             "ROOMAMT","BOARDAMT","RMBRDAMT","BAGR100","BAGR150","BAGR200","L4GR100","L4GR150","L4GR200",
                             "pc75","pc100","pc150","pc200","Factor","IGRNT_P","IGRNT_A","TotCstInHi","TotCstOutHi",
                             "TotCstInLo","TotCstOutLo","GTotCstInHi","GTotCstOutHi","PCADMRT","PCFTETOT")]
SchoolData <- SchoolData %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) # change "na" to "0"

#Add "No Match" record for schools
SchoolNull1 <- list("No Match", "No Match","","","",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
SchoolData <- rbind(SchoolData, SchoolNull1)

saveRDS(SchoolData, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Schools.rds")


# Occupations.rds ********************** CREATE OCCUPATIONS OCC_Detail FILE ************************* ----
#combine occupation data into a single file, name and set numeric columns, and save as an RDS file
#Load OCC entry data table and keep only three columns to get Entry_Degree by OCCCODE
#combine occupation data into a single file, name and set numeric columns, and save as an RDS file
OCC_Detail7 <- read_excel(path = "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/occupation.xlsx", sheet = "Table 1.7", skip = 3,
                       col_names = c("OCCNAME", "OCCCODE2018", "OCCTYPE", "Emply2019", "Emply2029", 
                                    "EmplyChg", "EmplyPC", "SelfEmpl", "Openings", "MedWage", 
                                    "Entry_Degree", "Experience", "OJT"),
                       col_types = c("text", "text", "text", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "text", "text", "text"))


#The following file only has detailed occupational codes. Summary codes are not included in forcast or quintile data
#The source file requires manual selection of "duplicate" records in the table
OCCCODE1 <- read_excel(path = "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/soc_2010_to_2018_crosswalk.xlsx", 
                       skip = 8) #This file will need to be replaced once the forecast info is available by the 2018 SOC codes
OCCCODE2 <- rename(OCCCODE1, "OCCCODE2010" = "2010 SOC Code")  # rename "2010 SOC Code" to "OCCCODE2010" 
OCCCODE2 <- rename(OCCCODE2, "OCCCODE2018" = "2018 SOC Code")  # rename "2018 SOC Code" to "OCCCODE" 

OCCCODE3 <- filter(OCCCODE2, Duplicate == "No")  #delete manually selected "duplicate" records

#convert SOC 2010 codes to SOC 2018 codes and update occ forcast with updated OCC Codes
OCCFcst2 <- merge(OCCCODE3, OCC_Detail7, by = "OCCCODE2018", all = TRUE)
OCCFcst2 <- OCCFcst2 %>% filter(OCCTYPE %in% "Line item") #delete summary occupations, etc
OCCFcst3 <- OCCFcst2[ -c(2,3,4,5)]    # delete unused columns including 2010 SOC codes and  names
OCCFcst3 <- rename(OCCFcst3, "OCCCODE" = "OCCCODE2018")  # rename "OCCCODE2018" to "OCCCODE" in order to merge with OCCFcst
OCCFcst4 <- merge(x = OCCFcst3, y = DegreeCrosswalk, by="Entry_Degree", all = TRUE)  # add "Entry_Code" to dataframe
OCCFcst4$Entry_Code <- as.character(OCCFcst4$Entry_Code)  #Change Entry_Code from Integer to Character
OCCFcst4 <- OCCFcst4[ -c(17,19:22)]    # delete unused columns 

OCCFcst <- unique(OCCFcst4)                 # delete duplicates


# Load and clean quintile data
OCCQint1 <- read_excel("C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/all_data_M_2020.xlsx", sheet = "All May 2020 data",
                       col_names = TRUE,
#                      c("area", "area_title", "area_type", "prim_state", "naics", "naics_title", "i_group", "own_code", "occ_code", "occ_title", "o_group", 
#                                    "tot_emp", "emp_prse", "jobs_1000", "loc_quotient", "pct_total", "h_mean", "a_mean", "mean_prse", 
#                                    "h_pct10", "h_pct25", "h_pct50", "h_pct75", "h_pct90", "X10p", "X25p", "X50p", "X75p", "X90p", 
#                                    "annual", "hourly"),
                       col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text"))

#Filter file by area, naics, and o_group
OCCQint2 <- OCCQint1 %>% filter(AREA_TITLE %in% "U.S.")
OCCQint3 <- OCCQint2 %>% filter(NAICS %in% "000000")
OCCQint4 <- OCCQint3 %>% filter(O_GROUP %in% "detailed")
OCCQint5 <- OCCQint4[ -c(1,2,3,4,5,6,7,8,13,14,15,16,19)]  # removed unused columns
OCCQint <- rename(OCCQint5, OCCCODE = OCC_CODE)  # rename occ_code to OCCCODE in order to merge with OCCFcst

# Merge forecast data using 2018 codes and salary quintile data
OCC_Detail1 <- merge(x=OCCFcst, y=OCCQint, by="OCCCODE", all = TRUE)  #Merge OCC forecast & OCC salary data
OCC_Detail2 <- OCC_Detail1 %>% mutate_if(is.double, ~replace(., is.na(.), 0)) # change "na" to "0"
OCC_Detail3 <- OCC_Detail2 %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) # change "na" to "0"
OCC_Detail4 <- OCC_Detail3 %>% mutate_if(is.character, ~replace(., is.na(.), 0)) # change "na" to "0"

# Set comment field to show salary data has been imputed
OCC_Detail4$comment <- if_else(OCC_Detail4$A_PCT90 == "0" & OCC_Detail4$MedWage != "0", 
                              "Some missing data has been imputed", OCC_Detail4$comment <- "")

# When no quintile data is available, build quintile data from the Median salary in the Forecast data file
# factors are derived from the matrix below
OCC_Detail4$A_PCT90 <- if_else(OCC_Detail4$HOURLY == "0" & OCC_Detail4$H_PCT10 == "0" & OCC_Detail4$MedWage != "0", 
                              OCC_Detail4$MedWage * 2.528, OCC_Detail4$A_PCT90)             # difference between MEDIAN and 90TH percentile
OCC_Detail4$A_PCT75 <- if_else(OCC_Detail4$HOURLY == "0" & OCC_Detail4$H_PCT10 == "0" & OCC_Detail4$MedWage != "0", 
                              OCC_Detail4$MedWage * 1.607, OCC_Detail4$A_PCT75)               # difference between MEDIAN and 75TH percentile
OCC_Detail4$A_MEDIAN <- if_else(OCC_Detail4$HOURLY == "0" & OCC_Detail4$H_PCT10 == "0" & OCC_Detail4$MedWage != "0", 
                                OCC_Detail4$MedWage * 1, OCC_Detail4$A_MEDIAN)               # difference between MEDIAN and 50TH percentile
OCC_Detail4$A_PCT25 <- if_else(OCC_Detail4$HOURLY == "0" & OCC_Detail4$H_PCT10 == "0" & OCC_Detail4$MedWage != "0", 
                              OCC_Detail4$MedWage * .692, OCC_Detail4$A_PCT25)              # difference between MEDIAN and 25TH percentile
OCC_Detail4$A_PCT10 <- if_else(OCC_Detail4$HOURLY == "0" & OCC_Detail4$H_PCT10 == "0" & OCC_Detail4$MedWage != "0", 
                              OCC_Detail4$MedWage * .544, OCC_Detail4$A_PCT10)               # difference between MEDIAN and 10TH percentile
OCC_Detail5 <- OCC_Detail4

# Fill in missing salary data using average difference between qintiles for "national "U.S." dataset NAICS "000000"
# The source of this data is "all_data_M_2020.xlsx", sheet = "All May 2020 Data" US total OCC_CODE 00-0000
#a_10pct a_25pct a_50pct a_75pct a_90pct
# 22,810	29,020	41,950	67,410	106,050
#          1.272	 1.446	 1.607	 1.573    (This is the increase factor between each percentile)

OCC_Detail5$X10p <- if_else(OCC_Detail5$HOURLY == "TRUE", OCC_Detail5$H_PCT10 * 2080, if_else(OCC_Detail5$ANNUAL == "TRUE", OCC_Detail5$A_PCT10, OCC_Detail5$A_PCT10))
OCC_Detail5$X25p <- if_else(OCC_Detail5$HOURLY == "TRUE", OCC_Detail5$H_PCT25 * 2080, if_else(OCC_Detail5$A_PCT25 == 0, OCC_Detail5$X10p * 1.246,OCC_Detail5$A_PCT25))
OCC_Detail5$X50p <- if_else(OCC_Detail5$HOURLY == "TRUE", OCC_Detail5$H_MEDIAN * 2080, if_else(OCC_Detail5$A_MEDIAN == 0, OCC_Detail5$X25p * 1.278,OCC_Detail5$A_MEDIAN))
OCC_Detail5$X75p <- if_else(OCC_Detail5$HOURLY == "TRUE", OCC_Detail5$H_PCT75 * 2080, if_else(OCC_Detail5$A_PCT75 == 0, OCC_Detail5$X50p * 1.27,OCC_Detail5$A_PCT75))
OCC_Detail5$X90p <- if_else(OCC_Detail5$HOURLY == "TRUE", OCC_Detail5$H_PCT90 * 2080, if_else(OCC_Detail5$A_PCT90 == 0, OCC_Detail5$X75p * 1.208,OCC_Detail5$A_PCT90))
OCC_Detail5$X17p <- (OCC_Detail5$X10p + OCC_Detail5$X25p)/2
OCC_Detail5$X82p <- (OCC_Detail5$X75p + OCC_Detail5$X90p)/2

#set all salary data to integer datatype
OCC_Detail5$X10p = as.integer(as.numeric(OCC_Detail5$X10p)) #changes numeric column to integer
OCC_Detail5$X17p = as.integer(as.numeric(OCC_Detail5$X17p)) #changes numeric column to integer
OCC_Detail5$X25p = as.integer(as.numeric(OCC_Detail5$X25p)) #changes numeric column to integer
OCC_Detail5$X50p = as.integer(as.numeric(OCC_Detail5$X50p)) #changes numeric column to integer
OCC_Detail5$X75p = as.integer(as.numeric(OCC_Detail5$X75p)) #changes numeric column to integer
OCC_Detail5$X82p = as.integer(as.numeric(OCC_Detail5$X82p)) #changes numeric column to integer
OCC_Detail5$X90p = as.integer(as.numeric(OCC_Detail5$X90p)) #changes numeric column to integer
OCC_Detail6<- OCC_Detail5

#Create percentile rank of 17th percentile (starting) Wages
#OCC_Detail6 <- filter(OCC_Detail6, X17p > 0)
OCC_Detail6 <- OCC_Detail6[order(-OCC_Detail6$X17p),]
OCC_Detail6$PCX17p <- OCC_Detail6$X17p   #initialize new variable
NumRow <- nrow(OCC_Detail6)
for(i in 1:NumRow) {
  OCC_Detail6$PCX17p[i] <- perc.rank(OCC_Detail6$X17p, OCC_Detail6$X17p[i])
}

# Generate total factors for salary forecast over time: Low, Med, Hi refers to competency level. Late is at retirement age
# See file "C:\Users\lccha\OneDrive\NVS\NVS_EPIC\Source Data\Salary_Expense_Benchmark_Engine.xlsx" in tab "Wages by Age" cell AX10
OCC_Detail6$LowLate <- OCC_Detail6$X10p * 1.750698045
OCC_Detail6$MedLate <- OCC_Detail6$X17p * 1.750698045
OCC_Detail6$HiLate <- OCC_Detail6$X25p * 1.750698045

# Generate annualized factors for salary increases over 50 years
OCC_Detail6$LowOccF <- (OCC_Detail6$X75p/OCC_Detail6$LowLate)^(1/40)
OCC_Detail6$MedOccF <- (OCC_Detail6$X82p/OCC_Detail6$MedLate)^(1/40)
OCC_Detail6$HiOccF <- (OCC_Detail6$X90p/OCC_Detail6$HiLate)^(1/40)

# set column headings for the OCC_Detail file
OCC_Detail6 <- OCC_Detail6[,c ("OCCNAME", "OCCCODE", "Emply2019", "Emply2029", 
                              "EmplyChg", "EmplyPC", "SelfEmpl", "Openings", "MedWage", 
                              "Entry_Code", "Entry_Degree", "Experience", 
                              "OJT", "comment", "X10p", "X17p", "X25p", "X50p", "X75p", "X82p", "X90p",
                              "LowLate", "MedLate", "HiLate", "LowOccF", "MedOccF", "HiOccF")]
OCC_Detail7 <- unique(OCC_Detail6)
#Create percentile rank of Occupation Growth Rates
#OCC_Detail7 <- filter(OCC_Detail7, MedWage > 0) This drops occupations like anesthesiologist
OCC_Detail7 <- OCC_Detail7[order(-OCC_Detail7$EmplyPC),]
OCC_Detail7$PCEmplyPC <- OCC_Detail7$EmplyPC                #initialize new variable
NumRow <- nrow(OCC_Detail7)
for(i in 1:NumRow) {
  OCC_Detail7$PCEmplyPC[i] <- perc.rank(OCC_Detail7$EmplyPC, OCC_Detail7$EmplyPC[i])
}

#Create percentile rank of Self-Employment Rate

OCC_Detail7$PCSelfEmpl <- OCC_Detail7$SelfEmpl                #initialize new variable
OCC_Detail7 <- OCC_Detail7 %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) # change "na" to "0"
OCC_Detail7 <- OCC_Detail7[order(-OCC_Detail7$SelfEmpl),]              
NumRow <- nrow(OCC_Detail7)
for(i in 1:NumRow) {
  OCC_Detail7$PCSelfEmpl[i] <- perc.rank(OCC_Detail7$SelfEmpl, OCC_Detail7$SelfEmpl[i])
}

#Create percentile rank of Median Wages
OCC_Detail7 <- OCC_Detail7[order(-OCC_Detail7$X50p),]
OCC_Detail7$PCMedWage <- OCC_Detail7$X50p                #initialize new variable
NumRow <- nrow(OCC_Detail7)
for(i in 1:NumRow) {
  OCC_Detail7$PCMedWage[i] <- perc.rank(OCC_Detail7$X50p, OCC_Detail7$X50p[i])
}


# Create Occupation "No Match" record
OCCNull1 <- list("No Match", "No Match",0,0,0,0,0,0,0,"N/A","N/A","","","",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 
OCC_Detail <- rbind(OCC_Detail7, OCCNull1, drop = FALSE)

#Replace "NaN" elements with 0
OCC_Detail$LowOccF <- sub("NaN",0,OCC_Detail$LowOccF)  #replace "NaN" with "0"
OCC_Detail$LowOccF = as.numeric(as.character(OCC_Detail$LowOccF)) #make column a numeric
OCC_Detail$MedOccF <- sub("NaN",0,OCC_Detail$MedOccF) #replace "NaN" with "0"
OCC_Detail$MedOccF = as.numeric(as.character(OCC_Detail$MedOccF)) #make column a numeric
OCC_Detail$HiOccF <- sub("NaN",0,OCC_Detail$HiOccF) #replace "NaN" with "0"
OCC_Detail$HiOccF = as.numeric(as.character(OCC_Detail$HiOccF)) #make column a numeric


# save as RDS file
saveRDS(OCC_Detail, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Occupations.rds")

# Backbone.rds ******************* CREATE BACKBONE FILE ************************************ ----

#Read CIP OCC crosswalk file, keep character format for codes, includes "no match" information
OCC_CIP_CW1 <- read_excel(path = "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/CIP2020_SOC2018_Crosswalk.xlsx",
                         sheet = "CIP-SOC", col_names = TRUE)

#Insert a dash "-" into the OCCCODE 
#OCC_CIP_CW$pre <- substr(OCC_CIP_CW$OCCCODE_OLD, 1, 2)
#OCC_CIP_CW$post <- substr(OCC_CIP_CW$OCCCODE_OLD, 3, 6)
#OCC_CIP_CW$OCCCODE <- paste0(OCC_CIP_CW$pre, "-", OCC_CIP_CW$post)
#OCC_CIP_CW <- OCC_CIP_CW[ -c(1,2,4,5,6,7)]  # removed unused columns

#rename columns
#rename(iris, petal_length = Petal.Length)  # Rename column headings
OCC_CIP_CW1 <- rename(OCC_CIP_CW1, CIPCODE = CIP2020Code)
OCC_CIP_CW1 <- rename(OCC_CIP_CW1, OCCCODE = SOC2018Code)
OCC_CIP_CW1 <- rename(OCC_CIP_CW1, OCCNAME = SOC2018Title)
OCC_CIP_CW <- rename(OCC_CIP_CW1, CIPNAME = CIP2020Title)
# Merge CIP_Data file with the OCC <> CIP crosswalk file
Backbone1 <- merge(x = OCC_Detail, y = OCC_CIP_CW, by="OCCCODE", all = TRUE)

#Create MedWage by CIP-OCC Percentile Rank Combination ----
OCC_CIP_CW <- Backbone1[,c("CIPCODE", "OCCCODE", "MedWage")]
OCC_CIP_CW <- OCC_CIP_CW %>% drop_na()   #drop "NA" records
OCC_CIP_CW <- OCC_CIP_CW[order(-OCC_CIP_CW$MedWage),]
OCC_CIP_CW$PC_CIP_MedWage <- OCC_CIP_CW$MedWage                #initialize new variable
NumRow <- nrow(OCC_CIP_CW)
for(i in 1:NumRow) {
  OCC_CIP_CW$PC_CIP_MedWage[i] <- perc.rank(OCC_CIP_CW$MedWage, OCC_CIP_CW$MedWage[i])
}
# save as RDS file
saveRDS(OCC_CIP_CW, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/PC_OCC_CIP_Wage.rds")


# Continue to build backbone
Backbone1 <- merge(x = Backbone1, y =DegreeCrosswalk, by="Entry_Code", all = TRUE)

Backbone2 <- merge(x = CIP_Data, y = OCC_CIP_CW, by="CIPCODE", all = TRUE)  # Merge to Add Entry_Code field
Backbone3 <- merge(x = Backbone1, y = Backbone2, by=c("CIPCODE","OCCCODE","AWLEVEL"), all = TRUE)  # Merge to Add Entry_Code field

Backbone4 <- Backbone3[,c ("UNITID", "CIPCODE", "AWLEVEL", "CTOTALT", "OCCCODE", "Entry_Code")]  # select fields to keep


# Set AWLEVEL for Entry_Degree of "No Ed" or "HS" combined with CIPCODE of "No MATC" to AWLEVEL of "01" or "05".
#Backbone3$AWLEVEL <- if_else(Backbone3$AWLEVEL == "",
#                             if_else(Backbone3$Entry_Code == "01","01",
#                                     if_else(Backbone3$Entry_Code == "05","05",
#                                             Backbone3$AWLEVEL)),Backbone3$AWLEVEL) 

Backbone5 <- unique(Backbone4)
Backbone6 <- Backbone5[order(Backbone5$UNITID, Backbone5$CIPCODE, Backbone5$AWLEVEL, Backbone5$OCCCODE),
                      c(1,2,3,4,5)] #sort columns
#row.names(Backbone) <- 1:nrow(Backbone)   #renumber the rows sequentially
Backbone7 <- Backbone6 %>% mutate_if(is.character, ~replace(., is.na(.), "No Match")) # change "na" to "0"
Backbone8 <- Backbone7 %>% mutate_if(is.numeric, ~replace(., is.na(.),0)) # change "na" to "0"
Backbone9 <- tibble::rowid_to_column(Backbone8, "ID")

# Change CTOTALT column from character to number data type
#Backbone$CTOTALT = as.character(as.numeric(Backbone$CTOTALT)) #changes character column to numberic

# Save RDS file
saveRDS(Backbone, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Backbone.rds")

#Backbone$Index <- rownames(Backbone) #Create index using Rownames will return rownumbers present in Dataset,df=DataFrame name.
#Backbone$Index = as.numeric(as.character(Backbone$Index)) # change index from text to number

#Household Income by Structure Table ----
Lifestyle <- read_excel("C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Lifestyle.xlsx", skip = 0)
# Save RDS file
saveRDS(Lifestyle, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Lifestyle.rds")

# School offering analysis ----
Offerings1 <- Backbone
Offerings2 <- merge(x=Offerings1, y=SchoolData, by="UNITID", all = FALSE)
Offerings3 <- Offerings2[ -c(7:13, 16:48)]  # removed unused columns
CIP_ListNew <- rename(CIP_List, CIPCODE = Codevalue)
Offerings4 <- merge(x=Offerings3, y=CIP_ListNew, by="CIPCODE", all = FALSE)
Offerings5 <- merge(x=Offerings4, y=OCC_Detail, by="OCCCODE", all = FALSE)
Offerings5$EntryMatch <- if_else(Offerings5$Entry_Code == Offerings5$AWLEVEL,1,0)
TotMedWage <- aggregate(cbind(MedWage)~(CIPCODE), data=Offerings5, FUN = mean)
Offerings6 <- merge(x = Offerings5, y = TotMedWage, by="CIPCODE", all = FALSE)
TotDegree <- aggregate(cbind(CTOTALT)~(UNITID), data=CIP_Data, FUN = sum)
Offerings7 <- merge(x = Offerings6, y = TotDegree, by="UNITID", all = FALSE)
Offerings7$Tot_Wages <- Offerings7$MedWage.y * Offerings7$CTOTALT.x
Offerings8 <- filter(Offerings7, EntryMatch == 1 & Experience == "None")
Offerings8 <- Offerings8[ c(1,2,4,5,6,7,37,38,39,40,42)]
Offerings8 <- unique(Offerings8)
TotWage <- aggregate(cbind(Tot_Wages)~(UNITID), data=Offerings8, FUN = sum)
MatDegree <- aggregate(cbind(CTOTALT.x)~(UNITID), data=Offerings8, FUN = sum)


Offerings9 <- merge(x = Offerings7, y = TotWage, by="UNITID", all = FALSE)
Offerings9 <- merge(x = Offerings9, y = MatDegree, by="UNITID", all = FALSE)
Offerings9 <- rename(Offerings9, TotDegrees = CTOTALT.y, MatDegrees = CTOTALT.x.y)
Offerings9$PC_Match <- Offerings9$MatDegrees / Offerings9$TotDegrees
#Offerings10 <- merge(x = Offerings9, y = TotWage, by = "UNITID", all = TRUE)
Offerings9$PerCapita <- Offerings9$Tot_Wages.y / Offerings9$MatDegrees
Offerings9$OCCCODE <- as.character(Offerings9$OCCCODE)

Offerings10 <- SchoolData[c(1,10,11)]
Offerings10 <- merge (x = Offerings9, y = Offerings10, by = "UNITID", all = TRUE)

#Individual school information
write.csv(Offerings9, "c:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Offerings.csv")#write.csv(Offerings10, "c:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Offerings.csv")

#Individual CIP information across all schools
Offerings11 <- Offerings10[-c(1,6,39:44)]
Offerings11 <- unique(Offerings11)

write.csv(Offerings9, "c:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/AllOfferings.csv")

#   ********** Text Data Quality ************************ ----
# Check "Offerings.rds" file for the following:
# make sure "Middle School" teachers exists and have data
# make sure OCCCODE 29-12XX shows a variety of medical professions
# Check also OCCCODEs 11-2030, 25-2022, 29-1211

# AltTitle.rds ********************* Create Alternate Titles file *********************************** ----
AltTitle0 <- read_excel("C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/Alternate Titles.xlsx", skip = 1, col_names = c("OCCCODE", "OCCNAME", "AltName", "ShortName", "Source"))
# Trim OCCCODE to 7 Digits from 10 digits
AltTitle0$OCCCODE <- strtrim(AltTitle0$OCCCODE, 7)  
#Retain only OCCCODE and AltTitle columns
AltTitle1 <- AltTitle0[ -c(2,4:5)]

#save only 2 columns and rename columns
OCCCODE4 <- OCCCODE3[ -c(1,2,5)]
OCCCODE5 <- rename(OCCCODE4, OCCCODE = OCCCODE2018)  # Rename column headings
OCCCODE6 <- rename(OCCCODE5, AltName = soc_2010_to_2018_crosswalk)

#Map 2010 OCC codes to 2018 OCC codes
AltTitle2 <- bind_rows(AltTitle1, OCCCODE6)
AltTitle<- AltTitle2[order(AltTitle2$OCCCODE, AltTitle2$AltName),c(1,2)] #sort by Alt Name

# save the file as an RDS file
saveRDS(AltTitle, "C:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/alt_title_all.rds")


#do not forget this, otherwise you lock access database from editing.
close(channela) 
close(channelb)

# Diagnotic Tables
# Schools
SchoolDataDetail <- SchoolData %>% full_join(CIP_Data, by = "UNITID")
SchoolDataDetail <- SchoolDataDetail %>% full_join(OCC_CIP_CW, by = "CIPCODE")
SchoolDataDetail <- SchoolDataDetail[ -c(41,42,43,45,46)]
SchoolDataDetail <- unique(SchoolDataDetail)
write.csv(SchoolDataDetail, "c:/Users/lccha/OneDrive/NVS/NVS_EPIC/Source Data/Master Data/schooldata.csv")
