library(RPostgreSQL)
library(rmarkdown)
library(futile.logger)
library(dplyr)
library(jsonlite)
library(lubridate)

# # get a driver
# drv <- dbDriver("PostgreSQL")
# 
# # database parameters
# dbname = "ce_db"
# host = "---"
# port = ---
# user = "---"
# password = "---"
# 
# # creates a connection to the postgres database
# # note that "conn" will be used later in each connection to the database
# conn <- dbConnect(drv, dbname = dbname,
#                   host = host, port = port,
#                   user = user, password = password)

# get tables from the database
#property <- dbGetQuery(conn, "SELECT * FROM tblProperty")

# pulling in CSVs for now instead of database
codeDetail <- read.csv("data_share/tblCodeEnforcement-Detail.csv", header=TRUE, sep=",")
codeEnforcement <- read.csv("data_share/tblCodeEnforcement.csv", header=TRUE, sep=",")
property <- read.csv("data_share/tblProperty.csv", header=TRUE, sep=",")
codeDescription <- read.csv("data_share/tblIssueCode.csv", header=TRUE, sep=",", fileEncoding="LATIN1")
muni.codes <- fromJSON("data_share/municode.json")
# commented out for privacy
#convert <- read.csv("data_share/parcels_xy_only.csv", header=TRUE)

# join xy coords to violations report
# commented out for privacy
#property <- left_join(property, convert, by=c("ParID" = "parid"))


# dates as dates, logicals as logicals
codeDetail$DateResolved <- as.Date(codeDetail$DateResolved, "%m/%d/%Y")
codeDetail$Resolved. <- as.logical(codeDetail$Resolved.)
codeEnforcement$EventDate <- as.Date(codeEnforcement$EventDate, "%m/%d/%Y")
codeEnforcement$CourtDate <- as.Date(codeEnforcement$CourtDate, "%m/%d/%Y")
codeEnforcement$ComplianceDate <- as.Date(codeEnforcement$ComplianceDate, "%m/%d/%Y")
codeEnforcement$CitationFiledDate <- as.Date(codeEnforcement$CitationFiledDate, "%m/%d/%Y")
property$StatusDate <- as.Date(property$StatusDate, "%m/%d/%Y")
codeDescription$municode_1 <- as.factor(codeDescription$municode_1)
codeDescription$ViolationID <- as.factor(codeDescription$ViolationID)
codeDetail$ViolationID <- as.factor(codeDetail$ViolationID)
codeDetail <- left_join(codeDescription, codeDetail, by="ViolationID")
#summary(property$PropertyType) ## need to re-label these for consistency if we use them

# join municipal names to codes
property$municode_1 <- as.factor(property$municode_1)
property <- left_join(property, muni.codes, by=c("municode_1" = "municode"))

# generate necessary time variables
##### GO BACK MORE MONTHS FOR NOW FOR TESTING #####
reportMonthDate <- floor_date(Sys.Date(), "month") - months(7)
yearMonthChar <- format(reportMonthDate, "%Y-%m")
monthName <- month.abb[month(reportMonthDate)]
yearChar <- as.character(year(reportMonthDate))

## Data Cleaning -----------

# Join dataframes
muni.data <- property %>%
  # join with code enforcement df
  left_join(codeEnforcement, by = "PropertyID") %>%
  # select only needed columns
  select(PropertyID, ParID, UseDescription,
         #Address, ApartmentNo, 
         Zoning, OpenIssueType, PropertyType, ResidenceType, VacantLand.,
         PropertyStatus, StatusDate, EventID, EventDate, ComplianceDate, muniname
         #long, lat
         ) %>%
  # keep only properies with violations
  filter(!is.na(EventID)) %>%
  # join to code enforcement details
  left_join(codeDetail, by = "EventID") %>%
  group_by(EventID) %>%
  mutate(
    # Purpose of this variable is to capture if overall the Event is open or closed
    # It considers current status of all separate EventDetails within the Event
    # Only considered closed if all of most recent interacts are closed
    OverallStatus = ifelse(all(Resolved.[EventDate==max(EventDate)]), "Closed",
                           ifelse(any(!Resolved.[EventDate==max(EventDate)]), "Open", NA))
  )

## These separate out open, closed, and NA cases into separate dataframes
## uses same logic as above

### NOTE
### The above variable, the one inside the dataframe, ends up with one NA EventID
### for both Open and Closed
### so the Open and Closed counts above are one higher than the length of unique EventIDs
### that you get from the dataframes belwo

# all closed cases
closed <- muni.data %>%
  group_by(EventID) %>%
  filter(all(Resolved.[EventDate==max(EventDate)]))

# open cases
open <- muni.data %>%
  group_by(EventID) %>%
  filter(any(!Resolved.[EventDate==max(EventDate)]))

# NA cases
na.cases <- muni.data %>%
  group_by(EventID) %>%
  filter(any(is.na(Resolved.[EventDate==max(EventDate)])))

# this month's cases
nextMonth <- reportMonthDate + months(1)
monthCases <- muni.data %>%
  group_by(EventID) %>%
  filter(min(EventDate) > reportMonthDate, min(EventDate) < nextMonth)

monthOpen <- monthCases %>%
  group_by(EventID) %>%
  filter(any(!Resolved.[EventDate==max(EventDate)]))

monthClosed <- monthCases %>%
  group_by(EventID) %>%
  filter(all(Resolved.[EventDate==max(EventDate)]))

muni.year <- muni.data %>%
  # first group by EventID and filter by the first (min) EventDate for a given 
  # EventID is within the past year
  group_by(EventID) %>%
  filter(min(EventDate) > (reportMonthDate - months(12)))

#### DATA INTERPRETATION PROBLEM
## It appears that EventDate is the date a case was opened
## And ResolvedDate is when it's closed
## But there's missingness in the ResolvedDate
## And also does not appear to have a date for
## follow visits
## dataframe below is for figuring out differnce between
## ComplianceDate, DateResolved, and how they connect to EventDate
testingDates <- muni.data %>%
  select(EventID, EventDate, ComplianceDate, DateResolved, OverallStatus)

# # close the connection
# RPostgreSQL::postgresqlCloseConnection(conn)
# 
# # setup logging
# flog.appender(appender.file("logs/make_reports.log"), name="log")
# flog.threshold(DEBUG, name="log")


generateReport <- function(month, municipality, property, codeEnforcement, codeDetail){
  #' Generates a family report and stores it as the family id with .docx
  #' appended in the /output directory.
  #' @param month          string of month plus year of report
  #' @param municipality     municipal name
  #' @param property         Property dataframe
  #' @param codeEnforcement   codeEnforcement dataframe
  #' @param codeDetail         codeDetail dataframe
  require(lubridate)
  require(rmarkdown)
  require(dplyr)
  
  # only make the report if it doesn't already exist
  if(!file.exists(paste("output/", municipality, month, ".pdf", sep=""))){
    rmarkdown::render("violations_template_share.rmd",
                      #output_format = pdf_document,
                      output_file = paste(month, "_", municipality, ".pdf", sep=""),
                      output_dir = paste("ReportOutput_share/", municipality, sep=""),
                      runtime = "static",
                      envir = new.env(),
                      intermediates_dir = "temp",
                      params=list(
                        municipality = municipality,
                        property = property,
                        codeEnforcement = codeEnforcement,
                        codeDetail = codeDetail,
                        month = month
                        )
      )
  }
}


makeAllReports <- function(month, muniname, property, codeEnforcement, codeDetail){
  #' Calls generateReport for each family that is eligable to recieve a report.
  #' @param month          string of month plus year of report
  #' @param municipality    municipal name
  #' @param property         Property dataframe
  #' @param codeEnforcement   codeEnforcement dataframe
  #' @param codeDetail         codeDetail dataframe
  
  for(municipality in muni.codes$muniname){
    generateReport(month, municipality, property, codeEnforcement, codeDetail)
  }
}

# make all reports
makeAllReports(yearMonthChar, muniname, property, codeEnforcement, codeDetail)
