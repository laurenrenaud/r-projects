library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)

# preprocess the data ------
crashes <- readr::read_csv("crashes_2015.csv")

codeCollision <- function(code){
  #' Get the string collision representation for 
  #' the coded collision
  #' @param code Numeric collision code.
  #' @return String collision description.
  code.collision <- list(
    "0" = "Non collision",
    "1" = "Rear-end",
    "2" = "Head-on",
    "3" = "Rear-to-rear (Backing",
    "4" = "Angle",
    "5" = "Sideswipe (same dir.)",
    "6" = "Sideswipe (Opposite dir.)",
    "7" = "Hit fixed object",
    "8" = "Hit pedestrian",
    "9" = "Other or Unkown"
  )
  code = as.character(code)
  code.collision[[code]]
}

codeWeather <- function(code){
  #' Get the string weather representation for 
  #' the coded weather
  #' @param code Numeric weather code.
  #' @return String weather description.
  code.weather <- list(
    "1" = "No adverse conditions",
    "2" = "Rain",
    "3" = "Sleet (hail)",
    "4" = "Snow",
    "5" = "Fog",
    "6" = "Rain and fog",
    "7" = "Sleet and fog",
    "8" = "Other",
    "9" = "Unkown"
  )
  code = as.character(code)
  code.weather[[code]]
}

codeDayOfWeek <- function(code){
  #' Get the string day of week representation for 
  #' the coded day
  #' @param code Numeric day code.
  #' @return String day description.
  code.day <- list(
    "1" = "Sunday",
    "2" = "Monday",
    "3" = "Tuesday",
    "4" = "Wednesday",
    "5" = "Thursday",
    "6" = "Friday",
    "7" = "Saturday"
  )
  code = as.character(code)
  code.day[[code]]
}

# decode
crashes$COLLISION_TYPE <- as.factor(sapply(crashes$COLLISION_TYPE, codeCollision))
crashes$WEATHER <- as.factor(sapply(crashes$WEATHER, codeWeather))
crashes$DAY_OF_WEEK <- as.factor(sapply(crashes$DAY_OF_WEEK, codeDayOfWeek))

# int to bool
crashes$FATAL_OR_MAJ_INJ <- as.logical(crashes$FATAL_OR_MAJ_INJ)
crashes$CELL_PHONE <- as.logical(crashes$CELL_PHONE)
crashes$IMPAIRED_DRIVER <- as.logical(crashes$IMPAIRED_DRIVER)
crashes$FATIGUE_ASLEEP <- as.logical(crashes$FATIGUE_ASLEEP)
crashes$SPEEDING_RELATED <- as.logical(crashes$SPEEDING_RELATED)
crashes$BICYCLE <- as.logical(crashes$BICYCLE)
crashes$PEDESTRIAN <- as.logical(crashes$PEDESTRIAN)

# bringing in diff df
crashesBigger <- select(crashes, COLLISION_TYPE, FATAL_OR_MAJ_INJ,
                        DAY_OF_WEEK, CRASH_MONTH, CELL_PHONE, IMPAIRED_DRIVER,
                        FATIGUE_ASLEEP, SPEEDING_RELATED,
                        WEATHER, SPEED_LIMIT, DEC_LAT, DEC_LONG,
                        # new variables
                        BICYCLE, PEDESTRIAN, CRASH_YEAR)
write.csv(crashesBigger, "data/crashes_2015_bigger.csv")

# select the variables we care about
crashes <- select(crashes, COLLISION_TYPE, FATAL_OR_MAJ_INJ,
                  DAY_OF_WEEK, CRASH_MONTH, CELL_PHONE, IMPAIRED_DRIVER,
                  FATIGUE_ASLEEP, SPEEDING_RELATED,
                  WEATHER, SPEED_LIMIT, DEC_LAT, DEC_LONG)
write.csv(crashes, "data/crashes_2015_clean.csv")



# create some charts -----

crashes %>%
  dplyr::group_by(FATAL_OR_MAJ_INJ, SPEED_LIMIT) %>%
  dplyr::summarise(
    count_crashes = n()
  ) %>%
  ggplot(aes(x=SPEED_LIMIT, y=count_crashes, color=FATAL_OR_MAJ_INJ)) + 
  geom_point() + 
  geom_line()

# create a map --------

pal <- colorFactor(c("#0000FF", "#FF0000"), domain = c(F, T))

leaflet(data=crashes) %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng=~DEC_LONG,
    lat=~DEC_LAT,
    color=~pal(SPEEDING_RELATED),
    stroke = FALSE, fillOpacity = 0.5,
    radius=5
  )


addMarkers(~DEC_LONG, ~DEC_LAT, 
           popup = ~FATAL_OR_MAJ_INJ,
           clusterOptions = markerClusterOptions())


names(crashes2015)
crashes2015$PEDESTRIAN