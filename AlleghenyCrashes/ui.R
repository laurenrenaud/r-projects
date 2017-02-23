# First import our dependencies
library(shiny)
library(leaflet)
# using plotly to render in javascript
library(plotly)

# Define the UI for the application
# fluidPage makes page that adapts to size of page
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Allegheny County Crashes"),
  
  # Let's use the sidebar layout to layout our application. As the name
  # implies, the sidebar layout has a sidebar (default to the left side)
  # and then a main panel on the right.
  sidebarLayout(
    # Create a sidebar that gives the user the option to 
    # specify grant date ranges, grant duration in years, 
    # and the grant amount range.
    sidebarPanel(
      # Add any number of UI inputs in the sidebar panel
      
      
      # generally:
      # inputType("variable_name",
      #            "variable text",
      #           values or choices)
      
      # First, let's add a speed limit range option
      # this becomes input$speed_range on server.R
      # sliderInput creates slider options
      sliderInput("year_range",
                  "Year Range",
                  min = 2004,
                  max = 2015,
                  step = 1,  
                  sep = "",
                  # value sets default value
                  value = c(2010, 2015)),
      
      # Add slider for injury count
      sliderInput("injury_range",
                  "Injury Count Range",
                  min = 0,
                  max = 50,
                  step = 1,
                  # value sets default value
                  value = c(0, 10)),
      
      # Day of the week
      # select input gives drop down / text box selection
      selectInput("days_of_week", "Days of the week", 
                  choices = c(
                    "Sun",
                    "Mon",
                    "Tue",
                    "Wed",
                    "Thr",
                    "Fri",
                    "Sat"
                  ), selected = NULL, multiple = TRUE,
                  selectize = TRUE), 
      
      # Time of Day (Types)
      # select input gives drop down / text box selection
      selectInput("time_of_day_type", "Time of Day", 
                  choices = c(
                    "Morning",
                    "Midday",
                    "Evening",
                    "Night",
                    "Unknown"
                  ), selected = NULL, multiple = TRUE,
                  selectize = TRUE),
      
      # Months
      # select input gives drop down / text box selection
      selectInput("months", "Months", 
                  choices = c(
                    "Jan",
                    "Feb",
                    "Mar",
                    "Apr",
                    "May",
                    "Jun",
                    "Jul",
                    "Aug",
                    "Sep",
                    "Oct",
                    "Nov",
                    "Dec"
                  ), selected = NULL, multiple = TRUE,
                  selectize = TRUE),
      
      # Fatalities
      radioButtons("fatal", "Fatalities", 
                   choices = c(
                     "No Filter" = "all",
                     "At Least One Fatality" = TRUE,
                     "No Fatalities" = FALSE
                   )),
      
      # Bicycles
      radioButtons("bikes", "Bicyclists", 
                   choices = c(
                     "No Filter" = "all",
                     "At Least One Cyclist" = TRUE,
                     "No Bikes Involved" = FALSE
                   )),
      
      # Pedestrians
      radioButtons("peds", "Pedestrians", 
                   choices = c(
                     "No Filter" = "all",
                     "At Least One Pedestrian" = TRUE,
                     "No Pedestrians" = FALSE
                   )),
      
      # Speeding
      radioButtons("speeding_related", "Speeding related", 
                   choices = c(
                    "No Filter" = "all",
                    "Speeding Related" = TRUE,
                    "Not Speeding Related" = FALSE
                   )),
    
      # Impaired
      radioButtons("impaired_driver", "Driver Impaired by Drugs or Alcohol", 
                   choices = c(
                     "No Filter" = "all",
                     "At Lease One Impaired" = TRUE,
                     "No Drivers Impaired" = FALSE
                   ))
    ),
    
    # Now that the user inputs have been specified, let's add
    # output to the main panel of the UI.
    mainPanel(
      
      #total_crashes is refering to output$total_crashes from server.R side
      h2(textOutput("total_crashes")),
      
      # Because we have a few charts and tables, let's first organize
      # everything into tabs by using the tabsetPanel(...) function.
      tabsetPanel(
        
        # The first tab we'll create will let the user explore 
        # the number of collisions by speed limit
        tabPanel("By Year",
          plotlyOutput("crash_year_chart")
        ),
        
        tabPanel("By Injury Count",
                 plotlyOutput("crash_by_injury")
        ),
        
        tabPanel("By Collision Type",
                 plotlyOutput("crash_by_type")
        ),
        
        tabPanel("By Month",
                 plotlyOutput("crash_by_month")
        ),
        
        # tabPanel("By Weather",
        #          plotlyOutput("crash_by_weather")
        # ),
        
        # The second tab will show a map of where the crashes occured
        tabPanel("Map by Injury Count",
          leafletOutput("map", width = "100%", height = 650)
        )
        
      )
    )
  )
))