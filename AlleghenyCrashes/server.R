# Import our dependencies
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)

# Anything that happens here, before the shinyServer(...) function
# is loaded once when the application is started. This is a 
# good place to load our data

# load the data before starting up the application
crashes <- read.csv("data/crashes_cut.csv")

# re-order factor levels

crashes <- transform(crashes, CRASH_MONTH = factor(CRASH_MONTH, levels = c("Jan", "Feb", "Mar", "Apr",
                                                                           "May",
                                                                           "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                           "Nov", "Dec")))

crashes <- transform(crashes, COLLISION_TYPE = factor(COLLISION_TYPE, 
                                                      levels = c("Hit Fixed Object", "Rear-End", "Angle", 
                                                                 "Head-on", "Hit Pedestrian", 
                                                                 "Sideswipe Same Dir",
                                                                 "Sideswipe Oppo Dir",
                                                                 "Rear-to-Rear",
                                                                 "Non Collision", "Other/Unknown")))
 
# Define server logic
shinyServer(function(input, output) {
  # Everything in here is after the application is started
  # and is specific to each user session. That means
  # if one user subsets the data in a specific way, the 
  # change is only reflected in that one user's session,
  # not across all sessions.
  
  # setup our data --------
  
  # setting up our data here, everything below is going to use this data
  crashData <- reactive({
    #' Reactive function that subsets our data based on 
    #' user input and returns a data frame.
    #' note that 'crashes' is defined before shinyServer
    #' anything that happens before calling shinyServer is in the environment
    #' and available to be used
    crash_data <- dplyr::filter(crashes,
      # year
      # where year_range comes from input on ui.R side
      CRASH_YEAR >= input$year_range[1],
      CRASH_YEAR <= input$year_range[2],
      INJURY_COUNT >= input$injury_range[1],
      INJURY_COUNT <= input$injury_range[2]
    )
    
    # on these filters below, only filtering if a filter is selected
    # want to ignore filter if, for example, no days of week subset
    # because otherwise would display no data
    
    # check if we have specified any days of the week
    if(length(input$days_of_week) > 0){
      crash_data <- crash_data %>%
        dplyr::filter(
          DAY_OF_WEEK %in% input$days_of_week
        )
    }
    
    # check if we have specified any time of day
    if(length(input$time_of_day_type) > 0){
      crash_data <- crash_data %>%
        dplyr::filter(
          day.part %in% input$time_of_day_type
        )
    }
    
    # check if we have specified months
    if(length(input$months) > 0){
      crash_data <- crash_data %>%
        dplyr::filter(
          CRASH_MONTH %in% input$months
        )
    }
    
    # check if we want to subset by fatalities
    if(input$fatal != "all"){
      crash_data <- crash_data %>%
        dplyr::filter(
          FATAL == input$fatal
        )
    }
    
    # check if we want to subset by bicycles
    if(input$bikes != "all"){
      crash_data <- crash_data %>%
        dplyr::filter(
          BICYCLE == input$bikes
        )
    }
    
    # check if we want to subset by pedestrians
    if(input$peds != "all"){
      crash_data <- crash_data %>%
        dplyr::filter(
          PEDESTRIAN == input$peds
        )
    }
    # check if we want to subset speeding related
    if(input$speeding_related != "all"){
      crash_data <- crash_data %>%
        dplyr::filter(
          SPEEDING_RELATED == input$speeding_related
        )
    }
    
    # check if we want to subset by impaired driver
    if(input$impaired_driver != "all"){
      crash_data <- crash_data %>%
        dplyr::filter(
          IMPAIRED_DRIVER == input$impaired_driver
        )
    }
    
    return(crash_data)
  })
  
  # create some output ----------
  
  # any time you want to be sending text over to the user interface
  # going to use renderText function
  output$total_crashes <- renderText({
    # Count the total number of crashes we are exploring
    # and return some text in the form "Exploring x crashes"
    # note that we're callingthe crashData() function here!
    crash.count <- format(nrow(crashData()), big.mark=",")
    paste("Exploring", crash.count, "crashes", sep=" ")
  })
  
  output$crash_year_chart <- renderPlotly({
    # Generate a bar chart with speed limits on the 
    # x axis and a count of the number of crashes on 
    # the y axis with ggplot, then render it using the
    # Plotly javascript library.
    crashData() %>%
      dplyr::group_by(CRASH_YEAR) %>%
      dplyr::summarise(
        count_crashes = n()
      ) %>%
      ggplot(aes(x=CRASH_YEAR, y=count_crashes)) + 
      geom_bar(stat="identity", fill="aquamarine4", color="white") + 
      labs(
        title="Number of crashes by year",
        x="Year",
        y="Number of crashes"
      )
    # call the ggplotly function from the plotly package 
    # to return our ggplot as a javasript chart rather 
    # than as an image. You can use ggplot images if you 
    # prefer, but Plotly gives us nice rollover effects.
    plotly::ggplotly()
  })
  
  output$crash_by_injury <- renderPlotly({
    crashData() %>%
      dplyr::group_by(INJURY_COUNT) %>%
      dplyr::summarise(
        count_crashes = n()
      ) %>%
      ggplot(aes(x=INJURY_COUNT, y=count_crashes)) + 
      geom_bar(stat="identity", fill="hotpink4", color="white") + 
      labs(
        title="Crashes by Injury Count",
        x="Number of Injuries",
        y="Number of crashes"
      )
    
    plotly::ggplotly()
  })
  
  output$crash_by_type <- renderPlotly({
    crashData() %>%
      dplyr::group_by(COLLISION_TYPE) %>%
      dplyr::summarise(
        count_crashes = n()
      ) %>%
      ggplot(aes(x=COLLISION_TYPE, y=count_crashes)) + 
      geom_bar(stat="identity", fill="cadetblue4", color="white") + 
      theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1)) +
      labs(
        title="Crashes by Collision Type",
        x="Collision Types",
        y="Number of crashes"
      )
    
    plotly::ggplotly()
  })
  
  output$crash_by_month <- renderPlotly({
    crashData() %>%
      dplyr::group_by(CRASH_MONTH) %>%
      dplyr::summarise(
        count_crashes = n()
      ) %>%
      ggplot(aes(x=CRASH_MONTH, y=count_crashes)) + 
      geom_bar(stat="identity", fill="mediumpurple4", color="white") + 
      labs(
        title="Crashes by Month",
        x="Months",
        y="Number of crashes"
      )
    
    plotly::ggplotly()
  })
  
  
  output$map <- renderLeaflet({
    # Let's add a map to our application that shows the 
    # user where the crashes took place. We will build the 
    # map with the Leaflet package, allowing the user
    # to interactively scroll around the map.
    
    leaflet(data=crashData()) %>%
      # use the CartoDB map for more subdued map tile colors
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.9959, lat = 40.4406, zoom = 10) %>%
      addCircleMarkers(~DEC_LONG, ~DEC_LAT, 
                       popup = paste("Crash Year:", crashData()$CRASH_YEAR,
                                     "<br> Injury Count:", crashData()$INJURY_COUNT),
                 # the marker clusters gives us a nice visual 
                 # effect where when we zoom out points cluster
                 # together, and as we zoom in they are pulled apart
                 # showing the individual markers.
                 #clusterOptions = markerClusterOptions()
                 radius = ~(INJURY_COUNT+2)*2,
                 stroke = TRUE,
                 color = "#323232",
                 weight = 3,
                 fillColor = "#7fcdbb",
                 fillOpacity = 0.7
                 ) 
    # %>%
    #   addCircleMarkers(
    #     radius = ~INJURY_COUNT,
    #     color = "#756bb1",
    #     stroke = TRUE, 
    #     fillOpacity = 0.5
    #   )
  })
  
})