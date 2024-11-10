# 'Libraries to be used in the document
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library(stringr)
library(dplyr)
library(leaflet)
library(sf)
library(shiny)
library(lubridate)

# Keep lat, long, date, wind speed, wind radius, year, iso_time
# Keep wind speeds in km


#
# Reading/Cleaning Hurricanes 1 data set
#
hurricanes_1_raw <- read_csv("Data/Historical Hurricane 1-Table 1.csv")
hurricanes_1 <- hurricanes_1_raw %>% 
  rename(name = ...10,
         date = ...11,
         hurricanes_in_year = ...7,
         latitude = ...13,
         longitude = ...14,
         wind_speed = ...15,
         ) %>%
  mutate(name = as.factor(name),
         date = as.Date(parse_date_time2(date, orders = "mdy HM", cutoff_2000 = 24)),
         hurricanes_in_year = as.numeric(hurricanes_in_year),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         wind_speed = as.numeric(wind_speed),
         #wind_speed = round(wind_speed * 1.852 ) conversion to km if needed
         ) %>%
  select(c(name, date, longitude, latitude, wind_speed, hurricanes_in_year))

hurricanes_1 <- hurricanes_1 %>%
  mutate(year = year(date))

hurricanes_1 <- hurricanes_1[-c(1, 2), ]

# There are no NA rows. 
na_rows <- hurricanes_1 %>%
  filter(if_any(everything(), is.na))

# There are no duplicate rows.
duplicate_rows <- hurricanes_1 %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  ungroup()

# The number of unique hurricane names
print(nlevels(hurricanes_1$name))

#hurricanes_1 <- hurricanes_1 %>%
#  group_by(name) %>%
#  mutate(wind_speed = median((wind_speed)))

mean_median <- hurricanes_1 %>%
  group_by(name) %>%
  mutate(mean_wind_speed = mean(wind_speed),
         median_wind_speed = median((wind_speed)))

#'
#' Reading/Cleaning Hurricanes 2 data set
#'
hurricanes_2_raw <- read_csv("Data/Historical Hurricane 2-Table 1.csv")
hurricanes_2 <- hurricanes_2_raw %>%
  rename(name = `http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt`,
         date = ...6,
         latitude = ...8,
         longitude = ...7,
         wind_speed = ...10,
         wind_radius = ...11,
         quadrant = ...9) %>%
  mutate(name = as.factor(name),
         date = as.Date(parse_date_time2(date, orders = "mdy HM", cutoff_2000 = 24)),
         quadrant = as.factor(quadrant),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         wind_speed = as.numeric(wind_speed),
         wind_radius = as.numeric(wind_radius)) %>%
  select(c(name, date, longitude, latitude, wind_speed, wind_radius, quadrant))

hurricanes_2 <- hurricanes_2[-c(1, 2), ]

# There are NA rows when the wind raduis should be 0. 
na_rows <- hurricanes_2 %>%
  filter(if_any(everything(), is.na))

# Replacing NA in wind radius with 0.
hurricanes_2 <- hurricanes_2 %>%
  mutate(wind_radius = ifelse(is.na(wind_radius), 0, wind_radius))

na_rows <- hurricanes_2 %>%
  filter(if_any(everything(), is.na))

# There are no duplicate rows.
duplicate_rows <- hurricanes_2 %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  ungroup()

#'
#' Reading/Cleaning Exposures data set
#'
exposures_raw = read_csv('Data/Exposures-Table 1.csv')
exposures <- exposures_raw %>%
  rename(longitude = ...3,
         latitude = ...2,
         total_insured_value = ...4,
         premium = ...5,
         losses_non_catastrophe = ...6,
         policy_year = ...7,
         location_id = `Company has had the same Location profile for all of history`) %>%
  mutate(total_insured_value = gsub(',', '', total_insured_value),
         premium = gsub(',', '', premium),
         losses_non_catastrophe = gsub(',', '', losses_non_catastrophe),
         location_id = as.factor(location_id),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         total_insured_value = as.numeric(total_insured_value),
         premium = as.numeric(premium),
         losses_non_catastrophe = as.numeric(losses_non_catastrophe),
         policy_year = as.numeric(policy_year)) %>%
  select(c(location_id, longitude, latitude, total_insured_value, premium, losses_non_catastrophe, policy_year))

exposures <- exposures[-c(1, 2, 3, 4), ]

# There are no NA rows. 
na_rows <- exposures %>%
  filter(if_any(everything(), is.na))

# There are no duplicate rows.
duplicate_rows <- exposures %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  ungroup()

# Adding column to exposures with the number of hurricanes within 1 degree of lat/lon of hurricane
exposures <- exposures %>%
  rowwise() %>%
  mutate(
    nearby_hurricanes =
      hurricanes_1 %>%
        filter(policy_year == .data$year) %>%
        filter(abs(latitude - .data$latitude) < 1 & abs(longitude - .data$longitude) < 1) %>%
        distinct(name, .keep_all = TRUE) %>%
        nrow()
    
  ) %>%
  ungroup()

# Creating a running average column for loss ratio
exposures <- exposures %>%
  arrange(location_id, policy_year) %>%
  group_by(location_id) %>%
  mutate(
    cum_loss_ratio = cummean(losses_non_catastrophe)/cummean(premium) * 100
  ) %>%
  ungroup()

# Creating a premium/100$ of total insured value column
exposures <- exposures %>%
  mutate(premium_per_100_total_insured_value = premium/(total_insured_value/100))

# Creating loss cost column
exposures <- exposures %>%
  arrange(location_id, policy_year) %>%
  group_by(location_id) %>%
  mutate(
    cum_loss_cost_ratio = cummean(losses_non_catastrophe)/total_insured_value * 100
  ) %>%
  ungroup()

write_csv(exposures, 'exposures.csv')

exposures_id <- exposures %>%
  filter(location_id == 1)

#'
#' Combining hurricane data
#'

combined_hurricanes <- inner_join(hurricanes_1, hurricanes_2, by = c("name", "date", "longitude", "latitude"))

combined_hurricanes <- combined_hurricanes %>% distinct()

summary(hurricanes_1)
summary(hurricanes_2)
summary(combined_hurricanes)

write_csv(combined_hurricanes, "combined_hurricanes.csv")

#'
#' Creating the interactive map for huuricane 1 data
#'

#'
#' Creating the interactive map for the hurricane 2 data
#'

hurricanes_2_average_wind_radius <- hurricanes_2 %>%
  group_by(name, date, longitude, latitude, wind_speed) %>%
  mutate(avg_wind_radius = mean(wind_radius)) %>%
  ungroup() %>%
  distinct(name, date, latitude, longitude, wind_speed, .keep_all = TRUE)

#'
#' Making a shiny ui for project
#'

# Define ui
ui <- fluidPage(
  titlePanel("CAS Data Visualization"),
  tabsetPanel(
    tabPanel(
      title = "Hurricane Map Display",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "names", 
            "Select Hurricanes:",
            choices = c('ALL Hurricanes', as.character(unique(hurricanes_2$name))),
            multiple = TRUE
          ),
          sliderInput("year", 
                      "Select Years:",
                      min = min(hurricanes_2$date), 
                      max = max(hurricanes_2$date), 
                      value = c(min(hurricanes_2$date), max(hurricanes_2$date))),
          h4('Enter proposed location: '),
          numericInput("longitude", "Longitude", value = 0),  # Default value for longitude
          numericInput("latitude", "Latitude", value = 0),
          actionButton("add_location", "Add Location"),
          actionButton("clear_locations", "Clear Locations")
        ),
        mainPanel(
          leafletOutput("map")
        )
      )
    ),
    
    tabPanel(
      title = "Property Portfolio",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "propertyName",
            "Select Property:",
            choices = sort(as.numeric(unique(exposures$location_id)))
          )
        ),
        mainPanel(
          plotOutput('plot1'),
          plotOutput('plot2'),
          plotOutput('plot3'),
          plotOutput('plot4')
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Create the leaflet map and render it when the button is pressed
  output$map <- renderLeaflet({
    if('ALL Hurricanes' %in% input$names) {
      selected_data <- hurricanes_2_average_wind_radius
    } else {
      selected_data <- hurricanes_2_average_wind_radius %>%
        filter(name %in% input$names)
    }
    selected_data <- selected_data %>%
      filter(date >= input$year[1],
             date <= input$year[2])
    
    leaflet(selected_data) %>%
      addTiles() %>% 
      addCircles(
        data = selected_data %>% filter(wind_speed == 34),
        lng = ~longitude,
        lat = ~latitude,
        radius = ~avg_wind_radius*1609.34,  # Set the radius of the circle (in meters)
        color = "",   # Circle color
        fillColor = "#4cff00",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addCircles(
        data = selected_data %>% filter(wind_speed == 50),
        lng = ~longitude, 
        lat = ~latitude, 
        radius = ~avg_wind_radius*1609.34,  # Set the radius of the circle (in meters)
        color = "",   # Circle color
        fillColor = "#fffe21",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addCircles(
        data = selected_data %>% filter(wind_speed == 64),
        lng = ~longitude, 
        lat = ~latitude, 
        radius = ~avg_wind_radius*1609.34,  # Set the radius of the circle (in meters)
        color = "",   # Circle color
        fillColor = "#ff2121",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addTiles()
      # %>%
      # addCircleMarkers(
      #   #clusterOptions = markerClusterOptions(),
      #   lng = ~longitude,
      #   lat = ~latitude,
      #   label = ~name,
      #   radius = 2,
      #   color = 'red',
      #   stroke = FALSE, fillOpacity = 1
      # ) 
  })
  
  # Adding Locations
  observeEvent(input$add_location, {
    leafletProxy("map") %>% 
      addCircleMarkers(lng = input$longitude, lat = input$latitude)  # Add location on map
  })
  
  # Removing Locations 
  observeEvent(input$clear_locations, {
    leafletProxy("map") %>% 
      clearMarkers()  # Clear previous locations
  })
  
  # Plotting Property Portfolio
  # Plotting Loss Ration Over Time
  output$plot1 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName)
    ggplot(exposures_id, aes(x = policy_year, y = cum_loss_ratio)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Loss Percentage',title = paste("Change in Loss Ratio of Property Over Time")) +
      theme_minimal()
  })
  
  # Plotting the Total Insured Value Over Time
  output$plot2 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName)
    ggplot(exposures_id, aes(x = policy_year, y = total_insured_value)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Total Insured Value',title = paste("Change in Total Insured Value Over Time")) +
      theme_minimal()
  })
  
  # Plotting the Premium per 100$ of total insured value over time
  output$plot3 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName)
    ggplot(exposures_id, aes(x = policy_year, y = premium_per_100_total_insured_value)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Premium/100$ of Total Insured Value',title = paste("Change in Premium/100$ of Total Insured Value Over Time")) +
      theme_minimal()
  })
  
  # Plotting Change in loss cost over time
  output$plot4 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName)
    ggplot(exposures_id, aes(x = policy_year, y = cum_loss_cost_ratio)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Loss Cost ',title = paste("Change in Loss Cost Ratio Over Time")) +
      theme_minimal()
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)

  
  