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
library(geosphere)

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

print(nlevels(hurricanes_2$name))

#'
#' Imputing Hurricane 2 Data and Combining the two datasets
#'

hurricanes_2_average_wind_radius <- hurricanes_2 %>%
  group_by(name, date, longitude, latitude, wind_speed) %>%
  mutate(avg_wind_radius = mean(wind_radius)) %>%
  ungroup() %>%
  distinct(name, date, latitude, longitude, wind_speed, .keep_all = TRUE)

combined_hurricanes_34 <- inner_join(hurricanes_1, hurricanes_2_average_wind_radius, by = c("name", "date", "longitude", "latitude")) %>%
  filter(wind_speed.y == 34) 
combined_hurricanes_50 <- inner_join(hurricanes_1, hurricanes_2_average_wind_radius, by = c("name", "date", "longitude", "latitude")) %>%
  filter(wind_speed.y == 50) 
combined_hurricanes_64 <- inner_join(hurricanes_1, hurricanes_2_average_wind_radius, by = c("name", "date", "longitude", "latitude")) %>%
  filter(wind_speed.y == 64) 

# Plotting Data to see what model should be used to impute data
ggplot(combined_hurricanes_34, aes(x = wind_speed.x, y = avg_wind_radius)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = 'blue') +
  labs(x = 'max speed', y = 'Radius ',title = paste("Wind speed/radius 34")) +
  theme_minimal()
ggplot(combined_hurricanes_50, aes(x = wind_speed.x, y = avg_wind_radius)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = 'blue') +
  labs(x = 'max speed', y = 'Radius ',title = paste("Wind speed/radius 50")) +
  theme_minimal()
ggplot(combined_hurricanes_64, aes(x = wind_speed.x, y = avg_wind_radius)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = 'blue') +
  labs(x = 'max speed', y = 'Radius ',title = paste("Wind speed/radius 64")) +
  theme_minimal()
# There is no function that fits this data well.
# This model shows that we should probably impute by taking the average for each max wind speed.
rm(combined_hurricanes_34, combined_hurricanes_50, combined_hurricanes_64)

# Combine using a full join
combined_hurricanes <- hurricanes_1  %>%
  full_join(hurricanes_2_average_wind_radius, by = c("name", "date", "longitude", "latitude"))

# Filter rows with NA in the "value" column and replicate them 3 times
combined_hurricanes_imputed <- combined_hurricanes %>%
  filter(is.na(wind_speed.y)) %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(wind_speed.y = rep(c(34, 50, 64), times = n() / 3))

# Filter out the original NA rows and bind the modified NA rows
combined_hurricanes_imputed <- combined_hurricanes %>%
  filter(!is.na(wind_speed.y)) %>%
  bind_rows(combined_hurricanes_imputed) %>%
  arrange(date)
  
# Filter out unwanted columns and rename columns
combined_hurricanes_imputed <- combined_hurricanes_imputed %>%
  rename(max_wind_speed = wind_speed.x,
         wind_speed_radius = wind_speed.y) %>%
  select(., -quadrant, -wind_radius, -year)

# Imputing data using average value for that max_wind_speed
averages <- combined_hurricanes_imputed %>%
  group_by(max_wind_speed, wind_speed_radius) %>%
  summarize(temp_mean = mean(avg_wind_radius, na.rm = TRUE)) %>%
  filter(!is.na(max_wind_speed))
averages <- averages %>%
  group_by(wind_speed_radius) %>%
  fill(temp_mean, .direction = 'down')

# Use these averages to fill missing values in the combined hurricanes data
combined_hurricanes_imputed <- combined_hurricanes_imputed %>%
  left_join(averages, by = c('max_wind_speed', 'wind_speed_radius')) %>%
  mutate(avg_wind_radius = if_else(is.na(avg_wind_radius), temp_mean, avg_wind_radius)) %>%
  select(-temp_mean)  # Remove the helper column

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
exposures_risky_hurricanes <- exposures %>%
  full_join(hurricanes_1, by = c('policy_year' = 'year')) %>%
  filter(abs(latitude.x - latitude.y) < 1 & abs(longitude.x - longitude.y) < 1) %>%
  distinct(location_id, policy_year, longitude.x, latitude.x, name, .keep_all = TRUE) %>%
  select(policy_year, location_id) %>%
  group_by(policy_year, location_id) %>%
  mutate(risky_hurricanes = n()) %>%
  ungroup() %>%
  distinct(location_id, policy_year, .keep_all = TRUE)

exposures <- exposures %>%
  full_join(exposures_risky_hurricanes, by = c('policy_year', 'location_id')) %>%
  mutate(risky_hurricanes = if_else(is.na(risky_hurricanes), 0, risky_hurricanes))

# Creating a total nearby hurricane column
exposures <- exposures %>%
  arrange(location_id, policy_year) %>%
  group_by(location_id) %>%
  mutate(
    cum_risky_hurricanes = cumsum(risky_hurricanes)
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


#'
#' Creating a column in exposures for approximate wind speeds each property
#'

chi_with_year <- combined_hurricanes_imputed %>%
  mutate(year = year(date))

exposures_wind_speeds <- exposures %>%
  full_join(chi_with_year, by = c('policy_year' = 'year')) %>%
  filter(avg_wind_radius != 0,
         longitude.y > -180) %>%
  mutate(avg_wind_radius = avg_wind_radius*1609.34*1.15078,
         distance = distVincentySphere(cbind(longitude.x, latitude.x), cbind(longitude.y, latitude.y))) %>%
  filter(distance < avg_wind_radius) %>%
  mutate(probable_wind_speed = ifelse(wind_speed_radius == 34, ifelse(max_wind_speed > 34, max_wind_speed-((1-((avg_wind_radius-distance)/avg_wind_radius)) * (max_wind_speed-34)), 34),
                                      ifelse(wind_speed_radius == 50, ifelse(max_wind_speed > 50, max_wind_speed-((1-((avg_wind_radius-distance)/avg_wind_radius)) * (max_wind_speed-50)), 50), 
                                             ifelse(max_wind_speed > 64, max_wind_speed-((1-((avg_wind_radius-distance)/avg_wind_radius)) * (max_wind_speed-64)), 64)))) %>%
  group_by(name, location_id, policy_year) %>%
  slice_max(order_by = probable_wind_speed, n = 1) %>%
  distinct(probable_wind_speed, name, location_id, policy_year, .keep_all = TRUE) %>%
  ungroup() %>%
  select(location_id, policy_year, probable_wind_speed)  %>%
  group_by(location_id, policy_year) %>%
  mutate(num_hurricanes_wind_speed = n(),
         avg_wind_speed = mean(probable_wind_speed)) %>%
  ungroup() %>%
  distinct(location_id, policy_year, .keep_all = TRUE) %>%
  select(-probable_wind_speed)

exposures <- exposures %>%
  full_join(exposures_wind_speeds, by = c('policy_year', 'location_id')) %>%
  mutate(num_hurricanes_wind_speed = if_else(is.na(num_hurricanes_wind_speed), 0, num_hurricanes_wind_speed),
         avg_wind_speed = if_else(is.na(avg_wind_speed), 0, avg_wind_speed))


# Writing files to a csv for storage
write_csv(exposures, 'exposures.csv')
write_csv(combined_hurricanes_imputed, 'hurricanes.csv')

#'
#' Making an r shiny ui for project
#'

# Define ui
ui <- fluidPage(
  titlePanel("CAS Data Visualization"),
  tabsetPanel(
    tabPanel(
      title = "Hurricane Map Display",
      sidebarLayout(
        sidebarPanel(
          h4('Choose Hurricanes to View'),
          selectInput(
            "names", 
            "Select Hurricanes:",
            choices = c('ALL Hurricanes', as.character(unique(combined_hurricanes_imputed$name))),
            multiple = TRUE
          ),
          sliderInput("hurricane_year", 
                      "Select Years For Hurricanes:",
                      min = min(combined_hurricanes_imputed$date), 
                      max = max(combined_hurricanes_imputed$date), 
                      value = c(min(combined_hurricanes_imputed$date), max(combined_hurricanes_imputed$date)),
                      step = 365, 
                      ticks = TRUE ),
          h4('Choose Property to View'),
          selectInput(
            "propertyName",
            "Select Property:",
            choices = sort(as.numeric(unique(exposures$location_id)))
          ),
          sliderInput("property_year", 
                      "Select Years For the Property:",
                      min = min(exposures$policy_year), 
                      max = max(exposures$policy_year), 
                      value = c(min(exposures$policy_year), max(exposures$policy_year)),
                      step = 1, 
                      ticks = TRUE,
                      sep = '')
        ),
        mainPanel(
          uiOutput('risk_level'),
          leafletOutput("map1"),
          plotOutput('losses_bar_chart'),
          plotOutput('risky_hurricanes_bar_chart'),
          plotOutput('windy_hurricanes_bar_chart'),
          plotOutput('wind_speeds_bar_chart')
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
          ),
          sliderInput("property_year", 
                      "Select Years For the Property:",
                      min = min(exposures$policy_year), 
                      max = max(exposures$policy_year), 
                      value = c(min(exposures$policy_year), max(exposures$policy_year)),
                      step = 1, 
                      ticks = TRUE,
                      sep = '')
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
  output$map1 <- renderLeaflet({
    if('ALL Hurricanes' %in% input$names) {
      selected_data <- combined_hurricanes_imputed
    } else {
      selected_data <- combined_hurricanes_imputed %>%
        filter(name %in% input$names)
    }
    selected_data <- selected_data %>%
      filter(date >= input$hurricane_year[1],
             date <= input$hurricane_year[2])
    
    exposures_id <- exposures %>%
      filter(policy_year == 2021, location_id == input$propertyName)
    
    leaflet(selected_data) %>%
      addTiles() %>% 
      addRectangles(
        data = selected_data %>% filter(wind_speed_radius == 34),
        lng1 = ~longitude - 1,  
        lat1 = ~latitude - 1,
        lng2 = ~longitude + 1,
        lat2 = ~latitude + 1,
        color = '',
        fillColor = "#43a3ff",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addCircles(
        data = selected_data %>% filter(wind_speed_radius == 34, avg_wind_radius != 0),
        lng = ~longitude,
        lat = ~latitude,
        radius = ~avg_wind_radius*1609.34*1.15078,  # Set the radius of the circle (in meters)
        color = "",   # Circle color
        fillColor = "#4cff00",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addCircles(
        data = selected_data %>% filter(wind_speed_radius == 50, avg_wind_radius != 0),
        lng = ~longitude, 
        lat = ~latitude, 
        radius = ~avg_wind_radius*1609.34*1.15078,  # Set the radius of the circle (in meters)
        color = "",   # Circle color
        fillColor = "#fffe21",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addCircles(
        data = selected_data %>% filter(wind_speed_radius == 64, avg_wind_radius != 0),
        lng = ~longitude, 
        lat = ~latitude, 
        radius = ~avg_wind_radius*1609.34*1.15078,  # Set the radius of the circle (in meters)
        color = "",   # Circle color
        fillColor = "#ff2121",  # Fill color of the circle
        fillOpacity = 0.3,  # Opacity of the circle fill
        opacity = 0.3
      ) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = exposures_id$longitude,
        lat = exposures_id$latitude,
        label = input$propertyName,
        radius = 6,
        color = 'blue',
        fillColor = 'blue',
        stroke = FALSE, fillOpacity = 1
      ) %>%
      addLegend(
        position = "topright",
        colors = c("#43a3ff", "#4cff00", "#fffe21", '#ff2121'),
        labels = c("Risky Hurricanes", "34-49 kts", "50-63 kts", "64+ kts"),
        title = "Wind Speeds"
      )
  })
  
  #Assess risk level
  output$risk_level <- renderUI({
    exposures_cum <- exposures %>%
      arrange(location_id, policy_year) %>%
      group_by(location_id) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2])) %>%
      mutate(
        cum_loss_ratio = cummean(losses_non_catastrophe)/cummean(premium) * 100,
        cum_risky_hurricanes = cumsum(risky_hurricanes),
        cum_windy_hurricanes = cumsum(num_hurricanes_wind_speed)
      ) %>%
      mutate(non_zero_cummean = if_else(avg_wind_speed != 0, avg_wind_speed, NA_real_)) %>%
      mutate(cum_wind_speeds = cumsum(replace_na(non_zero_cummean, 0)) / 
               cumsum(!is.na(non_zero_cummean))) %>%
      select(-non_zero_cummean) %>%
      mutate(cum_wind_speeds = replace_na(cum_wind_speeds, 0)) %>%
      ungroup() %>%
      filter(policy_year == input$property_year[2]) %>%
      mutate(cum_loss_ratio = (cum_loss_ratio - min(cum_loss_ratio)) / (max(cum_loss_ratio) - min(cum_loss_ratio)),
             cum_risky_hurricanes = (cum_risky_hurricanes - min(cum_risky_hurricanes)) / (max(cum_risky_hurricanes) - min(cum_risky_hurricanes)),
             cum_windy_hurricanes = (cum_windy_hurricanes - min(cum_windy_hurricanes)) / (max(cum_windy_hurricanes) - min(cum_windy_hurricanes)),
             cum_wind_speeds = (cum_wind_speeds - min(cum_wind_speeds)) / (max(cum_wind_speeds) - min(cum_wind_speeds))) %>%
      filter(location_id == input$propertyName)
    risk_number <- exposures_cum$cum_loss_ratio * 0.5 + exposures_cum$cum_wind_speeds * 0.25 + exposures_cum$cum_risky_hurricanes * 0.125 + exposures_cum$cum_windy_hurricanes * 0.125
    if(risk_number < 0.4) {
      tags$h3(style = paste("color:", '#1cce20', ";"), paste("Property ", input$propertyName, " is a Low Risk Property from  ", input$property_year[1], "-",  input$property_year[2]))
    } else if (risk_number < 0.6) {
      tags$h3(style = paste("color:", '#eea61b', ";"), paste("Property ", input$propertyName, " is a Medium Risk Property from  ", input$property_year[1], "-",  input$property_year[2]))
    } else {
      tags$h3(style = paste("color:", '#fb2525', ";"), paste("Property ", input$propertyName, " is a High Risk Property from  ", input$property_year[1], "-",  input$property_year[2]))
    }
  })
  
  output$losses_bar_chart <- renderPlot ({
    exposures_cum <- exposures %>%
      arrange(location_id, policy_year) %>%
      group_by(location_id) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2])) %>%
      mutate(
        cum_loss_ratio = cummean(losses_non_catastrophe)/cummean(premium) * 100
      ) %>%
      ungroup() %>%
      filter(policy_year == input$property_year[2])
    
    ggplot(exposures_cum, aes(x = reorder(location_id, cum_loss_ratio), y = cum_loss_ratio, fill = ifelse(location_id == input$propertyName, "blue", "red"))) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +  # Use the exact colors specified
      labs(x = "Location ID", y = "Cumulative Loss Ratio", title = 'Loss Ratio per Location') +
      theme_minimal()
  })
  
  output$risky_hurricanes_bar_chart <- renderPlot ({
    exposures_cum <- exposures %>%
      arrange(location_id, policy_year) %>%
      group_by(location_id) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2])) %>%
      mutate(
        cum_risky_hurricanes = cumsum(risky_hurricanes)
      ) %>%
      ungroup() %>%
      filter(policy_year == input$property_year[2])
    
    ggplot(exposures_cum, aes(x = reorder(location_id, cum_risky_hurricanes), y = cum_risky_hurricanes, fill = ifelse(location_id == input$propertyName, "blue", "red"))) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +  # Use the exact colors specified
      labs(x = "Location ID", y = "Cumulative Risky Hurricanes", title = 'Risky Hurricanes per Location') +
      theme_minimal()
  })
  
  output$windy_hurricanes_bar_chart <- renderPlot ({
    exposures_cum <- exposures %>%
      arrange(location_id, policy_year) %>%
      group_by(location_id) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2])) %>%
      mutate(
        cum_windy_hurricanes = cumsum(num_hurricanes_wind_speed)
      ) %>%
      ungroup() %>%
      filter(policy_year == input$property_year[2])
    
    ggplot(exposures_cum, aes(x = reorder(location_id, cum_windy_hurricanes), y = cum_windy_hurricanes, fill = ifelse(location_id == input$propertyName, "blue", "red"))) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +  # Use the exact colors specified
      labs(x = "Location ID", y = "Cumulative Windy Hurricanes", title = 'Windy Hurricanes per Location') +
      theme_minimal()
  })
  
  output$wind_speeds_bar_chart <- renderPlot ({
    exposures_cum <- exposures %>%
      arrange(location_id, policy_year) %>%
      group_by(location_id) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2])) %>%
      mutate(non_zero_cummean = if_else(avg_wind_speed != 0, avg_wind_speed, NA_real_)) %>%
      mutate(cum_wind_speeds = cumsum(replace_na(non_zero_cummean, 0)) / 
               cumsum(!is.na(non_zero_cummean))) %>%
      select(-non_zero_cummean) %>%
      mutate(cum_wind_speeds = replace_na(cum_wind_speeds, 0)) %>%
      ungroup() %>%
      filter(policy_year == input$property_year[2])
    
    ggplot(exposures_cum, aes(x = reorder(location_id, cum_wind_speeds), y = cum_wind_speeds, fill = ifelse(location_id == input$propertyName, "blue", "red"))) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +  # Use the exact colors specified
      labs(x = "Location ID", y = "Wind Speed Avg (Kts)", title = 'Average Wind Speeds per Location') +
      theme_minimal()
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
      filter(location_id == input$propertyName) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2]))
    ggplot(exposures_id, aes(x = policy_year, y = cum_loss_ratio)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Loss Percentage',title = paste("Change in Loss Ratio of Property Over Time")) +
      theme_minimal()
  })
  
  # Plotting the Total Insured Value Over Time
  output$plot2 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2]))
    ggplot(exposures_id, aes(x = policy_year, y = total_insured_value)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Total Insured Value',title = paste("Change in Total Insured Value Over Time")) +
      theme_minimal()
  })
  
  # Plotting the Premium per 100$ of total insured value over time
  output$plot3 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2]))
    ggplot(exposures_id, aes(x = policy_year, y = premium_per_100_total_insured_value)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Premium/100$ of Total Insured Value',title = paste("Change in Premium/100$ of Total Insured Value Over Time")) +
      theme_minimal()
  })
  
  # Plotting Change in loss cost over time
  output$plot4 <- renderPlot({
    exposures_id <- exposures %>%
      filter(location_id == input$propertyName) %>%
      filter(policy_year >= as.numeric(input$property_year[1]),
             policy_year <= as.numeric(input$property_year[2]))
    ggplot(exposures_id, aes(x = policy_year, y = cum_loss_cost_ratio)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = FALSE) +
      labs(x = 'Policy Year', y = 'Loss Cost ',title = paste("Change in Loss Cost Ratio Over Time")) +
      theme_minimal()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

  
  