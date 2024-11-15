# Setup

Clone the repository on your computer.

Creat a folder in the repository called **Data** which holds a **.csv** download of the provided excel files. 

Make sure you have the following R libraries installed:
* tidyverse
* ggplot2
* ggrepel
* ggcorrplot
* DT
* stringr
* dplyr
* leaflet
* sf
* shiny
* lubridate
* geosphere

Highlight all of the code and run it, or click the 'Run App' button if you are using RStudio. You can also run the code in the terminal by typing ```R -e "shiny::runApp('Source.R')"``` while in the working directory for the project
Once you run the app, you can visit the app on your local browser by copying the url the app gives you.

# Using the App

The app has two panels. These panels can be selected at the top right of the app. 

The first panel displays risk. It has a map for viewing a property with the surrounding hurricane data. The years and name for the hurricane and property can be selected using the side panel. The map changes depend on hurricane years and name and property ID. The other bar graphs below the map change only based on property years and ID.

The second panel contains infromation requested via management request 1. It has a sidebar where the user can select the property years and ID to view.
