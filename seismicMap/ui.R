#### load required libraries
library(dplyr); library(tidyr); library(leaflet)
library(RColorBrewer); library(ggmap); library(shiny)

#### perform the client-side code
shinyUI(fluidPage(
    title = "Global Seismic Events",
    div(class = "outer",
      tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
      ),
      leafletMap("map", width="100%", height="100%",
                 options = list(
                     center = c(37.45, -93.85),
                     zoom = 3,
                     maxBounds = list(list(-90, -300), list(90, 360)))),
      absolutePanel(
        #### panel properties
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = "auto",
          right = "20", bottom = "auto",  width = 500,
          height = "auto", 

          #### Title, etc.
          h2("Global Seismic Events"),
          helpText("March 1, 2015 - April 15, 2015"),
          helpText("Doug Raffle"),
          hr(),

          #### set up checkboxes for types
          checkboxGroupInput("ui.type", label = h4("Filter by Event Type:"), 
                             choices = list(
                                 "Earthquake" = 2,
                                 "Explosion" = 3,
                                 "Landslide" = 4,
                                 "Quarry Blast" = 6),
                             selected = 1:6, inline=TRUE),
          hr(),

          #### Slider for magnitude range
          sliderInput("ui.mag", "Magnitude Range:", min = 1, max = 8,
                      value = c(1, 8), width = 500, step = 0.5),
          hr(),
          h4("Seismic Event Counts by Type"),
          plotOutput("barplot")
      ) ## close absolutePanel
    ) ## close div
  ) ## close page
) ## close UI      

