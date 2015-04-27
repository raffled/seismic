#### load required libraries
library(dplyr); library(tidyr); library(leaflet)
library(RColorBrewer); library(ggmap); library(shiny)
library(lubridate)

#### set up initial data
seismic <- read.csv("data/seismic_data.csv") %>%
    select(-magType, -nst, -net, -updated) %>%
        mutate(place = as.character(place),
               time = as.Date(time)) %>%
         filter(time >= as.Date("2015-03-01"))

#### set up the server-side work
shinyServer(function(input, output, session){
  #### create the map
  l.map <- createLeafletMap(session, "map")
  
  #### set up the map session and add our points
  session$onFlushed(once = TRUE, function(){    
    paint.obs <- observe({
       #### get map boundries
       bounds <- input$map_bounds
       output$foo2 <- renderPrint({bounds})
       #### now subset per user specs and map bounds
       seismic.sub <- seismic %>%
          filter(as.integer(type) %in% input$ui.type,
                 mag >= input$ui.mag[1],
                 mag <= input$ui.mag[2],
                 latitude > bounds$south,
                 latitude < bounds$north,
                 longitude > bounds$west,
                 longitude < bounds$east)
       seismic.sub <- seismic.sub %>% mutate(type = factor(type))
       color.vec <- brewer.pal(length(levels(seismic.sub$type)), "Dark2")
       seismic.sub <- seismic.sub %>% mutate(color = color.vec[type])

       #### generate a barplot of the types
       output$barplot <- renderPlot({
           if(nrow(seismic.sub) > 0){
               ps <- element_text(size = 10)
               ps2 <- element_text(size = 15)
               ggplot(seismic.sub, aes(x = type, fill = type)) +
                   geom_bar(width = 0.5) + coord_flip() +
                       scale_fill_manual(values = color.vec) +
                           theme(legend.text = ps,
                                 legend.title = ps,
                                 axis.text.x = ps,
                                 axis.title.x = ps2,
                                 axis.text.y = element_blank(),
                                 axis.title.y = element_blank(),
                                 legend.position = "top")
           }
       })
       #### Clear existing circles before drawing
       l.map$clearShapes()
       #### draw the map.
       sub.size <- 500
       sapply(seq(1, nrow(seismic.sub), sub.size), function(from){
                     to <- min(nrow(seismic.sub), from + sub.size)
                     seismic.sub2 <- seismic.sub[from:to,]
                     try(l.map$addCircle(  
                         seismic.sub2$latitude,
                         seismic.sub2$longitude,
                         radius = seismic.sub2$mag*10000,
                         options = list(stroke=TRUE, fill=TRUE, fillOpacity=0.4),
                         eachOptions = list(color = seismic.sub2$color)
                      ))
       })
    })
    session$onSessionEnded(paint.obs$suspend)
  }) ## end point draw
  #### show an event description popup
  show.event.desc <- function(lat, lng){
      ## grab seismic event
      lat <- round(lat, 3); lng <- round(lng, 3)
      this.event <- with(seismic,
                    seismic[round(latitude,3) == lat & round(longitude,3) == lng,])
      desc <- sapply(1:nrow(this.event), function(i){
                as.character(tagList(
                    tags$h6("Event Description"),
                    sprintf("%1.2f magnitude %s", this.event[i,5],
                            this.event[i,11]),
                    tags$br(),
                    sprintf("\n%s", this.event[i,10]),
                    tags$br(),
                    sprintf("on %s, %s %s, %s.", weekdays(this.event[i,1]),
                            months(this.event[i,1]),
                            format(this.event[i,1], "%d"),
                            format(this.event[1,1], "%Y"))
                ))
      })
      desc <- paste(desc, collapse = "")
      l.map$showPopup(lat=lat, lng = lng, desc)
  }
  #### set up click event
  click.obs <- observe({
      l.map$clearPopups()
      event <- input$map_shape_click
      if(is.null(event)) return()
      isolate(show.event.desc(event$lat, event$lng))
  })
  session$onSessionEnded(click.obs$suspend)
})
