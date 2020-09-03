#install.packages("sf")
#install.packages("tidyverse")
#install.packages("raster")
#install.packages("leaflet.minicharts")
#install.packages('rmapshaper')
#install.packages("shinydashboard")

library(shiny)
library(sf)
library(sp)
library(raster)
library(leaflet)
library(dplyr)
library(rstudioapi)
library(leaflet.minicharts)
library(rmapshaper)
library(ggplot2)
library(shinydashboard)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# bee data files are the same but differently formatted depending on usage
bees = read.csv("bees.csv")
bees_stacked = read.csv("bees_stacked.csv")

# # getting spacial data of US states
# usa_shape = raster::getData("GADM", country="USA", level=1)
# usa_shape = readRDS("gadm36_USA_1_sp.rds")
# usa_shape = sf::st_as_sf(usa_shape)

#  # Reducing detail of polygons in order to load map faster
# usa_shape = rmapshaper::ms_simplify(usa_shape)
# st_write(usa_shape, "usa_shape", driver = 'ESRI Shapefile')

#the above code creates the shape file below which is loaded for efficiency
usa_shape = st_read("usa_shape/")

# function in order to show data for the year the user selects
select_year = function(year){
  year_data = bees[bees$Year == year,]
  colnames(year_data)[1] = "NAME_1"
  return (year_data)
}

# creating ui for shiny app
ui = fluidPage(
  h1("Correlation between decline of bee populations
              and use of pesticides", align = "center", style = "color:green; font-size: 24px" ),
  sidebarLayout(
    sidebarPanel(
      selectInput("choropleth", "What would you like to see?", 
                  c("Number of bee colonies" = "Colonies",
                    "Honey yield per colony" = "YieldPerCol")),
      checkboxInput("pesticides", "Show pesticide information in KG per year for California, Florida,
      Mississippi, North Dakota and Texas"),
      checkboxInput("cblind", "See colour-blind friendly version")),
    mainPanel(
      # adding space
      div(style = "font-size: 10px; padding: 0px 0px; margin-bottom:2em",
          h5("Bee populations are known to be decreasing worldwide and
      pesticides are thought to be a contribution factor to this phenomenon.
      In this app you can explore the change in the number of bee populations
      as well as honey yield production across states in the US 
      from 1991 to 2017. For some selected states you can also explore what kind
      of pesticides the state uses and how much of it.", style = "color:darkblue")),
      # scroll bar for years
      sliderInput("year", "Select year", min = 1991, max = 2017, value = 2005))),
  
  #choropleth map
  leafletOutput("us_map"),
  #leaflet mini charts
  leafletOutput("pie_chart"),
  #reducing space between map and reference
  div(style = "font-size: 10px; padding: 0px 0px; margin-top:-35em",
      h6("Source: Zmith,K. (2018) 'Honeybees and Neonic Pesticides'. Retrieved from: ", tags$a(href="https://www.kaggle.com/kevinzmith/honey-with-neonic-pesticide", "https://www.kaggle.com/kevinzmith/honey-with-neonic-pesticide"))
      
  )
)

server = function(input,output) {
  
  
  output$us_map = renderLeaflet({
    #merge the bee data for year user selects with the spacial data for US
    usa = merge(x=usa_shape,y=select_year(input$year),by="NAME_1",all.x=TRUE)
    choice = input$choropleth
    usa_choice = as.data.frame(usa)[,choice]
    #states for pesticide data
    usa_states = usa[(usa$NAME_1 == "California" |
                        usa$NAME_1 == "Florida" |
                        usa$NAME_1 == "Mississippi" |
                        usa$NAME_1 == "North Dakota" |
                        usa$NAME_1 == "Texas"), ]
    
    #add lon and lat to selected US states
    usa_states$lon = c(-119.417931, -81.760254, -90.000000, -100.437012, -100.000000)
    usa_states$lat = c(36.778259, 27.994402, 33.000000, 47.650589, 31.000000)
    
    # colours for red/green choropleth map
    if (input$cblind == FALSE){
      colour1 = "red"
      colour2 = "green"
      colour3 = "#FFFF33"
    }
    #colour-blind friendly version
    else {
      colour1 = "yellow"
      colour2 = "blue"
      colour3 = "orange"
    }
    
    pal = colorNumeric(palette = c(colour1,colour2), 
                       domain = usa_choice, 
                       n = 5, reverse = FALSE)
    colors = c("#000099", "#CC0066", "#009999", colour3, "#FF33FF")
    
    # Define legend heading
    if (input$choropleth == "YieldPerCol") {
      selection = "Honey yield per colony in pounds"
    }
    else {
      selection = "Number of bee colonies"
    }
    
    # map without pie charts
    mymap = leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(-91, 40, zoom = 4) %>%
      addPolygons(data = usa,
                  stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.6,
                  color = ~pal(get(input$choropleth)),
                  popup = paste("State: ", usa$NAME_1, "<br>",
                                "Value: ", usa_choice, "<br>")) %>%
      addLegend(position = "bottomright", pal = pal, values = usa_choice,
                title = selection,
                opacity = 1) 
    if (input$pesticides == TRUE) {
      #leaflet mini charts
      mymap = mymap %>% addMinicharts(
        usa_states$lon,
        usa_states$lat,
        type = "pie",
        chartdata = as.data.frame(usa_states)[,c("Clothianidin", "Imidacloprid", "Thiamethoxam", "Acetamiprid", "Thiacloprid")], 
        colorPalette = colors, 
        width = 60 * sqrt(usa_states$TotalNeonic) / sqrt(max(usa_states$TotalNeonic)), 
        transitionTime = 0, legendPosition = "bottomleft")
    }
    print(mymap)
  })
  
}

shinyApp(ui,server)


# References
# 
# GIS-Blog, Martin. (2015). Create a leaflet map using R. 
# Retrieved from https://www.gisblog.com/create-a-leaflet-map-using-r/#comments
#   
# Guillem, F. (2019). Introduction to leaflet.minicharts. 
# Retrieved from https://cran.rproject.org/web/packages/leaflet.minicharts/vignettes/introduction.html
# 
# Jazzurro (stackoverflow username). (2014). Answer to Question 
# How to create a choropleth on a leaflet Map R. 
# Retrieved from https://stackoverflow.com/questions/47571337/how-tocreate-a-choropleth-on-a-leaflet-map-r

