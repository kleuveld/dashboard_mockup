#Load packages

#project management
library(here)

#data management
library(dplyr)
library(sf)

#shiny
library(shiny)
library(bslib)
library(leaflet)
library(shinydashboard )
library(DT)
library(RColorBrewer)


#shapefile downloaded here: https://67-20-120-230.unifiedlayer.com/downloads/world_borders.php
# Read this shape file with the sf library.
world_sf <- read_sf(here("shinyapp/DATA/TM_WORLD_BORDERS_SIMPL-0.3.shp"))

# Clean the data object

countries <- c("CN","US","IN","RU","JP","DE","KR","ID","SA","CA","BR","ZA","MX","AUS",
                     "GB","VN","IT","FR","AR","NG")

world_sf <- 
  bind_rows(
    world_sf %>%
      select(NAME, ISO2) %>%
      filter(ISO2 %in% countries) %>%
      mutate(scenario = "BaU"),
    world_sf %>%
      select(NAME, ISO2) %>%
      filter(ISO2 %in% countries) %>%
      mutate(scenario = "Other")     
  ) %>%

  mutate(biodiversityloss = runif(nrow(.),100, 500)) %>%
  mutate(tooltip = paste0("Country: ", NAME, " (", ISO2, ")<br/>",
                          "Biodiversity loss: ", round(biodiversityloss,0))) 

# generate a default data frame that the app starts with  
default_data <- world_sf %>% 
        filter(scenario == "BaU")            

# Create a color palette with handmade bins.
mybins <- c(0, 100, 200, 300 , 400, 500)
mypalette <- colorBin(
  palette = "YlOrBr", domain = c(100,500),
  na.color = "transparent", bins = 5
)

ui <- page_sidebar(

  title = "Biodiversity Dashboard",

  sidebar = sidebar(
      selectInput("scenario", "Scenario", choices = c("BaU", "Other")),
      checkboxGroupInput(
        "selected_countries",
        "Select countries",
        choices = unique(world_sf$ISO2),
        selected = countries
      )
  ),
  leafletOutput("mymap"),
  valueBoxOutput("vbox"),
)



server <- function(input, output, session) {

  # render default map
  output$mymap <- renderLeaflet({
    leaflet(default_data) %>%
      addTiles() %>%
      setView(lat = 10, lng = 0, zoom = 2) %>%
      addPolygons(
        fillColor = ~ mypalette(biodiversityloss),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = default_data$tooltip %>% map(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette, values = ~biodiversityloss, opacity = 0.9,
        title = "Biodiversity loss", position = "bottomleft"
      )

  })

  # reactively filter data to be displayed on the map
  data <- 
    reactive({
        world_sf %>% 
          filter(ISO2 %in% input$selected_countries) %>%
          filter(scenario == input$scenario)
    })

  #dynamically update polygons
  observe({
    

    leafletProxy("mymap", data = data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ mypalette(biodiversityloss),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = data()$tooltip %>% map(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      )
  })


  # render a simple text box
  output$vbox <- renderValueBox({
    valueBox(
          "Total Biodiversity Loss", 
          round(sum(data()$biodiversityloss),0)
    )
        
  })



}

shinyApp(ui, server)