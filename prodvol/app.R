#Load packages

# data management
library(readr)
library(readxl)
library(dplyr)
library(purrr)


# shiny
library(shiny)
library(bslib)

# geo and maps
library(leaflet)
library(sf)

# visualization
library(RColorBrewer)
library(plotly)



# get data from github
geo <-   
  st_read('https://raw.githubusercontent.com/kleuveld/dashboard_mockup/main/data/geo/custom.geo.json') %>%
    select(adm0_iso) 

production_volume <- read_csv('https://raw.githubusercontent.com/kleuveld/dashboard_mockup/main/data/Production_volume.csv')


production_volume_geo <-
  geo  %>%
  inner_join(production_volume, b= join_by(adm0_iso == REG))


# create default data that is loaded on start

scenarios <- unique(production_volume_geo$Scenario)
default_scenario <- "GDPEndoSSP2_noWaste"

ACTSs <- unique(production_volume_geo$ACTS)
default_ACTS <- "pdr"

years <- unique(production_volume_geo$Year)
default_year <- 2017

default_data <- 
  production_volume_geo %>%
  filter(Scenario == default_scenario) %>%
  filter(ACTS == default_ACTS) %>%
  filter(Year == default_year)

default_pallete <- colorBin(
  palette = "YlOrBr", domain = default_data$Value,
  na.color = "transparent", bins = 5
)


# UI ----
ui <- fluidPage(

  # App title ----
  titlePanel("Biodiversity Dashboard!"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("scenario", "Scenario", choices = scenarios, selected = default_scenario),
      selectInput("ACTS", "ACTS", choices = ACTSs, selected = default_ACTS),
      selectInput("year", "Year", choices = years, selected = default_year),

  ),

    # Main panel for displaying outputs ----
    mainPanel(width = 9,
      leafletOutput("mymap"),
      plotlyOutput("myplot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {


  # render default map
  output$mymap <- renderLeaflet({

    leaflet(default_data) %>%
            addTiles() %>%
            setView(lat = 10, lng = 0, zoom = 2) %>%
            addPolygons(
              fillColor = ~ default_pallete(Value),
              stroke = TRUE,
              fillOpacity = 0.9,
              color = "white",
              weight = 0.3,
            ) %>%
            addLegend(
              pal = default_pallete, values = ~Value, opacity = 0.9,
              title = "LEGEND TEXT", position = "bottomleft"
            )

  })

  # reactively filter data to be displayed on the map
  data <- 
    reactive({

      production_volume_geo %>%
        filter(ACTS == input$ACTS) %>%
        filter(Scenario == input$scenario) %>%
        filter(Year == input$year)
    })


  #dynamically update polygons
  observe({
    mypalette <- colorBin(
      palette = "YlOrBr", domain = data()$Value,
      na.color = "transparent", bins = 9
    )

    leafletProxy("mymap", data = data()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~mypalette(Value),
                  stroke = TRUE,
                  fillOpacity = 0.9,
                  color = "white",
                  weight = 0.3)
      
  })

  # Use a separate observer to recreate the legend as needed.
  observe({


    mypalette <- colorBin(
      palette = "YlOrBr", domain = data()$Value,
      na.color = "transparent", bins = 9
    )


    leafletProxy("mymap", data = data()) %>%
      clearControls() %>%
      addLegend(
        pal = mypalette, values = ~Value, opacity = 0.9,
        title = "LEGEND TEXT", position = "bottomleft"
      )   
    
  })


  output$myplot <- renderPlotly({
    plot <-
      data() %>%
      ggplot(aes(x = adm0_iso, y = Value)) + 
      geom_bar(stat = "identity") 

    ggplotly(plot)

  })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)




