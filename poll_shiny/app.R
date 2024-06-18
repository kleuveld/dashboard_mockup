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
poll_link <- 'https://raw.githubusercontent.com/kleuveld/dashboard_mockup/main/data/pollination/pollination.csv'
poll_temp <- tempfile()
download.file(poll_link,poll_temp)

pollination <- read_csv(poll_temp)

geo_link <- 'https://raw.githubusercontent.com/kleuveld/dashboard_mockup/main/data/geo/custom.geo.json'
geo_temp <- tempfile()
download.file(geo_link,geo_temp)

geo <-   st_read(geo_temp)



# merge data together
pollination_geo <-
  geo  %>%
  select(adm0_iso) %>%
  right_join(pollination) 


# create default data that is loaded on start

scenarios <- unique(pollination_geo$Scenario)
default_scenario <- "Pollination_20perc"

outcomes <- unique(pollination_geo$outcome)
default_outcome <- "pct_change"

sectors <- unique(pollination_geo$Sector)
default_sector <- "All"


default_data <- 
  pollination_geo %>%
  filter(Scenario == default_scenario) %>%
  filter(outcome == default_outcome) %>%
  filter(Sector == default_sector)

default_pallete <- colorBin(
  palette = "YlOrBr", domain = default_data$value,
  na.color = "transparent", bins = 5
)


# UI ----
ui <- fluidPage(

  # App title ----
  titlePanel("Biodiversity Dashboard!"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("scenario", "Scenario", choices = scenarios, selected = default_scenario),
      selectInput("sector", "Sector", choices = sectors, selected = default_sector),
      selectInput("outcome", "Outcome", choices = outcomes, selected = default_outcome),

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
              fillColor = ~ default_pallete(value),
              stroke = TRUE,
              fillOpacity = 0.9,
              color = "white",
              weight = 0.3,
            ) %>%
            addLegend(
              pal = default_pallete, values = ~value, opacity = 0.9,
              title = "LEGEND TEXT", position = "bottomleft"
            )

  })

  # reactively filter data to be displayed on the map
  data <- 
    reactive({
      pollination_geo %>%
      filter(Scenario == input$scenario) %>%
      filter(outcome == input$outcome) %>%
      filter(Sector == input$sector)
    })


  #dynamically update polygons
  observe({
    mypalette <- colorBin(
      palette = "YlOrBr", domain = data()$value,
      na.color = "transparent", bins = 5
    )

    leafletProxy("mymap", data = data()) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~mypalette(value),
                  stroke = TRUE,
                  fillOpacity = 0.9,
                  color = "white",
                  weight = 0.3)
      
  })

  # Use a separate observer to recreate the legend as needed.
  observe({


    mypalette <- colorBin(
      palette = "YlOrBr", domain = data()$value,
      na.color = "transparent", bins = 5
    )


    leafletProxy("mymap", data = data()) %>%
      clearControls() %>%
      addLegend(
        pal = mypalette, values = ~value, opacity = 0.9,
        title = "LEGEND TEXT", position = "bottomleft"
      )   
    
  })


  output$myplot <- renderPlotly({
    plot <-
      data() %>%
      ggplot(aes(x = Region, y = value)) + 
      geom_bar(stat = "identity") 

    ggplotly(plot)

  })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)




