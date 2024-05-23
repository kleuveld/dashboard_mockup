#Load packages

#project management
library(here)

#data management
library(dplyr)
library(purrr)


#shiny
library(shiny)
library(bslib)
library(leaflet)
library(RColorBrewer)

library(sf)


countriesISO2 <- c("CN","US","IN","RU","JP","DE","KR","ID","SA","CA","BR","ZA","MX","AUS",
                     "GB","VN","IT","FR","AR","NG")

countriesISO3 <- c("ARG", "BRA", "CAN", "CHN", "FRA", "DEU", "IND", "ITA", "JPN", "KOR", "MEX", "NGA",                                                     
"RUS", "SAU", "ZAF", "GBR", "USA", "VNM", "IDN")    

link <- 'https://raw.githubusercontent.com/kleuveld/dashboard_mockup/main/data/custom.geo.json'
temp <- tempfile()
download.file(link,temp)


world_sf <- 
  st_read(temp) %>%
  select(name,adm0_iso)  %>%
  mutate(scenario = "BaU") %>%
  bind_rows({.} %>% mutate(scenario = "Other"))%>%
  mutate(biodiversityloss = runif(nrow(.),100, 500)) %>%
  mutate(tooltip = paste0("Country: ", name, " (", adm0_iso, ")<br/>",
                          "Biodiversity loss: ", round(biodiversityloss,0)))


default_data <- world_sf %>% 
        filter(scenario == "BaU")


# Create a color palette with handmade bins.
mybins <- c(0, 100, 200, 300 , 400, 500)
mypalette <- colorBin(
  palette = "YlOrBr", domain = c(100,500),
  na.color = "transparent", bins = 5
)


# UI ----
ui <- fluidPage(

  # App title ----
  titlePanel("Biodiversity Dashboard"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("scenario", "Scenario", choices = c("BaU", "Other")),
      checkboxGroupInput(
        "selected_countries",
        "Select countries",
        choices = countriesISO3,
        selected = countriesISO3
      ) 
    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 9,
      leafletOutput("mymap"),

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
          filter(adm0_iso %in% input$selected_countries) %>%
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

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)