#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(sf)
library(terra)
library(spData)
library(dplyr)

worldDF <- spData::world
worldDF <- worldDF %>%
  mutate(popLog=log(pop), .after=pop) %>%
  filter(!is.na(iso_a2))

## Code to read in data
niv <- read.csv("niv.csv")
niv$iso_a2[niv$Nationality=="Namibia"] <- "NA"

niv <- niv %>%
  full_join(worldDF,
            by=join_by(iso_a2)) %>%
  mutate(yearMon=if_else(Month %in% c("October","November","December"),
                         zoo::as.yearmon(paste(Month, (Year-1)),
                                         format="%B %Y"),
                         zoo::as.yearmon(paste(Month, Year),
                                         format="%B %Y")),
         calYear=lubridate::year(yearMon),
         Year=as.numeric(Year)) %>%
  st_as_sf()

fiscal_months <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
names(fiscal_months) <- 1:12
niv_grouped <- niv %>%
  group_by(across(setdiff(names(.), c("Visa.Class", "Issuances")))) %>%
  summarise(issuances=sum(Issuances, na.rm=TRUE), .groups="drop") %>%
  mutate(logIssuances=log(issuances),
         monthAbb=factor(lubridate::month(yearMon,
                                          label=TRUE, abbr=TRUE),
                         levels=fiscal_months, ordered=TRUE)
  )



library(shiny)
library(leaflet)

# UI
ui <- fluidPage(
  titlePanel(textOutput("mapTitle")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Fiscal Year:",
                  min = min(niv_grouped$Year, na.rm = TRUE),
                  max = max(niv_grouped$Year, na.rm = TRUE),
                  value = min(niv_grouped$Year, na.rm = TRUE),
                  sep = ""),
      sliderInput("month", "Select Month:",
                  min = 1,
                  max = 12,
                  value = 1,
                  step = 1,
                  sep = "",
                  ticks = FALSE,
                  animate = TRUE)
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$year, input$month)
    selected_month <- fiscal_months[as.character(input$month)]
    
    niv_grouped %>%
      filter(Year == input$year,
             monthAbb == selected_month,
             !is.na(geom))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  observe({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    pal <- colorNumeric("YlOrRd", domain = data$logIssuances, na.color = "#CCCCCC")
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(logIssuances),
        color = "white",
        weight = 0.5,
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        popup = ~paste0(
          "<strong>", Nationality, "</strong><br>",
          "Issuances: ", formatC(issuances, big.mark = ","), "<br>",
          "Month: ", as.character(monthAbb), "<br>",
          "Year: ", Year
        ),
        label = ~paste0(
          Nationality, ": ", formatC(issuances, big.mark = ",")
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px",
          style = list("font-weight" = "bold")
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) #%>%
    #addLegend("bottomright", 
    #          pal = pal, 
    #          values = data$issuances,
    #          title = "Log Visa Issuances",
    #          labFormat = labelFormat(big.mark = ","))
  })
  
  output$mapTitle <- renderText({
    selected_month <- fiscal_months[as.character(input$month)]
    paste("NIV Issuances –", selected_month, input$year)
  })
}


# Run the application 
shinyApp(ui, server)
