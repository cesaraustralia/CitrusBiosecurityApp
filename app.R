library(shiny)
library(shinythemes)
library(shinycssloaders)
library(leaflet)
library(mapview)

# load the switch code
source("Rsource/SwitchButton.R")

# render mapview doesn't work; this function works
myRenderMapview <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) 
    expr = substitute(mapview:::mapview2leaflet(expr))
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env, 
                                 quoted = TRUE)
}

# read layer
aus_SA2 <- setNames(sf::read_sf("shiny-resources/aus_SA2.gpkg"), c("SA2_CODE21", "SA2_NAME21", "Region", "State", "Incursion risk", "Climatic suitability", "Establishment likelihood", "Tourist pathway", "Returning resident pathway", "Visiting friends & family pathway", "Sea cargo pathway", "Natural dispersal pathway", "Budwood pathway", "geom"))

# roads <- sf::read_sf("shiny-resources/roads.gpkg")

# layers
all_layers <- c("Incursion risk", "Climatic suitability", "Establishment likelihood", "Tourist pathway", "Returning resident pathway", "Visiting friends & family pathway", "Sea cargo pathway", "Natural dispersal pathway", "Budwood pathway")

data_full <-
  aus_SA2 %>%
  tibble::as_tibble() %>%
  dplyr::select(-geom)

org_counts <-
  data_full %>%
  dplyr::count(State) %>%
  dplyr::pull(n)

names(org_counts) <- sort(unique(data_full$State))

ui <- shinyUI(
  navbarPage("Citrus Biosecurity",
             selected = "Risk maps",
             theme = shinytheme("flatly"),
             
             
             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Risk maps",
               
               splitLayout(
                 selectizeInput(inputId = "select_map", 
                                label = "Select layer",
                                options = list(dropdownParent = 'body',
                                               create = 0),
                                choices = all_layers),
                 
                 sliderInput(inputId = "select_quant_map",
                             label = "Select cutoff (quantile)",
                             min = 0,
                             max = 1,
                             value = 0,
                             round = -2)
               ),
               
               # map prediction maps
               uiOutput("maps") %>%
                 withSpinner(color = "#2C3E50", type = 6)# "#0dc5c1"
               
             ),
             
             # Panel 2 -----------------------------------------------------------------
             tabPanel(
               "Top risk",
               includeCSS("www/button.css"),
               
               splitLayout(
                 selectizeInput(inputId = "select_filt", 
                                label = "Select filter",
                                options = list(dropdownParent = 'body',
                                               create = 0),
                                choices = all_layers),
                 
                 switchButton(inputId = "state",
                              label = "By state?",
                              value = FALSE,
                              col = "GB",
                              type = "TF"),
                 
                 sliderInput(inputId = "select_quant_dt",
                             label = "Select cutoff (quantile)",
                             min = 0,
                             max = 1,
                             value = 0.9,
                             round = -2)
               ),
               
               
               # filtered table
               tableOutput("table_ui") %>%
                 withSpinner(color = "#2C3E50", type = 6)# "#0dc5c1"
               
             )
  )
)


server <- function(input, output, session){
  
  map <- reactive({
    aus_SA2_filt <- aus_SA2 %>%
      dplyr::filter(.data[[input$select_map]] >= quantile(.data[[input$select_map]], .env$input$select_quant_map))
      
    ncols <- max(round(length(unique(aus_SA2_filt %>% dplyr::pull(input$select_map)))/10, 0), 2)
    
    aus_SA2_filt %>%
      mapview(map.types = "Esri.WorldStreetMap", 
              layer.name = input$select_map,
              zcol = input$select_map,
              label = aus_SA2$SA2_NAME21,
              col.regions = viridis::inferno(n = ncols))
  })
  
  # the maps
  output$maps <- renderUI({
    
    myRenderMapview(map())
    
  })
  
  tab <- reactive({
    if(input$state){
      data <- data_full %>%
        dplyr::group_by(State) %>%
        dplyr::filter(.data[[input$select_filt]] >= quantile(.data[[input$select_filt]], .env$input$select_quant_dt)) %>%
        dplyr::arrange(State, dplyr::desc(.data[[input$select_filt]]))
    } else {
      data <- data_full %>%
        dplyr::filter(.data[[input$select_filt]] >= quantile(.data[[input$select_filt]], .env$input$select_quant_dt)) %>%
        dplyr::arrange(dplyr::desc(.data[[input$select_filt]]))
    }
    
    counts <- data %>%
      dplyr::count(State) %>%
      dplyr::pull(n)
    
    names(counts) <- sort(unique(data$State))
    
    filt_states <- names(which(!(org_counts - counts == 0)))
    
    data %>%
      dplyr::filter(State %in% filt_states)
  })
  
  output$table_ui <- renderUI({
    if(nrow(tab()) == 0)
      return("All SA2 above cutoff - select different value")
    
    DT::DTOutput("table")
  })
  
  output$table <- DT::renderDT({tab()},
                               filter = "top",
                               options = list(
                                 pageLength = 20
                               ))
}

shinyApp(ui, server)
