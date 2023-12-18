library(shiny)
library(shinythemes)
library(leaflet)
library(mapview)

# render mapview doesn't work; this function works
myRenderMapview <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) 
    expr = substitute(mapview:::mapview2leaflet(expr))
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env, 
                                 quoted = TRUE)
}

# read layer
aus_SA2 <- setNames(sf::read_sf("shiny-resources/aus_SA2.gpkg"), c("SA2_CODE21", "SA2_NAME21", "Region", "State", "Incursion risk", "Climatic suitability", "Establishment likelihood", "Tourist pathway", "Returning resident pathway", "Visiting friends & family pathway", "Sea cargo pathway", "Natural dispersal pathway", "Budwood pathway", "geom"))

roads <- sf::read_sf("shiny-resources/roads.gpkg")

# layers
all_layers <- c("Incursion risk", "Climatic suitability", "Establishment likelihood", "Tourist pathway", "Returning resident pathway", "Visiting friends & family pathway", "Sea cargo pathway", "Natural dispersal pathway", "Budwood pathway")

data <-
  aus_SA2 %>%
  tibble::as_tibble() %>%
  dplyr::select(-geom)

org_counts <-
  data %>%
  dplyr::count(State) %>%
  dplyr::pull(n)

names(org_counts) <- sort(unique(data$State))

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
               uiOutput("maps")
               
             ),
             
             # Panel 2 -----------------------------------------------------------------
             tabPanel(
               "Top risk",
               
               splitLayout(
                 selectizeInput(inputId = "select_filt", 
                                label = "Select filter",
                                options = list(dropdownParent = 'body',
                                               create = 0),
                                choices = all_layers),
                 
                 sliderInput(inputId = "select_quant_dt",
                             label = "Select cutoff (quantile)",
                             min = 0,
                             max = 1,
                             value = 0.9,
                             round = -2)
               ),
               
               
               # filtered table
               tableOutput("table_ui")
               
             )
  )
)


server <- function(input, output, session){
  
  map <- reactive({
    mapview(map.types = "Esri.WorldStreetMap", roads, layer.name = "Roads", legend = FALSE, label = roads$name, hide = TRUE) +
      aus_SA2 %>%
      dplyr::filter(.data[[input$select_map]] >= quantile(.data[[input$select_map]], .env$input$select_quant_map)) %>%
      mapview(layer.name = input$select_map,
              zcol = input$select_map,
              label = aus_SA2$SA2_NAME21,
              col.regions = viridis::inferno(n = 100))
  })
  
  # the maps
  output$maps <- renderUI({
    
    myRenderMapview(map())
    
  })
  
  tab <- reactive({
    data <- data %>%
      dplyr::group_by(State) %>%
      dplyr::filter(.data[[input$select_filt]] >= quantile(.data[[input$select_filt]], .env$input$select_quant_dt)) %>%
      dplyr::arrange(State, dplyr::desc(.data[[input$select_filt]]))
    
    counts <- data %>%
      dplyr::count(State) %>%
      dplyr::pull(n)
    
    names(counts) <- sort(unique(data$State))
    
    filt_states <- names(which(!(org_counts - counts == 0)))
    
    data <- data %>%
      filter(State %in% filt_states) 
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
