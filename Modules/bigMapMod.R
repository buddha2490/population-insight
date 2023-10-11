### Big map mod

bigMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fillPage(
      leafletOutput(ns("systemsMap"), height = "90vh", width = "85vw")),
    absolutePanel(id = "controls", class = "panel panel-default",
                  top = 500, left = 270, width = 250, fixed=F,
                  draggable = TRUE, height = "auto", cursor = "move",
                  h3("Geographical overview", align = "center"),
                  helpText(" National and subnational population count comparisons"),
                  br(),
                  br(),
                  pickerInput(inputId = ns("mapInput"), "Select population metric",   
                              choices = c("Eligible health system members", "Enrolled health system members", "Total population"), 
                              selected = c("Eligible health system members"),
                              multiple = FALSE),
                  br(),
                  uiOutput(ns("bigMapSource")))
  )
}
bigMapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    

# Load database -----------------------------------------------------------

     # myDB <- dbConnect(RSQLite::SQLite(),  "data/ShinyData.DB") 
      

# Reactive data -----------------------------------------------------------
      bigMapData <- reactive({
        
        # I need to simulate some babylon data by state
        babylon <- tbl(myDB, "State_Abbreviations") %>%
          filter(!name == "Puerto Rico") %>%
          data.frame()
        
        # States with current babylon business
        # Vector unused - kept here as a reminder for now
        babStates <- c("Mississippi", "California", "New York", "Missouri", "Georgia")
        
        # 1.  Simulate total 'eligible' babylon customers
        simulate <- function(min, max) {
          set.seed(1234)
          runif(1, min=min, max = max)
        }
        
        babylon$eligible <- 0
        babylon$eligible[babylon$name == "Mississippi"] <- simulate(10000, 25000) %>% floor()
        babylon$eligible[babylon$name == "California"] <- simulate(40000, 75000) %>% floor()
        babylon$eligible[babylon$name == "New York"] <- simulate(25000, 60000) %>% floor()
        babylon$eligible[babylon$name == "Missouri"] <- simulate(15000,30000) %>% floor()
        babylon$eligible[babylon$name == "Georgia"] <- simulate(20000, 30000) %>% floor()
        
        # 2.  Simulate total 'enrolled' babylon customoers
        babylon$enrolled <- NA
        for (i in 1:nrow(babylon)) {
          babylon$enrolled[i] <- (babylon$eligible[i] * simulate(0.05, 0.9)) %>% floor()
        }
        
        # Grab my data for the map - right now only real data is population
        if (input$mapInput == "Eligible health system members") {
          dat <- select(babylon, name, Value = eligible) %>%
            mutate(Source = "Simulated Babylon internal data")
        }
        if (input$mapInput == "Enrolled health system members") {
          dat <- select(babylon, name, Value = enrolled) %>%
            mutate(Source = "Simulated Babylon internal data")
        }
        if (input$mapInput == "Total population") {
          dat <- tbl(myDB, "AGE_DISTRIBUTION") %>%
            filter(!Location %in% c("United Kingdom", "England", "Scotland", "Northern Ireland", "Wales")) %>%
            data.frame()
          # Sum male and female counts across states
          dat <- lapply(unique(dat$Location), function(x) {
            local <- filter(dat, Location == x)
            male <- sum(local$Value[local$Sex == "Male"], na.rm = T)
            female <- sum(local$Value[local$Sex == "Female"], na.rm = T)
            data.frame(name = x, Value = floor(male + female))
          }) %>%
            do.call("rbind", .) %>%
            mutate(Source = "IHME 2019 (Accessed Feburary 2022)")
        }
        
        return(dat)
      })  # Prepares data for the big national map
   

# Reactive UI elements ----------------------------------------------------
      output$bigMapSource <- renderUI({
        # Adds a sourcing footnote to the bigMap page based on the metric
        source <- bigMapData()$Source[1]
        helpText(strong("Source: "), source)
      })  # creates a footnote source on the big map page


# Render the map ----------------------------------------------------------
      observeEvent(input$mapInput, {
        output$systemsMap <- renderLeaflet({
          dat <- filter(bigMapData())
          
          # Data are prepared, now I need the map
          states <-  geojson_read("data/us-states.json", what = "sp") 
          
          states@data <- left_join(states@data, dat, by = "name")
          pal <- colorNumeric(
            palette = paletteer_c("ggthemes::Orange-Gold", 20) %>% as.character(),
            domain = (states$Value),
            na.color = NA)
          
          labels <- paste0("<b>",input$mapInput, ": ", "</b>", 
                           format(states$Value, big.mark =",")) %>%
            lapply(htmltools::HTML)
          
          
          leaflet(states, 
                  sizingPolicy = leafletSizingPolicy(defaultWidth = "100%")) %>%
            setView(-96, 37.8, 3.5, zoom = 4) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addTiles() %>%
            addMiniMap() %>%
            addPolygons(
              fillColor = ~pal(Value),
              weight = 1,
              opacity = 0.25,
              color = "#708090",
              dashArray = "3",
              fillOpacity = 1,
              layerId = states$name,
              highlight = highlightOptions(
                weight = 2,
                color = "royalblue",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto")
            )    %>%
            addPolylines(color = "#000000",
                         weight = 1.5) %>%
            addLegend(pal = pal, values = states$Value, na.label = "Missing", opacity = 0.7, title = input$mapInput,
                      position = "topleft")
        })
      })      
        
})
}