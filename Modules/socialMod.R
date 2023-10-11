# Social determinants of health

socialUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3("Social determinants of health", align = "center"),
      fluidRow(column(6, align = "center", uiOutput(ns("selectSocialStateUI"))),
               column(6, align = "center", 
                      pickerInput(inputId = ns("stateMeasure"),
                                  choices = c("% High school or less education", 
                                              "% Some college education or more", 
                                              "Poverty rate", 
                                              "Unemployment rate", 
                                              "Mean household income", 
                                              "County population", 
                                              "% Living in a food desert",
                                              "Air quality (PM 2.5)",
                                              "Socio-demographic index",
                                              "Healthcare access and quality index"),   
                                  selected = "Mean household income",
                                  label = "Select a social determinant of health",
                                  multiple = FALSE))),
      fluidRow(
        column(4, align = "center",
               box(width = 12, status = "success", solidHeader = F, 
                   h5(tags$b("Social determinants of health"), align = "center"),
                   helpText("Social Determinants of Health (SDoH) are the conditions in the environments where people are born, live, learn, 
                                  work, play, worship, and age that affect a wide range of health, functioning, and quality-of-life outcomes and risks."),
                   br(),
                   helpText("Understanding data on social determinants of health that can enhance or hinder health, such as income, educational level, 
                                  employment, language and literacy skills, and access to health care, safe housing, nutritious foods, and 
                                  physical activity opportunities, can help focus efforts to improve people’s health on a local level."
                   )),
               br(),
               uiOutput(ns("stateMessageUI"))),
        column(8, align = "center", 
               box(width = 12, status = "success", align = "left", solidHeader = F, 
                   withSpinner(leafletOutput(ns("stateMap"), height = "50vh", width = "50vw"),
                               type = 4, color = "#400099"),
                   uiOutput(ns("stateMapRef"))))))
  )
}

socialServer <- function(id, close) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
 

# Reactive values ---------------------------------------------------------
      rVals <- reactiveValues(
        county = "",
        clicky = 0,
      )
           

# Reactive actions --------------------------------------------------------
      observeEvent(close(), {
        rVals$county <- ""
        rVals$clicky <- 0
        runjs("Shiny.setInputValue('plotly_selected-A', null);")
        removeModal()
      })
      observeEvent(input$stateMap_shape_click, {
        click <- input$stateMap_shape_click
        rVals$county <- click$id
        rVals$clicky <- 1
      })
      
# Reactive data -----------------------------------------------------------
      countyData <- reactive({
        myState <- input$selectSocialState
        
        states <- tbl(myDB, "State_Abbreviations") %>% 
          data.frame() %>%
          filter(name == myState)
        
        county <- data.frame()
        
        if (input$stateMeasure == "% High school or less education") {
          county <- tbl(myDB, "County_Education") %>%
            select(State, County, HS1 = `Less than high school`, HS2 = `High school only`,
                   C1 = `Some college`, C2 = `Collage degree`, Source) %>%
            mutate(Value = (HS1 + HS2) / (C1 + C2 + HS1 + HS2)) %>%
            data.frame() %>%
            filter(State == states$ABBREVIATION) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = paste0(format(Value*100, digits = 3, trim = T), "%"),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
        }
        if (input$stateMeasure == "% Some college education or more") {
          county <- tbl(myDB, "County_Education") %>%
            select(State, County, HS1 = `Less than high school`, HS2 = `High school only`,
                   C1 = `Some college`, C2 = `Collage degree`, Source) %>%
            mutate(Value = (C1 + C2) / (C1 + C2 + HS1 + HS2)) %>%
            data.frame() %>%
            filter(State == states$ABBREVIATION) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = paste0(format(Value*100, digits = 3, trim = T), "%"),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
        }
        if (input$stateMeasure == "Poverty rate") {
          county <- tbl(myDB, "County_Poverty") %>%
            select(State, County, Value = PovertyRate, Source) %>%
            data.frame() %>%
            filter(State == states$ABBREVIATION) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = paste0(format(Value, digits = 2, trim = T), "%"),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
        }
        if (input$stateMeasure == "Unemployment rate") {
          county <- tbl(myDB, "County_Unemployed") %>%
            select(State, County, Value = `Unemployment`, Source) %>%
            data.frame() %>%
            filter(State == states$ABBREVIATION) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = paste0(format(Value, digits = 2, trim = T), "%"),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
        }
        if (input$stateMeasure == "Mean household income") {
          county <- tbl(myDB, "County_Income") %>%
            data.frame() %>% 
            filter(State == states$ABBREVIATION) %>%
            rename(Value = mean_income) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = paste0("$", format(Value, digits =2, big.mark = ",", trim = T)),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
        } 
        if (input$stateMeasure == "County population") {
          county <- tbl(myDB, "County_Population") %>%
            data.frame() %>%
            filter(State == myState) %>%
            rename(Value = Population) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = format(Value, digits =2, big.mark = ",", trim = T),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
        }
        if (input$stateMeasure == "% Living in a food desert") {
          county <- tbl(myDB, "County_Food_Deserts") %>%
            data.frame() %>%
            filter(State == myState) %>%
            rename(Value = PropDesert) %>%
            mutate(Label_text_var = input$stateMeasure,
                   Format_var = paste0(format(Value*100, digits =2, trim = T),"%"),
                   Title_var = paste0(input$stateMeasure,"<br>by county"))
          
        }
        if (input$stateMeasure == "Air quality (PM 2.5)") {
          county <- tbl(myDB, "County_Air") %>%
            filter(name == myState) %>%
            rename(Value = PM2_5) %>%
            data.frame() %>%
            mutate(Label_text_var = paste0(input$stateMeasure, " (24-hr)"),
                   Format_var = Value,
                   Title_var = paste0(input$stateMeasure, "<br>by county"))
        }
        county$CountyName <- sub(" County", "", county$County)
        county$CountyName <- sub(" Count", "", county$CountyName)
        county$CountyName <- sub(" Coun", "", county$CountyName)
        county$CountyName <- sub(" Cou", "", county$CountyName)
        
        return(county)
      })  # Prepares the data for county level
      getStateMap <- reactive({
        states <- tbl(myDB, "State_Abbreviations") %>% 
          data.frame() %>%
          filter(name == input$selectSocialState)
        
        # Shape data for the map   
        shape <- tigris::counties(state = states$ABBREVIATION, class = "sf")
        geo_join(shape, countyData(), by_sp = "NAME", by_df = "CountyName", how = "left")
        
      })  # Prepares the state level heatmaps for leaflet
      getNationalMap <- reactive({
        states <-  geojson_read("data/us-states.json", what = "sp") 
        keep <- states@data$name
        
        if (input$stateMeasure == "Socio-demographic index") {
          dat <- tbl(myDB, "SDI") %>%
            filter(Location %in% keep) %>%
            select(name = Location, Value) %>%
            mutate(Source = "IHME 2019 (Accessed February 2022)") %>%
            data.frame()
        }
        if (input$stateMeasure == "Healthcare access and quality index") {
          dat <- tbl(myDB, "US_HAQ") %>%
            filter(Location %in% keep & Year == 2019) %>%
            select(name = Location, Value) %>%
            mutate(Source = "IHME 2019 (Accessed February 2022)") %>%
            data.frame()
        }
        
        states@data <- left_join(states@data, dat, by = "name")
        return(states)
        
      }) # prepares National leaflet heatmaps for SDI and HAQ
      

# Dynamic UI elements -----------------------------------------------------
      output$selectSocialStateUI <- renderUI({
        states <- tbl(myDB, "State_Abbreviations") %>% data.frame()
        pickerInput(inputId = ns("selectSocialState"),
                    choices = states$name,
                    selected = "California",
                    label = "Select a US State",
                    multiple = F)
      })  # Selects state for social determinants of 
      output$stateMapRef <- renderUI({
        source <- countyData()$Source[1]
        div(
          helpText(strong("Source: "), source, align = "left"),
          helpText("Click on an area of the map to see more information", align = "left"))
      })  # adds a source to the state/county level heatmaps
      output$stateMessageUI <- renderUI({
        box(width = 12, status = "success", solidHeader = T, title = input$stateMeasure,
            uiOutput(ns("stateMessage")))
      })  # adds a box in the social heatmaps for a dynamic message from Tahera
      observeEvent(input$stateMeasure, {
        
        
        if (input$stateMeasure == "% High school or less education") {
          output$stateMessage <- renderUI({
            div(
              helpText("Education and health and wellbeing are intrinsically linked. 
          The scientific literature behind the importance of education as a 
          determinant of health is amongst the most compelling."),
              br(),
              helpText("Education is strongly associated with life expectancy, morbidity, health behaviours,
          and educational attainment plays an important role in health by shaping opportunities,
          employment, and income."))
          })
        }
        if (input$stateMeasure == "% Some college education or more") {
          output$stateMessage <- renderUI({
            div(
              helpText("Education and health and wellbeing are intrinsically linked. 
          The scientific literature behind the importance of education as a 
          determinant of health is amongst the most compelling."),
              br(),
              helpText("Education is strongly associated with life expectancy, morbidity, health behaviours,
          and educational attainment plays an important role in health by shaping opportunities,
          employment, and income."))
          })
        }
        if (input$stateMeasure == "Poverty rate") {
          output$stateMessage <- renderUI({
            div(
              helpText("Something about poverty..."),
              helpText("This box can be removed for this measure - mostly a placeholder for now"))
          })
        }
        if (input$stateMeasure == "Unemployment rate") {
          output$stateMessage <- renderUI({
            div(
              helpText("Unemployment is associated with a range of health risks and 
                   health inequalities caused both by the event of becoming 
                   unemployed as well as the reduced income, deprivation and 
                   poverty due to being out of work. The risk of ill health 
                   increases as the duration of unemployment increases."))
          })
        }
        if (input$stateMeasure == "Mean household income") {
          output$stateMessage <- renderUI({
            div(
              helpText("One of the fundamental causes of health inequalities is the 
                   unequal distribution of incomes across the population. 
                   Studies indicate that higher incomes lead to better health. 
                   The level and distribution of income, and poverty, is a 
                   well known cause of health inequalities within populations. 
                   It influences health directly through the goods and services 
                   that people buy which can support, or damage, their health. 
                   It also influences a wide variety of factors that have an 
                   indirect impact on health, including social status and 
                   control over unforeseen events."))
          })
        } 
        if (input$stateMeasure == "County population") {
          output$stateMessage <- renderUI({
            div(
              helpText("Something about the county population..."),
              helpText("This box can be removed for this measure - mostly a placeholder for now"))
          })
        }
        if (input$stateMeasure == "% Living in a food desert") {
          output$stateMessage <- renderUI({
            div(
              helpText("Food deserts are geographic areas where residents have few to no convenient options
            for securing affordable and healyh foods, particularly fresh fruits and vegetables.  They are
            disproportionately found in high-poverty areas.  Food deserts create extra, everyday hurdles
             that can make it harder to make healthier choices.")
            )
          })
        }
        if (input$stateMeasure == "Air quality (PM 2.5)") {
          output$stateMessage <- renderUI({
            helpText("Air pollution can affect everyone. Exposure to air pollution 
                 has various different health effects, which come about at every 
                 stage of life, from a foetus’ first weeks in the womb all the 
                 way through to old age. The health effects of air pollution 
                 are complex, and range in severity of impact. In some cases, 
                 damage can be gradual and may not become apparent for many years.")
          })
        }
        if (input$stateMeasure == "Socio-demographic index") {
          output$stateMessage <- renderUI({
            div(
              helpText("The Socio-demographic Index (SDI) is a summary measure that 
                   identifies where countries or other geographic areas sit 
                   on the spectrum of development."),
              br(),
              helpText("Expressed on a scale of 0 to 1, SDI is a composite average 
                   of the rankings of the incomes per capita, average 
                   educational attainment, and fertility rates."))
          })
        }
        if (input$stateMeasure == "Healthcare access and quality index") {
          output$stateMessage <- renderUI({
            div(
              helpText("The Healthcare Access and Quality (HAQ) index, created by 
                   the Institute of Health Metrics and Evaluation’s, 
                   Global Burden of Diseases, Injuries, and Risk Factors 
                   Study (GBD) collaborators, is based on amenable mortality, 
                   defined as deaths that should not occur in the presence of 
                   timely and effective care."),
              br(),
              helpText("The US health care system serves some populations better than others. 
                   Health insurance coverage varies by age and state because 
                   state governments can expand benefits and eligibility 
                   for programs beyond the minimum federal requirements, 
                   with the exception of federally provided Medicare for 
                   individual aged 65 or older or individuals who are 
                   disabled and eligible for Social Security benefits 
                   or have end-stage kidney disease."))
          })
        }
        
      }) # populates the state heatmap box with whatever message
      
      


# Outputs -----------------------------------------------------------------
      output$stateMap  <- renderLeaflet({
        if (!input$stateMeasure %in% c("Socio-demographic index", "Healthcare access and quality index")) {
          map <- getStateMap() 
          
          # create color palette 
          pal <- colorNumeric(
            palette =  paletteer_c("ggthemes::Orange-Gold", 100) %>% as.character(),
            domain = (map$Value),
            na.color = NA)
          
          # New code
          labels <- paste0(
            "County: ", map$NAMELSAD, "<br>",
            map$Label_text_var, ": ",
            map$Format_var) %>%
            lapply(htmltools::HTML)
          
          titleVar <- map$Title_var[!is.na(map$Title_var)]
          
          myMap <-  leaflet(map) %>%
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addMiniMap() %>%
            # add zip codes
            addPolygons(fillColor = ~pal(Value),
                        weight = 1,
                        opacity = 0.25,
                        color = "#708090",
                        dashArray = "3",
                        fillOpacity = 1,
                        layerId = map$County,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#000000",
                                                     dashArray = "",
                                                     fillOpacity = 0.2,
                                                     bringToFront = TRUE),
                        label = labels
            )  %>%
            # add legend
            addLegend(pal = pal, 
                      values = ~Value, 
                      opacity = 0.7, 
                      title = htmltools::HTML(titleVar[1]),
                      position = "bottomleft")
        } 
        if (input$stateMeasure %in% c("Socio-demographic index", "Healthcare access and quality index")) {
          map <- getNationalMap()
          pal <- colorNumeric(
            palette = paletteer_c("ggthemes::Orange-Gold", 20) %>% as.character(),
            domain = (map$Value),
            na.color = NA)
          labels <- paste0("<b>", input$stateMeasure,"</b><br>",
                           "<b>", map@data$name,": ","</b>", format(round(map@data$Value,2), nsmall = 2)) %>%
            lapply(htmltools::HTML)
          
          myMap <- leaflet(map, 
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
              layerId = map$name,
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
            addLegend(pal = pal, values = map$Value, na.label = "Missing", opacity = 0.7, title = input$stateMeasure,
                      position = "bottomleft")
          
        }
        myMap
      })
      

# Modals ------------------------------------------------------------------
      observeEvent(rVals$clicky, {
        shiny::validate(need(rVals$clicky == 1, ""))
        if (!input$stateMeasure %in% c("Socio-demographic index", "Healthcare access and quality index")) {
          showModal(
            modalDialog(id = "socialModal",
                        title = NULL,
                        footer = actionButton(
                          inputId = "closeModal",
                          label = "Dismiss",
                          icon = icon("backspace")),
                        easyClose = FALSE,
                        size = "l",
                        fluidPage(
                          div(
                            h3("State: ", input$selectSocialState, align = "center"),
                            h4("County: ", rVals$county, align = "center"),
                            uiOutput(ns("localInfo"))
                          ))))
        }
        if (input$stateMeasure %in% c("Socio-demographic index", "Healthcare access and quality index")) {
          showModal(
            modalDialog(id = "socialModal",
                        title = NULL,
                        footer = actionButton(
                          inputId = "closeModal",
                          label = "Dismiss",
                          icon = icon("backspace")),
                        easyClose = FALSE,
                        size = "l",
                        fluidPage(
                          div(
                            h3("State: ", input$selectSocialState, align = "center"),
                            uiOutput(ns("localInfo"))
                          ))))
        }
        
        output$localInfo <- renderUI({
          div(
            helpText("We can put text in this screen based on whatever variable was selected (income, food deserts, life expectancy, whatever)"),
            fluidPage(
              withSpinner(
              plotlyOutput(ns("localPlot")),
              type = 4, color = "#400099"))
          )
        })
        
      })
      

# Modal plots -------------------------------------------------------------
      output$localPlot <- renderPlotly({
        if (!input$stateMeasure %in% c("Socio-demographic index", "Healthcare access and quality index")) {  
          mystate <- countyData()
          mystate$flag <- ifelse(mystate$County == rVals$county, "#FE9FB8", "#8C66C2")
          mystate <- mystate[order(mystate$Value, decreasing = T),]
          mystate$order <- 1:nrow(mystate)
          
          p <- plot_ly(type = "bar") 
          p <- p %>%
            add_trace(
              data = mystate,
              x = ~order,
              y = ~Value,
              marker = list(color = ~flag),
              text = mystate$County,
              textposition = "outside",
              hovertext = paste0("County: ", mystate$County,"<br>",
                                 input$stateMeasure,": ", 
                                 mystate$Format_var),
              #paste0("$",format(round(mystate$Value,2),big.mark = ","))),
              hoverinfo = "text",
              showlegend = F
            ) %>%
            layout(
              xaxis = list(title = "",
                           showticklabels = F,
                           showline = T),
              yaxis = list(
                title = ""))
        }
        if (input$stateMeasure %in% c("Socio-demographic index", "Healthcare access and quality index")) {
          dat <- getNationalMap()@data 
          dat$flag <- ifelse(dat$name == rVals$county, "#FE9FB8", "#8C66C2")
          dat <- dat[order(dat$Value, decreasing = T),]
          dat$order <- 1:nrow(dat)
          p <- plot_ly(type = "bar") 
          p <- p %>%
            add_trace(
              data = dat,
              x = ~name,
              y = ~Value,
              marker = list(color = ~flag),
              #text = dat$name,
              #textposition = "outside",
              hovertext = paste0("State: ", dat$name,"<br>",
                                 input$stateMeasure,": ", 
                                 format(round(dat$Value, 2), nsmall = 2)),
              #paste0("$",format(round(mystate$Value,2),big.mark = ","))),
              hoverinfo = "text",
              showlegend = F
            ) %>%
            layout(
              xaxis = list(title = "",
                           showticklabels = T,
                           tickangle = 45,
                           showline = T),
              yaxis = list(
                title = ""))
          
        }
        p
      })  

      
            
})
}