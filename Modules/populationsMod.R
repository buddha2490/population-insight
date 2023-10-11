## Population section module

popUI <- function(id) {
  ns <- NS(id)
  tagList(
h3("Population level data by national and local geography", align = "center"),
uiOutput(ns("popInputs")),
tabBox(id = "populationBox", width = 12, height = "auto",
       tabPanel(title = "Geographical differences", 
                fluidPage(
                  box(solidHeader = T, width = 12, collapsible = T, title = "Age distribution (%)", status = "success",
                      plotlyOutput(ns("popAgePlot"), height = "50%"),
                      helpText(strong("Source: "), "IHME 2019 (Accessed Feburary 2022)")),
                  hr())),
       tabPanel(title = "Race and ethnicity", 
                fluidPage(
                h4(tags$b("Race and ethnicity"), align = "center"),
                fluidRow(
                  column(6, align = "center", uiOutput(ns("pop1RacePlotUI"))),
                  column(6, align = "center", uiOutput(ns("pop1BabylonRacePlotUI")))
                ),
                fluidRow(
                  column(6, align = "center", uiOutput(ns("pop2RacePlotUI"))),
                  column(6, align = "center", uiOutput(ns("pop2BabylonRacePlotUI")))
                )
       )),
       tabPanel(title = "Life expectancy",
                fluidPage(
                  box(width = 12, solidHeader = T, status = "success", collapsible = T, title = "Life Expectancy from birth in the United States",
                      plotlyOutput(ns("lifeExpectPlot")),
                      helpText(strong("Source: "), "National Center for Health Statistics, National Vital Statistics System, 2018 data.")
                  )))
      ))
}

popServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      

# Functions ---------------------------------------------------------------
   colors <- c("#400099", "#24D9D9", "#FFCB00", "#FE9FB8", "#4481FF",
               "#6633AD", "#66E4E4", "#FFD533", "#FEB2C6", "#7CA7FF",
               "#8C66C2", "#91ECEC", "#FFE066", "#FEC5D4", "#A1C0FF",
               "#c5cdfa", "#BDF4F4", "#FFEA99", "#FFD9E3", "#C7D9FF")
   
   makeTitle <- function(title, size = 20) {
     list(
       text = title,
       xref = "paper",
       yref = "paper",
       yanchor = "bottom",
       xanchor = "center",
       align = "center",
       x = 0.5,
       y = 1,
       font = list(size = size),
       showarrow = FALSE 
     )
   }
   ageDistribution <- function(dat,Location) {
     
     dat$Age <- factor(
       dat$Age,
       levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                  "70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
     
     
     men <- list()
     fem <- list()
     
     men$men <- filter(dat, Sex == "Male")
     fem$fem <- filter(dat, Sex == "Female")
     
     # I need to overlay babylon population to the age range, but only for certain states
     # This is fake data for now
     fakeMen <- men$men
     fakeFem <- fem$fem
     
     for (i in 1:nrow(fakeMen)) {
       fakeMen$Value[i] <- fakeMen$Value[i] * runif(1, 0.85, 1.15)
       fakeFem$Value[i] <- fakeFem$Value[i] * runif(1, 0.85, 1.15)
     }
     men$babylonMen <- fakeMen
     fem$babylonFem <- fakeFem
     
     
     men <- lapply(men, function(x) {
       x$Prop <- x$Value / sum(x$Value)
       x$N <- -1 * x$Prop
       x$Text <- paste0(format(x$Prop*100, nsmall = 1, digits = 1, trim = 2),"%")
       return(x)
     })  
     
     fem <- lapply(fem, function(x) {
       x$Prop <- x$Value / sum(x$Value)
       x$N <- x$Prop
       x$Text <- paste0(format(x$Prop*100, nsmall = 1, digits = 1, trim = 2),"%")
       return(x)
     })  
     #head(men$men); head(men$babylonMen)
     
     
     # Figure out the cut points on the x axis
     tickvals <- c(-0.1, 0, 0.1)
     ticktext <- c("10%, 0, 10%")
     
     babStates <- c("United States","Mississippi", "California", "New York", "Missouri", "Georgia")
     if (Location %in% babStates) {
       
       myPlots  <- plot_ly(type = "bar", orientation = "h", height = 800)
       myPlots <- myPlots %>%
         add_trace(data = men$men,
                   y = ~Age,
                   x = ~N,
                   name = "Male",
                   marker = list(color = colors[15]),
                   text = ~Text,
                   textposition = "outside",
                   hovertext = paste0("Male aged ", men$men$Age,":<br>", men$men$Text),
                   hoverinfo = "text",
                   textfont = list(size = 12)) %>%
         add_trace(data = fem$fem,
                   y = ~Age,
                   x = ~N,
                   name = "Female",
                   marker = list(color = colors[18]),
                   text = ~Text,
                   hovertext = paste0("Female aged ", fem$fem$Age,":<br>", fem$fem$Text),
                   hoverinfo = "text",
                   textposition = "outside",
                   textfont = list(size = 12)) %>%
         add_trace(data = men$babylonMen,
                   type = "scatter",
                   mode = "lines",
                   x = ~N,
                   y = ~levels(Age),
                   name = "Health system patients",
                   line = list(color = "black", dash = "dot", shape = "spline", width = 2),
                   hovertext = paste0("Male aged ", men$babylonMen$Age,":<br>", men$babylonMen$Text),
                   hoverinfo = "text") %>%
         add_trace(data = fem$babylonFem,
                   type = "scatter",
                   mode = "lines",
                   x = ~N,
                   y = ~levels(Age),
                   name = "Health system patients",
                   line = list(color = "black", dash = "dot", shape = "spline", width = 2),
                   hovertext = paste0("Female aged ", fem$babylonFem$Age,":<br>", fem$babylonFem$Text),
                   hoverinfo = "text",
                   textfont = list(size = 12),
                   showlegend = F) %>%
         layout(barmode = "overlay",
                xaxis = list(title = "",
                             range = list(-0.1, 0.1),
                             showticklabels = F,
                             showline = F,
                             tickangle = 20
                ),
                yaxis = list(title = "",
                             showline = F,
                             tickfont = list(size = 15),
                             tickvals = levels(dat$Age)),
                legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = -0.03, font = list(size = 20)),
                annotations = list(makeTitle(Location, size = 18)))
     } 
     if (!Location %in% babStates){
       myPlots <- plot_ly(type = "bar", orientation = "h", height = 800)
       myPlots <- myPlots  %>%
         add_trace(data = men$men,
                   y = ~Age,
                   x = ~N,
                   name = "Male",
                   marker = list(color = colors[5]),
                   text = ~Text,
                   textposition = "outside",
                   hovertext = paste0("Male aged ", men$men$Age,":<br>", men$men$Text),
                   hoverinfo = "text",
                   textfont = list(size = 12)) %>%
         add_trace(data = fem$fem,
                   y = ~Age,
                   x = ~N,
                   name = "Female",
                   marker = list(color = colors[3]),
                   text = ~Text,
                   hovertext = paste0("Female aged ", fem$fem$Age,":<br>", fem$fem$Text),
                   hoverinfo = "text",
                   textposition = "outside",
                   textfont = list(size = 12)) %>%
         layout(barmode = "overlay",
                xaxis = list(title = "",
                             range = list(-0.1, 0.1),
                             showticklabels = F,
                             showline = F,
                             tickangle = 20
                ),
                yaxis = list(title = "",
                             showline = F,
                             tickfont = list(size = 15),
                             tickvals = levels(dat$Age)),
                legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = -0.03, font = list(size = 20)),
                annotations = list(makeTitle(Location, size = 18)))
     }
     return(myPlots)
   }
   reshapeRace <- function(dat) {
     names(dat) <- sub(".", " ", names(dat), fixed = T)
     rbind(
       select(dat, Location, P = White) %>% mutate(Race = "White"),
       select(dat, Location, P = Black) %>% mutate(Race = "Black"),
       select(dat, Location, P = Hispanic) %>% mutate(Race = "Hispanic"),
       select(dat, Location, P = Asian) %>% mutate(Race = "Asian"),
       select(dat, Location, P = `American Indian`) %>% mutate(Race = "American Indian"),
       select(dat, Location, P = `Pacific Islander`) %>% mutate(Race = "Pacific Islander"),
       select(dat, Location, P = `Mixed Race`) %>% mutate(Race = "Mixed Race")
     )  
   }
   myPie <- function(dat, labelvar, proportionvar)  {
     
     plot_ly(type = "pie") %>%
       add_pie(data = dat,
               labels = dat[[labelvar]],
               values = dat[[proportionvar]],
               textinfo = 'percent',
               hoverinfo = "text",
               hovertext = paste0(dat$Race,": ", paste0(format(dat$P*100, digits = 4), "%")),
               insidetextfont = list(size = 15),
               outsidetextfont = list(size = 1, color = "white"),
               marker = list(colors = colors[c(1:5,11:14)], line = list(color = '#FFFFFF', width = 1)),
               showlegend = T) %>%
       layout(
         margin = list(b = 0, l = 0, r = 10, t = 35),
         legend = list(orientation = "V", xanchor = "left", xref = "paper", x = 1, y =0.5, 
                       font = list(size = 12),
                       bgcolor = 'rgba(0,0,0,0)'))
   }
   


# Reactive data -----------------------------------------------------------
   race1 <- reactive({
     # UK is different, code it out if select, then plot everything
     if (input$selectPop1 == "United Kingdom") {
       df <- tbl(myDB, "UK_RACE") %>%
         data.frame()
       vars <- c("White", "Mixed/Multiple", "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian",
                 "Black/African/Caribbean/Black British", "Other")
       
       names(df) <- c("Age", vars)
       summed <- as.numeric()
       for (i in 1:length(vars)) {
         summed[i] <- sum(df[[vars[i]]], na.rm=T)
       }
       state1 <- data.frame(Race = vars, N = summed)
       state1$P <- state1$N / sum(state1$N)
       
     } else {
       state1 <- tbl(myDB, "US_RACE") %>%
         data.frame() %>%
         filter(Location == input$selectPop1) %>%
         reshapeRace()
       state1$P[is.na(state1$P)] <- 0
     }
     state1$Race[state1$Race == "Black/African/Caribbean/Black British"] <- "Black"
     return(state1)
     
   })  # generate the data for the first race pie graph
   race2 <- reactive({
     # UK is different, code it out if select, then plot everything
     if (input$selectPop2 == "United Kingdom") {
       df <- tbl(myDB, "UK_RACE") %>%
         data.frame()
       vars <- c("White", "Mixed/Multiple", "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian",
                 "Black/African/Caribbean/Black British", "Other")
       
       names(df) <- c("Age", vars)
       summed <- as.numeric()
       for (i in 1:length(vars)) {
         summed[i] <- sum(df[[vars[i]]], na.rm=T)
       }
       state1 <- data.frame(Race = vars, N = summed)
       state1$P <- state1$N / sum(state1$N)
       
     } else {
       state1 <- tbl(myDB, "US_RACE") %>%
         data.frame() %>%
         filter(Location == input$selectPop2) %>%
         reshapeRace()
       state1$P[is.na(state1$P)] <- 0
     }
     state1$Race[state1$Race == "Black/African/Caribbean/Black British"] <- "Black"
     return(state1)
     
   }) # generate the data for the second race pie graph
   
 

# Dynamic UI elements -----------------------------------------------------
   output$popInputs <- renderUI({
     states <- tbl(myDB, "State_Abbreviations") %>% data.frame()
     locations <- c("United Kingdom", "United States", states$name)
     locations <- locations[!locations == "Puerto Rico"]
     
     div(
       fluidRow(
         column(6, align = "center", pickerInput(inputId = ns("selectPop1"),
                                                 choices = locations,
                                                 selected = "United States",
                                                 multiple = F)),
         column(6, align = "center", pickerInput(inputId = ns("selectPop2"),
                                                 choices = locations,
                                                 selected = "Missouri",
                                                 multiple = F))
       ))
     
   })  # select a state for comparisons, generates dropdowns
   output$pop1RacePlotUI <- renderUI({
     box(title = input$selectPop1, width = 12, align = "center", status = "success", solidHeader = T, 
         plotlyOutput(ns("pop1RacePlot")), 
         uiOutput(ns("raceSource1")),
         uiOutput(ns("raceNote1")))
   })
   output$pop2RacePlotUI <- renderUI({
     box(title = input$selectPop2, width = 12, status = "success", solidHeader = T, 
         plotlyOutput(ns("pop2RacePlot")), 
         uiOutput(ns("raceSource2")),
         uiOutput(ns("raceNote2")))
   })
   
   babStates <- c("United States","Mississippi", "California", "New York", "Missouri", "Georgia")
   output$pop1BabylonRacePlotUI <- renderUI({
     if (input$selectPop1 %in% babStates) {
       box(title = paste0("Health coverage over ",input$selectPop1), 
           width = 12, align = "center", status = "success", solidHeader = T, 
           plotlyOutput(ns("pop1BabylonRacePlot")),
           helpText(strong("Source: "), "Simulated health system internal data", align = "left"))
     }
   })
   output$pop2BabylonRacePlotUI <- renderUI({
     if (input$selectPop2 %in% babStates) {
       box(title = paste0("Health coverage over ",input$selectPop2), 
           width = 12, align = "center", status = "success", solidHeader = T, 
           plotlyOutput(ns("pop2BabylonRacePlot")),
           helpText(strong("Source: "), "Simulated health system internal data", align = "left"))
     }
   })
   
   observeEvent(input$selectPop1, {
     if (input$selectPop1 == "United Kingdom") {
       output$raceSource1 <- renderUI(helpText(strong("Source: "), "Office for National Statistics 2019 (Accessed January 2022)", align = "left"))
       output$raceNote1 <- renderUI(helpText(strong("Disclaimer: "), "The US and UK ethnicity groupings are not comparable due to differences in the data sources.", align = "left"))
     } else {
       output$raceSource1 <- renderUI(helpText(strong("Source: "), "Kaiser Family Foundation 2019 (Accessed January 2022)", align = "left"))
     }
   }) # Footnotes and source for racePlot1
   observeEvent(input$selectPop2, {
     if (input$selectPop2 == "United Kingdom") {
       output$raceSource2 <- renderUI(helpText(strong("Source: "), "Office for National Statistics 2019 (Accessed January 2022)", align = "left"))
       output$raceNote2 <- renderUI(helpText(strong("Disclaimer: "), "The US and UK ethnicity groupings are not comparable due to differences in the data sources.", align = "left"))
     } else {
       output$raceSource2 <- renderUI(helpText(strong("Source: "), "Kaiser Family Foundation 2019 (Accessed January 2022)", align = "left"))
     }
   }) # Footnotes and source for racePlot2
   
   

# graphics -----------------------------------------------------------

   
   # Age pyramids
   output$popAgePlot <- renderPlotly({
     
     loc1 <- input$selectPop1
     loc2 <- input$selectPop2
     
     one <- tbl(myDB, "AGE_DISTRIBUTION") %>%
       filter(Location == loc1) %>%
       data.frame() %>%
       ageDistribution(input$selectPop1)
     
     two <- tbl(myDB, "AGE_DISTRIBUTION") %>%
       filter(Location == loc2) %>%
       data.frame() %>%
       ageDistribution(input$selectPop2)
     
     plotly::subplot(
       style(one, showlegend = F),
       two,
       margin = 0.02,
       shareY = T,
       shareX = T
     )
   })
   output$pop1RacePlot <- renderPlotly({
     shiny::validate(need(input$selectPop1 != "", ""))
     myPie(race1(), "Race", "P")
   })
   output$pop2RacePlot <- renderPlotly({
     shiny::validate(need(input$selectPop2 != "", ""))
     myPie(race2(), "Race", "P")
   })
   
   # These will require real data
   output$pop1BabylonRacePlot <- renderPlotly({
     shiny::validate(need(input$selectPop1 != "", ""))
     myPie(race1(), "Race", "P")
   })
   output$pop2BabylonRacePlot <- renderPlotly({
     shiny::validate(need(input$selectPop2 != "", ""))
     myPie(race2(), "Race", "P")
   })
   
   # Life expectancy
   output$lifeExpectPlot <- renderPlotly({
     state1 <- input$selectPop1
     state2 <- input$selectPop2
     USA <- data.frame(State = "USA", Value = 78.64, name = "United States", 
                       Source = "National Center for Health Statistics, National Vital Statistics System, 2018 data.")
     UK <- data.frame(State = "UK", Value = 81.20, name = "United Kingdom",
                      Source = "World Bank, 2019")
     dat <- tbl(myDB, "State_life_expectancy") %>% 
       data.frame() %>%
       rbind(USA) %>%
       rbind(UK) %>%
       arrange(desc(Value))
     dat$flag <- "#6633AD"
     dat$flag[dat$name == state1] <- "#24D9D9"
     dat$flag[dat$name == state2] <- "#FFCB00"
     
     p <- plot_ly(type = "bar")
     p %>%
       add_trace(data = dat,
                 x = ~name,
                 y = ~Value,
                 marker = list(color = dat$flag),
                 hovertext = paste0("<b>",dat$name,": </b>",dat$Value),
                 hoverinfo = "text",
                 showlegend = F) %>%
       layout(
         xaxis = list(title = "",
                      showticklabels = T,
                      tickangle = 45,
                      showline = T),
         yaxis = list(
           title = "",
           range = list(60,85)))
     
   })  
   
        
    })
}

    