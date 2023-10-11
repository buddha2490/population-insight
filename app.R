library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(stringr)
library(bslib)
library(scales)
library(rhandsontable)
library(shinyalert)
library(plotly)
library(shinydashboard)
library(data.table)
library(tableHTML)
library(leaflet)
library(geojsonio)
library(english)
library(DBI)
library(odbc)
library(RSQLite)
library(dbplyr)
library(paletteer)
library(tigris)
library(ggthemes)
library(weights)
library(shinycssloaders)
library(pROC)
library(caret)
library(drifter)
library(ingredients)
options(shiny.maxRequestSize = 300*1024^2)
rm(list = ls())


source("functions.R")

source("Modules/bigMapMod.R")
source("Modules/populationsMod.R")
source("Modules/socialMod.R")



# menu updateDates
files <- list.files("Modules") %>%
  lapply(function(x) file.info(file.path("Modules", x)))
names(files) <- list.files("Modules")
files <- lapply(files, function(x) {
  as.Date(x$mtime)
})




# Header ------------------------------------------------------------------

header <- dashboardHeader(
  title = span("Population Insight Tool", style = "color:#400099; font-weight: bold;"),
  titleWidth = 400,
  dropdownMenuCustom(type = "message",
                     customSentence = customSentence,
                     icon = icon("fa-solid fa-envelope-open-text", "fa-2x"),
                     messageItem(
                       from = "Brian Carter",
                       message = "",
                       icon = icon("envelope"),
                       href = "mailto:brian.carter@procogia.com"),
                     messageItem(
                       from = "Allen Obrien",
                       message = "",
                       icon = icon("envelope"),
                       href = "mailto:allen.obrien@phsa.ca"
                     )
  ),
  dropdownMenuCustom(type = "message",
                     customSentence = customSentence_share,
                     icon = icon("fa-solid fa-share", "fa-2x"),
                     messageItem(
                       from = "Twitter",
                       message = "",
                       icon = icon("twitter"),
                       href = "https://twitter.com/intent/tweet?url=http://www.phsa.ca/&text=Population Health Insight tool from PHSA!"
                     ),
                     messageItem(
                       from = "Facebook",
                       message = "",
                       icon = icon("facebook"),
                       href = "https://www.facebook.com/sharer/sharer.php?url=http://www.phsa.ca/"
                     ),
                     messageItem(
                       from = "LinkedIn",
                       message = "",
                       icon = icon("linkedin"),
                       href = "http://www.linkedin.com/shareArticle?mini=true&url=http://www.phsa.ca&title=Population Health Insight tool from PHSA!"
                     ))

)
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='http://www.phsa.ca',
                                             tags$img(src = "logo.png",
                                                      target = "_blank",
                                                      alt = "PHSA Logo",
                                                      height = "30",
                                                      width = "120",
                                                      style = "vertical-align:left"))


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = "tabs",
    style = "position: relative; overflow: visible;",
    collapsible = F,
    hr(),
    menuItem("Geographical overview", tabName = "bigmap", badgeLabel = files$bigMapMod.R, badgeColor = "purple"),
    menuItem("Population comparisons", tabName="populations", badgeLabel = files$populationsMod.R, badgeColor = "purple"),
    menuItem("Social determinants of health", tabName = "social", badgeLabel = files$socialMod.R, badgeColor = "purple")
    ))



# Body --------------------------------------------------------------------



body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet - edits.css")),
  tags$style("html, body {overflow: visible !important;"),
  useShinyjs(),
  fluidPage(
    

# tabItems ----------------------------------------------------------------

    tabItems(
      tabItem(tabName = "bigmap",  
              bigMapUI(id = "bigMapMod")),
      tabItem(tabName = "populations",
              popUI(id = "populationMod")),
      tabItem(tabName = "social",
              socialUI(id = "socialMod"))

    )))



# Build the UI ------------------------------------------------------------

ui <- dashboardPage(
  title = "Population Insight Tool",
  header,
  sidebar,
  body
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  


# Data --------------------------------------------------------------------
# this was in a database, but we have to fake it now
myDB <<- dbConnect(RSQLite::SQLite(),  ":memory:") 
files <- list.files("data/rds")
lapply(files, function(x) {
  tableName <- sub(".RDS", "", x)
  df <- readRDS(file.path("data/rds", x))
  dbWriteTable(myDB, tableName, df)
})

  
  
observeEvent(input$tabs, {
  if (input$tabs == "bigmap") {
    bigMapServer(id = "bigMapMod")
  }
  if (input$tabs == "populations") {
    popServer(id = "populationMod")
  }
  if (input$tabs == "social") {
    socialServer(id = "socialMod", reactive(input$closeModal))
  }
    })
}

shinyApp(ui, server)

