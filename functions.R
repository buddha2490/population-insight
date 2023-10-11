# Functions
customSentence <- function(numItems, type, message) {
  paste("Bug reports and feedback")
}

customSentence_share <- function(numItems, type) {
  paste("Love it? Share it!")
}

dropdownMenuCustom <-  function (..., type = c("messages", "notifications", "tasks"), 
                                 badgeStatus = "primary", icon = NULL, .list = NULL, 
                                 customSentence = "") 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}


colors <- c("#400099", "#24D9D9", "#FFCB00", "#FE9FB8", "#4481FF",
            "#6633AD", "#66E4E4", "#FFD533", "#FEB2C6", "#7CA7FF",
            "#8C66C2", "#91ECEC", "#FFE066", "#FEC5D4", "#A1C0FF",
            "#c5cdfa", "#BDF4F4", "#FFEA99", "#FFD9E3", "#C7D9FF")


