library(shiny)
library(DT)

shinyUI(fluidPage(
  tags$head(
    tags$script(HTML("Shiny.setInputValue('search_trigger', 0);"))
  ),
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      h3("Side panel"),
      selectizeInput(
        inputId = "searchme",
        label = "Search for a player",
        choices = NULL,
        options = list(
          placeholder = "Type player name...",
          onType = I("
            function(str) {
              if (str.length >= 3) {
                Shiny.setInputValue('search_trigger', str, {priority: 'event'});
              }
            }
          ")
        )
      )
    ),
    mainPanel(
      h3("Main panel"),
      DT::dataTableOutput("tank_stats")
    )
  )
))