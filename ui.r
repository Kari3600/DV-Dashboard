library(shiny)
library(DT)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("WoT Stats"),
  sidebarLayout(
    sidebarPanel(
      textInput("search_text", "Search player"),
      uiOutput("search_results")
    ),
    mainPanel(
      fluidRow(
        column(6, tags$h3(
          textOutput("ranking_points"),
          style = "white-space: pre-wrap; text-align: center; font-size: 24px; color: white;"
        )),
        column(6, plotOutput("winratio"))
      ),
      fluidRow(
        column(6, plotOutput("tier_dist")),
        column(6, plotOutput("type_dist"))
      ),
      DT::dataTableOutput("tank_stats"),
      plotOutput("tank_stats_damage"),
    )
  )
))