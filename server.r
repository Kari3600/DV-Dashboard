library(shiny)
library(httr)
library(jsonlite)
library(dotenv)
library(tidyr)
library(dplyr)
library(DT)
dotenv::load_dot_env()

api_key <- Sys.getenv("API_KEY")

print(paste("Using API key:", api_key))

shinyServer(
  function(input, output, session) {
    static_tanks_df <- reactiveVal(NULL)

    observe({
      res <- GET("https://api.worldoftanks.eu/wot/encyclopedia/vehicles/", query = list(
        application_id = api_key
      ))

      if (res$status_code == 200) {
        json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
        if (json$status == "ok") {
          df <- do.call(rbind, json$data)
          df <- as.data.frame(df)
          static_tanks_df(df)
        }
      }
    })

    observeEvent(input$search_trigger, {
      search_term <- input$search_trigger

      res <- tryCatch({
        GET("https://api.worldoftanks.eu/wot/account/list/",
            query = list(application_id = api_key, search = search_term, limit = 5))
      }, error = function(e) return(NULL))

      if (is.null(res) || res$status_code != 200) return()


      json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

      if (json$status == "ok") {
        players <- as.data.frame(json$data, stringsAsFactors = FALSE)
        choices <- setNames(players$account_id, players$nickname)
        updateSelectizeInput(session, "searchme", choices = choices, server = TRUE)
      }
    })

    player_tank_stats <- reactive({
      req(input$searchme)
      account_id <- input$searchme

      print("Account id")
      print(account_id)

      res <- GET("https://api.worldoftanks.eu/wot/account/tanks/", query = list(
        application_id = api_key,
        account_id = account_id
      ))

      if (res$status_code != 200) return(NULL)

      json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

      if (json$status != "ok") return(NULL)

      df <- json$data[[as.character(account_id)]]
      if (is.null(df)) return(NULL)

      df_unnested <- df %>%
        unnest(cols = c(statistics))

      df_unnested
    })

    print("Rendering table")

    output$tank_stats <- DT::renderDataTable({
      req(static_tanks_df(), player_tank_stats())

      roman_numerals <- c("I","II","III","IV","V","VI","VII","VIII","IX","X")

      merge(player_tank_stats(), static_tanks_df(), by = "tank_id") %>%
        mutate(
          tier = as.numeric(unlist(tier)),
          name = as.character(name),
          type = as.character(type)
        ) %>%
        mutate(
          wr = round(100 * wins / battles, 1),
          tier_num = tier,
          tier = roman_numerals[tier],
          type = case_when(
            type == "lightTank" ~ '<img src="type_icons/lightTank.png" height="20">',
            type == "mediumTank" ~ '<img src="type_icons/mediumTank.png" height="20">',
            type == "heavyTank" ~ '<img src="type_icons/heavyTank.png" height="20">',
            type == "AT-SPG" ~ '<img src="type_icons/AT-SPG.png" height="20">',
            type == "SPG" ~ '<img src="type_icons/SPG.png" height="20">',
            TRUE ~ type
          )
        ) %>%
        select(name, tier, tier_num, type, battles, wins, wr) %>%
        DT::datatable(
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            columnDefs = list(
              list(visible = FALSE, targets = 2),
              list(orderData = 2, targets = 1)
            )
          ),
          rownames = FALSE,
          escape = FALSE,
          colnames = c("Name", "Tier", "tier_num", "Type", "Battles Played", "Wins", "Winratio")
        )
    })
  }
)