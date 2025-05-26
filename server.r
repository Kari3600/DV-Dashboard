library(shiny)
library(httr)
library(jsonlite)
library(dotenv)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(ggimage)
library(ggradar)
dotenv::load_dot_env()

api_key <- Sys.getenv("API_KEY")

print(paste("Using API key:", api_key))

type_to_img <- function(name) {
  paste()
}

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

    debounced_search <- debounce(reactive(input$search_text), 500)

    players_found <- reactive({
      player_name <- debounced_search()
      req(player_name)
      res <- tryCatch({
        GET("https://api.worldoftanks.eu/wot/account/list/",
            query = list(application_id = api_key, search = player_name, limit = 5))
      }, error = function(e) return(NULL))

      if (is.null(res) || res$status_code != 200) return(data.frame())


      json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

      if (json$status != "ok") return(data.frame())

      as.data.frame(json$data)
    })

    output$search_results <- renderUI({
      players <- players_found()
      if (nrow(players) == 0) return(NULL)

      lapply(1:nrow(players), function(i) {
        player <- players[i, ]
        actionButton(
          inputId = paste0("player_select_", player$account_id),
          label = paste0(player$nickname),
          class = "btn-primary",
          style = "margin: 5px;"
        )
      })
    })

    player_id <- reactiveVal(NULL)

    observe({
      players <- players_found()
      req(nrow(players) > 0)

      lapply(players$account_id, function(id) {
        observeEvent(input[[paste0("player_select_", id)]], {
          player_id(id)
        })
      })
    })

    player_tank_stats <- reactive({
      req(player_id())

      res <- GET("https://api.worldoftanks.eu/wot/account/tanks/", query = list(
        application_id = api_key,
        account_id = player_id()
      ))

      if (res$status_code != 200) return(NULL)

      json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

      if (json$status != "ok") return(NULL)

      df <- json$data[[as.character(player_id())]]
      if (is.null(df)) return(NULL)

      df %>%
        unnest(cols = c(statistics)) %>%
        merge(static_tanks_df(), by = "tank_id") %>%
        mutate(
          tier = as.numeric(unlist(tier)),
          name = as.character(name),
          type = as.character(type)
        ) %>%
        mutate(
          wr = round(100 * wins / battles, 1),
          tier_roman = as.character(as.roman(tier)),
          type_icon = paste('<img src="type_icons/', type, '.png" height="20">', sep = "")
        ) %>%
        select(tank_id, name, tier, tier_roman, type, type_icon, battles, wins, wr)
    })

    player_stats <- reactive({
      req(player_id())

      res <- GET("https://api.worldoftanks.eu/wot/account/info/", query = list(
        application_id = api_key,
        account_id = player_id()
      ))

      if (res$status_code != 200) return(NULL)

      json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

      if (json$status != "ok") return(NULL)

      data <- as.list(json$data[[1]])

      data
    })

    print("Rendering table")

    output$tank_stats <- DT::renderDataTable({
      req(player_tank_stats())
      DT::datatable(
        player_tank_stats(),
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = 0),
            list(visible = FALSE, targets = 4),
            list(visible = FALSE, targets = 2),
            list(orderData = 2, targets = 3)
          )
        ),
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        colnames = c("tank_id", "Name", "tier_num", "Tier", "type_name", "Type", "Battles Played", "Wins", "Winratio")
      )
    })

    output$tier_dist <- renderPlot({
      req(player_tank_stats())
      player_tank_stats() %>%
        group_by(tier) %>%
        summarise(total_battles = sum(battles), .groups = "drop") %>%
        mutate(tier_roman = as.character(as.roman(tier))) %>%
        ggplot(aes(x = reorder(tier_roman, tier), y = total_battles)) +
        geom_col(fill = "darkorange") +
        geom_text(
          aes(label = tier_roman),
          size = 5,
          fontface = "bold",
          color = "white"
        ) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "white", size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(color = "white", size = 10),
          plot.margin = margin(10, 10, 10, 10)
        )
    }, bg = "transparent")

    output$type_dist <- renderPlot({
      req(player_tank_stats())
      player_tank_stats() %>%
        group_by(type) %>%
        summarise(total_battles = sum(battles), .groups = "drop") %>%
        mutate(type_icon = paste("www/type_icons/", type, ".png", sep = "")) %>%
        ggplot(aes(x = type, y = total_battles)) +
        geom_col(fill = "darkorange") +
        geom_image(
          aes(image = type_icon), 
          size = 0.05,
          by = "width",
          asp = 1.5
        ) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "white", size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(color = "white", size = 10),
          plot.margin = margin(10, 10, 10, 10)
        )
    }, bg = "transparent")

    output$ranking_points <- renderText({
      req(player_stats())

      stats <- player_stats()
      paste(
        "Ranking:",stats$global_rating,"\n",
        "Account created:",as.Date(as.POSIXct(stats$created_at)),"\n",
        "Last played:",as.Date(as.POSIXct(stats$last_battle_time))
      )
    })

    output$winratio <- renderPlot({
      req(player_stats())

      stats <- player_stats()$statistics$all

      data.frame(
        Type = c(
          "WINS",
          "DRAWS",
          "LOSSES"
        ),
        Value = c(
          stats$wins,
          stats$draws,
          stats$losses
        )
      ) %>%
        ggplot(aes(x = "", y = Value, fill = Type)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste(Type, "\n", Value)), 
                  position = position_stack(vjust = 0.5),
                  color = "white",
                  size = 3) +
        scale_fill_brewer(palette = "Dark2") +
        theme_void() +
        ggtitle(paste("Battles played:",stats$battles)) +
        theme(
          plot.title = element_text(color = "white", size = 16, hjust = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "white", size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(color = "white", size = 10),
          plot.margin = margin(10, 10, 10, 10)
        )
    }, bg = "transparent")

    selected_tank_stats <- reactive({
      req(input$tank_stats_rows_selected)
      selected_row <- input$tank_stats_rows_selected

      tank_id <- player_tank_stats()[selected_row, ]$tank_id

      print("Tank id")
      print(tank_id)

      res <- GET("https://api.worldoftanks.eu/wot/tanks/stats/", query = list(
        application_id = api_key,
        account_id = player_id(),
        tank_id = tank_id
      ))

      if (res$status_code != 200) return(NULL)

      json <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

      if (json$status != "ok") return(NULL)

      as.list(json$data[[1]])
    })

    output$tank_stats_damage <- renderPlot({
      req(selected_tank_stats())

      stats <- as.list(selected_tank_stats()$all)

      df <- data.frame(
        Type = c(
          "DEALT",
          "RECEIVED",
          "BLOCKED",
          "SPOTTED",
          "TRACK ASSISTED",
          "STUN ASSISTED"
        ),
        Value = c(
          stats$damage_dealt / stats$battles,
          stats$damage_received / stats$battles,
          stats$avg_damage_blocked,
          stats$avg_damage_assisted_radio,
          stats$avg_damage_assisted_track,
          stats$avg_damage_assisted_stun
        )
      )

      max_y <- ceiling(max(df$Value) / 250) * 250

      plot <- df %>%
        #mutate(Value = Value / max(Value)) %>%
        pivot_wider(names_from = Type, values_from = Value) %>%
        mutate(Player = "Tank 1") %>%
        select(Player, everything()) %>%
        ggradar(
                values.radar = c("0", max_y/2, max_y),
                grid.min = 0,
                grid.mid = max_y/2,
                grid.max = max_y,
                group.line.width = 1.5,
                group.point.size = 4,
                fill = TRUE,
                background.circle.colour = "gray10",
                gridline.mid.colour = "gray40",
                axis.label.size = 3.5,
                group.colours = "darkorange",
                legend.position = "none") +
        theme(
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA)
        )

      plot$layers[[1]]$aes_params <- c(plot$layers[[1]]$aes_params, colour = "white")
      plot$layers[[5]]$aes_params <- c(plot$layers[[5]]$aes_params, colour = "white")
      plot$layers[[6]]$aes_params <- c(plot$layers[[6]]$aes_params, colour = "white")
      plot$layers[[12]]$aes_params <- c(plot$layers[[12]]$aes_params, colour = "white")
      plot$layers[[13]]$aes_params <- c(plot$layers[[13]]$aes_params, colour = "white")
      plot$layers[[14]]$aes_params <- c(plot$layers[[14]]$aes_params, colour = "white")

      plot
    }, bg = "transparent")
  }
)