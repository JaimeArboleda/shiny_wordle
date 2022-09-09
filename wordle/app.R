#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.tree)
library(shinyjs)
library(plyr)
library(base)
library(markdown)
source("scripts/utils.R")

assistant_files <- list.files("trees", full.names = TRUE) 
assistant_names <- (assistant_files %>% str_match("/(.+)\\."))[, 2]
names(assistant_files) <- assistant_names
assistants <- load_assistants(assistant_files)

scores <- read_csv("data/wordle_puntos.csv") %>% mutate(score = puntos, player = persona, .keep = "unused")
scores <- filter(scores, player != "Eva")
scores[["type"]] <- "human"
historic_words <- read_csv("data/wordle_palabras.csv") %>% mutate(word = tolower(palabra), .keep = "unused")
assistant_scores <- read_csv("data/assistant_scores.csv", col_types = "ccci")
assistant_scores[["type"]] <- "bot"
all_words <- read_csv("data/words.csv", col_names = "word")
all_letters <- tibble(letter = str_split("abcdefghijklmnñopqrstuvwxyz", "")[[1]])
scores <- left_join(scores, historic_words, by = "dia")
all_scores <- bind_rows(scores, assistant_scores) %>% mutate(day = dia, .keep = "unused")
human_players <- unique(scores[["player"]])
all_days <- range(scores[["dia"]])
starter_entropies <- read_csv("data/entropies.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Wordle"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estadísticas históricas", tabName = "historic_stats"),
      menuItem("Estadísticas del juego", tabName = "game_stats"),
      menuItem("Asistente de juego", tabName = "assistant"),
      menuItem("Información sobre el algoritmo", tabName = "assistant_info")
    )
  ), 
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "historic_stats",
        fluidRow(
          box(
            title = "Controles",
            checkboxGroupInput("bots", "Bots: ",
                               assistant_names),
            checkboxGroupInput("humans", "Humanos:",
                               human_players,
                               selected = c("M.Angel")),
            textInput("word_filter", "Filtra palabras usando expresiones regulares"),
            sliderInput("day_slider", label = "Selecciona rango de días", 
                        min = all_days[1], max = all_days[2], value = all_days),
            h3("Mostrando estadísticas para:"),
          ),
          box(
            title = "Resumen estadístico",
            textOutput("stats_summary"),
            tableOutput("table_stats")
          )
        ),
        fluidRow(
          box(plotOutput("plot_stats"), width = 12)
        )
      ),
      tabItem(
        tabName = "game_stats",
        h2("Estadísticas del juego"),
        fluidRow(
          box(
            width=12,
            title = "Análisis de las letras",
            plotOutput("letters")
          )
        ),
        fluidRow(
          box(
            width=12,
            title = "Análisis de posibles aperturas",
            plotOutput("openings")
          )
        )
      ),
      tabItem(
        tabName = "assistant",
        h2("Asistente de Juego"),
        selectInput("selected_assistant", "Selecciona una apertura:", assistant_names),
        actionButton("play", "Jugar"),
        br(),
        textOutput("play_info"),
        p("Teclea esta palabra en la aplicación de wordle y pulsa en las letras hasta introducir los colores del resultado obtenido"),
        htmlOutput("game_summary"),
        span(
          actionButton("letter1", ""),
          actionButton("letter2", ""),
          actionButton("letter3", ""),
          actionButton("letter4", ""),
          actionButton("letter5", ""),
        ),
        br(),
        actionButton("inform", "Enviar resultado")
      ),
      tabItem(
        tabName = "assistant_info",
        includeMarkdown("docs/algorithm.md")
      )
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    players_to_show <- c(input$bots, input$humans)
    if (is_null(players_to_show)) {
      return (tibble(freq=0, score=0, player=""))
    }
    filter_words <- input$word_filter
    if (filter_words == "") {
      filter_words <- "."
    }
    first_day <- input$day_slider[1]
    last_day <- input$day_slider[2]
    all_scores %>% 
      filter(player %in% players_to_show) %>% 
      filter(between(day, first_day, last_day)) %>% 
      filter(str_detect(word, filter_words))
  })
  
  stats_data <- reactive({
    filtered_data() %>% 
      group_by(player, score) %>%
      dplyr::summarise(
        score = cur_group()[["score"]],
        freq = 100 * n() / dim(all_scores[all_scores["player"] == cur_group()[["player"]], ])[1]
      )
  })
  
  letters_data <- reactive({
    all_letters %>% 
      mutate(letra = letter) %>% 
      group_by(letra) %>% 
      dplyr::summarize(frecuencia = sum(str_detect(all_words[["word"]], cur_group()[["letra"]])) / length(all_words[["word"]])) 
  })
  openings_data <- reactive({
    starter_entropies %>% 
      mutate(palabra = word) %>% 
      slice_min(entropy, n=20)
  })
  
  output$plot_stats <- renderPlot({
    ggplot(stats_data(), aes(x=score, y=freq, fill=player)) + 
      geom_bar(stat="identity", position=position_dodge())
  })
  
  output$stats_summary <- renderText({
    paste(
      "Número de partidas analizadas: ",
      length(unique(filtered_data()[["word"]]))
    )
  })
  
  output$table_stats <- renderTable({
    average_score <- filtered_data() %>% 
      mutate(jugador = player, .keep = "unused") %>% 
      mutate(score = as.numeric(str_replace(score, "X", "7"))) %>% 
      group_by(jugador) %>% 
      dplyr::summarize(puntuacion_media = sum(score) / n())
    filtered_data() %>% 
      mutate(jugador = player, .keep = "unused") %>% 
      group_by(jugador, score) %>% 
      dplyr::summarize(partidas = n()) %>% 
      pivot_wider(names_from = score, values_from = partidas) %>% 
      left_join(average_score, by = "jugador")
  })
  
  output$letters <- renderPlot({
    ggplot(letters_data(), aes(x=letra, y=frecuencia)) + 
      geom_bar(stat="identity")
  })
  output$openings <- renderPlot({
    ggplot(openings_data(), aes(x=palabra, y=entropy)) + 
      geom_bar(stat="identity")
  })
  
  v <- reactiveValues(
    play_info = "",
    play_word = "",
    color_values = c(0, 0, 0, 0, 0),
    play_summary = character(0),
    current_node = NULL
  )
  
  output$play_info <- renderText({
    v$play_info
  })
  
  output$game_summary <- renderUI({
    if (length(v$play_summary) == 0) {
      HTML("<br>")
    } else {
      HTML(
        paste(
          "<p>Palabras jugadas hasta el momento: </p>",
          "<ul>",
          paste(
            "<li>",
            toupper(v$play_summary),
            "</li>",
            collapse=""
          ),
          "</ul>"
        )
      )
    }
  })
  
  observeEvent(
    input$inform,
    {
      outcome <- mapvalues(v$color_values, c(0, 1, 2), c("grey", "yellow", "green"))
      outcome <- encode_outcome(outcome)
      v$current_node <- v$current_node[[outcome]]
      if (is_null(v$current_node)){
        v$play_info <- "No se ha encontrado ninguna palabra. "
      } else {
        v$play_info <- paste("Número esperado de movimientos ", v$current_node$expected_moves)
        v$play_word <- v$current_node$move
        v$play_summary[length(v$play_summary) + 1] <- v$play_word
        v$color_values = c(0, 0, 0, 0, 0)
        runjs('document.getElementById("letter1").style.backgroundColor = "grey";')
        runjs('document.getElementById("letter2").style.backgroundColor = "grey";')
        runjs('document.getElementById("letter3").style.backgroundColor = "grey";')
        runjs('document.getElementById("letter4").style.backgroundColor = "grey";')
        runjs('document.getElementById("letter5").style.backgroundColor = "grey";')
        updateActionButton(session, "letter1", label = toupper(substring(v$play_word, 1, 1)))
        updateActionButton(session, "letter2", label = toupper(substring(v$play_word, 2, 2)))
        updateActionButton(session, "letter3", label = toupper(substring(v$play_word, 3, 3)))
        updateActionButton(session, "letter4", label = toupper(substring(v$play_word, 4, 4)))
        updateActionButton(session, "letter5", label = toupper(substring(v$play_word, 5, 5)))
      }
    }
  )
  
  observeEvent(
    input$play, 
    {
      v$current_node <- assistants[[input$selected_assistant]]
      v$play_info <- paste("Número esperado de movimientos ", v$current_node$expected_moves)
      v$play_word <- v$current_node$move
      v$play_summary <- v$play_word
      v$color_values = c(0, 0, 0, 0, 0)
      runjs('document.getElementById("letter1").style.backgroundColor = "grey";')
      runjs('document.getElementById("letter2").style.backgroundColor = "grey";')
      runjs('document.getElementById("letter3").style.backgroundColor = "grey";')
      runjs('document.getElementById("letter4").style.backgroundColor = "grey";')
      runjs('document.getElementById("letter5").style.backgroundColor = "grey";')
      updateActionButton(session, "letter1", label = toupper(substring(v$play_word, 1, 1)))
      updateActionButton(session, "letter2", label = toupper(substring(v$play_word, 2, 2)))
      updateActionButton(session, "letter3", label = toupper(substring(v$play_word, 3, 3)))
      updateActionButton(session, "letter4", label = toupper(substring(v$play_word, 4, 4)))
      updateActionButton(session, "letter5", label = toupper(substring(v$play_word, 5, 5)))
    }
  )
  observeEvent(
    input$letter1,
    {
      v$color_values <- (v$color_values + c(1, 0, 0, 0, 0)) %% 3
      if (v$color_values[1] == 0){
        runjs('document.getElementById("letter1").style.backgroundColor = "grey";')
      } else if (v$color_values[1] == 1) {
        runjs('document.getElementById("letter1").style.backgroundColor = "yellow";')
      } else {
        runjs('document.getElementById("letter1").style.backgroundColor = "green";')
      }
    }
  )
  observeEvent(
    input$letter2,
    {
      v$color_values <- (v$color_values + c(0, 1, 0, 0, 0)) %% 3
      if (v$color_values[2] == 0){
        runjs('document.getElementById("letter2").style.backgroundColor = "grey";')
      } else if (v$color_values[2] == 1) {
        runjs('document.getElementById("letter2").style.backgroundColor = "yellow";')
      } else {
        runjs('document.getElementById("letter2").style.backgroundColor = "green";')
      }
    }
  )
  observeEvent(
    input$letter3,
    {
      v$color_values <- (v$color_values + c(0, 0, 1, 0, 0)) %% 3
      if (v$color_values[3] == 0){
        runjs('document.getElementById("letter3").style.backgroundColor = "grey";')
      } else if (v$color_values[3] == 1) {
        runjs('document.getElementById("letter3").style.backgroundColor = "yellow";')
      } else {
        runjs('document.getElementById("letter3").style.backgroundColor = "green";')
      }
    }
  )
  
  observeEvent(
    input$letter4,
    {
      v$color_values <- (v$color_values + c(0, 0, 0, 1, 0)) %% 3
      if (v$color_values[4] == 0){
        runjs('document.getElementById("letter4").style.backgroundColor = "grey";')
      } else if (v$color_values[4] == 1) {
        runjs('document.getElementById("letter4").style.backgroundColor = "yellow";')
      } else {
        runjs('document.getElementById("letter4").style.backgroundColor = "green";')
      }
    }
  )
  observeEvent(
    input$letter5,
    {
      v$color_values <- (v$color_values + c(0, 0, 0, 0, 1)) %% 3
      if (v$color_values[5] == 0){
        runjs('document.getElementById("letter5").style.backgroundColor = "grey";')
      } else if (v$color_values[5] == 1) {
        runjs('document.getElementById("letter5").style.backgroundColor = "yellow";')
      } else {
        runjs('document.getElementById("letter5").style.backgroundColor = "green";')
      }
    }
  )
}

shinyApp(ui, server)