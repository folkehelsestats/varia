# Word cloud display app -----------------------------------------------------
# Run from this folder with: shiny::runApp()
# Required packages: shiny, highcharter, googlesheets4

library(shiny)
library(highcharter)
library(googlesheets4)

# These settings must match the input app.
sheet_id <- "108kPND1ySv8XQss6xt0DYo5kxYsAC1aTZpgxoJUuBuU"
worksheet_name <- "words"
credentials_file <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "service-task-survey.json")

if (!file.exists(credentials_file)) {
  stop(
    "Google service-account JSON was not found. Set GOOGLE_SERVICE_ACCOUNT_JSON ",
    "to its path, or place service-task-survey.json in this app folder."
  )
}
googlesheets4::gs4_auth(path = credentials_file)

get_word_counts <- function() {
  dat <- googlesheets4::read_sheet(ss = sheet_id, sheet = worksheet_name)

  if (!"word" %in% names(dat) || nrow(dat) == 0) {
    return(data.frame(name = character(), weight = numeric()))
  }

  words <- tolower(trimws(as.character(dat$word)))
  words <- words[!is.na(words) & nzchar(words)]

  if (!length(words)) {
    return(data.frame(name = character(), weight = numeric()))
  }

  freq <- as.data.frame(table(words), stringsAsFactors = FALSE)
  names(freq) <- c("name", "weight")
  freq$weight <- as.numeric(freq$weight)
  freq <- freq[order(-freq$weight, freq$name), , drop = FALSE]
  rownames(freq) <- NULL
  freq
}

empty_words <- data.frame(name = character(), weight = numeric())

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://code.highcharts.com/modules/wordcloud.js"),
    tags$style(HTML("
      .viewer-controls { margin: 12px 0 18px 0; }
      .viewer-status { margin-left: 12px; font-weight: 600; }
    "))
  ),
  titlePanel(""),
  fluidRow(
    column(
      width = 12,
      div(
        class = "viewer-controls",
        actionButton("pause_updates", "Pause", class = "btn-warning"),
        actionButton("resume_updates", "Oppdatere", class = "btn-success"),
        span(class = "viewer-status", textOutput("viewer_status", inline = TRUE))
      ),
      highchartOutput("wordcloud", height = "78vh")
    )
  )
)

server <- function(input, output, session) {
  updates_enabled <- reactiveVal(TRUE)
  cached_words <- reactiveVal(empty_words)
  viewer_message <- reactiveVal("Oppdatering fortsetter")
  load_message <- reactiveVal("")

  # Load once at startup so the chart can appear immediately.
  tryCatch(
    cached_words(get_word_counts()),
    error = function(e) {
      load_message(paste("Initial read failed:", conditionMessage(e)))
    }
  )

  output$viewer_status <- renderText({
    paste(viewer_message(), load_message())
  })

  # Poll the shared sheet every 3 seconds while live updates are enabled.
  observe({
    invalidateLater(3000, session)
    if (!updates_enabled()) return()

    tryCatch({
      cached_words(get_word_counts())
      load_message("")
    }, error = function(e) {
      load_message(paste("Refresh failed:", conditionMessage(e)))
    })
  })

  observeEvent(input$pause_updates, {
    updates_enabled(FALSE)
    viewer_message("Pause - ingen oppdatering")
  })

  observeEvent(input$resume_updates, {
    updates_enabled(TRUE)
    viewer_message("Oppdatering pågår")
    # Refresh immediately on resume rather than waiting for the next poll.
    tryCatch({
      cached_words(get_word_counts())
      load_message("")
    }, error = function(e) {
      load_message(paste("Refresh failed:", conditionMessage(e)))
    })
  })

  output$wordcloud <- renderHighchart({
    freq <- cached_words()

    if (nrow(freq) == 0) {
      return(
        highcharter::highchart() |>
          highcharter::hc_title(text = "Ingen ord sendt ennå") |>
          highcharter::hc_credits(enabled = FALSE)
      )
    }

    highcharter::highchart() |>
      highcharter::hc_title(text = "Folkehelsestatistikk") |>
      highcharter::hc_add_series(
        data = highcharter::list_parse(freq),
        type = "wordcloud",
        name = "Ord",
        spiral = "archimedean"
      ) |>
      highcharter::hc_credits(enabled = FALSE) |>
      highcharter::hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = paste0(
          "<div style='padding:5px;'>",
          "<span style='font-size:16px;'><b>{point.name}</b></span><br>",
          "<span>Forekomster: <b>{point.weight}</b></span>",
          "</div>"
        )
      )
  })
}

shinyApp(ui = ui, server = server)
