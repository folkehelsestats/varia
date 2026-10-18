# Word cloud display app -----------------------------------------------------
# Run from this folder with: shiny::runApp()
# Required packages: shiny, highcharter, googlesheets4, wordcloud, wordcloud2,
#                    RColorBrewer, readxl

library(shiny)
library(highcharter)
library(googlesheets4)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(readxl)
library(data.table)
library(stringdist)

hdir_colors <- c(
  "#025169", "#0069E8",
  "#7C145C", "#C68803",
  "#047FA4", "#38A389",
  "#FFBF00", "#366558",
  "#BF78DE", "#212121",
  "#6996CE", "#767676"
)

source(file.path("functions", "normalize-words.R"))
source(file.path("functions", "helpers.R"))

## App
## -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://code.highcharts.com/modules/wordcloud.js"),
    tags$style(HTML("
      .viewer-controls { margin: 12px 0 8px 0; }
      .viewer-status { margin-left: 12px; font-weight: 600; }
      .cloud-panel { padding-top: 15px; }
      .upload-row { display: flex; align-items: flex-start; gap: 10px; margin-bottom: 12px; }
      .upload-row .shiny-input-container { margin-bottom: 0 !important; }
      .upload-row .form-group { margin-bottom: 0 !important; }
      .upload-row .progress { margin-bottom: 0; }
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
        downloadButton("download_words", "Last ned orddata"),
        span(class = "viewer-status", textOutput("viewer_status", inline = TRUE))
      ),
      div(
        class = "upload-row",
        uiOutput("upload_ui", inline = TRUE),
        actionButton("use_google", "Reaktiver autogenerator", class = "btn-default")
      ),
      tabsetPanel(
        id = "cloud_view",
        tabPanel(
          "highdir",
          div(class = "cloud-panel", highchartOutput("wordcloud", height = "70vh"))
        ),

        tabPanel(
          "alt01",
          div(class = "cloud2-panel",
              wordcloud2::wordcloud2Output(
                "r_wordcloud2",
                height = "70vh",
                width = "100%"
              ))
        ),
        tabPanel(
          "alt02",
          div(class = "cloud-panel",
              plotOutput("r_wordcloud", height = "70vh"),
              downloadButton("download_r_wordcloud", "Last ned ordsky (PNG)"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  updates_enabled <- reactiveVal(TRUE)
  cached_words <- reactiveVal(empty_words)
  viewer_message <- reactiveVal("Oppdatering pågår")
  load_message <- reactiveVal("")

  # "google" = live Google Sheet, "upload" = uploaded file (sheet is not read)
  data_source <- reactiveVal("google")
  # Changing this re-creates the file input so the same file can be re-uploaded
  upload_reset <- reactiveVal(0)

  refresh_from_google <- function() {
    tryCatch({
      cached_words(get_word_counts())
      load_message("")
    }, error = function(e) {
      load_message(paste("Refresh failed:", conditionMessage(e)))
    })
  }

  # Load once at startup so the chart can appear immediately.
  tryCatch(
    cached_words(get_word_counts()),
    error = function(e) {
      load_message(paste("Initial read failed:", conditionMessage(e)))
    }
  )

  output$upload_ui <- renderUI({
    upload_reset()
    fileInput(
      "upload_file",
      label = NULL,
      accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls"),
      buttonLabel = "Last opp CSV/Excel",
      placeholder = "Ord og frekvens (kolonne 1 og 2)",
      width = "360px"
    )
  })

  output$viewer_status <- renderText({
    paste(viewer_message(), load_message())
  })

  # Poll the shared sheet every 3 seconds, but only while the Google Sheet is
  # the active source and live updates are enabled.
  observe({
    invalidateLater(3000, session)
    if (data_source() != "google" || !updates_enabled()) return()
    refresh_from_google()
  })

  observeEvent(input$pause_updates, {
    if (data_source() != "google") return()
    updates_enabled(FALSE)
    viewer_message("Pause - ingen oppdatering")
  })

  observeEvent(input$resume_updates, {
    if (data_source() != "google") return()
    updates_enabled(TRUE)
    viewer_message("Oppdatering fortsetter")
    # Refresh immediately on resume rather than waiting for the next poll.
    refresh_from_google()
  })

  # Upload: stop using the Google Sheet and show the uploaded data instead.
  observeEvent(input$upload_file, {
    req(input$upload_file)
    f <- input$upload_file

    result <- tryCatch(
      read_uploaded_words(f$datapath, f$name),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      showNotification(
        paste("Kunne ikke lese filen:", conditionMessage(result)),
        type = "error",
        duration = 10
      )
      return()
    }

    data_source("upload")
    cached_words(result)
    load_message("")
    viewer_message(paste0("Viser opplastet fil: ", f$name, " (", nrow(result), " ord)"))
  })

  # Go back to the live Google Sheet.
  observeEvent(input$use_google, {
    data_source("google")
    updates_enabled(TRUE)
    upload_reset(upload_reset() + 1)
    viewer_message("Oppdatering fortsetter")
    refresh_from_google()
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
#       highcharter::hc_title(text = "Folkehelsestatistikk") |>
      highcharter::hc_add_series(
        data = highcharter::list_parse(freq),
        type = "wordcloud",
        name = "Ord",
        colors = hdir_colors,
        spiral = "archimedean"
      ) |>
      highcharter::hc_credits(enabled = FALSE) |>
      highcharter::hc_exporting(enabled = TRUE) |>
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

  # Highcharts provides its own browser-side export menu (PNG, JPEG, PDF, SVG).
  # The CSV download is useful for saving the exact counts behind either view.
  output$download_words <- downloadHandler(
    filename = function() paste0("word-counts-", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(cached_words(), file, row.names = FALSE)
  )

  draw_r_wordcloud <- function() {
    freq <- cached_words()
    if (nrow(freq) == 0) {
      plot.new()
      text(0.5, 0.5, "Ingen ord sendt ennå", cex = 1.3)
      return(invisible(NULL))
    }
    wordcloud::wordcloud(
      words = freq$name,
      freq = freq$weight,
      min.freq = 1,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.15,
      colors = hdir_colors
#       colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
  }

  output$r_wordcloud2 <- renderWordcloud2({
    freq <- cached_words()

    if (nrow(freq) == 0) {
      return(NULL)
    }

    wordcloud2::wordcloud2(
      data = freq,
      size = 1,
      color = hdir_colors,
      backgroundColor = "white"
    )
  })

  output$r_wordcloud <- renderPlot({
    draw_r_wordcloud()
  }, res = 96)

  output$download_r_wordcloud <- downloadHandler(
    filename = function() paste0("wordcloud-", Sys.Date(), ".png"),
    content = function(file) {
      grDevices::png(file, width = 1600, height = 1100, res = 150)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_r_wordcloud()
    }
  )
}

shinyApp(ui = ui, server = server)
