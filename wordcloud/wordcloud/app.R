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

empty_words <- data.frame(name = character(), weight = numeric())

# Clean spelling error or similar words ----------------------------------------
# A simple attempt to find words that are similar and group together
###############################################################################
# Common prefix ratio
###############################################################################

common_prefix_ratio <- function(a, b) {

  a_chars <- strsplit(a, "", fixed = TRUE)[[1]]
  b_chars <- strsplit(b, "", fixed = TRUE)[[1]]

  n <- min(length(a_chars), length(b_chars))

  if (n == 0) {
    return(0)
  }

  match_len <- 0L

  for (i in seq_len(n)) {

    if (a_chars[i] != b_chars[i]) {
      break
    }

    match_len <- i
  }

  match_len / n
}

###############################################################################
# Main function
###############################################################################

normalize_words_fast <- function(
    words,
    max_dist = 0.20,
    prefix_ratio_threshold = 0.80,
    prefix_length = 4,
    short_word_threshold = 0.10,
    min_cluster_freq = 2,
    distance_method = "osa",
    raw = FALSE
) {

  ###########################################################################
  # Frequency table
  ###########################################################################

  freq_dt <- data.table(
    word = tolower(trimws(words))
  )[
    ,
    .(freq = .N),
    by = word
  ]

  ###########################################################################
  # Most frequent words first
  ###########################################################################

  setorder(freq_dt, -freq)

  ###########################################################################
  # Blocking key
  ###########################################################################

  freq_dt[
    ,
    block := substr(
      word,
      1,
      pmin(prefix_length, nchar(word))
    )
  ]

  ###########################################################################
  # Initialize
  ###########################################################################

  freq_dt[
    ,
    `:=`(
      cluster = NA_integer_,
      assigned = FALSE
    )
  ]

  cluster_id <- 1L

  ###########################################################################
  # Process block-by-block
  ###########################################################################

  for (current_block in unique(freq_dt$block)) {

    rows <- which(freq_dt$block == current_block)

    block_dt <- copy(freq_dt[rows])

    for (i in seq_len(nrow(block_dt))) {

      if (block_dt$assigned[i]) {
        next
      }

      #####################################################################
      # Allow only sufficiently frequent words to become centers
      #####################################################################

      if (block_dt$freq[i] < min_cluster_freq) {
        next
      }

      center_word <- block_dt$word[i]

      remaining <- which(
        !block_dt$assigned
      )

      candidates <- block_dt$word[
        remaining
      ]

      #####################################################################
      # Relative edit distance
      #####################################################################

      dist <- stringdist(
        center_word,
        candidates,
        method = distance_method
      )

      max_len <- pmax(
        nchar(center_word),
        nchar(candidates)
      )

      rel_dist <- dist / max_len

      threshold <- ifelse(
        max_len <= 4,
        short_word_threshold,
        max_dist
      )

      #####################################################################
      # Prefix similarity
      #####################################################################

      prefix_ratio <- vapply(
        candidates,
        function(x) {
          common_prefix_ratio(
            center_word,
            x
          )
        },
        numeric(1)
      )

      #####################################################################
      # Cluster membership
      #####################################################################

      members <- remaining[
        rel_dist <= threshold |
        prefix_ratio >= prefix_ratio_threshold
      ]

      block_dt[
        members,
        `:=`(
          assigned = TRUE,
          cluster = cluster_id
        )
      ]

      cluster_id <- cluster_id + 1L
    }

    #######################################################################
    # Attach remaining rare words
    #######################################################################

    unassigned <- which(
      is.na(block_dt$cluster)
    )

    assigned <- which(
      !is.na(block_dt$cluster)
    )

    if (
      length(unassigned) > 0 &&
      length(assigned) > 0
    ) {

      centers <- block_dt$word[assigned]

      for (u in unassigned) {

        candidate <- block_dt$word[u]

        dist <- stringdist(
          candidate,
          centers,
          method = distance_method
        )

        rel_dist <- dist / pmax(
          nchar(candidate),
          nchar(centers)
        )

        prefix_ratio <- vapply(
          centers,
          function(x) {
            common_prefix_ratio(
              candidate,
              x
            )
          },
          numeric(1)
        )

        hits <- which(
          rel_dist <= max_dist |
          prefix_ratio >= prefix_ratio_threshold
        )

        if (length(hits) > 0) {

          best <- hits[
            which.min(rel_dist[hits])
          ]

          block_dt$cluster[u] <-
            block_dt$cluster[
              assigned[best]
            ]
        }
      }
    }

    #######################################################################
    # Remaining become own clusters
    #######################################################################

    still_unassigned <- which(
      is.na(block_dt$cluster)
    )

    if (length(still_unassigned) > 0) {

      for (u in still_unassigned) {

        block_dt$cluster[u] <- cluster_id

        cluster_id <- cluster_id + 1L
      }
    }

    freq_dt[rows] <- block_dt
  }

  ###########################################################################
  # Canonical word = most frequent word in cluster
  ###########################################################################

  canonical_lookup <- freq_dt[
    order(cluster, -freq)
  ][
    ,
    .SD[1],
    by = cluster
  ][
    ,
    .(
      cluster,
      canonical = word
    )
  ]

  freq_dt <- merge(
    freq_dt,
    canonical_lookup,
    by = "cluster"
  )

  ###########################################################################
  # Apply lookup
  ###########################################################################

  mapping <- freq_dt[
   ,
     .(
       word,
       canonical
     )
  ]

  result <- mapping[
    data.table(
      raw = words,
      word = tolower(trimws(words))
    ),
    on = "word"
  ]

  if (raw) {

    return(
      result[
       ,
         .(
           raw,
           canonical
         )
      ]
    )

  } else {

    return(result$canonical)

  }

}

# Google Sheet source ----------------------------------------------------------
get_word_counts <- function() {
  dat <- googlesheets4::read_sheet(ss = sheet_id, sheet = worksheet_name)

  if (!"word" %in% names(dat) || nrow(dat) == 0) {
    return(empty_words)
  }

  ## words <- tolower(trimws(as.character(dat$word)))
  words <- normalize_words_fast(as.character(dat$word))
  words <- words[!is.na(words) & nzchar(words)]

  if (!length(words)) {
    return(empty_words)
  }

  freq <- as.data.frame(table(words), stringsAsFactors = FALSE)
  names(freq) <- c("name", "weight")
  freq$weight <- as.numeric(freq$weight)
  freq <- freq[order(-freq$weight, freq$name), , drop = FALSE]
  rownames(freq) <- NULL
  freq
}

# Uploaded file source ---------------------------------------------------------
# Reads a CSV (comma, semicolon or tab separated; UTF-8 or latin1) as text.
read_csv_flex <- function(path) {
  lines <- readLines(path, warn = FALSE)
  enc <- if (validUTF8(paste(lines, collapse = "\n"))) "UTF-8-BOM" else "latin1"
  first <- lines[1]
  seps <- c(";", ",", "\t")
  counts <- vapply(
    seps,
    function(s) lengths(regmatches(first, gregexpr(s, first, fixed = TRUE))),
    integer(1)
  )
  sep <- seps[which.max(counts)]

  utils::read.csv(
    path,
    sep = sep,
    fileEncoding = enc,
    colClasses = "character",
    check.names = FALSE,
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )
}

to_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- gsub("[[:space:]]", "", as.character(x))
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

# Turns an uploaded table into the same data.frame(name, weight) that the
# Google Sheet produces. Column 1 = word, column 2 = frequency by default;
# common header names (word/ord, freq/antall/weight ...) are recognised.
# A single-column file is treated as a raw word list and counted.
read_uploaded_words <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))

  dat <- switch(
    ext,
    csv = , txt = , tsv = read_csv_flex(path),
    xlsx = , xls = as.data.frame(readxl::read_excel(path, sheet = 1)),
    stop("Filtypen .", ext, " støttes ikke. Bruk .csv, .xlsx eller .xls.")
  )

  if (nrow(dat) == 0 || ncol(dat) == 0) {
    stop("Filen inneholder ingen data.")
  }

  if (ncol(dat) == 1) {
    ## words <- tolower(trimws(as.character(dat[[1]])))
    words <- normalize_words_fast(as.character(dat[[1]]))
    words <- words[!is.na(words) & nzchar(words)]
    if (!length(words)) stop("Fant ingen ord i filen.")
    freq <- as.data.frame(table(words), stringsAsFactors = FALSE)
    names(freq) <- c("name", "weight")
  } else {
    nms <- tolower(trimws(names(dat)))
    word_idx <- which(nms %in% c("word", "words", "ord", "name", "navn"))[1]
    freq_idx <- which(nms %in% c(
      "freq", "frequency", "weight", "count", "n",
      "antall", "frekvens", "forekomster", "vekt"
    ))[1]
    if (is.na(word_idx)) word_idx <- 1
    if (is.na(freq_idx) || freq_idx == word_idx) {
      freq_idx <- setdiff(seq_len(ncol(dat)), word_idx)[1]
    }

    freq <- data.frame(
      name = tolower(trimws(as.character(dat[[word_idx]]))),
      weight = to_numeric(dat[[freq_idx]]),
      stringsAsFactors = FALSE
    )
    freq <- freq[!is.na(freq$name) & nzchar(freq$name) &
                   !is.na(freq$weight) & freq$weight > 0, , drop = FALSE]
    if (nrow(freq) == 0) {
      stop("Fant ingen gyldige rader. Forventer ord i første kolonne og frekvens (tall) i andre kolonne.")
    }
    # Sum duplicates of the same word
    freq <- stats::aggregate(weight ~ name, data = freq, FUN = sum)
  }

  freq$weight <- as.numeric(freq$weight)
  freq <- freq[order(-freq$weight, freq$name), , drop = FALSE]
  rownames(freq) <- NULL
  freq
}

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
