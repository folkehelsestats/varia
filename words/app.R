############################################################
# PACKAGES
############################################################

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(googlesheets4)
library(data.table)

############################################################
# GOOGLE SHEETS CONFIGURATION
############################################################

googlesheets4::gs4_auth(
  path = "service-task-survey.json"
)

sheet_id <- "108kPND1ySv8XQss6xt0DYo5kxYsAC1aTZpgxoJUuBuU"
worksheet_name <- "words"

############################################################
# STOP WORDS
############################################################

stop_words <- unique(c(

  # Norwegian
  "og","i","jeg","det","at","en","et","den","til",
  "er","som","på","de","med","han","av","ikke",
  "ikkje","der","så","var","meg","seg","men","ett",
  "har","om","vi","min","mitt","ha","hadde","hun",
  "nå","over","da","ved","fra","du","ut","sin",
  "dem","oss","opp","man","kan","hans","hvor",
  "eller","hva","skal","selv","sjøl","her",

  # English
  "the","and","a","an","of","to","for","with",
  "in","on","at","is","it","that","this",
  "from","be","are","was","were","or","by",
  "as","about"
))

############################################################
# INITIALIZE WORKSHEET
############################################################

initialize_sheet <- function() {

  sheet_info <- googlesheets4::sheet_properties(
    sheet_id
  )

  if (!worksheet_name %in% sheet_info$name) {

    googlesheets4::sheet_add(
      ss = sheet_id,
      sheet = worksheet_name
    )

    googlesheets4::sheet_write(
      data.frame(
        timestamp = character(),
        word = character()
      ),
      ss = sheet_id,
      sheet = worksheet_name
    )
  }
}

initialize_sheet()

############################################################
# VALIDATE USER INPUT
############################################################

validate_word <- function(word) {

  word <- trimws(word)

  if (is.null(word) || word == "") {

    return(list(
      valid = FALSE,
      message = "Et ord om gangen."
    ))
  }

  if (grepl("\\s", word)) {

    return(list(
      valid = FALSE,
      message = "Kun ett ord tillatt."
    ))
  }

  word <- gsub(
    "[^[:alnum:]æøåÆØÅ-]",
    "",
    word
  )

  if (word == "") {

    return(list(
      valid = FALSE,
      message = "Ugyldig ord."
    ))
  }

  if (nchar(word) > 30) {

    return(list(
      valid = FALSE,
      message = "Maksimum 30 tegn."
    ))
  }

  word <- tolower(word)

  if (word %in% stop_words) {

    return(list(
      valid = FALSE,
      message = "Vanlige stoppeord er ikke tillatt."
    ))
  }

  list(
    valid = TRUE,
    word = word
  )
}

############################################################
# APPEND WORD
############################################################

append_word <- function(word) {

  new_row <- data.frame(
    timestamp = as.character(Sys.time()),
    word = word,
    stringsAsFactors = FALSE
  )

  googlesheets4::sheet_append(
    ss = sheet_id,
    sheet = worksheet_name,
    data = new_row
  )
}

############################################################
# RESET SHEET
############################################################

reset_responses <- function() {

  googlesheets4::sheet_write(

    data.frame(
      timestamp = character(),
      word = character()
    ),

    ss = sheet_id,
    sheet = worksheet_name
  )
}

############################################################
# READ WORDS
############################################################

get_word_counts <- function() {

  dat <- googlesheets4::read_sheet(
    ss = sheet_id,
    sheet = worksheet_name
  )

  dt <- data.table::as.data.table(dat)

  if (nrow(dt) == 0) {

    return(
      data.table::data.table(
        name = character(),
        weight = numeric()
      )
    )
  }

  dt[, word := trimws(
    tolower(word)
  )]

  dt <- dt[
    !is.na(word) &
      word != ""
  ]

  if (nrow(dt) == 0) {

    return(
      data.table::data.table(
        name = character(),
        weight = numeric()
      )
    )
  }

  freq <- dt[
    ,
    .(weight = .N),
    by = word
  ][
    order(-weight)
  ]

  data.table::setnames(
    freq,
    "word",
    "name"
  )

  freq[]
}

############################################################
# UI
############################################################

ui <- fluidPage(

  shinyjs::useShinyjs(),

  shinyjs::extendShinyjs(
    text = "
      shinyjs.bindEnter = function() {

        $('#word').keypress(function(e) {

          if (e.which == 13) {

            $('#send_word').click();

            return false;
          }
        });
      }
    ",
    functions = "bindEnter"
  ),

  tags$head(

    tags$script(
      src = "https://code.highcharts.com/modules/wordcloud.js"
    )
  ),

  titlePanel(
    "Hvem er vi?"
  ),

  tabsetPanel(

    ########################################################
    # INPUT TAB
    ########################################################

    tabPanel(

      "Send ord",

      br(),

      fluidRow(

        column(

          width = 6,

          textInput(
            "word",
            "Skriv ett ord",
            placeholder = "avdelingen"
          ),

          actionBttn(
            inputId = "send_word",
            label = "Send",
            style = "material-flat",
            color = "primary"
          ),

          br(),
          br(),

          verbatimTextOutput(
            "status"
          )
        )
      )
    ),

    ########################################################
    # WORD CLOUD TAB
    ########################################################

    tabPanel(

      "Ord",

      br(),

      fluidRow(

        column(

          width = 12,

          highchartOutput(
            "wordcloud",
            height = "700px"
          )
        )
      )
    ),

    ########################################################
    # ADMIN TAB
    ########################################################

    tabPanel(

      "Admin",

      br(),

      fluidRow(

        column(

          width = 4,

          actionButton(
            "pause_updates",
            "Pause updates",
            class = "btn-warning"
          ),

          br(),
          br(),

          actionButton(
            "resume_updates",
            "Resume updates",
            class = "btn-success"
          ),

          br(),
          br(),

          actionButton(
            "reset_sheet",
            "Reset responses",
            class = "btn-danger"
          ),

          br(),
          br(),

          verbatimTextOutput(
            "admin_status"
          )
        )
      )
    )
  )
)

############################################################
# SERVER
############################################################

server <- function(
  input,
  output,
  session
) {

  ##########################################################
  # ENABLE ENTER KEY
  ##########################################################

  shinyjs::js$bindEnter()

  ##########################################################
  # STATUS
  ##########################################################

  status_message <- reactiveVal("")

  admin_message <- reactiveVal(
    "Updates are running."
  )

  output$status <- renderText({
    status_message()
  })

  output$admin_status <- renderText({
    admin_message()
  })

  ##########################################################
  # SESSION UPDATE FLAG
  ##########################################################

  updates_enabled <- reactiveVal(TRUE)

  ##########################################################
  # CACHE WORD DATA
  ##########################################################

  cached_words <- reactiveVal(

    data.table::data.table(
      name = character(),
      weight = numeric()
    )
  )

  ##########################################################
  # INITIAL LOAD
  ##########################################################

  try({

    cached_words(
      get_word_counts()
    )

  }, silent = TRUE)

  ##########################################################
  # REFRESH GOOGLE SHEET
  ##########################################################

  observe({

    invalidateLater(
      3000,
      session
    )

    if (!updates_enabled()) {
      return()
    }

    tryCatch(

      {

        latest_words <- get_word_counts()

        cached_words(
          latest_words
        )

      },

      error = function(e) {

        message(
          "Refresh error: ",
          e$message
        )
      }
    )
  })

  ##########################################################
  # SUBMIT WORD
  ##########################################################

  observeEvent(
    input$send_word,
    {

      validation <- validate_word(
        input$word
      )

      if (!validation$valid) {

        status_message(
          paste(
            "❌",
            validation$message
          )
        )

        return()
      }

      tryCatch(

        {

          append_word(
            validation$word
          )

          status_message(
            paste(
              "✅ Sendt:",
              validation$word
            )
          )

          updateTextInput(
            session,
            "word",
            value = ""
          )

          shinyjs::runjs(
            "$('#word').focus();"
          )

        },

        error = function(e) {

          status_message(
            paste(
              "❌",
              e$message
            )
          )
        }
      )
    }
  )

  ##########################################################
  # PAUSE
  ##########################################################

  observeEvent(
    input$pause_updates,
    {

      updates_enabled(FALSE)

      admin_message(
        "Oppdateringer stoppet for denne sesjonen."
      )
    }
  )

  ##########################################################
  # RESUME
  ##########################################################

  observeEvent(
    input$resume_updates,
    {

      updates_enabled(TRUE)

      admin_message(
        "Oppdateringer gjenopptatt."
      )
    }
  )

  ##########################################################
  # RESET
  ##########################################################

  observeEvent(
    input$reset_sheet,
    {

      tryCatch(

        {

          reset_responses()

          cached_words(
            data.table::data.table(
              name = character(),
              weight = numeric()
            )
          )

          admin_message(
            "Alle ord er slettet."
          )

        },

        error = function(e) {

          admin_message(
            paste(
              "Feil:",
              e$message
            )
          )
        }
      )
    }
  )

  ##########################################################
  # WORD CLOUD
  ##########################################################

  output$wordcloud <- renderHighchart({

    freq <- cached_words()

    highcharter::highchart() |>

  highcharter::hc_title(
    text = "Folkehelsestatistikk"
  ) |>

  highcharter::hc_add_series(
    data = highcharter::list_parse(freq),
    type = "wordcloud",
    name = "Ord",
    spiral = "archimedean"
  ) |>

  highcharter::hc_credits(
    enabled = FALSE
  ) |>

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

############################################################
# RUN APP
############################################################

shiny::shinyApp(
  ui = ui,
  server = server
)
