# Word input app -------------------------------------------------------------
# Run from this folder with: shiny::runApp()
# Required packages: shiny, shinyjs, shinyWidgets, googlesheets4

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(googlesheets4)

# highdir colors
source("https://raw.githubusercontent.com/folkehelsestats/varia/refs/heads/main/colors.R")

# Configure these for your deployment.
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

# Keep the input app responsible for creating the worksheet if it does not exist.
initialize_sheet <- function() {
  sheet_info <- googlesheets4::sheet_properties(sheet_id)
  if (!worksheet_name %in% sheet_info$name) {
    googlesheets4::sheet_add(ss = sheet_id, sheet = worksheet_name)
    googlesheets4::sheet_write(
      data.frame(timestamp = character(), word = character()),
      ss = sheet_id,
      sheet = worksheet_name
    )
  }
}
initialize_sheet()

stop_words <- unique(c(
  # Norwegian
  "og","i","jeg","det","at","en","et","den","til","er","som","på","de","med",
  "han","av","ikke","ikkje","der","så","var","meg","seg","men","ett","har",
  "om","vi","min","mitt","ha","hadde","hun","nå","over","da","ved","fra",
  "du","ut","sin","dem","oss","opp","man","kan","hans","hvor","eller","hva",
  "skal","selv","sjøl","her",
  # English
  "the","and","a","an","of","to","for","with","in","on","at","is","it",
  "that","this","from","be","are","was","were","or","by","as","about"
))

validate_word <- function(word) {
  if (is.null(word) || length(word) == 0 || is.na(word) || !nzchar(trimws(word))) {
    return(list(valid = FALSE, message = "Et ord om gangen."))
  }

  word <- trimws(word)
  if (grepl("\\s", word)) {
    return(list(valid = FALSE, message = "Kun ett ord tillatt."))
  }

  # Keep letters/numbers plus Norwegian characters and hyphens.
  word <- gsub("[^[:alnum:]æøåÆØÅ-]", "", word)
  if (!nzchar(word)) {
    return(list(valid = FALSE, message = "Ugyldig ord."))
  }
  if (nchar(word) > 30) {
    return(list(valid = FALSE, message = "Maksimum 30 tegn."))
  }

  word <- tolower(word)
  if (word %in% stop_words) {
    return(list(valid = FALSE, message = "Vanlige stoppeord er ikke tillatt."))
  }
  list(valid = TRUE, word = word)
}

append_word <- function(word) {
  googlesheets4::sheet_append(
    ss = sheet_id,
    sheet = worksheet_name,
    data = data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      word = word,
      stringsAsFactors = FALSE
    )
  )
}

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(
    text = "
      shinyjs.bindEnter = function() {
        $('#word').on('keydown', function(e) {
          if (e.which === 13) {
            e.preventDefault();
            $('#send_word').click();
          }
        });
      }
    ",
    functions = "bindEnter"
  ),

  h1(
    "Avd. folkehelsestatistikk",
    style = "
    color: #025169;
    font-size: 35px;
    margin-bottom: 5px;
  "
  ),

  h4(
    "Beskrev hvem er vi...",
    style = "color: #0069E8; text-align: left; margin-top: -10px;"
  ),

  p(
    "Bruk så mange ord du vil, men kun ett ord om gangen",
    style = "font-size: 16px; text-align: left;"
  ),

  fluidRow(
    column(
      width = 6,
      textInput("word", "Skriv ett ord om gangen", placeholder = "adjektiv"),
      actionBttn(
        inputId = "send_word",
        label = "Send",
        style = "material-flat",
        color = "primary"
      ),
      br(), br(),
      role = "status",
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  shinyjs::js$bindEnter()
  status_message <- reactiveVal("Klar til å ta imot ord 🫰 ")

  output$status <- renderText(status_message())

  observeEvent(input$send_word, {
    validation <- validate_word(input$word)

    if (!validation$valid) {
      status_message(paste("❌", validation$message))
      return()
    }

    tryCatch({
      append_word(validation$word)
      status_message(paste("✅ Sendt:", validation$word))
      updateTextInput(session, "word", value = "")
      shinyjs::runjs("$('#word').trigger('focus');")
    }, error = function(e) {
      status_message(paste("❌ Kunne ikke lagre ordet:", conditionMessage(e)))
    })
  })
}

shinyApp(ui = ui, server = server)
