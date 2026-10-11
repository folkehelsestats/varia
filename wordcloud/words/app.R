# Word input app -------------------------------------------------------------
# Run from this folder with: shiny::runApp()
# Required packages: shiny, shinyjs, shinyWidgets, googlesheets4

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(googlesheets4)


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
  if (is.null(word) || length(word) == 0 || is.na(word) ||
      !nzchar(trimws(word))) {
    return(list(valid = FALSE, message = "Feltet er tomt."))
  }

  # Trim outer whitespace and normalize repeated whitespace.
  word <- trimws(word)
  word <- gsub("\\s+", " ", word)

  # Allow letters, numbers, spaces, Norwegian characters, and hyphens.
  if (grepl("[^[:alnum:]æøåÆØÅ -]", word)) {
    return(list(valid = FALSE, message = "Ugyldige tegn."))
  }

  if (nchar(word) > 30) {
    return(list(valid = FALSE, message = "Maksimum 30 tegn per felt."))
  }

  normalized_word <- tolower(word)

  # Reject stop words when the entire entry is a stop word.
  if (normalized_word %in% stop_words) {
    return(list(
      valid = FALSE,
      message = "Vanlige stoppeord er ikke tillatt."
    ))
  }

  list(valid = TRUE, word = normalized_word)
}

append_words <- function(words) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  googlesheets4::sheet_append(
    ss = sheet_id,
    sheet = worksheet_name,
    data = data.frame(
      timestamp = rep(timestamp, length(words)),
      word = words,
      stringsAsFactors = FALSE
    )
  )
}


ui <- fluidPage(
  useShinyjs(),

  extendShinyjs(
    text = "
      shinyjs.bindEnter = function() {
        $('#word1, #word2, #word3, #word4, #word5')
          .on('keydown', function(e) {
            if (e.which === 13) {
              e.preventDefault();
              $('#send_words').click();
            }
          });
      }
    ",
    functions = "bindEnter"
  ),

  h2(
    HTML("Avd.<br>Folkehelsestatistikk"),
    style = "color: #025169; margin-bottom: 8px;"
  ),

  h4(
    "Beskriv hvem vi er...",
    style = "color: #0069E8; text-align: left; margin-top: -10px;"
  ),

  p(
    "Skriv minst 1 ord eller maks 5 ord om gangen. Kan sende flere ord flere ganger også ☺️",
    style = "font-size: 16px; text-align: left;"
  ),

  fluidRow(
    column(
      width = 6,

      textInput("word1", "Ord 1",
                placeholder = "..."),
      textInput("word2", "Ord 2",
                placeholder = "..."),
      textInput("word3", "Ord 3",
                placeholder = "..."),
      textInput("word4", "Ord 4",
                placeholder = "..."),
      textInput("word5", "Ord 5",
                placeholder = "..."),

      actionBttn(
        inputId = "send_words",
        label = "Send",
        style = "material-flat",
        color = "primary"
      ),

      br(), br(),

      tags$div(
        role = "status",
        `aria-live` = "polite",
        verbatimTextOutput("status")
      )
    )
  )
)



server <- function(input, output, session) {
  shinyjs::js$bindEnter()

  status_message <- reactiveVal("Klar til å ta imot ord 🫰")

  output$status <- renderText(status_message())

  observeEvent(input$send_words, {
    # Collect all five input values.
    input_values <- lapply(
      1:5,
      function(i) input[[paste0("word", i)]]
    )

    # Keep track of the original field numbers.
    filled <- vapply(
      input_values,
      function(x) !is.null(x) &&
        length(x) > 0 &&
        !is.na(x) &&
        nzchar(trimws(x)),
      logical(1)
    )

    if (!any(filled)) {
      status_message("❌ Skriv inn minst ett ord .")
      return()
    }

    # Extract only populated fields.
    words_to_check <- unlist(input_values[filled], use.names = FALSE)
    field_numbers <- which(filled)

    # Validate every entry before saving.
    validations <- lapply(words_to_check, validate_word)

    valid_entries <- vapply(
      validations,
      function(x) x$valid,
      logical(1)
    )

    if (!all(valid_entries)) {
      invalid_indices <- which(!valid_entries)

      errors <- vapply(
        invalid_indices,
        function(i) {
          paste0(
            "Felt ", field_numbers[i], ": ",
            validations[[i]]$message
          )
        },
        character(1)
      )

      status_message(
        paste0("❌ ", paste(errors, collapse = "\n"))
      )
      return()
    }

    # Extract validated entries.
    validated_words <- vapply(
      validations,
      function(x) x$word,
      character(1)
    )

    # Save all entries together.
    tryCatch({
      append_words(validated_words)

      status_message(
        paste0(
          "✅ Sendt ", length(validated_words), " oppføring(er): ",
          paste(validated_words, collapse = "; ")
        )
      )

      # Clear all five inputs after successful saving.
      for (i in 1:5) {
        updateTextInput(
          session,
          inputId = paste0("word", i),
          value = ""
        )
      }

      shinyjs::runjs("$('#word1').trigger('focus');")

    }, error = function(e) {
      status_message(
        paste("❌ Kunne ikke lagre oppføringene:",
              conditionMessage(e))
      )
    })
  })
}

shinyApp(ui = ui, server = server)
