############################################################
# Task Prioritization Survey
#
# Stores data in Google Sheets
# Displays average scores using highdir
#
# Google Sheet columns:
# ID | Time | Tasks | Scores
#
# Worksheet name:
# tasksavd
############################################################

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(googlesheets4)
library(dplyr)

############################################################
# GOOGLE SHEETS AUTHENTICATION
############################################################

gs4_auth(
  path = "service-task-survey.json"
)

sheet_id <- "108kPND1ySv8XQss6xt0DYo5kxYsAC1aTZpgxoJUuBuU"

############################################################
# TASKS
############################################################

tasks <- c(
  "Planlegging",
  "Rapportering",
  "Møte",
  "Kreativ",
  "Analyser",
  "Annen"
)

ttxt <- c(
  "Planlegging av ting og tang",
  "Rapportering internasjonal osv.",
  "Møte virksomhet",
  "Kreative og innovative arbeid",
  "Analyser og statistikk",
  "Alt annen"
)

df <- setNames(ttxt, tasks)

############################################################
# GOOGLE SHEETS FUNCTIONS
############################################################

read_responses <- function() {

  tryCatch({

    read_sheet(
      ss = sheet_id,
      sheet = "tasksavd"
    )

  }, error = function(e) {

    data.frame(
      ID = character(),
      Time = character(),
      Tasks = character(),
      Scores = numeric()
    )

  })

}

save_response <- function(scores) {

  response_id <- paste0(
    as.integer(Sys.time()),
    "_",
    sample(100000:999999, 1)
  )

  submission <- data.frame(
    ID = response_id,
    Time = as.character(Sys.time()),
    Tasks = tasks,
    Scores = scores,
    stringsAsFactors = FALSE
  )

  sheet_append(
    ss = sheet_id,
    sheet = "tasksavd",
    data = submission
  )

}

############################################################
# TASK INPUT MODULE
############################################################

taskFormUI <- function(id) {

  ns <- NS(id)

  tagList(

    uiOutput(ns("task_inputs")),

    br(),

    strong(textOutput(ns("total_text"))),

    br(),

    textOutput(ns("status_text")),

    br(),

    actionButton(
      ns("reset"),
      "Reset",
      class = "btn-warning"
    ),

    tags$span(" "),

    actionButton(
      ns("submit"),
      "Submit",
      class = "btn-default"
    )

  )

}

taskFormServer <- function(id, tasks, on_submit) {

  moduleServer(id, function(input, output, session) {

    disable("submit")

    ########################################################
    # Create task inputs
    ########################################################

    output$task_inputs <- renderUI({

      tagList(

        lapply(seq_along(tasks), function(i) {

          autonumericInput(
            inputId = session$ns(paste0("task_", i)),
            label = unname(tasks)[i],

            value = 0,

            minimumValue = 0,

            decimalPlaces = 0,

            emptyInputBehavior = "zero"
          )

        })

      )

    })

    ########################################################
    # Total calculation
    ########################################################

    total_score <- reactive({

      scores <- sapply(seq_along(tasks), function(i) {

        value <- input[[paste0("task_", i)]]

        if (is.null(value) || is.na(value)) {
          0
        } else {
          value
        }

      })

      sum(scores)

    })

    ########################################################
    # Display total
    ########################################################

    output$total_text <- renderText({

      paste(
        "Total:",
        total_score(),
        "/ 100"
      )

    })

    ########################################################
    # Validation message
    ########################################################

    output$status_text <- renderText({

      if (total_score() < 100) {

        paste(
          "Remaining points:",
          100 - total_score()
        )

      } else if (total_score() > 100) {

        paste(
          "Too many points:",
          total_score() - 100
        )

      } else {

        "Ready to submit"

      }

    })

    ########################################################
    # Enable submit only when total = 100
    ########################################################

    observe({

      if (total_score() == 100) {

        enable("submit")

        removeClass("submit", "btn-default")

        addClass("submit", "btn-success")

      } else {

        disable("submit")

        removeClass("submit", "btn-success")

        addClass("submit", "btn-default")

      }

    })

    ########################################################
    # Reset button
    ########################################################

    observeEvent(input$reset, {

      for (i in seq_along(tasks)) {

        updateAutonumericInput(
          session = session,
          inputId = paste0("task_", i),
          value = 0
        )

      }

    })

    ########################################################
    # Submit
    ########################################################

    observeEvent(input$submit, {

      scores <- sapply(seq_along(tasks), function(i) {

        value <- input[[paste0("task_", i)]]

        if (is.null(value)) 0 else value

      })

      on_submit(scores)

      showNotification(
        "Thank you for your response.",
        type = "message"
      )

      for (i in seq_along(tasks)) {

        updateAutonumericInput(
          session,
          inputId = paste0("task_", i),
          value = 0
        )

      }

    })

  })

}

############################################################
# DASHBOARD MODULE
############################################################

dashboardUI <- function(id) {

  ns <- NS(id)

  tagList(

    strong(
      textOutput(ns("respondent_count"))
    ),

    br(),
    br(),

    highchartOutput(
      ns("chart"),
      height = "600px"
    )

  )

}

dashboardServer <- function(id, survey_data, tasks) {

  moduleServer(id, function(input, output, session) {

    ########################################################
    # Respondent count
    ########################################################

    respondent_count <- reactive({

      df <- survey_data()

      if (nrow(df) == 0) {
        return(0)
      }

      n_distinct(df$ID)

    })

    output$respondent_count <- renderText({

      paste(
        "Number of respondents:",
        respondent_count()
      )

    })

    ########################################################
    # Highcharter plot
    ########################################################

    output$chart <- renderHighchart({

      df <- survey_data()

      if (nrow(df) == 0) {

        avg_scores <- rep(0, length(tasks))

      } else {

        df$Scores <- as.numeric(df$Scores)

        summary_df <- df %>%
          group_by(Tasks) %>%
          summarise(
            total_score = sum(Scores, na.rm = TRUE),
            .groups = "drop"
          )

        summary_df <- merge(
          data.frame(Tasks = tasks),
          summary_df,
          by = "Tasks",
          all.x = TRUE
        )

        summary_df$total_score[
          is.na(summary_df$total_score)
        ] <- 0

        respondents <- respondent_count()

        if (respondents == 0) {

          avg_scores <- rep(0, length(tasks))

        } else {

          avg_scores <- round(
            summary_df$total_score /
              respondents,
            1
          )

        }

      }

      highchart() |>

        hc_chart(type = "column") |>

        hc_title(
          text = "Average Task Prioritization"
        ) |>

        hc_subtitle(
          text = paste(
            "Respondents:",
            respondent_count()
          )
        ) |>

        hc_xAxis(
          categories = tasks
        ) |>

        hc_yAxis(
          min = 0,
          max = 100,
          title = list(
            text = "Average Percentage (%)"
          )
        ) |>

        hc_tooltip(
          pointFormat =
            "<b>{point.y:.1f}%</b>"
        ) |>

        hc_plotOptions(
          column = list(
            colorByPoint = TRUE,
            dataLabels = list(
              enabled = TRUE,
              format = "{point.y:.1f}%"
            )
          )
        ) |>

        hc_add_series(
          name = "Average %",
          data = avg_scores
        )

    })

  })

}

############################################################
# USER INTERFACE
############################################################

ui <- fluidPage(

  useShinyjs(),

  titlePanel(
    "Task Prioritization Survey"
  ),

  fluidRow(

    column(
      width = 4,

      taskFormUI("survey")
    ),

    column(
      width = 8,

      dashboardUI("dashboard")
    )

  )

)

############################################################
# SERVER
############################################################

server <- function(input, output, session) {

  ##########################################################
  # Poll Google Sheets every 5 seconds
  ##########################################################

  survey_data <- reactivePoll(

    intervalMillis = 5000,

    session = session,

    checkFunc = function() {
      Sys.time()
    },

    valueFunc = function() {
      read_responses()
    }

  )

  ##########################################################
  # Task Form Module
  ##########################################################

  taskFormServer(
    id = "survey",
    tasks = df,
    on_submit = save_response
  )

  ##########################################################
  # Dashboard Module
  ##########################################################

  dashboardServer(
    id = "dashboard",
    survey_data = survey_data,
    tasks = tasks
  )

}

############################################################
# RUN APPLICATION
############################################################

shinyApp(ui, server)
