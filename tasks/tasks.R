library(shiny)
library(shinyjs)
library(highcharter)
library(googlesheets4)
library(dplyr)

# --------------------------------------------------
# GOOGLE SHEETS AUTHENTICATION
# --------------------------------------------------

gs4_auth(
  path = "service-task-survey.json"
)

sheet_id <- "108kPND1ySv8XQss6xt0DYo5kxYsAC1aTZpgxoJUuBuU"

# --------------------------------------------------
# TASKS
# --------------------------------------------------

tasks <- c(
  "Task 1",
  "Task 2",
  "Task 3",
  "Task 4",
  "Task 5",
  "Task 6",
  "Task 7",
  "Task 8"
)

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(

  useShinyjs(),

  titlePanel("Task Prioritization"),

  h4("Distribute exactly 100 points across the tasks"),

  fluidRow(

    column(
      width = 4,

      uiOutput("task_inputs"),

      br(),

      strong(textOutput("total_text")),

      textOutput("status_text"),

      br(),

      actionButton(
        "reset_btn",
        "Reset Scores",
        class = "btn-warning"
      ),

      tags$span(" "),

      actionButton(
        "submit_btn",
        "Submit",
        class = "btn-default"
      ),

      br(),
      br(),

      strong(textOutput("respondents_text"))
    ),

    column(
      width = 8,

      highchartOutput(
        "priority_chart",
        height = "600px"
      )
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {

  disable("submit_btn")

  # ----------------------------------------------
  # Dynamic task inputs
  # ----------------------------------------------

  output$task_inputs <- renderUI({

    tagList(

      lapply(seq_along(tasks), function(i) {

        numericInput(
          inputId = paste0("task_", i),
          label = tasks[i],
          value = 0,
          min = 0,
          step = 1
        )

      })

    )

  })

  # ----------------------------------------------
  # Total points
  # ----------------------------------------------

  total_points <- reactive({

    vals <- sapply(seq_along(tasks), function(i) {

      x <- input[[paste0("task_", i)]]

      if (is.null(x) || is.na(x) || x < 0) {
        0
      } else {
        x
      }

    })

    sum(vals)

  })

  # ----------------------------------------------
  # Display total
  # ----------------------------------------------

  output$total_text <- renderText({
    paste("Total:", total_points(), "/ 100")
  })

  # ----------------------------------------------
  # Status text
  # ----------------------------------------------

  output$status_text <- renderText({

    if (total_points() < 100) {

      paste(
        "Remaining points:",
        100 - total_points()
      )

    } else if (total_points() > 100) {

      paste(
        "Too many points:",
        total_points() - 100
      )

    } else {

      "Ready to submit"

    }

  })

  # ----------------------------------------------
  # Enable submit only when total = 100
  # ----------------------------------------------

  observe({

    if (total_points() == 100) {

      enable("submit_btn")

      removeClass("submit_btn", "btn-default")
      addClass("submit_btn", "btn-success")

    } else {

      disable("submit_btn")

      removeClass("submit_btn", "btn-success")
      addClass("submit_btn", "btn-default")

    }

  })

  # ----------------------------------------------
  # Reset button
  # ----------------------------------------------

  observeEvent(input$reset_btn, {

    for (i in seq_along(tasks)) {

      updateNumericInput(
        session,
        paste0("task_", i),
        value = 0
      )

    }

  })

  # ----------------------------------------------
  # Submit responses
  # ----------------------------------------------

  observeEvent(input$submit_btn, {

    response_id <- paste0(
      as.integer(Sys.time()),
      "_",
      sample(100000:999999, 1)
    )

    scores <- sapply(seq_along(tasks), function(i) {
      input[[paste0("task_", i)]]
    })

    submission <- data.frame(
      response_id = response_id,
      timestamp = Sys.time(),
      task = tasks,
      score = scores
    )

    sheet_append(
      ss = sheet_id,
      sheet = "tasksavd",
      data = submission
    )

    showNotification(
      "Response submitted successfully.",
      type = "message",
      duration = 3
    )

    for (i in seq_along(tasks)) {

      updateNumericInput(
        session,
        paste0("task_", i),
        value = 0
      )

    }

  })

  # ----------------------------------------------
  # Read all responses
  # ----------------------------------------------

  survey_data <- reactivePoll(

    intervalMillis = 5000,

    session = session,

    checkFunc = function() {
      Sys.time()
    },

    valueFunc = function() {

      tryCatch({

        read_sheet(
          sheet_id,
          sheet = "tasksavd",
          col_types = "cccc"
        )

      }, error = function(e) {

        data.frame(
          response_id = character(),
          timestamp = character(),
          task = character(),
          score = numeric()
        )

      })
    }
  )

  # ----------------------------------------------
  # Number of respondents
  # ----------------------------------------------

  respondent_count <- reactive({

    df <- survey_data()

    if (nrow(df) == 0) {
      return(0)
    }

    dplyr::n_distinct(df$response_id)

  })

  output$respondents_text <- renderText({

    paste(
      "Number of respondents:",
      respondent_count()
    )

  })

  # ----------------------------------------------
  # Highcharter graph
  # ----------------------------------------------

  output$priority_chart <- renderHighchart({

    df <- survey_data()

    if (nrow(df) == 0) {

      avg_scores <- rep(0, length(tasks))

    } else {

      df$score <- as.numeric(df$score)

      respondents <- respondent_count()

      summary_df <- df %>%
        group_by(task) %>%
        summarise(
          total_score = sum(score, na.rm = TRUE),
          .groups = "drop"
        )

      summary_df <- merge(
        data.frame(task = tasks),
        summary_df,
        by = "task",
        all.x = TRUE
      )

      summary_df$total_score[
        is.na(summary_df$total_score)
      ] <- 0

      avg_scores <-
        round(
          summary_df$total_score / respondents,
          1
        )

    }

    highchart() |>

      hc_chart(type = "column") |>

      hc_title(
        text = "Average Priority Scores"
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
          "<b>{point.y:.1f}%</b><br/>Average score across respondents"
      ) |>

      hc_plotOptions(
        column = list(
          dataLabels = list(
            enabled = TRUE,
            format = "{point.y:.1f}%"
          )
        )
      ) |>

      hc_add_series(
        name = "Average %",
        data = avg_scores,
        colorByPoint = TRUE
      )

  })

}

# --------------------------------------------------
# START APP
# --------------------------------------------------

shinyApp(ui, server)
