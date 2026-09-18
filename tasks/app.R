############################################################
# Task Prioritization Survey
#
# Stores responses in Google Sheets
# Displays average task priorities in a Shinydashboard
#
# Google Sheet columns:
# ID | Time | Tasks | Scores
#
# Worksheet:
# tasksavd
############################################################

############################################################
# PACKAGES
############################################################

library(shiny)
library(shinydashboard)
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
# TASK DEFINITIONS
############################################################

tasks <- c(
  "Statistikkarbeid",
  "Rapportering",
  "Samarbeid",
  "Utviklingsarbeid",
  "Koordinering",
  "Kompetanseutvikling",
  "Kunnskapsinnhenting",
  "Annet"
)

task_text <- c(
  "Tilrettelegging, kvalitetssikring og analyse av mottatte data for statistikkproduksjon.",
  "Utarbeidelse, publisering og formidling av statistikk, analyser og andre resultater.",
  "Faglig støtte, rådgivning og samarbeid med brukere og eksterne aktører.",
  "Utvikling av nye løsninger, metoder, systemer og gjennomføring av prosjekter.",
  "Planlegging, koordinering og oppfølging av aktiviteter, møter og beslutninger.",
  "Deltakelse i kurs, opplæring og andre aktiviteter for faglig utvikling.",
  "Innhenting og vurdering av fagkunnskap, metoder og relevant informasjon.",
  "Andre arbeidsoppgaver som ikke passer naturlig inn i kategoriene ovenfor."
)


task_labels <- setNames(
  task_text,
  tasks
)

############################################################
# TOOLTIP HELPER
############################################################

# Creates a polished Bootstrap tooltip for each task label.
task_tooltip <- function(task, description) {

  tags$span(
    class = "task-label-tooltip",
    task,
    tags$i(
      class = "fa fa-info-circle task-info-icon",
      `aria-hidden` = "true"
    ),
    `data-toggle` = "tooltip",
    `data-placement` = "right",
    `data-container` = "body",
    `data-html` = "false",
    title = description
  )

}

############################################################
# GOOGLE SHEETS FUNCTIONS
############################################################

# Read responses from sheet.
# Returns an empty data frame if the sheet cannot be read.
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
      Scores = numeric(),
      stringsAsFactors = FALSE
    )

  })

}

# Save one survey submission.
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
# SURVEY MODULE UI
############################################################

taskFormUI <- function(id) {

  ns <- NS(id)

  tagList(

    uiOutput(ns("task_inputs")),

    br(),

    strong(
      textOutput(ns("total_text"))
    ),

    textOutput(ns("status_text")),

    br(),

    actionButton(
      ns("reset"),
      "Nullstille",
      class = "btn-warning"
    ),

    tags$span(" "),

    actionButton(
      ns("submit"),
      "Send",
      class = "submit-notready"
    )

  )

}

############################################################
# SURVEY MODULE SERVER
############################################################

taskFormServer <- function(
                           id,
                           tasks,
                           on_submit
                           ) {

  moduleServer(id, function(
                            input,
                            output,
                            session
                            ) {

    disable("submit")

    ########################################################
    # Create numeric inputs dynamically
    ########################################################

    output$task_inputs <- renderUI({

      tagList(

        lapply(seq_along(tasks), function(i) {

          autonumericInput(
            inputId = session$ns(
              paste0("task_", i)
            ),
            label = task_tooltip(
              task = names(tasks)[i],
              description = unname(tasks)[i]
            ),
            value = 0,
            minimumValue = 0,
            decimalPlaces = 0,
            emptyInputBehavior = "zero"
          )

        })

      )

    })

    ########################################################
    # Total score
    ########################################################

    total_score <- reactive({

      sum(
        sapply(seq_along(tasks), function(i) {

          value <- input[[paste0(
            "task_",
            i
          )]]

          if (
            is.null(value) ||
              is.na(value)
          ) {
            0
          } else {
            value
          }
        })
      )

    })

    ########################################################
    # Total display
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
          "Tilgjengelig skåre:",
          100 - total_score()
        )

      } else if (total_score() > 100) {

        paste(
          "Mer enn 100 skåre:",
          total_score() - 100
        )

      } else {

        "Klar til å sende"

      }

    })

    ########################################################
    # Enable submit only at exactly 100
    ########################################################


    ########################################################
    # Reset
    ########################################################

    observeEvent(input$reset, {

      for (i in seq_along(tasks)) {

        updateAutonumericInput(
          session = session,
          inputId = paste0(
            "task_",
            i
          ),
          value = 0
        )

      }

    })

    ########################################################
    # Submit
    ########################################################

    observe({

      if (total_score() == 100) {

        enable("submit")

        removeClass(
          id = "submit",
          class = "submit-notready"
        )

        addClass(
          id = "submit",
          class = "submit-ready"
        )

      } else {

        disable("submit")

        removeClass(
          id = "submit",
          class = "submit-ready"
        )

        addClass(
          id = "submit",
          class = "submit-notready"
        )

      }

    })
    
    observeEvent(input$submit, {

      scores <- sapply(
        seq_along(tasks),
        function(i) {

          value <- input[[paste0(
            "task_",
            i
          )]]

          if (is.null(value)) {
            0
          } else {
            value
          }

        }
      )

      on_submit(scores)

      showNotification(
        "Ditt svar er nå sendt.",
        type = "message"
      )

      # Reset after submit
      for (i in seq_along(tasks)) {

        updateAutonumericInput(
          session,
          inputId = paste0(
            "task_",
            i
          ),
          value = 0
        )

      }

    })

  })

}

############################################################
# RESULTS MODULE UI
############################################################

resultsDashboardUI <- function(id) {

  ns <- NS(id)

  tagList(

    fluidRow(

      valueBoxOutput(
        ns("respondent_box"),
        width = 4
      )

    ),

    fluidRow(

      box(
        width = 12,
        title = NULL,
        solidHeader = FALSE,

        highchartOutput(
          ns("main_chart"),
          height = "calc(100vh - 180px)"
          # height = "650px" #fit windows
        )
      )

    )

  )

}

############################################################
# RESULTS MODULE SERVER
############################################################

resultsDashboardServer <- function(
                                   id,
                                   survey_data,
                                   tasks
                                   ) {

  moduleServer(id, function(
                            input,
                            output,
                            session
                            ) {

    ########################################################
    # Count respondents
    ########################################################

    respondent_count <- reactive({

      df <- survey_data()

      if (nrow(df) == 0) {
        return(0)
      }

      dplyr::n_distinct(df$ID)

    })

    ########################################################
    # Value box
    ########################################################

    output$respondent_box <- renderValueBox({

      valueBox(
        value = respondent_count(),
        subtitle = "Antall svar",
        icon = icon("users"),
        color = "blue"
      )

    })

    ########################################################
    # Chart
    ########################################################

    output$main_chart <- renderHighchart({

      df <- survey_data()

      if (nrow(df) == 0) {

        averages <- rep(
          0,
          length(tasks)
        )

      } else {

        df$Scores <- as.numeric(df$Scores)

        summary_df <- df %>%
          group_by(Tasks) %>%
          summarise(
            total_score = sum(
              Scores,
              na.rm = TRUE
            ),
            .groups = "drop"
          )

        summary_df <- merge(
          data.frame(
            Tasks = tasks
          ),
          summary_df,
          by = "Tasks",
          all.x = TRUE
        )

        summary_df$total_score[
          is.na(summary_df$total_score)
        ] <- 0

        respondents <- respondent_count()

        if (respondents == 0) {

          averages <- rep(
            0,
            length(tasks)
          )

        } else {

          averages <- round(
            summary_df$total_score /
              respondents,
            1
          )

        }

      }

      highchart() |>
        hc_chart(
          type = "column"
        ) |>
        hc_title(
          text = "Aktiviteter og tidsbruk"
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
            text = "Gjennomsnitt %"
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
              format =
                "{point.y:.1f}%"
            )
          )
        ) |>
        hc_add_series(
          name = "Gjennomsnitt %",
          data = averages
        )

    })

  })

}

############################################################
# UI
############################################################

ui <- dashboardPage(

  dashboardHeader(
    title = "Tidsbruk"
  ),

  dashboardSidebar(

    sidebarMenu(

      menuItem(
        "Aktiviteter",
        tabName = "survey",
        icon = icon("edit")
      ),

      menuItem(
        "Oversikt",
        tabName = "results",
        icon = icon("bar-chart")
      )

    )

  ),

    dashboardBody(

      useShinyjs(),

      tags$head(

        tags$style(HTML("

      /* Polished task tooltip styling */
      .task-label-tooltip {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        cursor: help;
        color: #333333;
        border-bottom: 1px dotted #337ab7;
        transition: color 0.15s ease, border-color 0.15s ease;
      }

      .task-label-tooltip:hover,
      .task-label-tooltip:focus {
        color: #337ab7;
        border-bottom-color: #337ab7;
      }

      .task-info-icon {
        color: #337ab7;
        font-size: 0.9em;
      }

      .tooltip {
        font-size: 14px;
        line-height: 1.45;
      }

      .tooltip-inner {
        max-width: 360px;
        padding: 10px 12px;
        text-align: left;
        border-radius: 6px;
        box-shadow: 0 3px 10px rgba(0, 0, 0, 0.18);
      }

      .submit-ready {
        background-color: #28a745 !important;
        border-color: #28a745 !important;
        color: white !important;
      }

      .submit-notready {
        background-color: #dc3545 !important;
        border-color: #dc3545 !important;
        color: white !important;
      }

    ")),

       tags$script(HTML("$(document).on('shiny:connected', function() { $('[data-toggle=\"tooltip\"]').tooltip({ trigger: 'hover focus', container: 'body' }); }); $(document).on('shiny:value shiny:bound', function() { $('[data-toggle=\"tooltip\"]').tooltip({ trigger: 'hover focus', container: 'body' }); });"))

    ),

    tabItems(

      ######################################################
    # Survey Tab
    ######################################################

    tabItem(
      tabName = "survey",
      fluidRow(
        box(
          width = 12,
          title = "Aktiviteter (summen skal være 100)",
          status = "primary",
          solidHeader = TRUE,
#           p(
#             "Allocate exactly 100 points across the tasks."
#           ),
          br(),
          taskFormUI("survey")
        )
      )
    ),

    ######################################################
    # Results Tab
    ######################################################

    tabItem(
      tabName = "results",
      resultsDashboardUI(
        "dashboard"
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
  # Refresh Google Sheet every 5 seconds
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
  # Survey Module
  ##########################################################

  taskFormServer(
    id = "survey",
    tasks = task_labels,
    on_submit = save_response
  )

  ##########################################################
  # Results Module
  ##########################################################

  resultsDashboardServer(
    id = "dashboard",
    survey_data = survey_data,
    tasks = tasks
  )

}

############################################################
# RUN APP
############################################################

shinyApp(
  ui = ui,
  server = server
)
