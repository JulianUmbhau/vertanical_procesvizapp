#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import plotly
#' @import shinyTime
#' @noRd

vert_theme <- bslib::bs_theme(
  version = 5,
  bg = "#f1ede8",
  fg = "#18646e",
  primary = "#18646e",
  secondary = "#18646e",
  success = "#47935e",
  info = "#8acded",
  warning = "#ddcc77",
  danger = "#cc6677"
)


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      title = div(
        img(src = "www/vertanical-gmbh.png",
            style= "border-top-style: solid;border-top-width: 0px;margin-top: 25px;margin-right: 20px;")),
      windowTitle = "processViz",
      theme = vert_theme,
      tabPanel(
        "Upload Data",
        # Sidebar layout with input and output definitions ----
        sidebarLayout(

          # Sidebar panel for inputs ----
          sidebarPanel(

            "OBS: For 'Openinterface' files -> Manual start time should be 'On'",
            hr(),

            radioButtons(
              inputId="ManualTime",
              label="Manual start time?",
              choiceNames = c("On","Off"),
              choiceValues = c("On","Off")
            ),
            dateInput(
              inputId="DateInput",
              label="Starting date for measurements:",
              format = "yyyy-mm-dd",
              startview = "month",
              weekstart = 0,
              language = "en",
              width = NULL,
              autoclose = TRUE,
              datesdisabled = NULL,
              daysofweekdisabled = NULL
            ),

            timeInput(
              inputId = "TimeInput",
              "Time:"),

            numericInput(
              inputId="SecondsToSubtract",
              label="Seconds subtracted from original time:",
              value=0,
              min = 0,
              max = NA
            ),

            radioButtons(
              inputId="InputType",
              label="Input type",
              choiceNames = c("Open Interface","SD Card"),
              choiceValues = c("OpenInterface","SDCard")
            ),


            # Input: Select a file ----
            fileInput(inputId = "file1", label = "Choose Text File",
                      multiple = FALSE,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                      )
            ),
          ),

          # Main panel for displaying outputs ----
          mainPanel(

            # Output: Data file ----
            DT::DTOutput("contents")
          )
        )
      ),
      tabPanel(
        "Visualization",

        sidebarLayout(

          # Sidebar panel for inputs ----
          sidebarPanel(

            # Input: Select a file ----
            selectInput("y_axis",
                        label = h5("Process Value to plot on the Y axis", style = "margin-bottom: 20px;"),
                        choices = c(
                          "PressureAct (in mbar)",
                          "BathAct (in Degrees C)",
                          "ChillerAct (in Degrees C)",
                          "Rotation (in rpm)",
                          "Vapor (in Degrees C)",
                          "Foam present (binary)",
                          "Foam control (binary)"
                        ),
                        selected = "pressure_act_mbar"
            ),
            radioButtons("radio",
                         label = h5("Where applicable, change acceptance criteria based on process type:", style = "margin-bottom: 20px;"),
                         choices = c("Evaporation" = 1, "Decarboxylation" = 2),
                         selected = 1
            ),
            tags$div(DT::DTOutput("info"))
          ),
          # Main panel for displaying outputs ----
          mainPanel(

            # Output: Data file ----
            shinycssloaders::withSpinner(plotlyOutput(outputId = "plot", height = "600px"),
                                         type = 3, color = "#18646e", size = 1, color.background = "#f1ede8"
            ),
            tags$div(DT::DTOutput("accept"), style = "width:90%; height:auto; margin-top: 50px;")
          )
        )


      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'processViz',
      all_files = TRUE
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

