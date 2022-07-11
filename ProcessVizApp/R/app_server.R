#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import echarts4r
#' @import readr
#' @import plotly
#' @import DT
#' @import shinycssloaders
#' @noRd
app_server <- function( input, output, session ) {

  options(shiny.maxRequestSize = 300 * 1024^2)

  # Two different cleans based on different filetype
  # - two filetypes for openinterface? txt/csv
  # set plots up for different columns
  # set timer up for different times?
  # generalize functions

  plot_df <- reactive({
    req(input$file1)

    type <- input$InputType

    if (type=="SDCard") {
      df <- CleanSDCardData(input)
    } else if (type=="OpenInterface") {
      df <- cleanOpenInterfaceData(input)
    }

    df

  })

  # Your application server logic
  output$contents <- DT::renderDT({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    DT::datatable(plot_df(),
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = "Blfrtip",
                    lengthMenu = list(
                      c(10, 25, 50, -1),
                      c(10, 25, 50)
                    )
                  )
    )


  })


  output$plot <- renderPlotly({

    hline <- function(y = 0, color = "#df973ab9") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dash")
      )
    }

    hline_2 <- function(y = 0, color = "#970909") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color)
      )
    }


    if (input$y_axis == "PressureAct (in mbar)" & input$radio == 1) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "PressureSet (in mbar)")

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "PressureAct", line = list(color = "#18646e")) %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 3], name = "PressureSet", line = list(color = "#970909")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8", hovermode = "x unified")

    } else if (input$y_axis == "PressureAct (in mbar)" & input$radio == 2) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "PressureSet (in mbar)")

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "PressureAct", line = list(color = "#18646e")) %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 3], name = "PressureSet", line = list(color = "#970909")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8", shapes = list(hline(190), hline(180)), hovermode = "x unified")


    } else if (input$y_axis == "BathAct (in Degrees C)" & input$radio == 1) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "BathSet (in Degrees C)")

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "BathAct", line = list(color = "#18646e")) %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 3], name = "BathSet", line = list(color = "#970909")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(
          plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8",
          font = list(family = "Quicksand"), yaxis = list(
            range = list(0, 100)
          ), shapes = list(hline(69), hline(75)), hovermode = "x unified"
        )


    } else if (input$y_axis == "BathAct (in Degrees C)" & input$radio == 2) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "BathSet (in Degrees C)")

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "BathAct", line = list(color = "#18646e")) %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 3], name = "BathSet", line = list(color = "#970909")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8", yaxis = list(
          range = list(0, 100)
        ), shapes = list(hline(77), hline(83)), hovermode = "x unified")

    } else if (input$y_axis == "ChillerAct (in Degrees C)" & input$radio == 1) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "ChillerSet (in Degrees C)")

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "ChillerAct", line = list(color = "#18646e")) %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 3], name = "ChillerSet", line = list(color = "#970909")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8", hovermode = "x unified")


    } else if (input$y_axis == "ChillerAct (in Degrees C)" & input$radio == 2) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "ChillerSet (in Degrees C)")

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "ChillerAct", line = list(color = "#18646e")) %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 3], name = "ChillerSet", line = list(color = "#970909")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8",hovermode = "x unified")

    } else if (input$y_axis == "Rotation (in rpm)") {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis)

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = "Rotation", line = list(color = "#18646e")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(
          plot_bgcolor = "#f1ede8",
          paper_bgcolor = "#f1ede8",
          font = list(family = "Quicksand"),
          hovermode = "x unified",
          yaxis = list(
            range = list(0, 160)
          )
        )

    } else {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis)

      plot_ly(dy, type = "scatter", mode = "lines") %>%
        add_trace(x = ~ dy[, 1], y = ~ dy[, 2], name = input$y_axis, line = list(color = "#18646e")) %>%
        layout(
          showlegend = F,
          xaxis = list(rangeslider = list(visible = T), title = "Timepoint"),
          yaxis = list(title = input$y_axis)
        ) %>%
        layout(plot_bgcolor = "#f1ede8", paper_bgcolor = "#f1ede8", hovermode = "x unified")

    }

  })


  output$accept <- DT::renderDT({


    if (input$y_axis == "BathAct (in Degrees C)" & input$radio == 1) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "BathSet (in Degrees C)", "Time (in seconds)")

      dy <- dy %>%
        filter(`Time (in seconds)` > 3600) %>%
        filter(`BathAct (in Degrees C)` < 69 | `BathAct (in Degrees C)` > 75)

      DT::datatable(dy,
                    caption = tags$caption(
                      style = "caption-side: top; text-align: center;",
                      "Observations below or above the acceptance criteria (after ramp up time of 1 hour)"
                    ),
                    rownames = FALSE,
                    extensions = c("Buttons"),
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = "Blfrtip",
                      lengthMenu = list(
                        c(10, 25, 50, -1),
                        c(10, 25, 50)
                      )
                    )
      )

    } else if (input$y_axis == "BathAct (in Degrees C)" & input$radio == 2) {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis, "BathSet (in Degrees C)", "Time (in seconds)")

      dy <- dy %>%
        filter(`Time (in seconds)` > 3600) %>%
        filter(`BathAct (in Degrees C)` < 77 | `BathAct (in Degrees C)` > 83)

      DT::datatable(dy,
                    caption = tags$caption(
                      style = "caption-side: top; text-align: center;",
                      "Observations below or above the acceptance criteria (after ramp up time of 1 hour)"
                    ),
                    rownames = FALSE,
                    extensions = c("Buttons"),
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = "Blfrtip",
                      lengthMenu = list(
                        c(10, 25, 50, -1),
                        c(10, 25, 50)
                      )
                    )
      )
    } else if (input$y_axis == "Foam present (binary)") {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis)

      dy <- dy %>%
        group_by(`Foam present (binary)`) %>%
        summarise(N = n()) %>%
        mutate("Relative Frequency" = N / sum(N))

      DT::datatable(dy,
                    caption = tags$caption(
                      style = 'caption-side: top; text-align: center;',
                      'Frequency Table'),
                    rownames = FALSE,
                    extensions = c("Buttons"),
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = "Blfrtip",
                      lengthMenu = list(
                        c(10, 25, 50, -1),
                        c(10, 25, 50)
                      )
                    )
      )

    } else if (input$y_axis == "Foam control (binary)") {

      dy <- dplyr::select(plot_df(), "Date", input$y_axis)

      dy <- dy %>%
        group_by(`Foam control (binary)`) %>%
        summarise(N = n()) %>%
        mutate("Relative Frequency" = N / sum(N))

      DT::datatable(dy,
                    caption = tags$caption(
                      style = 'caption-side: top; text-align: center;',
                      'Frequency Table'),
                    rownames = FALSE,
                    extensions = c("Buttons"),
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = "Blfrtip",
                      lengthMenu = list(
                        c(10, 25, 50, -1),
                        c(10, 25, 50)
                      )
                    )
      )

    }

  })


  output$info <- DT::renderDT({


    info <- data.frame("Process Value" = c(
      "PressureAct",
      "PressureSet",
      "BathAct",
      "BathSet",
      "ChillerAct",
      "ChillerSet",
      "Rotation",
      "Vapor",
      "Foam present",
      "Foam control"
    ), Unit = c(
      "mbar",
      "mbar",
      "Degrees C  ",
      "Degrees C  ",
      "Degrees C  ",
      "Degrees C  ",
      "rpm ",
      "Degrees C  ",
      "bin ",
      "bin "
    ), Evaporation = c(
      "none",
      "setpoint: 185 mbar",
      "72 Degrees C +- 3 Degrees C",
      "setpoint: 72 Degrees C",
      "none",
      "n/a",
      "none",
      "none",
      "n/a",
      "n/a"
    ), Decarboxylation = c(
      "185 mbar +- 5 mbar ",
      "setpoint: 185 mbar",
      "80 Degrees C +- 3 Degrees C ",
      "setpoint: 80 Degrees C",
      "none",
      "n/a",
      "none",
      "none",
      "n/a",
      "n/a"
    ))

    DT::datatable(info,
                  caption = tags$caption(
                    style = "caption-side: top; text-align: center;",
                    "Acceptance Criteria"
                  ),
                  rownames = FALSE,
                  options = list(
                    scrollX = TRUE,
                    dom = "t",
                    ordering = FALSE
                  )
    )
  })

}


#' @title cleanOpenInterfaceData
#'
#' @param input user input
#'
#' @return df
cleanOpenInterfaceData <- function(input) {

  if (input$ManualTime == "On") {
    t_start <- clean_time_date_input(input)
  } else {
    t_start <- file.info(input$file1$datapath)["mtime"][1] %>%
      t %>%
      lubridate::ymd_hms()
  }

  df <- read.csv(
    input$file1$datapath,
    header = T) %>%
    janitor::clean_names()

  df <- df %>%
    mutate(time_s_orig = time_s) %>%
    mutate(time_s = time_s-input$SecondsToSubtract) %>%
    select(-hold,
           -auto_dest_diff,
           -auto_dest_in,
           -auto_dest_out,
           -lift_act,
           -lift_end,
           -pump_act_0_1,
           -vac_open) %>%
    as.data.frame() %>%
    mutate(Date = time_s + t_start) %>%
    mutate(Time_hours = (time_s / 60) / 60) %>%
    select(time_s, time_s_orig, everything())

  names_full <- c(
    "Time (in seconds)",
    "Original Time (in seconds)",
    "PressureAct (in mbar)",
    "PressureSet (in mbar)",
    "BathAct (in Degrees C)",
    "BathSet (in Degrees C)",
    "ChillerAct (in Degrees C)",
    "ChillerSet (in Degrees C)",
    "Rotation (in rpm)",
    "Vapor (in Degrees C)",
    "Foam control (binary)",
    "Date",
    "Time (in hours)"
  )

  names(df) <- names_full

  df
}


#' @title CleanSDCardData
#'
#' @param input user input
#'
#' @import dplyr
#' @import janitor
#' @import readr
#' @import lubridate
#' @import utils
#' @return df
CleanSDCardData <- function(input) {

  file_type <- tools::file_ext(input$file1$datapath)

  if (file_type == "txt") {

    time <- read_csv(input$file1$datapath,
                     n_max = 1,
                     col_names = T) %>%
      janitor::clean_names()

    t_start <- find_time_start_SDCard(
      input,
      time)

    df <- read.csv(
      file = input$file1$datapath,
      skip = 2,
      header = F) %>%
      select(-V15)

    names(df) <- names(time)


  } else if(file_type == "csv") {
    time <- read_csv2(
      input$file1$datapath,
      n_max = 1,
      col_names = TRUE
    ) %>%
      janitor::clean_names()

    t_start <- find_time_start_SDCard(
      input,
      time)

    df <- read.csv2(
      file = input$file1$datapath,
      header = F,
      skip = 2
    )

    names(df) <- names(time)

  }

  df <- df %>%
    select(
      -hold,
      -auto_dest_diff,
      -cw_act) %>%
    as.data.frame() %>%
    mutate(Date = time_s + t_start) %>%
    mutate(Time_hours = (time_s / 60) / 60)

  names_full <- c(
    "Time (in seconds)",
    "PressureAct (in mbar)",
    "PressureSet (in mbar)",
    "BathAct (in Degrees C)",
    "BathSet (in Degrees C)",
    "ChillerAct (in Degrees C)",
    "ChillerSet (in Degrees C)",
    "Rotation (in rpm)",
    "Vapor (in Degrees C)",
    "Foam present (binary)",
    "Foam control (binary)",
    "Date",
    "Time (in hours)"
  )

  names(df) <- names_full

  df
}


#' find_time_start_SDCard
#'
#' @param input user input
#' @param time time from user input
#'
#' @import dplyr
#' @import lubridate
#'
find_time_start_SDCard <- function(input, time) {

  if (input$ManualTime == "On") {
    t_start <- clean_time_date_input(input)
  } else {
    t_start <- time %>%
      mutate(
        time_s = lubridate::dmy_hm(
          time_s)) %>%
      pull(time_s)
  }
  t_start
}

#' clean_time_date_input
#'
#' @param input user input
#'
clean_time_date_input <- function(input) {
  TimeInput <- strftime(
    input$TimeInput,
    format="%H:%M:%S")

  t_start <- lubridate::ymd_hms(
    paste(input$DateInput,
          TimeInput))

  t_start
}
