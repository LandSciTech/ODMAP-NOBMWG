
years_ui <- function(id) {
  ns <- NS(id)

  # Current year and ranges
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  available_years <- 1900:current_year
  future_years_range <- (current_year + 1):(current_year + 20)

  tagList(
    h5("Select a single year (e.g., `1980`) or two years to represent a range (e.g., `1970` `1995`) and then submit.", style = "font-weight: bold"),

    # Past years select input
    selectInput(
      ns("selected_years_past"),
      label = "Select Past/Current Year(s):",
      choices = available_years,
      selected = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),

    # Future years select input
    selectInput(
      ns("selected_years_future"),
      label = "Select Future Year(s):",
      choices = future_years_range,
      selected = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),

    actionButton(ns("record_selection_btn"), "Record Selection"),

    hr(),

    p("Current Selection:"),
    tableOutput(ns("current_overall_range_table")),

    hr(),

    p("Recorded selection:"),
    tableOutput(ns("selection_history_table"))
  )
}

years_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive value to store history
    overall_range_history_df <- reactiveVal(
      data.frame(
        SelectedYearsRaw = character(0),
        OverallRange = character(0),
        stringsAsFactors = FALSE
      )
    )

    # Reactive for current overall range string
    current_overall_range_string <- reactive({
      selected_past_years <- input$selected_years_past
      selected_future_years <- input$selected_years_future

      all_selected_years <- c(
        if (!is.null(selected_past_years)) as.numeric(selected_past_years),
        if (!is.null(selected_future_years)) as.numeric(selected_future_years)
      )

      if (length(all_selected_years) == 0) {
        "No years selected."
      } else if (length(all_selected_years) == 1) {
        as.character(all_selected_years)
      } else {
        paste0(min(all_selected_years), "-", max(all_selected_years))
      }
    })

    # Output current selection table
    output$current_overall_range_table <- renderTable({
      data.frame(
        "Overall Range" = current_overall_range_string(),
        stringsAsFactors = FALSE
      )
    })

    # Observe record selection button
    observeEvent(input$record_selection_btn, {
      current_selected_past_years_raw <- input$selected_years_past
      current_selected_future_years_raw <- input$selected_years_future

      combined_selected_years_raw <- c(current_selected_past_years_raw, current_selected_future_years_raw)
      current_range_str <- current_overall_range_string()

      if (!is.null(combined_selected_years_raw) &&
          length(combined_selected_years_raw) > 0 &&
          current_range_str != "No years selected.") {

        sorted_selected_years_str <- paste(sort(as.numeric(combined_selected_years_raw)), collapse = ",")
        current_history_df <- overall_range_history_df()

        if (!sorted_selected_years_str %in% current_history_df$SelectedYearsRaw) {
          new_row_data <- data.frame(
            SelectedYearsRaw = sorted_selected_years_str,
            OverallRange = current_range_str,
            stringsAsFactors = FALSE
          )
          updated_history_df <- rbind(current_history_df, new_row_data)
          overall_range_history_df(updated_history_df)
          showNotification(paste("Recorded:", current_range_str), type = "message", duration = 2)
        } else {
          showNotification("This selection is already recorded.", type = "warning", duration = 2)
        }
      } else {
        showNotification("Please select at least one year to record.", type = "error", duration = 3)
      }
    })

    # Output history table
    output$selection_history_table <- renderTable({
      history_df <- overall_range_history_df()
      if (nrow(history_df) == 0) {
        data.frame("Overall Range" = character(0), stringsAsFactors = FALSE)
      } else {
        data.frame("Overall Range" = history_df$OverallRange, stringsAsFactors = FALSE)
      }
    })
    return(overall_range_history_df)
  })
}

years_test <- function() {
  ui <- fluidPage(
    years_ui("test1")
  )

  server <- function(input, output, session) {
    years_server("test1")
  }

  shinyApp(ui, server)
}

render_years = function(element_id, element_placeholder){
  years_ui(element_id)
}
