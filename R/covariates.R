# covariates_.R

covariates_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h5(tags$b("Select covariates from the list, then submit")),

    tags$p("1. Select from the list"),
    tags$br(),

    tags$p("1a. If not in the list: add new entry:"),
    textInput(ns("new_category"), "Category"),
    textInput(ns("new_definition"), "Definition"),
    textInput(ns("new_source"), "Source"),
    textInput(ns("new_resolution"), "Resolution"),
    actionButton(ns("add_new_entry"), "Add New Entry to List"),
    tags$hr(),
    DTOutput(ns("my_table")),
    actionButton(ns("submit_selection"), "2. Submit Selection"),
    tags$br(), tags$br(),
    tags$p("Covariates selected:"),
    DTOutput(ns("selected_data_table")),
    br()
  )
}


covariates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load data
    if (file.exists(here::here("inst/app/www/datasets_solutions/Covariates_BAM.csv"))) {
      data_covariates <- read.csv(here::here("inst/app/www/datasets_solutions/Covariates_BAM.csv")) %>%
        dplyr::select(category, definition, source, resolution)
    } else {
      data_covariates <- data.frame(
        category = c("Category A", "Category B", "Category C"),
        definition = c("Definition for A", "Definition for B", "Definition for C"),
        source = c("Source A", "Source B", "Source C"),
        resolution = c("Resolution A", "Resolution B", "Resolution C")
      )
      warning("CSV file not found. Using dummy data for demonstration.")
    }

    data <- reactiveVal(data_covariates)

    # new_entries <- reactiveVal(
    #   data.frame(
    #     category = character(0),
    #     definition = character(0),
    #     source = character(0),
    #     resolution = character(0),
    #     stringsAsFactors = FALSE
    #   )
    # )

    observeEvent(input$add_new_entry, {
      req(input$new_category, input$new_definition, input$new_source, input$new_resolution)

      new_row <- data.frame(
        category = input$new_category,
        definition = input$new_definition,
        source = input$new_source,
        resolution = input$new_resolution,
        stringsAsFactors = FALSE
      )
#
#       current_entries <- new_entries()
#       new_entries(rbind(current_entries, new_row))

      data(rbind(new_row, data()))

      # Clear inputs
      updateTextInput(session, "new_category", value = "")
      updateTextInput(session, "new_definition", value = "")
      updateTextInput(session, "new_source", value = "")
      updateTextInput(session, "new_resolution", value = "")
    })

    output$my_table <- renderDT({
      datatable(
        data(),
        extensions = 'Select',
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          select = list(style = 'multi', selector = 'td:first-child'),
          scrollY = "300px",
          scrollX = TRUE,
          scrollCollapse = TRUE
        ),
        selection = 'none',
        callback = JS(
          "table.on('click', 'td:first-child', function(){",
          "  var row_index = table.cell(this).index().row;",
          "  var row_data = table.row(row_index).data();",
          "  var checkbox = $(this).find('input[type=\"checkbox\"]');",
          "  if (checkbox.prop('checked')) {",
          "    table.row(row_index).deselect();",
          "  } else {",
          "    table.row(row_index).select();",
          "  }",
          "});",
          "table.on('select', function(e, dt, type, indexes){",
          "  if (type === 'row') {",
          "    var checkbox = dt.cells(indexes, 0).nodes().to$().find('input[type=\"checkbox\"]');",
          "    checkbox.prop('checked', true);",
          "  }",
          "});",
          "table.on('deselect', function(e, dt, type, indexes){",
          "  if (type === 'row') {",
          "    var checkbox = dt.cells(indexes, 0).nodes().to$().find('input[type=\"checkbox\"]');",
          "    checkbox.prop('checked', false);",
          "  }",
          "});",
          "return table.column(0).nodes().to$().each(function(i, td) {",
          "  $(td).html('<input type=\"checkbox\">');",
          "});"
        )
      )
    }, server = FALSE)

    submitted_final_data <- eventReactive(input$submit_selection, {
      selected_indices <- input$my_table_rows_selected
      existing_selected_data <- data.frame()

      if (length(selected_indices) > 0) {
        existing_selected_data <- data()[selected_indices, ]
      }

      final_data <- rbind(existing_selected_data)

      if (nrow(final_data) > 0) {
        return(final_data)
      } else {
        return(data.frame(
          category = character(0),
          definition = character(0),
          source = character(0),
          resolution = character(0),
          stringsAsFactors = FALSE
        ))
      }
    })

    output$selected_data_table <- renderDT({
      req(submitted_final_data())
      datatable(
        submitted_final_data(),
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          scrollY = "300px",
          scrollX = TRUE,
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      )
    })
  })
}

covariates_test <- function() {
  # Make sure covariates_ui and covariates_server are already defined or sourced
  ui <- fluidPage(
    covariates_ui("test")
  )

  server <- function(input, output, session) {
    covariates_server("test")
  }

  shinyApp(ui = ui, server = server)
}

render_covariates <- function(element_id){
  covariates_ui(element_id)
}
