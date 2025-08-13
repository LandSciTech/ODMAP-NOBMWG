data_integration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h5(tags$b("Select and Edit Data Types")),

    tags$p("1. Select from the list"),
    tags$br(),
    tags$p("1a. If not in the list: add new entry:"),

    textInput(ns("new_datatype"), "Methods"),
    textInput(ns("new_source"), "Description"),
    textInput(ns("new_comments"), "Comments"),
    actionButton(ns("add_new_entry"), "Add New Entry to List"),

    tags$hr(),

    DTOutput(ns("my_table")),
    actionButton(ns("submit_selection"), "2. Submit Selection"),

    tags$br(), tags$br(),
    tags$p("Selected and Submitted Covariates:"),
    DTOutput(ns("selected_data_table"))
  )
}

data_integration_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    initial_df <- read.csv(here::here("www/Data_integration.csv"))

    current_data <- reactiveVal(initial_df)

    observeEvent(input$add_new_entry, {
      req(input$new_datatype, input$new_source, input$new_comments)

      new_row <- data.frame(
        Methods = input$new_datatype,
        Description = input$new_source,
        Comments = input$new_comments,
        stringsAsFactors = FALSE
      )

      current_data(rbind(current_data(), new_row))

      updateTextInput(session, "new_datatype", value = "")
      updateTextInput(session, "new_source", value = "")
      updateTextInput(session, "new_comments", value = "")
    })

    output$my_table <- renderDT({
      datatable(
        current_data(),
        editable = list(target = "cell", disable = list(columns = c(1,2))), # disable editing Methods and Description (col 1 & 2)
        extensions = "Select",
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

    observeEvent(input$my_table_cell_edit, {
      info <- input$my_table_cell_edit
      data_edit <- current_data()
      data_edit[info$row, info$col] <- info$value
      current_data(data_edit)
    })

    submitted_final_data <- eventReactive(input$submit_selection, {
      selected_indices <- input$my_table_rows_selected
      if (length(selected_indices) > 0) {
        current_data()[selected_indices, , drop = FALSE]
      } else {
        data.frame(
          Methods = character(0),
          Description = character(0),
          Comments = character(0),
          stringsAsFactors = FALSE
        )
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


data_integration_test <- function() {
  library(shiny)
  library(DT)
  library(dplyr)
  library(here)

  # Load or define your data here:
  df <<- read.csv(here::here("www/Data_integration.csv"))

  ui <- fluidPage(
    data_integration_ui("test")
  )

  server <- function(input, output, session) {
    data_integration_server("test")
  }

  shinyApp(ui, server)
}

render_data_integration <- function(element_id){
  data_integration_ui(element_id)
}
