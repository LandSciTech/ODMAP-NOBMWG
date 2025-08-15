
# Module UI (returns tagList, no sidebarLayout)
datatype_source_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Select and Edit Data Types", style = "font-weight: bold"),
    p("1. Select from the list"),
    br(),
    p("1a. If not in the list: add new entry:"),
    textInput(ns("new_datatype"), "Data_type"),
    textInput(ns("new_source"), "Data_source"),
    textInput(ns("new_comments"), "Comments"),
    actionButton(ns("add_new_entry"), "Add New Entry to List"),
    hr(),
    DTOutput(ns("my_table")),
    br(),
    actionButton(ns("submit_selection"), "2. Submit Selection"),
    br(), br(),
    p("Selected and Submitted Covariates:"),
    DTOutput(ns("selected_data_table")),
    br()
  )
}

# Module Server
datatype_source_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Initial data frame
    initial_df <- data.frame(
      Data_type = c("Presence-only", "Presence/Absence", "Point occurrence", "Counts", "Richness", "Integration of aeral and point data"),
      Data_source = rep("Add the data source here (website, repo, etc.)", 6),
      Comments = rep("Add your comments/explain here", 6),
      stringsAsFactors = FALSE
    )

    ns <- session$ns

    current_data <- reactiveVal(initial_df)

    observeEvent(input$add_new_entry, {
      req(input$new_datatype, input$new_source, input$new_comments)
      new_row <- data.frame(
        Data_type = input$new_datatype,
        Data_source = input$new_source,
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
        editable = list(target = 'cell', disable = list(columns = 0)),
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
      data_to_edit <- current_data()
      data_to_edit[info$row, info$col] <- info$value
      current_data(data_to_edit)
    })

    submitted_final_data <- eventReactive(input$submit_selection, {
      selected_indices <- input$my_table_rows_selected
      if (length(selected_indices) > 0) {
        current_data()[selected_indices, ]
      } else {
        data.frame(Data_type=character(0), Data_source=character(0), Comments=character(0), stringsAsFactors = FALSE)
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

# Test function
test_datatype_source <- function() {
  shinyApp(
    ui = fluidPage(
      datatype_source_ui("test1")
    ),
    server = function(input, output, session) {
      datatype_source_server("test1")
    }
  )
}

render_datatype_source <- function(element_id, element_placeholder){
  datatype_source_ui(element_id)
}
