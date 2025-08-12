
modelling_packages_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),

    p("Select packages used for modelling, then double click to add the Version and Submit Selection"),

    fluidRow(
      column(12,
             p("Add new entry (if not in the list):"),
             textInput(ns("new_Software"), "Software"),
             textInput(ns("new_Package"), "Package"),
             textInput(ns("new_Version"), "Version"),
             textInput(ns("new_Description"), "Description"),
             textInput(ns("new_Source"), "Source URL"),
             actionButton(ns("add_new_entry"), "Add New Entry to List"),
             uiOutput(ns("add_entry_message"))
      )
    ),

    fluidRow(
      column(12,
             DTOutput(ns("my_table")),
             tags$hr(),
             actionButton(ns("submit_selection"), "Submit Selection"),
             p("Packages selected:"),
             DTOutput(ns("selected_data_table"))
      )
    ),

    # JS for checkbox click to send original row id to Shiny module input
    tags$script(HTML(sprintf("
      $(document).on('change', '#%s .row_selector', function() {
        var original_row_id = $(this).data('original-row-id');
        var is_checked = $(this).prop('checked');
        Shiny.setInputValue('%smy_table_checkbox_clicked', {
          row: original_row_id,
          checked: is_checked
        }, {priority: 'event'});
      });
    ", ns("my_table"), ns(""))))
  )
}

modelling_packages_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ... Load your CSV and prepare data_packages exactly as before ...
    data_packages <- tryCatch({
      read.csv(here::here("inst/app/www/datasets_solutions/Modelling_SDMs_Packages.csv"),
               stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(paste("Could not read CSV file:", e$message))
      data.frame(
        Software = character(0),
        Package = character(0),
        Version = character(0),
        Description = character(0),
        URL = character(0),
        stringsAsFactors = FALSE
      )
    })

    # Build clickable source link column
    if ("URL" %in% colnames(data_packages) &&
        ("Package" %in% colnames(data_packages) || "Name" %in% colnames(data_packages))) {
      package_col <- ifelse("Package" %in% colnames(data_packages), "Package", "Name")
      data_packages$Source <- paste0(
        "<a href='", data_packages$URL, "' target='_blank'>",
        data_packages[[package_col]],
        "</a>")
    } else {
      data_packages$Source <- ""
      warning("Missing 'URL' or 'Package'/'Name' column. Source links might not be generated correctly.")
    }

    data_packages <- data_packages %>%
      dplyr::select(Software, Package, Version, Description, Source)

    app_data <- reactiveValues(full_table = data_packages)

    selected_rows_indices <- reactiveVal(integer(0))

    # Add new entry observe
    observeEvent(input$add_new_entry, {
      req(input$new_Software, input$new_Package, input$new_Version, input$new_Description, input$new_Source)

      if (!grepl("^https?://", input$new_Source)) {
        output$add_entry_message <- renderUI({
          HTML("<p style='color:red;'>Source must be a valid URL (starts with http:// or https://)</p>")
        })
        return()
      }

      new_source_link <- paste0("<a href='", input$new_Source, "' target='_blank'>", input$new_Package, "</a>")

      new_row <- data.frame(
        Software = input$new_Software,
        Package = input$new_Package,
        Version = input$new_Version,
        Description = input$new_Description,
        Source = new_source_link,
        stringsAsFactors = FALSE
      )

      app_data$full_table <- rbind(app_data$full_table, new_row)

      updateTextInput(session, "new_Software", value = "")
      updateTextInput(session, "new_Package", value = "")
      updateTextInput(session, "new_Version", value = "")
      updateTextInput(session, "new_Description", value = "")
      updateTextInput(session, "new_Source", value = "")

      output$add_entry_message <- renderUI({
        HTML("<p style='color:green;'>Entry added successfully!</p>")
      })
      shinyjs::delay(3000, output$add_entry_message <- renderUI(NULL))
    })

    # Render DT with checkboxes
    output$my_table <- renderDT({
      df <- app_data$full_table
      selected_idx_original <- selected_rows_indices()

      df_with_original_id <- df %>% mutate(original_row_id = seq_len(n()))

      df_display <- df_with_original_id %>%
        mutate(
          Checkbox = purrr::map_chr(row_number(), ~{
            original_id <- df_with_original_id$original_row_id[.x]
            checked_attr <- if (original_id %in% selected_idx_original) "checked" else ""
            sprintf("<input type='checkbox' class='row_selector' data-original-row-id='%d' %s>", original_id, checked_attr)
          })
        ) %>%
        dplyr::select(Checkbox, Software, Package, Version, Description, Source)

      datatable(
        df_display,
        editable = list(target = "cell", columns = 3),
        escape = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          ordering = FALSE,
          scrollY = "300px",
          scrollX = TRUE,
          scrollCollapse = TRUE,
          columnDefs = list(list(orderable = FALSE, className = "dt-center", targets = 0))
        ),
        selection = "none",
        rownames = FALSE
      )
    }, server = TRUE)

    # Checkbox click handler from JS
    observeEvent(input$my_table_checkbox_clicked, {
      info <- input$my_table_checkbox_clicked
      row_original_id <- as.numeric(info$row)
      is_checked <- info$checked

      current_selected <- selected_rows_indices()

      if (is_checked) {
        if (!row_original_id %in% current_selected) {
          selected_rows_indices(sort(c(current_selected, row_original_id)))
        }
      } else {
        selected_rows_indices(setdiff(current_selected, row_original_id))
      }
    })

    # Editable version column handler
    observeEvent(input$my_table_cell_edit, {
      info <- input$my_table_cell_edit
      if (info$col == 3) {
        version_col_index <- which(colnames(app_data$full_table) == "Version")
        app_data$full_table[info$row, version_col_index] <- info$value
        showNotification(paste("Version updated for row", info$row, "to", info$value), type = "message", duration = 2)
      } else {
        showNotification("Only the Version column is editable.", type = "warning", duration = 3)
      }
    })

    # Submit selection reactive
    submitted_final_data <- eventReactive(input$submit_selection, {
      selected_idx <- selected_rows_indices()
      df <- app_data$full_table
      if (length(selected_idx) > 0) {
        df[selected_idx, , drop = FALSE]
      } else {
        data.frame(
          Software = character(0),
          Package = character(0),
          Version = character(0),
          Description = character(0),
          Source = character(0),
          stringsAsFactors = FALSE
        )
      }
    })

    output$selected_data_table <- renderDT({
      req(submitted_final_data())
      datatable(
        submitted_final_data(),
        escape = FALSE,
        options = list(
          dom = "t",
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


modelling_packages_test <- function() {
  shinyApp(
    ui = fluidPage(
      modelling_packages_ui("mod1")
    ),
    server = function(input, output, session) {
      modelling_packages_server("mod1")
    }
  )
}

render_modelling_packages <- function(element_id, element_placeholder){
  modelling_packages_ui(element_id)
}

