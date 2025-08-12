# Module UI function
model_utility_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Please select the potential utility of your model.", style = "font-weight: bold"),
    "The following list will help you to find the potential uses and applications of your model. We organize them into categories (1) and options (2) within those categories. There is a field to add options in case you did not find one that accommodate your utility.",
    "Please select all options that fit your model and classify them using the scale below (3):",
    tags$ul(
      tags$li("High: Use of this model for this utility and/or applicability is HIGHLY appropriate"),
      tags$li("Medium: Use of this model for this utility and/or applicability is SOMEWHAT appropriate"),
      tags$li("Low: Use of this model for this utility and/or applicability is RARELY appropriate"),
      tags$li("Inappropriate: Use of this model for this utility and/or applicability is NOT appropriate")
    ),
    "In the comment section you can elaborate on the potential use and/or application of your model. You might provide examples of limitations and incorrect interpretations.",
    br(),


    selectInput(
      inputId = ns("main_items"),
      label = "1. Category:",
      choices = c("Select an option", NULL),
      multiple = FALSE
    ),
    hr(),
    uiOutput(ns("subitem_selection_ui")),
    hr(),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'Select an option' && input['%s'] != ''", ns("main_items"), ns("main_items")),
      p(strong("If not in the list, then create a new Option below: "), textOutput(ns("current_category_for_option"), inline = TRUE)),
      textInput(
        inputId = ns("new_subitem_name"),
        label = " ",
        placeholder = "Describe the new Option here..."
      ),
      actionButton(
        inputId = ns("add_new_subitem_btn"),
        label = "Add New Option",
        icon = icon("plus"),
        class = "btn-warning"
      )
    ),
    radioButtons(
      inputId = ns("classification"),
      label = "3. Appropriateness classification:",
      choices = c("High", "Medium", "Low", "Inappropriate"),
      selected = "Medium"
    ),
    textAreaInput(
      inputId = ns("comments"),
      label = "4. Comments:",
      rows = 3,
      placeholder = "Add your comments here..."
    ),
    actionButton(
      inputId = ns("add_to_list"),
      label = "5. Add to List",
      icon = icon("plus-circle")
    ),
    p("Summary of selected Items List", style = "font-weight: bold"),
    tableOutput(ns("selected_items_table")),
    # downloadButton(ns("download_data"), "Download List (CSV)"),
    hr(),
    # h6("Important Web Resources:", style = "font-weight: bold"),
    # hr(),
    # tags$div(
    #   id = ns("scrollableLinksContainer"),
    #   style = "height: 250px; overflow-y: scroll; border: 1px solid #ccc;
    #                padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
    #   uiOutput(ns("scrollableLinkList"))
    # )
  )
}

# Module server function
model_utility_mod_server <- function(id, model_utility_df, link_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to create category-items list
    create_category_items_list <- function(df) {
      unique_categories <- unique(df$Category)
      category_items_list <- list()
      for (category in unique_categories) {
        items_in_category <- df %>%
          filter(Category == category) %>%
          distinct(Options) %>%
          pull(Options)
        category_items_list[[category]] <- items_in_category
      }
      category_items_list
    }

    # Reactive data store for category items (mutable)
    reactive_items_data <- reactiveVal(create_category_items_list(model_utility_df))

    # Reactive for selected items list
    selected_items_list <- reactiveVal(
      data.frame(
        Category = character(),
        Option = character(),
        Appropriateness = character(),
        Comments = character(),
        stringsAsFactors = FALSE
      )
    )

    # Update main_items select choices when module loads or reactive_items_data changes
    observe({
      choices <- c("Select an option", names(reactive_items_data()))
      updateSelectInput(session, "main_items", choices = choices)
    })

    output$current_category_for_option <- renderText({
      if (input$main_items == "Select an option" || is.null(input$main_items) || input$main_items == "") {
        "None Selected"
      } else {
        input$main_items
      }
    })

    output$subitem_selection_ui <- renderUI({
      req(input$main_items)
      if (input$main_items == "Select an option") {
        selectInput(
          inputId = ns("sub_items"),
          label = "2. Option selected:",
          choices = "Please select a main option first",
          selected = "Please select a main option first",
          multiple = FALSE
        )
      } else {
        available_subitems <- reactive_items_data()[[input$main_items]]
        selectInput(
          inputId = ns("sub_items"),
          label = "2. Option:",
          choices = available_subitems,
          multiple = FALSE
        )
      }
    })

    observeEvent(input$add_new_subitem_btn, {
      req(input$main_items)
      req(input$new_subitem_name)

      if (input$main_items == "Select an option") {
        showNotification("Please select a main option before adding a new option.", type = "warning", duration = 3)
        return()
      }

      current_items_list <- reactive_items_data()
      current_main_item <- input$main_items
      new_subitem_to_add <- input$new_subitem_name

      if (new_subitem_to_add %in% current_items_list[[current_main_item]]) {
        showNotification(
          paste0("Option '", new_subitem_to_add, "' already exists for '", current_main_item, "'."),
          type = "warning", duration = 5
        )
        return()
      }

      current_items_list[[current_main_item]] <- sort(unique(c(current_items_list[[current_main_item]], new_subitem_to_add)))
      reactive_items_data(current_items_list)

      updateTextInput(session, "new_subitem_name", value = "")
      showNotification(
        paste0("Option '", new_subitem_to_add, "' added to '", current_main_item, "'."),
        type = "message", duration = 3
      )

      updateSelectInput(session, "sub_items", selected = new_subitem_to_add)
    })

    observeEvent(input$add_to_list, {
      req(input$main_items, input$sub_items)

      if (input$main_items == "Select an option") {
        showNotification("Please select a valid main option before adding to list.", type = "warning", duration = 3)
        return()
      }
      if (input$sub_items == "Please select a main option first") {
        showNotification("Please select a sub-option before adding to list.", type = "warning", duration = 3)
        return()
      }

      new_row <- data.frame(
        Category = input$main_items,
        Option = input$sub_items,
        Appropriateness = input$classification,
        Comments = input$comments,
        stringsAsFactors = FALSE
      )

      updated_list <- rbind(selected_items_list(), new_row)
      selected_items_list(updated_list)

      updateSelectInput(session, "main_items", selected = "Select an option")
      updateSelectInput(session, "sub_items", selected = character(0))
      updateRadioButtons(session, "classification", selected = "Medium")
      updateTextAreaInput(session, "comments", value = "")
    })

    output$selected_items_table <- renderTable({
      if (nrow(selected_items_list()) == 0) {
        return(data.frame(Message = "No items added yet. Select options and click 'Add to List'."))
      }
      selected_items_list()
    })

    # output$download_data <- downloadHandler(
    #   filename = function() {
    #     paste("selected_items_", Sys.Date(), ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(selected_items_list(), file, row.names = FALSE)
    #   }
    # )

    # output$scrollableLinkList <- renderUI({
    #   if (nrow(link_data) == 0) {
    #     return(p("No links available to display."))
    #   }
    #   link_elements <- lapply(1:nrow(link_data), function(i) {
    #     current_name <- link_data$Author[i]
    #     current_url <- link_data$References[i]
    #     tags$li(
    #       tags$p(
    #         tags$a(href = current_url, target = "_blank", current_name,
    #                style = "font-weight: bold; margin-right: 5px; text-decoration: none; color: #0056b3;")
    #       )
    #     )
    #   })
    #   tags$ul(
    #     link_elements,
    #     style = "list-style-type: none; padding-left: 0; margin-top: 0;"
    #   )
    # })
    return(selected_items_list)
  })
}

render_model_utility = function(element_id, element_placeholder){
  model_utility_mod_ui(element_id)
}
