library(shiny)
library(dplyr)

data_cleaning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Select methods for data cleaning", style = "font-weight: bold"),

    selectInput(
      inputId = ns("main_item_select"),
      label = "Select Main Item:",
      choices = NULL  # we'll update choices in server
    ),
    hr(),

    uiOutput(ns("sub_item_selection_ui")),
    hr(),

    actionButton(ns("summarize_button"), "Summarize Selections"),
    tags$hr(),

    p("Summary of Selections:"),
    tableOutput(ns("summary_table"))
  )
}

data_cleaning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    main_items_data <- read.csv(here::here("inst/app/www/datasets_solutions/data_cleaning.csv"))

    # Update main item selectInput choices dynamically on start
    observe({
      updateSelectInput(session, "main_item_select", choices = names(main_items_data), selected = names(main_items_data)[1])
    })

    selected_items_df <- reactiveVal(
      data.frame(
        MainItem = character(),
        SubItem = character(),
        Comment = character(),
        stringsAsFactors = FALSE
      )
    )

    output$sub_item_selection_ui <- renderUI({
      req(input$main_item_select)
      current_main_item <- input$main_item_select
      sub_items <- main_items_data[[current_main_item]]
      sub_items <- subset(sub_items, subset = sub_items != "")

      if (length(sub_items) > 0) {
        lapply(sub_items, function(sub_item) {
          div(
            checkboxInput(
              inputId = ns(paste0("checkbox_", gsub(" ", "_", sub_item))),
              label = sub_item,
              value = FALSE
            ),
            conditionalPanel(
              condition = paste0("input['", ns(paste0("checkbox_", gsub(" ", "_", sub_item))), "'] == true"),
              textInput(
                inputId = ns(paste0("comment_", gsub(" ", "_", sub_item))),
                label = paste("Comment on:"),
                placeholder = "Enter your comment here"
              )
            ),
            hr()
          )
        })
      } else {
        p(paste("No sub-items available for", current_main_item, "."))
      }
    })

    observeEvent(input$summarize_button, {
      current_main_item <- input$main_item_select
      sub_items <- main_items_data[[current_main_item]]

      if (length(sub_items) > 0) {
        current_selections <- data.frame(
          MainItem = character(),
          SubItem = character(),
          Comment = character(),
          stringsAsFactors = FALSE
        )

        for (sub_item in sub_items) {
          checkbox_id <- paste0("checkbox_", gsub(" ", "_", sub_item))
          comment_id <- paste0("comment_", gsub(" ", "_", sub_item))

          if (isTRUE(input[[checkbox_id]])) {
            comment_text <- input[[comment_id]]
            if (is.null(comment_text)) comment_text <- ""
            current_selections <- rbind(
              current_selections,
              data.frame(
                MainItem = current_main_item,
                SubItem = sub_item,
                Comment = comment_text,
                stringsAsFactors = FALSE
              )
            )
          }
        }

        existing_data <- selected_items_df()
        updated_data <- existing_data %>%
          filter(MainItem != current_main_item) %>%
          rbind(current_selections)
        selected_items_df(updated_data)
      } else {
        existing_data <- selected_items_df()
        updated_data <- existing_data %>%
          filter(MainItem != current_main_item)
        selected_items_df(updated_data)
      }
    })

    output$summary_table <- renderTable({
      if (nrow(selected_items_df()) == 0) {
        data.frame(Message = "No items selected yet. Please make selections and click 'Summarize Selections'.")
      } else {
        selected_items_df()
      }
    })
    return(selected_items_df)
  })
}

data_cleaning_test <- function() {
  shinyApp(
    ui = fluidPage(
      data_cleaning_ui("test")
    ),
    server = function(input, output, session) {
      data_cleaning_server("test")
    }
  )
}

render_data_cleaning <- function(element_id){
  data_cleaning_ui(element_id)
}
