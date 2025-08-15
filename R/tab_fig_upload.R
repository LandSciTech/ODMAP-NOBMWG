library(shiny)
library(DT)
library(dplyr)

# Module UI
tab_fig_upload_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Upload Figures and Tables and describe them"),
    sidebarLayout(
      sidebarPanel(
        h4("1. Species"),
        fileInput(ns("figure1"), "Upload Figure 1 (Species Tab)",
                  multiple = FALSE,
                  accept = c("image/png", "image/jpeg", "image/gif")),
        textAreaInput(ns("comment_figure1"), "Comment for Figure 1:", ""),
        fileInput(ns("figure2"), "Upload Figure 2 (Species Tab)",
                  multiple = FALSE,
                  accept = c("image/png", "image/jpeg", "image/gif")),
        textAreaInput(ns("comment_figure2"), "Comment for Figure 2:", ""),
        tags$hr(),
        fileInput(ns("species_data_file"), "Upload Species-Specific Data (CSV)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput(ns("header_species"), "Header (Species CSV)", TRUE),
        radioButtons(ns("sep_species"), "Separator (Species CSV)",
                     choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                     selected = ","),
        radioButtons(ns("quote_species"), "Quote (Species CSV)",
                     choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                     selected = '"'),
        tags$hr(),
        uiOutput(ns("species_column_selector")),
        textAreaInput(ns("comment_species_table"), "Comment for Species Table:", ""),
        tags$hr(),

        h4("2. Predictors"),
        fileInput(ns("predictor_figure1"), "Upload Figure 1 (Predictors Tab)",
                  multiple = FALSE,
                  accept = c("image/png", "image/jpeg", "image/gif")),
        textAreaInput(ns("comment_predictor_figure1"), "Comment for Predictor Figure 1:", ""),
        fileInput(ns("predictor_figure2"), "Upload Figure 2 (Predictors Tab)",
                  multiple = FALSE,
                  accept = c("image/png", "image/jpeg", "image/gif")),
        textAreaInput(ns("comment_predictor_figure2"), "Comment for Predictor Figure 2:", ""),
        tags$hr(),
        fileInput(ns("file1"), "Choose Main CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput(ns("header_main"), "Header (Main CSV)", TRUE),
        radioButtons(ns("sep_main"), "Separator (Main CSV)",
                     choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                     selected = ","),
        radioButtons(ns("quote_main"), "Quote (Main CSV)",
                     choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                     selected = '"'),
        tags$hr(),
        uiOutput(ns("predictors_column_selector")),
        textAreaInput(ns("comment_predictors_table"), "Comment for Predictors Table:", ""),
        tags$hr(),

        h4("3. Model"),
        fileInput(ns("model_figure1"), "Upload Figure 1 (Model Tab)",
                  multiple = FALSE,
                  accept = c("image/png", "image/jpeg", "image/gif")),
        textAreaInput(ns("comment_model_figure1"), "Comment for Model Figure 1:", ""),
        fileInput(ns("model_figure2"), "Upload Figure 2 (Model Tab)",
                  multiple = FALSE,
                  accept = c("image/png", "image/jpeg", "image/gif")),
        textAreaInput(ns("comment_model_figure2"), "Comment for Model Figure 2:", ""),
        tags$hr(),
        fileInput(ns("model_data_file"), "Upload Model Data (CSV)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput(ns("header_model"), "Header (Model CSV)", TRUE),
        radioButtons(ns("sep_model"), "Separator (Model CSV)",
                     choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                     selected = ","),
        radioButtons(ns("quote_model"), "Quote (Model CSV)",
                     choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                     selected = '"'),
        tags$hr(),
        uiOutput(ns("model_column_selector")),
        textAreaInput(ns("comment_model_table"), "Comment for Model Table:", "")
      ),
      mainPanel(
        tabsetPanel(
          id = ns("data_tabs"),
          tabPanel("Species",
                   h4("Species Figures"),
                   fluidRow(
                     column(6,
                            htmlOutput(ns("display_comment_figure1")),
                            imageOutput(ns("plot1_output"))
                     ),
                     column(6,
                            htmlOutput(ns("display_comment_figure2")),
                            imageOutput(ns("plot2_output"))
                     )
                   ),
                   tags$hr(),
                   h4("Species Data Table"),
                   htmlOutput(ns("display_comment_species_table")),
                   DTOutput(ns("species_specific_table"))
          ),
          tabPanel("Predictors",
                   h4("Predictor Figures"),
                   fluidRow(
                     column(6,
                            htmlOutput(ns("display_comment_predictor_figure1")),
                            imageOutput(ns("predictor_plot1_output"))
                     ),
                     column(6,
                            htmlOutput(ns("display_comment_predictor_figure2")),
                            imageOutput(ns("predictor_plot2_output"))
                     )
                   ),
                   tags$hr(),
                   h4("Predictor Data Table"),
                   htmlOutput(ns("display_comment_predictors_table")),
                   DTOutput(ns("predictors_table"))
          ),
          tabPanel("Model",
                   h4("Model Figures"),
                   fluidRow(
                     column(6,
                            htmlOutput(ns("display_comment_model_figure1")),
                            imageOutput(ns("model_plot1_output"))
                     ),
                     column(6,
                            htmlOutput(ns("display_comment_model_figure2")),
                            imageOutput(ns("model_plot2_output"))
                     )
                   ),
                   tags$hr(),
                   h4("Model Data Table"),
                   htmlOutput(ns("display_comment_model_table")),
                   DTOutput(ns("model_table"))
          )
        )
      )
    )
  )
}

# Module Server
tab_fig_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Helper function to escape HTML special chars
    htmlspecialchars <- function(text) {
      text <- gsub("&", "&amp;", text)
      text <- gsub("<", "&lt;", text)
      text <- gsub(">", "&gt;", text)
      text <- gsub('"', "&quot;", text)
      text <- gsub("'", "&#039;", text)
      return(text)
    }

    # Species Data
    species_data_csv <- reactive({
      req(input$species_data_file)
      read.csv(input$species_data_file$datapath,
               header = input$header_species,
               sep = input$sep_species,
               quote = input$quote_species)
    })

    output$species_column_selector <- renderUI({
      df <- species_data_csv()
      if (is.null(df)) return(NULL)
      checkboxGroupInput(ns("selected_species_columns"), "Select Species Data Columns:",
                         choices = names(df), selected = names(df))
    })

    output$species_specific_table <- renderDT({
      df <- species_data_csv()
      req(input$selected_species_columns)
      datatable(df %>% select(input$selected_species_columns))
    })

    output$display_comment_species_table <- renderText({
      if (!is.null(input$comment_species_table) && input$comment_species_table != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_species_table), "</span>")
      } else {
        ""
      }
    })

    # Species Figures render
    output$plot1_output <- renderImage({
      req(input$figure1)
      list(src = input$figure1$datapath,
           alt = "Uploaded Figure 1 (Species Tab)",
           width = "100%",
           style = "max-width: 400px; height: auto;")
    }, deleteFile = FALSE)

    output$plot2_output <- renderImage({
      req(input$figure2)
      list(src = input$figure2$datapath,
           alt = "Uploaded Figure 2 (Species Tab)",
           width = "100%",
           style = "max-width: 400px; height: auto;")
    }, deleteFile = FALSE)

    output$display_comment_figure1 <- renderText({
      if (!is.null(input$comment_figure1) && input$comment_figure1 != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_figure1), "</span>")
      } else {
        ""
      }
    })

    output$display_comment_figure2 <- renderText({
      if (!is.null(input$comment_figure2) && input$comment_figure2 != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_figure2), "</span>")
      } else {
        ""
      }
    })

    # Predictors Data
    data_csv <- reactive({
      req(input$file1)
      read.csv(input$file1$datapath,
               header = input$header_main,
               sep = input$sep_main,
               quote = input$quote_main)
    })

    output$predictors_column_selector <- renderUI({
      df <- data_csv()
      if (is.null(df)) return(NULL)
      checkboxGroupInput(ns("selected_predictors_columns"), "Select Predictor Columns:",
                         choices = names(df), selected = names(df))
    })

    output$predictors_table <- renderDT({
      df <- data_csv()
      req(input$selected_predictors_columns)
      datatable(df %>% select(input$selected_predictors_columns))
    })

    output$display_comment_predictors_table <- renderText({
      if (!is.null(input$comment_predictors_table) && input$comment_predictors_table != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_predictors_table), "</span>")
      } else {
        ""
      }
    })

    output$predictor_plot1_output <- renderImage({
      req(input$predictor_figure1)
      list(src = input$predictor_figure1$datapath,
           alt = "Uploaded Predictor Figure 1",
           width = "100%",
           style = "max-width: 400px; height: auto;")
    }, deleteFile = FALSE)

    output$predictor_plot2_output <- renderImage({
      req(input$predictor_figure2)
      list(src = input$predictor_figure2$datapath,
           alt = "Uploaded Predictor Figure 2",
           width = "100%",
           style = "max-width: 400px; height: auto;")
    }, deleteFile = FALSE)

    output$display_comment_predictor_figure1 <- renderText({
      if (!is.null(input$comment_predictor_figure1) && input$comment_predictor_figure1 != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_predictor_figure1), "</span>")
      } else {
        ""
      }
    })

    output$display_comment_predictor_figure2 <- renderText({
      if (!is.null(input$comment_predictor_figure2) && input$comment_predictor_figure2 != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_predictor_figure2), "</span>")
      } else {
        ""
      }
    })

    # Model Data
    model_data_csv <- reactive({
      req(input$model_data_file)
      read.csv(input$model_data_file$datapath,
               header = input$header_model,
               sep = input$sep_model,
               quote = input$quote_model)
    })

    output$model_column_selector <- renderUI({
      df <- model_data_csv()
      if (is.null(df)) return(NULL)
      checkboxGroupInput(ns("selected_model_columns"), "Select Model Data Columns:",
                         choices = names(df), selected = names(df))
    })

    output$model_table <- renderDT({
      df <- model_data_csv()
      req(input$selected_model_columns)
      datatable(df %>% select(input$selected_model_columns))
    })

    output$display_comment_model_table <- renderText({
      if (!is.null(input$comment_model_table) && input$comment_model_table != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_model_table), "</span>")
      } else {
        ""
      }
    })

    output$model_plot1_output <- renderImage({
      req(input$model_figure1)
      list(src = input$model_figure1$datapath,
           alt = "Uploaded Model Figure 1",
           width = "100%",
           style = "max-width: 400px; height: auto;")
    }, deleteFile = FALSE)

    output$model_plot2_output <- renderImage({
      req(input$model_figure2)
      list(src = input$model_figure2$datapath,
           alt = "Uploaded Model Figure 2",
           width = "100%",
           style = "max-width: 400px; height: auto;")
    }, deleteFile = FALSE)

    output$display_comment_model_figure1 <- renderText({
      if (!is.null(input$comment_model_figure1) && input$comment_model_figure1 != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_model_figure1), "</span>")
      } else {
        ""
      }
    })

    output$display_comment_model_figure2 <- renderText({
      if (!is.null(input$comment_model_figure2) && input$comment_model_figure2 != "") {
        paste0("<span style='background-color: #FFFFCC; padding: 3px;'><b>Comment:</b> ",
               htmlspecialchars(input$comment_model_figure2), "</span>")
      } else {
        ""
      }
    })

  })
}

tab_fig_upload_test <- function() {
  ui <- fluidPage(
    tab_fig_upload_ui("tab_fig_upload_1")
  )

  server <- function(input, output, session) {
    tab_fig_upload_server("tab_fig_upload_1")
  }

  shinyApp(ui, server)
}

