
# Module UI
management_units_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h5("Select potential planning and management units covered by model outputs", style = "font-weight: bold"),

    selectInput(
      inputId = ns("main_item"),
      label = "Select Management Unit Type:",
      choices = NULL,  # Set in server
      multiple = TRUE
    ),

    uiOutput(ns("sub_item_selector")),

    actionButton(
      inputId = ns("submit_selection"),
      label = "Submit Selection"
    ),

    hr(),
    p("Selected Data:"),
    tableOutput(ns("report_table")),

    hr(),
    "Sources:",
    br(),
    tags$a(href = "https://www.birdscanada.org/bird-science/nabci-bird-conservation-regions", "Birds' Conservation BCRs (BCRs)"),
    br(),
    tags$a(href = "https://ecoregions.appspot.com/", "Ecoregions (Dinerstein et al. 2017)")
  )
}

# Module Server
management_units_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load and prepare data
    data <- reactive({

      # 1. BCRs
      bcrs_df <- read.csv(here::here("inst/app/www/datasets_solutions/bcr_NA.csv")) %>%
        select(2:3) %>%
        distinct() %>%
        na.omit() %>%
        mutate(
          Unit_Name = paste0("BCR_", bcr_label, " (", bcr_label_name, ")"),
          Management_unit_Type = "Birds Conservation Regions (BCRs)"
        ) %>%
        select(Unit_Name, Management_unit_Type)

      # 2. Provinces/Territories
      canada_df <- data.frame(Unit_Name = c(
        "Alberta", "British Columbia", "Manitoba", "New Brunswick",
        "Newfoundland and Labrador", "Nova Scotia", "Ontario",
        "Prince Edward Island", "Quebec", "Saskatchewan",
        "Northwest Territories", "Nunavut", "Yukon"
      )) %>%
        mutate(Management_unit_Type = "Provinces_Territories")

      # 3. Ecoregions
      ecoregions_df <- sf::st_read(here::here("inst/app/www/datasets_solutions/Ecoregions2017/Ecoregions2017.shp"), quiet = TRUE) %>%
        sf::st_drop_geometry() %>%
        select(ECO_NAME) %>%
        rename(Unit_Name = ECO_NAME) %>%
        distinct() %>%
        na.omit() %>%
        mutate(Management_unit_Type = "Ecoregions")

      # Combine
      bind_rows(bcrs_df, canada_df, ecoregions_df)
    })

    # For accumulating selected rows
    accumulated_report_data <- reactiveVal(
      data.frame(Unit_Name = character(0), Management_unit_Type = character(0), stringsAsFactors = FALSE)
    )

    # Update choices when data is loaded
    observe({
      updateSelectInput(
        session,
        inputId = "main_item",
        choices = unique(data()$Management_unit_Type)
      )
    })

    # Reactive filtered sub-categories
    filtered_sub_categories <- reactive({
      req(input$main_item)
      data() %>%
        filter(Management_unit_Type %in% input$main_item) %>%
        pull(Unit_Name) %>%
        unique()
    })

    # Render UI for sub-item selector
    output$sub_item_selector <- renderUI({
      req(filtered_sub_categories())
      selectInput(
        inputId = ns("sub_item"),
        label = "Select Unit Name:",
        choices = filtered_sub_categories(),
        multiple = TRUE
      )
    })

    # Submit button logic
    observeEvent(input$submit_selection, {
      req(input$main_item, input$sub_item)

      new_rows <- data() %>%
        filter(
          Management_unit_Type %in% input$main_item,
          Unit_Name %in% input$sub_item
        ) %>%
        select(Management_unit_Type, Unit_Name)

      accumulated_report_data(
        unique(bind_rows(accumulated_report_data(), new_rows))
      )

      # Reset inputs
      updateSelectInput(session, "main_item", selected = character(0))
      updateSelectInput(session, "sub_item", selected = character(0))
    })

    # Output report table
    output$report_table <- renderTable({
      accumulated_report_data()
    })
    return(accumulated_report_data)
  })
}

management_units_test <- function() {
  ui <- fluidPage(
    management_units_ui("management_ui_1")
  )

  server <- function(input, output, session) {
    management_units_server("management_ui_1")
  }

  shinyApp(ui = ui, server = server)
}

render_management_units = function(element_id, element_placeholder){
  management_units_ui(element_id)
}
