# assumptions_module.R

assumptions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h5(tags$b("Category, Assumptions, Potential Effects, and Potential Solutions Selector")),

        # Category selection
        uiOutput(ns("category_ui")),
        div(
          style = "display: flex; align-items: center;",
          textInput(ns("new_category_input"), "Add New Category:", value = ""),
          actionButton(ns("add_category_btn"), "Add Category", style = "margin-left: 5px; margin-top: 25px;")
        ),
        hr(),

        # Assumptions
        uiOutput(ns("assumptions_ui")),
        div(
          style = "display: flex; align-items: center;",
          textInput(ns("new_assumptions_input"), "Add New Assumption:", value = ""),
          actionButton(ns("add_assumptions_btn"), "Add Assumption", style = "margin-left: 5px; margin-top: 25px;")
        ),
        hr(),

        # Potential Effects
        uiOutput(ns("potential_effects_ui")),
        div(
          style = "display: flex; align-items: center;",
          textInput(ns("new_potential_effects_input"), "Add New Effect:", value = ""),
          actionButton(ns("add_potential_effects_btn"), "Add Effect", style = "margin-left: 5px; margin-top: 25px;")
        ),
        hr(),

        # Potential Solutions
        uiOutput(ns("potential_solutions_ui")),
        div(
          style = "display: flex; align-items: center;",
          textInput(ns("new_potential_solutions_input"), "Add New Solution:", value = ""),
          actionButton(ns("add_potential_solutions_btn"), "Add Solution", style = "margin-left: 5px; margin-top: 25px;")
        ),
        hr(),

        # Comment
        textInput(ns("comment_input"), "Add a comment for this selection:", value = "")
      )
    ),

    fluidRow(
      column(12,
             h5(tags$b("Current Selection:")),
             tableOutput(ns("selection_table")),
             actionButton(ns("submit_btn"), "Submit Selection"),
             br(),br(),
             p("Submitted Selections:"),
             DTOutput(ns("submitted_data_table_ui"))
      )
    )
  )
}

assumptions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    csv_path <- here::here("inst/app/www/datasets_solutions/Assumptions.csv")

    app_data <- read.csv(csv_path, stringsAsFactors = FALSE)
    expected_cols <- c("Category", "Assumptions", "Assumption_Description",
                       "Violation_of_Assumptions", "Potential_Effects",
                       "Potential_Solutions", "Comment")
    if (!all(expected_cols %in% names(app_data))) {
      stop("The CSV file must contain the correct columns.")
    }

    current_item_hierarchy <- reactiveVal(tibble())
    current_category_descriptions <- reactiveVal(tibble())
    user_added_categories <- reactiveVal(character(0))
    user_added_assumptions <- reactiveVal(character(0))
    user_added_potential_effects <- reactiveVal(character(0))
    user_added_potential_solutions <- reactiveVal(character(0))

    submitted_data <- reactiveVal(
      tibble(
        Category = character(),
        Assumptions = character(),
        `Assumption Description` = character(),
        `Violation of Assumptions` = character(),
        `Potential Effects` = character(),
        `Potential Solutions` = character(),
        Comment = character()
      )
    )

    observeEvent(session, {
      current_item_hierarchy(
        app_data %>%
          select(Category, Assumptions, Potential_Effects, Potential_Solutions) %>%
          distinct()
      )
      current_category_descriptions(
        app_data %>%
          select(Category, Assumption_Description, Violation_of_Assumptions) %>%
          distinct(Category, .keep_all = TRUE)
      )
    }, once = TRUE)

    all_categories_choices <- reactive({
      data_categories <- unique(current_item_hierarchy()$Category)
      sort(unique(c(data_categories, user_added_categories())))
    })

    output$category_ui <- renderUI({
      selectizeInput(ns("category_select"), "Select Category:",
                     choices = all_categories_choices(),
                     selected = all_categories_choices()[1],
                     multiple = FALSE)
    })

    observeEvent(input$add_category_btn, {
      new_cat <- trimws(input$new_category_input)
      if (new_cat != "" && !(new_cat %in% all_categories_choices())) {
        user_added_categories(c(user_added_categories(), new_cat))
        updateTextInput(session, "new_category_input", value = "")
        updateSelectizeInput(session, "category_select",
                             choices = all_categories_choices(), selected = new_cat)

        current_cd <- current_category_descriptions()
        new_row_desc <- tibble(
          Category = new_cat,
          Assumption_Description = paste("Description for", new_cat),
          Violation_of_Assumptions = paste("Violations for", new_cat)
        )
        current_category_descriptions(bind_rows(current_cd, new_row_desc))
      }
    })

    filtered_assumptions <- reactive({
      req(input$category_select)
      data_assumptions <- current_item_hierarchy() %>%
        filter(Category == input$category_select) %>%
        pull(Assumptions) %>%
        unique()
      sort(unique(c(data_assumptions, user_added_assumptions())))
    })

    output$assumptions_ui <- renderUI({
      selectizeInput(ns("assumptions_select"), "Select Assumptions:",
                     choices = filtered_assumptions(),
                     selected = filtered_assumptions()[1],
                     multiple = FALSE)
    })

    observeEvent(input$add_assumptions_btn, {
      new_assum <- trimws(input$new_assumptions_input)
      if (new_assum != "" && !(new_assum %in% filtered_assumptions())) {
        user_added_assumptions(c(user_added_assumptions(), new_assum))
        updateTextInput(session, "new_assumptions_input", value = "")
        updateSelectizeInput(session, "assumptions_select",
                             choices = filtered_assumptions(), selected = new_assum)
      }
    })

    filtered_potential_effects <- reactive({
      req(input$category_select, input$assumptions_select)
      data_effects <- current_item_hierarchy() %>%
        filter(Category == input$category_select,
               Assumptions == input$assumptions_select) %>%
        pull(Potential_Effects) %>%
        unique()
      sort(unique(c(data_effects, user_added_potential_effects())))
    })

    output$potential_effects_ui <- renderUI({
      selectizeInput(ns("potential_effects_select"), "Select Potential Effects:",
                     choices = filtered_potential_effects(),
                     selected = filtered_potential_effects()[1],
                     multiple = FALSE)
    })

    observeEvent(input$add_potential_effects_btn, {
      new_eff <- trimws(input$new_potential_effects_input)
      if (new_eff != "" && !(new_eff %in% filtered_potential_effects())) {
        user_added_potential_effects(c(user_added_potential_effects(), new_eff))
        updateTextInput(session, "new_potential_effects_input", value = "")
        updateSelectizeInput(session, "potential_effects_select",
                             choices = filtered_potential_effects(), selected = new_eff)
      }
    })

    filtered_potential_solutions <- reactive({
      req(input$category_select, input$assumptions_select, input$potential_effects_select)
      data_solutions <- current_item_hierarchy() %>%
        filter(Category == input$category_select,
               Assumptions == input$assumptions_select,
               Potential_Effects == input$potential_effects_select) %>%
        pull(Potential_Solutions) %>%
        unique()
      sort(unique(c(data_solutions, user_added_potential_solutions())))
    })

    output$potential_solutions_ui <- renderUI({
      selectizeInput(ns("potential_solutions_select"), "Select Potential Solutions:",
                     choices = filtered_potential_solutions(),
                     selected = filtered_potential_solutions()[1],
                     multiple = FALSE)
    })

    observeEvent(input$add_potential_solutions_btn, {
      new_sol <- trimws(input$new_potential_solutions_input)
      if (new_sol != "" && !(new_sol %in% filtered_potential_solutions())) {
        user_added_potential_solutions(c(user_added_potential_solutions(), new_sol))
        updateTextInput(session, "new_potential_solutions_input", value = "")
        updateSelectizeInput(session, "potential_solutions_select",
                             choices = filtered_potential_solutions(), selected = new_sol)
      }
    })

    output$selection_table <- renderTable({
      req(input$category_select, input$assumptions_select,
          input$potential_effects_select, input$potential_solutions_select)
      category_info <- current_category_descriptions() %>%
        filter(Category == input$category_select)
      tibble(
        Category = input$category_select,
        Assumptions = input$assumptions_select,
        `Assumption Description` = if (nrow(category_info) > 0) category_info$Assumption_Description else "N/A",
        `Violation of Assumptions` = if (nrow(category_info) > 0) category_info$Violation_of_Assumptions else "N/A",
        `Potential Effects` = input$potential_effects_select,
        `Potential Solutions` = input$potential_solutions_select,
        Comment = input$comment_input
      )
    })

    observeEvent(input$submit_btn, {
      req(input$category_select, input$assumptions_select,
          input$potential_effects_select, input$potential_solutions_select)
      category_info <- current_category_descriptions() %>%
        filter(Category == input$category_select)
      new_data <- tibble(
        Category = input$category_select,
        Assumptions = input$assumptions_select,
        `Assumption Description` = if (nrow(category_info) > 0) category_info$Assumption_Description else "N/A",
        `Violation of Assumptions` = if (nrow(category_info) > 0) category_info$Violation_of_Assumptions else "N/A",
        `Potential Effects` = input$potential_effects_select,
        `Potential Solutions` = input$potential_solutions_select,
        Comment = input$comment_input
      )
      current_data <- submitted_data()
      if (nrow(current_data) == 0) {
        updated_data <- new_data
      } else {
        new_unique_data <- anti_join(new_data, current_data, by = names(new_data))
        updated_data <- bind_rows(current_data, new_unique_data)
      }
      submitted_data(updated_data)
    })

    output$submitted_data_table_ui <- renderDT({
      datatable(submitted_data(), options = list(dom = 't'))
    })
  })
}

assumptions_test <- function() {
  # Make sure assumptions_ui and assumptions_server are already defined or sourced
  ui <- fluidPage(
    assumptions_ui("test")
  )

  server <- function(input, output, session) {
    assumptions_server("test")
  }

  shinyApp(ui = ui, server = server)
}

render_assumptions <- function(element_id){
  assumptions_ui(element_id)
}
