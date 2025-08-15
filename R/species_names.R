
# ----- UI Function -----
species_names_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Select Species Name(s) within Class", style = "font-weight: bold"),
    br(),
    selectInput(
      inputId = ns("selected_class"),
      label = "Select Class:",
      choices = c("Birds", "Mammals"),
      selected = "Birds"
    ),
    selectInput(
      inputId = ns("selected_sub_items"),
      label = "Select Species Name(s):",
      choices = NULL,
      multiple = TRUE
    ),
    hr(),
    actionButton(ns("save_selection_btn"), "Save Current Selection", class = "btn-primary"),
    br(), br(),
    actionButton(ns("clear_saved_btn"), "Clear All Saved Selections", class = "btn-danger"),
    p("Currently Selected Species (before saving):"),
    tableOutput(ns("output_current_selection")),
    hr(),
    p("All Saved Species Selections:"),
    tableOutput(ns("output_saved_species")),
    hr(),
    "Sources:",
    br(),
    tags$a(
      href = "https://open.canada.ca/data/en/dataset/0a2dfadd-57eb-4d64-a56d-ff53c431aaaa",
      "Birds' List from this source"
    )
  )
}

# ----- Server Function -----
species_names_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load bird and mammal data
    all_species_data <- reactive({
      url_birds <- "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fassess%2Fcanadian-breeding-bird-census-plots%2FCanadianBreedingBirdCensusSpecies_EspecesDeRecensementDesOiseauxNicheursDuCanada.csv"
      birds_data <- tryCatch({
        read.csv(url_birds, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE) %>%
          select(COMNAME, TAXON, CODE) %>%
          rename(
            Common_name = COMNAME,
            Scientific_name = TAXON,
            Code = CODE
          ) %>%
          distinct() %>%
          mutate(Class = "Birds") %>%
          arrange(Common_name) %>%
          filter(!duplicated(Common_name))
      }, error = function(e) {
        warning("Could not load bird data: ", e$message)
        data.frame(Common_name = character(), Scientific_name = character(), Code = character(), Class = character())
      })

      # Mammal Data (adjust path if needed)
      mammals_data <- read.csv("www/datasets_solutions/Mammals_Canada.csv", header = TRUE)
      bind_rows(birds_data, mammals_data)
    })

    saved_selections <- reactiveVal(
      data.frame(Common_name = character(0), Scientific_name = character(0), Code = character(0), Class = character(0), stringsAsFactors = FALSE)
    )

    observeEvent(input$selected_class, {
      req(all_species_data())
      filtered_data <- all_species_data() %>% filter(Class == input$selected_class)
      updateSelectInput(session, "selected_sub_items", choices = unique(filtered_data$Common_name), selected = NULL)
    })

    current_selection_df <- reactive({
      req(input$selected_class, input$selected_sub_items)
      all_species_data() %>%
        filter(Class == input$selected_class, Common_name %in% input$selected_sub_items)
    })

    output$output_current_selection <- renderTable({
      current_selection_df()
    })

    observeEvent(input$save_selection_btn, {
      current_rows <- current_selection_df()
      if (nrow(current_rows) > 0) {
        updated <- bind_rows(saved_selections(), current_rows) %>%
          distinct(Common_name, Scientific_name, Code, Class, .keep_all = TRUE) %>%
          arrange(Class, Common_name)
        saved_selections(updated)
        updateSelectInput(session, "selected_class", selected = "Birds")
        showNotification("Selection saved successfully!", type = "message", duration = 2)
      } else {
        showNotification("Please select species to save.", type = "warning", duration = 3)
      }
    })

    observeEvent(input$clear_saved_btn, {
      saved_selections(
        data.frame(Common_name = character(0), Scientific_name = character(0), Code = character(0), Class = character(0), stringsAsFactors = FALSE)
      )
      showNotification("All saved selections cleared.", type = "message", duration = 2)
    })

    output$output_saved_species <- renderTable({
      if (nrow(saved_selections()) == 0) {
        return(data.frame(Message = "No species saved yet. Select some and click 'Save Current Selection'."))
      } else {
        return(saved_selections())
      }
    })
    return(saved_selections)
  })
}

species_names_test <- function(){

  ui <- fluidPage(
    species_names_ui("species_selector1")
  )

  server <- function(input, output, session) {
    species_names_server("species_selector1")
  }

  shinyApp(ui, server)
}

render_species_names = function(element_id, element_placeholder){
  species_names_ui(element_id)
}
