library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
library(rangeModelMetadata)

server <- function(input, output, session) {
  odmap_dict = read.csv("www/odmap_dict.csv", header = T, stringsAsFactors = F)
  glossary = read.csv("www/glossary.csv", header = T, stringsAsFactors = F)
  rmm_dict = rmmDataDictionary()

  model_utility_df <- read.csv(here::here("inst/app/www/datasets_solutions/model_utility.csv"), header=TRUE, sep=",")
  model_utility_df <- model_utility_df[order(model_utility_df$Category, decreasing = FALSE),]
  link_data <- data.frame(
    Author = unlist(strsplit(model_utility_df$Author, ",")),
    References = unlist(strsplit(model_utility_df$References, ",")),
    stringsAsFactors = FALSE
  )


  #### adding a checkbox to select fields based on their relative importance

  field_importance = list("Essential" = pull(odmap_dict %>% filter(field_relevance == 1), element_id),
                          "Informative" = pull(odmap_dict %>% filter(field_relevance == 2), element_id),
                          "Complementary" = pull(odmap_dict %>% filter(field_relevance == 3), element_id)
  )

  # adding checkbox for fields
  output$category_checkboxes <- renderUI({
    shiny::checkboxGroupInput(inputId = "selected_categories", label= NULL,
                              choices = names(field_importance),
                              selected = names(field_importance))
  })

  # this is used to filter questions to display in render_section
  question_filter <- reactiveVal(character(0))

  # This causes all progress to be lost because it re-renders all the sections.
  # Need to figure out a better way. Maybe use shinyjs to hide the elements?
  observeEvent(input$selected_categories, {
    relevance_numbers <- c(0)
    if ("Essential" %in% input$selected_categories) {
      relevance_numbers <- c(relevance_numbers, 1)
    }
    if ("Informative" %in% input$selected_categories) {
      relevance_numbers <- c(relevance_numbers, 2)
    }
    if ("Complementary" %in% input$selected_categories) {
      relevance_numbers <- c(relevance_numbers, 3)
    }

    qs_to_keep <- odmap_dict %>%
      # Filter the data where the 'Category' column is present in the vector
      # of 'input$selected_categories' (the checkboxes the user has ticked).
      filter(field_relevance %in% relevance_numbers) %>%
      pull(element_id)

    question_filter(qs_to_keep)

    qs_to_hide <- odmap_dict %>%
      # Filter the data where the 'Category' column is present in the vector
      # of 'input$selected_categories' (the checkboxes the user has ticked).
      filter(!field_relevance %in% relevance_numbers) %>%
      pull(element_id)

    purrr::walk(paste0("div-",qs_to_keep), shinyjs::show)
    purrr::walk(paste0("div-",qs_to_hide), shinyjs::hide)
  })

  # Event handlers------------------------------------------------

  show_glossary <- function(input) {
    # Show guidelines with additional info for each section
    help_ins <- stringr::str_subset(names(input), "help")

    purrr::map(help_ins,
               ~observeEvent(input[[.x]], {
                 glossary_filter(.x)
               }, ignoreInit = TRUE))
  }

  glossary_filter <- function(id){
    element_id <- str_remove(id, "help_")
    DT::updateSearch(glossary_proxy, keywords = list(global = element_id, columns = NULL))
  }

  observe(show_glossary(input)) # Create Guideline buttons

  output$glossary_table <- DT::renderDataTable({
    DT::datatable(
      glossary, rownames = FALSE, escape = FALSE,
      fillContainer = TRUE,
      # height = 200,
      options = list(
        columnDefs = list(
          list(
            targets = 2,
            searchable = TRUE,
            visible = FALSE
          )
        )
      )
    )
  })

  glossary_proxy <- DT::dataTableProxy("glossary_table")

  # Study objective
  observeEvent(input$o_objective_1, {
    # Dynamically show/hide corresponding input fields
    shinyjs::show(selector = paste0("#", setdiff(elem_hidden, elem_hide[[input$o_objective_1]])))
    shinyjs::hide(selector = paste0("#", elem_hide[[input$o_objective_1]]))
    elem_hidden <<- elem_hide[[input$o_objective_1]]

    # Show/hide Prediction tab when study objective is inference
    if(input$o_objective_1 == "Inference and explanation"){
      hideTab("tabset", "Prediction")
    } else {
      showTab("tabset", "Prediction")
    }
  })

  # Extent Long/Lat
  observe({
    input_names = c("o_scale_1_xmin", "o_scale_1_xmax", "o_scale_1_ymax", "o_scale_1_ymin")
    print_message = F
    for(input_name in input_names){
      if(grepl("xmin|xmax", input_name)){
        value_range = c(-180, 180)
      } else {
        value_range = c(-90, 90)
      }

      if(is.null(input[[input_name]])){
        next
      } else if(input[[input_name]] == "") {
        next
      } else {
        input_value = ifelse(is.na(as.numeric(input[[input_name]])), -999, as.numeric(input[[input_name]]))  # Check if input is valid number
        if(input_value < value_range[1] | input_value > value_range[2]){
          updateTextAreaInput(session = session, inputId = input_name, value = "")
          print_message = T
        }
      }
    }
    if(print_message){
      showNotification("Please enter valid latitude/longitude coordinates", duration = 4, type = "error")

    }
  })

  # Optional fields -------------------------------------------
  observeEvent(input$hide_optional,{
    if(is.null(input$o_objective_1)){
      return(NULL)
    } else if(input$hide_optional == T & input$o_objective_1 == ""){
      showNotification("Please select a model objective under '1. Overview'", duration = 3, type = "message")
      Sys.sleep(0.3)
      updateMaterialSwitch(session, "hide_optional", value = F)
      updateTabsetPanel(session, "tabset", "Overview")
      # TODO jump to Input field
    } else {
      shinyjs::toggle(selector = paste0("#", setdiff(elem_optional, elem_hide[[input$o_objective_1]])), condition = !input$hide_optional)
    }
  })

  # Model algorithms and settings -------------------------------------------
  model_settings = reactiveValues(suggestions = rmm_dict %>% filter(field1 == "model" & field2 == "algorithm") %>% pull(field3) %>% unique() %>% trimws(),
                                  settings_tabset = NULL)
  model_settings_import = reactiveValues(algorithms = character(0))

  observeEvent(input$o_algorithms_1, {
    if(length(input$o_algorithms_1) > length(model_settings$settings_tabset)) { # New algorithm selected
      new_algs = setdiff(input$o_algorithms_1, model_settings$settings_tabset)
      for(new_alg in new_algs){
        # Create dataframe for new algorithm
        if(new_alg %in% model_settings_import[["algorithms"]]){
          model_settings[[new_alg]] = model_settings_import[[new_alg]]
        } else if(new_alg %in% filter(rmm_dict, field2 == "algorithm")$field3){
          model_settings[[new_alg]] = rmm_dict %>%
            filter(field1 == "model" & field2 == "algorithm" & field3 == new_alg) %>%
            mutate(setting = entity, value = as.character(NA)) %>%
            dplyr::select(setting, value)
        } else {
          model_settings[[new_alg]] = data.frame(setting = character(0), value = character(0))
        }

        # Add new dataframe to output and settings_tabset
        local({ # Needs local evaluation because of asynchronous execution of renderDataTable
          .new_alg = new_alg
          output[[.new_alg]] = renderDataTable(model_settings[[.new_alg]], editable = T, rownames = F,
                                               options = list(dom = "t", pageLength = 50, autoWidth = T, columnDefs = list(list(width = '50%', targets = "_all"))))
          observeEvent(input[[paste0(.new_alg, '_cell_edit')]], {
            model_settings[[.new_alg]][input[[paste0(.new_alg, '_cell_edit')]]$row, input[[paste0(.new_alg, '_cell_edit')]]$col + 1] = input[[paste0(.new_alg, '_cell_edit')]]$value
          })
        })
        appendTab(inputId = "settings_tabset", select = T, tab = tabPanel(title = new_alg, value = new_alg, dataTableOutput(outputId = new_alg)))
      }
      model_settings$settings_tabset = input$o_algorithms_1 # update name list of displayed tabs
    } else {
      hide_alg = setdiff(model_settings$settings_tabset, input$o_algorithms_1)
      removeTab(inputId = "settings_tabset", target = hide_alg)
      model_settings$settings_tabset = input$o_algorithms_1
    }

    if(length(model_settings$settings_tabset) > 0){
      updateTabsetPanel(session, "settings_tabset", selected = model_settings$settings_tabset[1])
    }
  }, ignoreNULL = F, ignoreInit = F, priority = 1)

  observeEvent(input$add_setting, {
    if(!is.null(input$settings_tabset)){
      empty_row = data.frame(setting = NA, value = NA)
      model_settings[[input$settings_tabset]] = rbind(model_settings[[input$settings_tabset]], empty_row)
    } else {
      showNotification("Please select or add a model algorithm under '1. Overview'", duration = 3, type = "message")
      updateTabsetPanel(session, "tabset", selected = "Overview")
      # TODO jump to Input field
    }
  })
  # -------Assumptions------------------------
  assumptions = reactiveValues(df = data.frame(
    assumption = odmap_dict %>% filter(element_id == "o_assumptions_1") %>%
      separate_longer_delim(suggestions, delim = ", ") %>% pull(suggestions),
    description = NA_character_
  ))

  output$assumptions_table = DT::renderDataTable({
    assumptions_dt = datatable(assumptions$df, escape = F, rownames = F, editable = T, colnames = c("Assumption", "Description"),
                               options = list(dom = "t", autoWidth = F,
                                              columnDefs = list(list(width = '20%', targets = c(0)),
                                                                list(width = '80%', targets = c(1)),
                                                                list(orderable = F, targets = c(0:1)))))
    return(assumptions_dt)
  })

  observeEvent(input$add_assumption, {
    assumptions$df <- rbind(assumptions$df,
                            data.frame(assumption = NA_character_,  description = NA_character_))
  })

  observeEvent(input$assumptions_table_cell_edit, {
    assumptions$df[input$assumptions_table_cell_edit$row, input$assumptions_table_cell_edit$col + 1] = input$assumptions_table_cell_edit$value
    output$assumptions_df = renderDataTable(assumptions$df)
  })

  # Authors -------------------------------------------
  authors = reactiveValues(df = data.frame("first_name" = character(0),  "middle_name" = character(0), "last_name" = character(0), "affiliation" = character(0)))

  output$authors_table = DT::renderDataTable({
    if(nrow(authors$df) == 0){

      authors_dt = datatable(authors$df, escape = F, rownames = F, colnames = NULL,
                             options = list(dom = "t", ordering = F, language = list(emptyTable = "Author list is empty"), columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    } else {
      authors_tmp = authors$df %>%
        rownames_to_column("row_id") %>%
        mutate(row_id = as.numeric(row_id),
               delete = sapply(1:nrow(.), function(row_id){as.character(actionButton(inputId = paste("remove_author", row_id, sep = "_"), label = NULL,
                                                                                     icon = icon("trash"),
                                                                                     onclick = 'Shiny.setInputValue(\"remove_author\", this.id, {priority: "event"})'))})) %>%
        dplyr::select(-row_id)

      authors_dt = datatable(authors_tmp, escape = F, rownames = F, editable = T, colnames = c("First name", "Middle name (initial)", "Last name", "Affiliation",  ""),
                             options = list(dom = "t", autoWidth = F,
                                            columnDefs = list(list(width = '10%', targets = c(2)),
                                                              list(width = '45%', targets = c(0:1)),
                                                              list(orderable = F, targets = c(0:2)))))
    }
    return(authors_dt)
  })

  observeEvent(input$add_author, {
    showModal(
      modalDialog(title = "Add new author", footer = NULL, easyClose = T,
                  textInput("first_name", "First name"),
                  textInput("middle_name", "Middle name (initial, if needed)"),
                  textInput("last_name", "Last name"),
                  textInput("affiliation", "Affiliation"),
                  actionButton("save_new_author", "Save")
      )
    )
  })

  observeEvent(input$save_new_author, {
    if(input$first_name == "" | input$last_name == ""){
      showNotification("Please provide first, middle name, last name and affiliation", duration = 4, type = "message")
    } else {
      new_author = data.frame("first_name" = input$first_name, "middle_name" = input$middle_name,"last_name" = input$last_name, "affiliation" = input$affiliation, stringsAsFactors = F)
      authors$df = rbind(authors$df, new_author)
      removeModal()
    }
  })

  observeEvent(input$remove_author, {
    item_remove = as.integer(parse_number(input$remove_author))
    authors$df = authors$df[-item_remove,]
    output$authors_df = renderDataTable(authors$df)
  })

  observeEvent(input$authors_table_cell_edit, {
    authors$df[input$authors_table_cell_edit$row, input$authors_table_cell_edit$col + 1] = input$authors_table_cell_edit$value
    output$authors_df = renderDataTable(authors$df)
  })

  # Module servers #------------

  model_utility_table <- model_utility_mod_server("o_utility_1", model_utility_df, link_data)

  species_table <- species_names_server("o_taxon_2")

  management_unit_table <- management_units_server("o_location_2")
  # ------------------------------------------------------------------------------------------#
  #                                   UI Elements                                             #
  # ------------------------------------------------------------------------------------------#
  # "Create a protocol" - mainPanel elements
  output$Overview_UI = render_section("Overview", odmap_dict,  model_settings)
  output$Data_UI = render_section("Data", odmap_dict, model_settings)
  output$Model_UI = render_section("Model", odmap_dict, model_settings)
  output$Assessment_UI = render_section("Assessment", odmap_dict, model_settings)
  output$Prediction_UI = render_section("Prediction", odmap_dict, model_settings)

  for(tab in c("Overview_UI", "Data_UI", "Model_UI", "Assessment_UI", "Prediction_UI")){
    outputOptions(output, tab, suspendWhenHidden = FALSE) # Add tab contents to output object before rendering
  }

  # Create a protocol - sidebarPanel elements ---------------------------
  get_progress = reactive({
    progress = list() # Use a list to store both percentage and counts

    # Calculate total sections (optional, but good for "X of Y" overall if needed later)
    # total_sections = length(unique(odmap_dict$section))

    for(sect in unique(odmap_dict$section)){
      all_elements = odmap_dict %>%
        filter(section == sect & element_id %in% question_filter() &
                 !element_type %in% c("model_setting", "author")) %>%
        mutate(element_id = ifelse(element_type == "extent", paste0(element_id, "_xmin"), element_id)) %>%
        pull(element_id)

      if(length(all_elements) == 0){
        # If no relevant elements in this section, assign 0% and 0/0 or handle as desired
        progress[[sect]] = list(percentage = 0, completed_count = 0, total_count = 0)

        next
      } else {
        completed_elements_count = sum(sapply(all_elements, function(x){
          !(identical(input[[x]], "") | identical(input[[x]], 0) | identical(input[[x]], NULL))
        }, USE.NAMES = T, simplify = T))

        percentage = (completed_elements_count / length(all_elements)) * 100

        # Store percentage and counts for this section
        progress[[sect]] = list(
          percentage = percentage,
          completed_count = completed_elements_count,
          total_count = length(all_elements)
        )
      }
    }
    return(progress)
  })

  output$progress_bars = renderUI({
    progress_data = get_progress() # Renamed to avoid conflict with 'progress' in loop

    progress_UI_list = lapply(names(progress_data), function(sect_name){ # Renamed 'sect' to 'sect_name'
      data_for_sect = progress_data[[sect_name]]

      # Create the title string with counts
      title_text = paste0(
        sect_name,
        " (",
        data_for_sect$completed_count,
        " of ",
        data_for_sect$total_count,
        ")"
      )

      progressBar(
        id = paste("progress", sect_name, sep = "_"), # 'id' is often preferred over unnamed first argument for clarity
        value = data_for_sect$percentage,
        title = title_text
      )
    })
    return(progress_UI_list)
  })

  output$protocol_download = downloadHandler(
    filename = function(){
      author_list = authors$df$last_name
      if(length(author_list) > 2){
        name_string = paste0(author_list[1],"_EtAl")
      } else if(length(author_list) == 2){
        name_string = paste0(author_list[1], author_list[2])
      } else if(length(author_list) == 1){
        name_string = author_list[1]
      } else {
        name_string = "Anonymous"
      }
      paste0("ODMAP_", name_string, "_", Sys.Date(), ".", input$document_format)
    },
    content = function(file){
      odmap_download = odmap_dict %>%
        filter(!element_id %in% elem_hide[[input$o_objective_1]]) %>% # use only relevent rows
        dplyr::select(section, subsection, element, element_id, element_type) %>%
        mutate(Value = NA)

      # Create .csv-files
      if(input$document_format == "csv"){
        # Create table
        for(i in 1:nrow(odmap_download)){
          odmap_download$Value[i] = switch(odmap_download$element_type[i],
                                           text = export_standard(odmap_download$element_id[i]),
                                           text_details = export_standard(odmap_download$element_id[i]),
                                           author = export_authors(odmap_download$element_id[i]),
                                           objective = export_standard(odmap_download$element_id[i]),
                                           suggestion = export_suggestion(odmap_download$element_id[i]),
                                           extent = export_extent(odmap_download$element_id[i]),
                                           model_algorithm = export_suggestion(odmap_download$element_id[i]),
                                           model_setting = export_model_setting(odmap_download$element_id[i]),
                                           model_assumptions = export_model_assumptions(odmap_download$element_id[i]),
                                           model_utility = export_model_utility(odmap_download$element_id[i]),
                                           "")
        }

        odmap_download$element_id = NULL
        odmap_download$element_type = NULL

        # Write output
        file_conn = file(file, open = "w")
        write.csv(odmap_download, file = file_conn, na = "", row.names = F)
        close(file_conn)

        # CREATE WORD FILES
      } else {
        src <- normalizePath("protocol_output.Rmd")

        # temporarily switch to the temp dir, in case of missing write permission in the current working directory
        wd_orig <- setwd(tempdir())
        on.exit(setwd(wd_orig))
        file.copy(src, "protocol_output.Rmd", overwrite = TRUE)
        odmap_download = rmarkdown::render("protocol_output.Rmd", rmarkdown::word_document(),
                                           params = list(study_title = paste(input$o_authorship_1), authors = paste(authors$df$first_name, authors$df$middle_name, authors$df$last_name, authors$df$affiliation, collapse = ", ")))
        file.rename(odmap_download, file)
      }
    }
  )


  # Protocol Viewer -------------------------------------------
  output$markdown = renderUI({includeMarkdown(knitr::knit("protocol_preview.Rmd", quiet = T))})


  # Upload / Import -------------------------------------------
  output$Upload_UI = renderUI({
    UI_list = list()
    if(!is.null(input$upload)){
      # Obtain file extension
      file_type = gsub( "(^.*)(\\.[A-z]*$)", replacement = "\\2", input$upload$datapath)
      if(!file_type %in% c(".csv", ".RDS")){
        showNotification("Please select and provide a .csv (ODMAP, RMMS) or .RDS file (RMMS).", duration = 3, type = "error")
        reset("upload")
        return()
      }

      # Read in file
      if(file_type == ".RDS"){
        tryCatch({
          protocol_upload = rmmToCSV(protocol_upload, input$upload$datapath)
        }, error = function(e){
          showNotification("Could not read file.", duration = 3, type = "error")
          reset("upload")
          return()
        })
      } else {
        protocol_upload = read.csv(file = input$upload$datapath, header = T, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL"))
      }

      # Identify protocol type
      if(all(c("section", "subsection", "element", "Value") %in% colnames(protocol_upload))){
        protocol_type = "ODMAP"
      } else if(all(c("Field.1", "Field.2", "Field.3", "Entity", "Value") %in% colnames(protocol_upload))){
        protocol_type = "RMMS"
      } else {
        showNotification("Please select a valid ODMAP or RMMS file", duration = 3, type = "error")
        reset("upload")
        return()
      }

      if(sum(!is.na(protocol_upload$Value))>0){
        UI_list[[1]] = p(paste0("File: ", input$upload$name, " (", protocol_type, " protocol, ", sum(!is.na(protocol_upload$Value)), " non-empty fields)"))
        UI_list[[2]] = radioButtons("replace_values", "Overwrite non-empty fields with uploaded values?", choices = c("Yes", "No"), selected = "No")
        UI_list[[3]] = actionButton(paste0(protocol_type, "_to_input"), "Copy to input form")
      } else{
        showNotification("Please select a ODMAP or RMMS file with at least one non-empty field", duration = 5, type = "error")
      }

    }
    return(UI_list)
  })

  # Monitor current progress
  elem_hide = list("Inference and explanation" = c(pull(odmap_dict %>% filter(inference == 0), element_id), # unused elements
                                                   unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(inference  == 0)), subsection_id)), # unused subsections
                                                   "p"),
                   "Prediction and mapping" =  c(pull(odmap_dict %>% filter(prediction == 0), element_id),
                                                 unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(prediction == 0)), subsection_id))),
                   "Projection and transfer" =  c(pull(odmap_dict %>% filter(projection == 0), element_id),
                                                  unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(projection  == 0)), subsection_id))))

  elem_optional = c(pull(odmap_dict %>% filter(optional == 1), element_id), # optional elements
                    unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(optional == 1)), subsection_id))) # optional subsections)

  elem_hidden = "" # keep track of hidden elements



  # Import  -------------------------------------------
  observeEvent(input$ODMAP_to_input, {
    protocol_upload = read.csv(input$upload$datapath, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL")) %>%
      right_join(odmap_dict, by = c("section", "subsection", "element"))  %>%
      mutate(Value = trimws(Value)) %>%
      filter(!is.na(Value))

    # Update ODMAP input fields with imported values
    for(i in 1:nrow(protocol_upload)){
      switch(protocol_upload$element_type[i],
             text = import_odmap_to_text(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             text_details = import_odmap_to_text(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             author = import_odmap_to_authors(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             objective = import_odmap_to_model_objective(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             suggestion = import_suggestion(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             extent = import_odmap_to_extent(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_algorithm = import_odmap_to_model_algorithm(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_setting = import_model_settings(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_assumptions = import_model_assumptions(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]))
    }

    # Switch to "Create a protocol"
    reset("upload")
    updateNavbarPage(session, "navbar", selected = "create")
    updateTabsetPanel(session, "Tabset", selected = "Overview")
  })

  observeEvent(input$RMMS_to_input, {
    protocol_upload = read.csv(input$upload$datapath, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL")) %>%
      mutate(Value = trimws(Value)) %>%
      filter(!is.na(Value))

    # 1. Prepare imported values
    if(nrow(protocol_upload>0)){
        imported_values = list()
        for(i in 1:nrow(protocol_upload)){ ## ignore first line?
          rmm_fields = c(protocol_upload$Field.1[i], protocol_upload$Field.2[i], protocol_upload$Field.3[i])
          rmm_fields = rmm_fields[!is.na(rmm_fields)]
          rmm_fields = paste0(rmm_fields, collapse = "$")
          rmm_entity = protocol_upload$Entity[i]

          odmap_subset = odmap_dict[grepl(rmm_fields, str_split(odmap_dict$rmm_fields, pattern = ","), fixed = T) & grepl(rmm_entity, str_split(odmap_dict$rmm_entities, pattern = ","), fixed = T),]
          if(nrow(odmap_subset) == 1){
            imported_values[[odmap_subset$element_id]][[paste(rmm_fields, rmm_entity, sep = "-")]] = protocol_upload$Value[i]
          }
        }

        model_settings = protocol_upload %>%
          dplyr::filter(Field.1 == "model" & Field.2 == "algorithm") %>%
          group_by(Field.3) %>%
          summarize(settings_string = paste(paste0(Entity, " (", Value, ")"), collapse = ", "))

        if(nrow(model_settings) > 0){
          imported_values[["m_settings_1"]] = model_settings %>%
            ungroup() %>%
            summarize(x = paste(paste0(Field.3, ": ", settings_string), collapse = "; ")) %>%
            pull(x)
        }

        # 2. Update ODMAP input fields with imported values
        for(i in 1:length(imported_values)){
          switch(odmap_dict$element_type[which(odmap_dict$element_id == names(imported_values)[i])],
                 text = import_rmm_to_text(element_id = names(imported_values)[i], values = imported_values[[i]]),
                 author = import_rmm_to_authors(element_id = names(imported_values)[i], values = imported_values[[i]]),
                 suggestion = import_suggestion(element_id = names(imported_values)[i], values = unlist(imported_values[[i]])),
                 extent = import_rmm_to_extent(element_id = names(imported_values)[i], values = imported_values[[i]]),
                 model_setting = import_model_settings(element_id = names(imported_values)[i], values = imported_values[[i]]))
        }

        # Switch to "Create a protocol"
        reset("upload")
        updateNavbarPage(session, "navbar", selected = "create")
        updateTabsetPanel(session, "Tabset", selected = "Overview")
    }

  })

  session$onSessionEnded(function() {
    stopApp()
  })

}
