
# ------------------------------------------------------------------------------------------#
#                           Rendering functions for UI elements                             #
# ------------------------------------------------------------------------------------------#

# wraps the output of the other elements rendering functions to allow the
# addition of an info box
render_element = function(element_id, element_ui, info = TRUE){
  if(info) {
    element <- fluidRow(
      column(10, element_ui),
      column(1, div(actionButton(paste0("help_", element_id), label = "Definitions",
                                 # goButton not found? icon = icon("goButton"),
                                 style="color: #fff; background-color: #bdbfbf; border-color: #2e6da4"),
                    style = "position: absolute;top: 15px;"))
    )
  } else {
    element <- fluidRow(
      column(9, element_ui),
      column(1)
    )
  }
  return(div(style = "padding: 0px 0px;margin-top:-4em", id = paste0("div-", element_id),
             element))
}

render_section = function(section, odmap_dict, settings){
  section_rendered = renderUI({
    req(settings)
    section_dict = filter(odmap_dict, section == !!section)

    section_UI_list = vector("list", nrow(section_dict)) # holds UI elements for all ODMAP elements belonging to 'section'
    subsection = ""
    for(i in 1:nrow(section_dict)){
      element_UI_list = vector("list", 3) # holds UI elements for current element

      # First element: Header
      if(subsection != section_dict$subsection_id[i]){
        subsection = section_dict$subsection_id[i]
        subsection_label = section_dict$subsection[i]
        element_UI_list[[1]] = div(id = section_dict$subsection_id[i], h5(subsection_label, style = "font-weight: bold"))
      }

      # Second element: Input field(s)
      render_fun <- get_fun("render", section_dict$element_type[i])

      render_args_in <- formals(render_fun)
      render_args <- section_dict %>% slice(i) %>% select(any_of(names(render_args_in))) %>% as.list()
      if("settings" %in% names(render_args_in)){
        render_args[["settings"]] <- settings
      }

      element_UI_list[[2]] = do.call(render_fun, render_args)

      element_UI_list[[2]] = render_element(section_dict$element_id[i], element_UI_list[[2]])

      # Third element: Next/previous button
      if(i == nrow(section_dict)){
        # TODO add next/previous buttons
      }

      # Reduce list to non-empty elements
      element_UI_list = Filter(Negate(is.null), element_UI_list)
      section_UI_list[[i]] = element_UI_list
    }
    return(section_UI_list)
  })
  return(section_rendered)
}

render_suggestion = function(element_id, element_placeholder, suggestions){
  suggestions = sort(trimws(unlist(strsplit(suggestions, ","))))

  selectizeInput(inputId = element_id, label = element_placeholder, choices = suggestions, multiple = TRUE, options = list(create = T,  dropdownParent = 'body',  maxOptions = 100, persist=F, placeholder = "Choose from list or insert new values"))
}

render_text = function(element_id, element_placeholder){
  textAreaInput(inputId = element_id, label = element_placeholder, height = "45px", resize = "vertical")
}

render_text_details = function(element_id, element_placeholder, details) {
  tagList(
    element_placeholder, br(),
    tags$details(
      # Add a custom class for CSS styling (optional but good practice)
      class = "collapsible-details",
      tags$summary(
        # The icon and text are wrapped in a div for better alignment if needed
        tags$div(
          icon("chevron-right", class = "chevron-icon"), # Chevron icon
          tags$span(em("See an example"), class = "example-text-span") # Your text
        )
      ),
      em(HTML(details))

    ),
    # Add a style tag to include the CSS for the chevron rotation and color
    tags$style(HTML("
      /* Basic style for the details summary to make it look clickable */
      .collapsible-details summary {
        cursor: pointer;
        list-style: none; /* Remove default marker */
      }
      .collapsible-details summary::-webkit-details-marker {
        display: none; /* For WebKit browsers */
      }

      /* Style for the chevron icon */
      .collapsible-details .chevron-icon {
        transition: transform 0.2s ease-in-out; /* Smooth transition for rotation */
        margin-right: 5px; /* Spacing between icon and text */
        color: #007bff; /* <-- ADDED: Change to your desired color, e.g., blue */
      }

      /* Rotate the chevron when the details are open */
      .collapsible-details[open] .chevron-icon {
        transform: rotate(90deg); /* Rotate 90 degrees to point down */
      }

      /* Style for the 'See an example' text span */
      .example-text-span {
        background-color: #cfeafa; /* ADDED: Light grey background, choose your color */
        padding: 2px 5px; /* Optional: Add some padding around the text */
        border-radius: 3px; /* Optional: Rounded corners */
        display: inline-block; /* Ensures padding and background apply correctly */
      }

    ")),
    textAreaInput(inputId = element_id, label = NULL, height = "45px", resize = "vertical",
                  placeholder = NULL)

  )

}

render_author = function(){

  div(
    p("Main author/contact"),
    dataTableOutput("authors_table", width = "100%"),
    actionButton("add_author", label = "Add new author", icon = icon("plus")),
    br(), br()
  )
}


render_objective = function(element_id, element_placeholder, details){
  selectizeInput(inputId = element_id, label = element_placeholder, multiple = F, options = list(create = F, placeholder = "Choose from list"),

                 choices = list("", "Inference and explanation", "Mapping and interpolation", "Forecast and transfer"))
}

# select_applicability = function(sub_element_id, application){
#   selectizeInput(inputId = sub_element_id, label = application, multiple = FALSE,
#                  options = list(create = T, placeholder = "Choose from list"),
#                  choices = list("", "Inappropriate", "Low", "Medium", "High"))
# }
#
# render_applicability = function(element_id, details){
#   # fancier option would be a table with dropdowns
#   # https://stackoverflow.com/questions/57215607/render-dropdown-for-single-column-in-dt-shiny
#   applics <- c(infer = "Inference and explanation", map = "Mapping and interpolation",
#                forecast = "Forecast and transfer")
#   inps <- purrr::imap(applics, \(x,idx) select_applicability(paste0(element_id, "_", idx), x))
#
#   div(
#   markdown(details),
#   tagList(inps)
#
#   )
# }

render_suggestion = function(element_id, element_placeholder, suggestions){
  suggestions = sort(trimws(unlist(strsplit(suggestions, ","))))
  selectizeInput(inputId = element_id, label = element_placeholder, choices = suggestions, multiple = TRUE, options = list(create = T,  dropdownParent = 'body',   persist=F, placeholder = "Choose from list or insert new values"))
}

render_extent = function(element_id){
  if(element_id == "o_scale_1"){
    tagList(
      p("Bounding box or spatial extent (Longitude/Latitude)"),
      splitLayout(
        cellWidths = c( "150px", "150px", "150px", "150px"),
        textAreaInput(inputId = paste0(element_id, "_xmin"), label = "xmin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_xmax"), label = "xmax", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymin"), label = "ymin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymax"), label = "ymax", height = "45px", resize = "none")
      )
    )
  } else {
    tagList(
      p("Bounding box or spatial extent (Longitude/Latitude)"),
      splitLayout(
        cellWidths = c("150px", "150px", "150px", "150px"),
        textAreaInput(inputId = paste0(element_id, "_xmin"), label = "xmin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_xmax"), label = "xmax", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymin"), label = "ymin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymax"), label = "ymax", height = "45px", resize = "none")
      )
    )
  }
}




render_model_algorithm = function(element_id, element_placeholder, settings){
  selectizeInput(inputId = element_id, label = element_placeholder,
                 choices = settings$suggestions, multiple = TRUE,
                 options = list(create = T,  placeholder = "Choose from list or insert new values"))
}

render_model_setting = function(){
  div(
    em(p("Edit fields by double clicking in the table. Add new settings with the plus sign.", style = "font-weight: 300;")),
    tabsetPanel(id = "settings_tabset"),
    actionButton("add_setting", label = NULL, icon = icon("plus")),
    br(),br()
  )
}

render_model_assumptions = function(){
  div(
    em(p("Edit fields by double clicking in the table. Add new assumptions with the plus sign.", style = "font-weight: 300;")),
    DT::dataTableOutput("assumptions_table"),
    actionButton("add_assumption", label = NULL, icon = icon("plus")),
    br(),br()
  )
}
