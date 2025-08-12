# ------------------------------------------------------------------------------------------#
#                            Rendering functions for Markdown Output                        #
# ------------------------------------------------------------------------------------------#
# Functions for dynamically knitting text elements
knit_section = function(section_id){
  section = unique(odmap_dict$section[which(odmap_dict$section_id == section_id)])
  cat("\n\n##", section, "\n")
}

knit_subsection= function(subsection_id){
  # Get all elements
  element_ids = odmap_dict$element_id[which(odmap_dict$subsection_id == subsection_id)]
  subsection = unique(odmap_dict$subsection[which(odmap_dict$subsection_id == subsection_id)])

  # Find out whether subsection needs to be rendered at all
  # Are all elements optional?
  all_optional = all((element_ids %in% elem_hide[[input$o_objective_1]] | element_ids %in% elem_optional))

  # If not, render header
  if(!all_optional){
    cat("\n\n####", subsection, "\n")
  } else { # if not, render header only when user provided optional inputs
    all_empty = T
    for(id in element_ids){
      if(input[[id]] != ""){
        all_empty = F
        break
      }
    }
    if(!all_empty){
      cat("\n\n####", subsection, "\n")
    }
  }
}

knit_text = function(element_id){
  if(input[[element_id]] == ""){
    knit_missing(element_id)
  } else {
    element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
    cat("\n", element_name, ": ", input[[element_id]], "\n", sep="")
  }
}

knit_authors = function(element_id){
  paste(authors$df$first_name, authors$df$middle_name, authors$df$last_name, authors$df$affiliation, collapse = ", ")
}

knit_extent = function(element_id){
  if(any(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]) == "")){
    knit_missing(element_id)
  } else {
    element_value = paste(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]],
                            input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]), collapse = ", ")
    cat("\nSpatial extent: ", element_value, " (xmin, xmax, ymin, ymax)\n", sep="")
  }
}

knit_suggestion = function(element_id){
  if(is.null(input[[element_id]])){
    knit_missing(element_id)
  } else {
    element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
    cat("\n", element_name, ": ", paste(input[[element_id]], collapse = ", "), "\n", sep="")
  }
}

knit_model_settings = function(element_id){
  if(is.null(input[["o_algorithms_1"]])){
    knit_missing(element_id)
  } else {
    for(alg in input[["o_algorithms_1"]]){
      settings_tab = model_settings[[alg]] %>% filter(value != "")
      if(nrow(settings_tab) == 0) {
        cat("\n\n <span style='color:#DC3522'>\\<", alg, "\\> </span>\n", sep = "")
      } else {
        settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
        cat("\n", alg, ": ", settings_char, "\n", sep="")
      }
    }
  }
}

knit_model_assumptions = function(element_id){
  element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]

  knitr::kable(assumptions$df, caption = element_name)
}

knit_model_utility = function(element_id){
  element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
  knitr::kable(model_utility_table(), caption = element_name)

}

knit_missing = function(element_id){
  if(!(element_id %in% elem_hide[[input$o_objective_1]] | element_id %in% elem_optional)){
    placeholder = odmap_dict$element[which(odmap_dict$element_id == element_id)]
    cat("\n\n <span style='color:#DC3522'>\\<", placeholder, "\\> </span>\n", sep = "")
  }
}
