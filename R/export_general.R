# ------------------------------------------------------------------------------------------#
#                                     Export functions                                      #
# ------------------------------------------------------------------------------------------#
export_standard = function(element_id){
  val = input[[element_id]]
  return(ifelse(!is.null(val), val, NA))
}

export_authors = function(element_id){
  return(ifelse(nrow(authors$df) > 0, paste(authors$df$first_name, authors$df$middle_name, authors$df$last_name, authors$df$affiliation, collapse = "; "), NA))
}

export_suggestion = function(element_id){
  val = input[[element_id]]
  return(ifelse(!is.null(val), paste(input[[element_id]], collapse = "; "), NA))
}

export_extent = function(element_id){
  if(any(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]) == "")){
    return(NA)
  } else {
    values = paste(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]],
                     input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]), collapse = ", ")
    return(paste0(values, " (xmin, xmax, ymin, ymax)"))
  }
}

export_model_setting = function(element_id){
  if(is.null(input[["o_algorithms_1"]])){
    return(NA)
  } else {
    settings = c()
    for(alg in input[["o_algorithms_1"]]){
      settings_tab = model_settings[[alg]] %>% filter(value != "")
      if(nrow(settings_tab) == 0) {
        settings[alg] = paste0(alg, ": no settings provided")
      } else {
        settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
        settings[alg] = paste0(alg, ": ", settings_char)
      }
    }
    return(paste0(settings, collapse = "; "))
  }
}

export_model_assumptions = function(element_id){
  assump_tab = assumptions$df %>% filter(assumption != "")
  assump_tab %>% knitr::kable(format = "pipe") %>% paste0(collapse = "\n")

}

export_model_utility = function(element_id){
  util_tab = model_utility_table()
  util_tab %>% knitr::kable(format = "pipe") %>% paste0(collapse = "\n")
}
