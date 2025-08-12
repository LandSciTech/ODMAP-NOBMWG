
# ------------------------------------------------------------------------------------------#
#                                   Import functions                                        #
# ------------------------------------------------------------------------------------------#
# ODMAP import functions
import_odmap_to_text = function(element_id, values){
  if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
    updateTextAreaInput(session = session, inputId = element_id, value = values)
  }
}

import_odmap_to_authors = function(element_id, values){
  if(nrow(authors$df) == 0 | input[["replace_values"]] == "Yes"){
    names_split = unlist(strsplit(values, split = "; "))
    names_split = regmatches(names_split, regexpr(" ", names_split), invert = TRUE)
    authors$df = authors$df[0,] # Delete previous entries
    for(i in 1:length(names_split)){
      author_tmp = names_split[[i]]
      authors$df = rbind(authors$df, data.frame("first_name" = author_tmp[1],  "middle_name" = author_tmp[2], "last_name" = author_tmp[3], "affiliation" = author_tmp[4]))
    }
  }
}

import_odmap_to_model_objective = function(element_id, values){
  if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
    updateSelectizeInput(session = session, inputId = "o_objective_1", selected = values)
  }
}

import_odmap_to_extent = function(element_id, values){
  values = gsub("(^.*)( \\(xmin, xmax, ymin, ymax\\)$)", "\\1", values)
  values_split = unlist(strsplit(values, ", "))
  names(values_split) = c("xmin", "xmax", "ymin", "ymax")
  for(i in 1:length(values_split)){
    if(input[[paste0(element_id, "_", names(values_split[i]))]] == "" | input[["replace_values"]] == "Yes"){
      updateTextAreaInput(session = session, inputId = paste0(element_id, "_", names(values_split[i])), value = as.numeric(values_split[i]))
    }
  }
}

import_odmap_to_model_algorithm = function(element_id, values){
  if(length(input[[element_id]]) == 0 | input[["replace_values"]] == "Yes"){
    values = unlist(strsplit(values, split = "; "))
    suggestions_new =  sort(trimws(c(model_settings$suggestions, as.character(values))))
    updateSelectizeInput(session = session, inputId = element_id, choices = suggestions_new, selected = values)
  }
}

# RMMS import functions
import_rmm_to_text = function(element_id, values){
  if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
    updateTextAreaInput(session = session, inputId = element_id,
                        value = paste(paste0(values, " (", names(values), ")"), collapse = ",\n"))
  }
}

import_rmm_to_authors = function(element_id, values){
  if(nrow(authors$df) == 0 | input[["replace_values"]] == "Yes"){
    names_split = unlist(strsplit(values[[1]], split = " and "))
    names_split = strsplit(names_split, split = ", ")
    authors$df = authors$df[0,] # Delete previous entries
    for(i in 1:length(names_split)){
      author_tmp = names_split[[i]]
      authors$df = rbind(authors$df, data.frame("first_name" = author_tmp[1],  "middle_name" = author_tmp[2], "last_name" = author_tmp[3], "affiliation" = author_tmp[4]))
    }
  }
}

import_rmm_to_extent = function(element_id, values){
  values = trimws(unlist(strsplit(values[[1]], ";")))
  for(i in 1:length(values)){
    values_split = unlist(strsplit(values[i], ": "))
    if(input[[paste0(element_id, "_", values_split[1])]] == "" | input[["replace_values"]] == "Yes"){
      updateTextAreaInput(session = session, inputId = paste0(element_id, "_", values_split[1]), value = as.numeric(values_split[2]))
    }
  }
}

# Generic import functions
import_model_settings = function(element_id, values){
  settings_all = unlist(strsplit(values, split = "; ",))
  algorithms = c()
  for(settings_tmp in settings_all){
    if(grepl("no settings provided", settings_tmp)){next}
    settings_split = unlist(regmatches(settings_tmp, regexpr(": ", settings_tmp), invert = TRUE)) # split at first instance of ":"
    alg = settings_split[1]
    algorithms = c(algorithms, alg)
    values_indices = gregexpr("\\((?>[^()]|(?R))*\\)", settings_split[2], perl = T) # indices of model settings in parentheses and string length per setting
    values_start = unlist(values_indices) + 1
    values_end = values_start + attr(values_indices[[1]], "match.length") - 3
    settings_start = c(1, values_end[-length(values_end)] + 4)
    settings_end = c(values_start - 3)
    values_extr = substring(settings_split[2], values_start, values_end)
    settings_extr = substring(settings_split[2], settings_start, settings_end)
    settings_df = data.frame(setting = settings_extr, value = values_extr, stringsAsFactors = F)
    model_settings_import[["algorithms"]] = c(model_settings_import[["algorithms"]], alg)
    model_settings_import[[alg]] = settings_df
  }
  updateSelectizeInput(session = session, inputId = "o_algorithms_1", selected = algorithms)
}

import_suggestion = function(element_id, values){
  if(length(input[[element_id]]) == 0 | input[["replace_values"]] == "Yes"){
    values = trimws(unlist(strsplit(values, split = ";")))
    suggestions = unlist(strsplit(odmap_dict$suggestions[odmap_dict$element_id == element_id], ","))
    suggestions_new =  sort(trimws(c(suggestions, as.character(values))))
    updateSelectizeInput(session = session, inputId = paste0(element_id), choices = suggestions_new, selected = as.character(values))
  }
}

import_model_assumptions = function(element_id, values){
  assumptions$df <- read.so::read.md(values)
}
