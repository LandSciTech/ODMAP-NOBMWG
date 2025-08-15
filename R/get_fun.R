get_fun <- function(fun_type, element_type){
  fun <- get(paste0(fun_type, "_", element_type), envir = parent.frame())
}
