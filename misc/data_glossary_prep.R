# add element_ids to the glossary based on which elements the terms are found in
# This is just a starting point for connecting the elements to the glossary

library(dplyr)
library(stringr)

gloss <- read.csv("www/glossary.csv")

dict <- read.csv("www/odmap_dict.csv")



match_element_id <- function(term, dict_df){
  # patterns to match
  pat_full <- str_extract(term, "(.*)\\(", group = 1) %>% str_trim()
  pat_acronym <- str_extract(term, ".*\\((.*)\\)", group = 1) %>% str_trim()
  
  dict_df %>% 
    filter(if_any(everything(),
                  \(x){
                    data.frame(pat1 = str_detect(x, regex(term, ignore_case = TRUE)),
                               pat2 = str_detect(x, regex(pat_full, ignore_case = TRUE)),
                               pat3 = str_detect(x, regex(pat_acronym, ignore_case = FALSE))) %>% 
                      rowwise() %>% 
                      mutate(any_true = any(pat1, pat2, pat3)) %>% 
                      pull(any_true)
                  })) %>% 
    pull(element_id) %>% 
    paste0(collapse = ";")
}

gloss_out <- gloss %>% rowwise() %>% 
  mutate(element_ids = match_element_id(Concept, dict))

write.csv(gloss_out, "www/glossary.csv", row.names = FALSE)
