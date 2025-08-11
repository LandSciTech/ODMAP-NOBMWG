library(shinytest2)
library(shiny)
appDir <-  system.file("app", package = "ODMAPNOBMWG")

# app <- AppDriver$new(appDir)
#
# app$get_value()

test_that("can modify and save", {
  testServer(app = appDir, {
    session$flushReact()
    # app initialized
    expect_s3_class(assumptions$df, "data.frame")

    # Set all the inputs to force them to exist
    odmap_dict2 <- odmap_dict %>% rowwise %>%
      mutate(element_id = case_when(
        element_type == "extent" ~ paste0(element_id, "_",
                                          c("xmin", "xmax", "ymin", "ymax"),
                                          collapse = ","),
        TRUE ~ element_id)) %>%
      separate_longer_delim(element_id, delim = ",")

    do.call(session$setInputs,
            rep("t", length(odmap_dict2$element_id)) %>% set_names(odmap_dict2$element_id) %>% as.list())
    # Can't test this with testServer because it requires JavaScript update
    # session$setInputs(
    #   upload = list(datapath = here::here(
    #     "misc/examples/ODMAP_BouchetMeeuwig_2020-03-12.csv"
    #   )))
    #
    # session$flushReact()
    #

    # session$setInputs(
    #   replace_values = "Yes",
    #   ODMAP_to_input = 1
    # )
    browser()
    session$setInputs(protocol_download = 1)
    # Not sure if this will work either... would need to supply file
  })




})
