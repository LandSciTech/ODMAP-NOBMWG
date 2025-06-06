
appDir <-  system.file("app", package = "ODMAPNOBMWG")

test_that("can load csv, modify and save", {
  testServer(app = appDir, {
    session$flushReact()
    # app initialized
    expect_s3_class(assumptions$df, "data.frame")

    session$setInputs(
      upload = list(datapath = here::here(
        "misc/examples/ODMAP_BouchetMeeuwig_2020-03-12.csv"
      )))
    session$flushReact()

    session$setInputs(
      replace_values = "Yes",
      ODMAP_to_input = 1
    )

  })

})
