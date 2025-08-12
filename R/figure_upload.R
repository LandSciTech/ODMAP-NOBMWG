

figure_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("image_file"), "Upload an Image",
              accept = c("image/png", "image/jpeg", "image/jpg", "image/gif")),
    imageOutput(ns("image_display"))
  )
}

figure_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$image_display <- renderImage({
      req(input$image_file)
      list(
        src = input$image_file$datapath,
        contentType = input$image_file$type,
        alt = "Uploaded Image",
        height = "400px",
        maxwidth = "100%"
      )
    }, deleteFile = FALSE)
  })
}

figure_upload_test <- function() {

  ui <- fluidPage(
    figure_upload_ui("img1")
  )

  server <- function(input, output, session) {
    figure_upload_server("img1")
  }

  shinyApp(ui, server)
}

render_figure_upload <- function(element_id){
  figure_upload_ui(element_id)
}
