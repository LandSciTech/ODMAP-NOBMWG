
raster_upload_ui <- function(id, raster_name, legend_upload = FALSE) {
  ns <- NS(id)
  tagList(
    h4(paste0(raster_name, " Raster")), # Add a subheading for clarity
    fileInput(ns("raster_upload"), paste0("Upload ", raster_name, " Raster (e.g., .tif, .asc)"),
              accept = c(".tif", ".tiff", ".asc", ".grd", ".nc", ".img")),
    checkboxInput(ns("show_raster"), paste0("Show ", raster_name), value = FALSE),
    if(legend_upload){
      wellPanel( # Add a panel for the legend image input
      h5(paste0(raster_name, " Legend Image (.png)")),
      fileInput(ns("legend_upload"), "Upload Legend Image",
                accept = ".png"),
      # Optional: Display a preview of the uploaded legend
      uiOutput(ns("legend_preview_ui"))
    )} else {
      NULL
    },
    # withSpinner(
      uiOutput(ns("raster_options_ui")),
      #  type = 6, color = "#0dc5c7"), # Dedicated UI for options

    tags$hr()
  )
}

raster_upload_server <- function(id, raster_name, leaflet_proxy, legend_upload = FALSE) {
  moduleServer(id, function(input, output, session) {
    # load the raster
    raster_loaded <- eventReactive(input$raster_upload, {
      load_and_process_raster(input$raster_upload, raster_name)
    })

    # Dynamic UI for raster options (NEW)
    output$raster_options_ui <- renderUI({
      ras <- raster_loaded()
      if (!is.null(ras)) {
        opacity_input <- sliderInput(session$ns("raster_opacity"), paste0(raster_name, " Opacity:"),
                                     min = 0, max = 1, value = 0.7, step = 0.05)
        palette_input <- selectInput(session$ns("raster_palette"), paste0(raster_name, " Color Palette:"),
                                     choices = c("YlOrRd (Sequential)" = "YlOrRd",
                                                 "Greens (Sequential)" = "Greens",
                                                 "Blues (Sequential)" = "Blues",
                                                 "Viridis (Sequential)" = "viridis",
                                                 "Magma (Sequential)" = "magma",
                                                 "Inferno (Sequential)" = "inferno",
                                                 "Plasma (Sequential)" = "plasma",
                                                 "Cividis (Sequential)" = "cividis",
                                                 "Rocket (Sequential)" = "rocket",
                                                 "Mako (Sequential)" = "mako",
                                                 "Turbo (Sequential)" = "turbo",
                                                 "Reds (Sequential)" = "Reds",
                                                 "Oranges (Sequential)" = "Oranges",
                                                 "Purples (Sequential)" = "Purples"), # Added more choices
                                     selected = "YlOrRd")
        if(legend_upload){
          return(
            tagList(
              opacity_input
            )
          )
        }else {
          return(
            tagList(
              palette_input,
              opacity_input
            )
          )
        }

      } else {
        return(NULL)
      }
    })

    # Individual Raster Display Logic for Detected Density (NEW)
    observeEvent({
      isTruthy(raster_loaded) & isTruthy(input$show_raster)&
        (!is.null(input$raster_palette)|!is.null(input$raster_opacity)|TRUE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE, {

      ras <- raster_loaded()
      show_raster <- input$show_raster
      opacity <- input$raster_opacity
      raster_id <- stringr::str_to_lower(raster_name) %>%
        stringr::str_replace_all(" ", "_")

      if (is.null(opacity)) opacity <- 0.7

      leaflet_proxy() %>%
        leaflet::clearGroup(raster_name) %>%
        leaflet::removeControl(paste0("uploaded_raster_legend", "_", raster_id))

      if (!is.null(ras) && show_raster) {
        pal_input <- ifelse(legend_upload, "PRGn", input$raster_palette)
        pal_info <- create_raster_palette_and_domain(ras, pal_input,
                                                     use_quantiles = TRUE)
        pal <- pal_info$pal
        domain <- pal_info$domain

        # need to aggregate raster if too large https://rstudio.github.io/leaflet/articles/raster.html#large-raster-warning
        ras_nc <- raster::ncell(ras)
        if(ras_nc > 1.5e06){
          # try to figure out if it should be treated as categorical
          ras_n_unique <- n_distinct(raster::freq(ras, digits = 3)[, "value"])

          if(raster::is.factor(ras)|ras_n_unique < 30){
            fun <- "modal"
          } else {
            fun <- "mean"
          }

          ras <- raster::aggregate(ras, fact = sqrt(ras_nc/1.5e06) %>% ceiling(),
                                   fun = fun)
        }

        leaflet_proxy() %>%
          leaflet::addRasterImage(
            ras,
            colors = pal,
            opacity = opacity,
            project = FALSE,
            group = raster_name
          ) %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = pal,
            values = domain,
            title = paste0(raster_name, " Value"),
            layerId = paste0("uploaded_raster_legend", "_", raster_id)
          )
      }
    })

    # Update layer control to include new raster layers for toggling
    observeEvent(c(raster_loaded()), {
      # TODO change this so it comes from outside the module and could include
      # other loaded layers
      current_overlays <- c(
        "Selected Country",
        "Selected Ecoregions",
        "Custom Bounding Boxes",
        "Raster Boundaries"
      )

      if (!is.null(raster_loaded())) { # NEW: Add this check
        current_overlays <- c(current_overlays, raster_name)
      }

      leaflet_proxy() %>%
        leaflet::clearControls() %>% # Clear existing controls
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap,
                         options = leaflet::providerTileOptions(noWrap = TRUE),
                         group = "Base Map") %>%
        leaflet::addControl(
          html = '<div id="map_coords" style="background-color: white; padding: 5px; border-radius: 5px; opacity: 0.8;"></div>',
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = "Base Map",
          overlayGroups = current_overlays,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    }, ignoreNULL = FALSE) # Run initially and whenever rasters are uploaded/cleared

    # Optional: Render a preview of the uploaded legend image
    output$legend_preview_ui <- renderUI({
      req(input$legend_upload)
      b64 <- base64enc::dataURI(file = input$legend_upload$datapath, mime = "image/png")
        tags$div(
          tags$p("Uploaded Legend Preview:"),
          img(src = b64,
              style = "max-width: 150px; height: auto; border: 1px solid #ddd;")
        )
    })


  }) # end server
}

raster_upload_test <- function() {
  ui <- sidebarLayout(
    sidebarPanel(
      raster_upload_ui("samp_dens", raster_name = "Sampling Density"),
      raster_upload_ui("bivariate_uncertainty", raster_name = "Bivariate Uncertainty",
                       legend_upload = TRUE)
    ),
    mainPanel(
      leaflet::leafletOutput("country_map", height = "600px")
    )

  )
  server <- function(input, output, session) {
    output$country_map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap,
                         options = leaflet::providerTileOptions(noWrap = TRUE),
                         group = "Base Map") %>%
        leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%
        htmlwidgets::onRender(
          "function(el, x) {
          var map = this;
          var $map_coords = $('#map_coords'); // Cache the jQuery object for efficiency

          // Mousemove for coordinates
          map.on('mousemove', function(e) {
            var lat = e.latlng.lat.toFixed(4);
            var lng = e.latlng.lng.toFixed(4);
            var coords = 'Lat: ' + lat + ', Lng: ' + lng;
            $map_coords.html(coords);
          });
        }"
        ) %>%
        leaflet::addControl(
          html = '<div id="map_coords" style="background-color: white; padding: 5px; border-radius: 5px; opacity: 0.8;"></div>',
          position = "bottomleft"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = "Base Map",
          overlayGroups = c(
            "Selected Country",
            "Selected Ecoregions",
            "Custom Bounding Boxes",
            "Raster Boundaries",
            "Detected Points",
            "Undetected Points",
            "Detected Density"
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    map_proxy <- reactive({leaflet::leafletProxy("country_map")})

    raster_upload_server("samp_dens", raster_name = "Sampling Density",
                         leaflet_proxy = map_proxy)

    raster_upload_server("bivariate_uncertainty", raster_name = "Bivariate Uncertainty",
                         leaflet_proxy = map_proxy, legend_upload = TRUE)
  }
  shinyApp(ui, server)
}


# helper funs
# --- Generic Raster Loading Function ---
load_and_process_raster <- function(file_input, raster_type_name) {
  req(file_input)

  file_path <- file_input$datapath
  raster_name <- file_input$name

  withProgress(message = paste("Loading", raster_type_name, "Raster:", raster_name), value = 0, {
    incProgress(0.1, detail = "Reading file...")
    ras <- NULL

    tryCatch({
      ras <- raster::raster(file_path)
      incProgress(0.4, detail = "Checking projection...")

      target_crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")

      current_crs_obj <- tryCatch({
        sf::st_crs(raster::crs(ras)) # Attempt to get sf CRS object from raster's CRS
      }, error = function(e) {
        warning("Could not parse raster CRS with st_crs for ", raster_name, " (", raster_type_name, "): ", e$message)
        NULL
      })

      needs_reprojection <- FALSE

      if (is.null(current_crs_obj) || is.na(current_crs_obj$IsGeographic) || !current_crs_obj$IsGeographic) {
        showNotification(paste0("Raster '", raster_name, "' (", raster_type_name, ") CRS is missing, invalid, or not geographic. Attempting to reproject."), type = "warning")
        needs_reprojection <- TRUE
      } else if (current_crs_obj$epsg != 4326) {
        message("Reprojecting uploaded raster '", raster_name, "' (", raster_type_name, ") to WGS84 (EPSG:4326)...")
        needs_reprojection <- TRUE
      }

      if (needs_reprojection) {
        incProgress(0.2, detail = "Reprojecting to WGS84...")
        ras <- tryCatch({
          raster::projectRaster(ras, crs = target_crs)
        }, error = function(e) {
          showNotification(paste("Error during reprojection of '", raster_name, "' (", raster_type_name, "):", e$message), type = "error", duration = 10)
          return(NULL)
        })
        if (is.null(ras)) return(NULL)
      }

      incProgress(0.2, detail = "Processing bands...")

      if (raster::nlayers(ras) > 1) {
        showNotification(
          paste0("Multi-band raster detected for '", raster_name, "' (", raster_type_name, ", ", raster::nlayers(ras), " bands). Displaying band 1."),
          type = "info", duration = 5
        )
        ras <- ras[[1]]
      }

      incProgress(0.1, detail = "Done.")
      return(ras)
    }, error = function(e) {
      showNotification(paste("Error loading or processing '", raster_name, "' (", raster_type_name, "):", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
}

# --- Generic Raster Color Palette and Domain Function ---
#  returns a list containing both the palette function
# and the domain (min/max values) it was created with.
create_raster_palette_and_domain <- function(ras, palette_name, use_quantiles = FALSE) {
  req(ras, palette_name)

  # Handle case where raster might be empty or have no values
  if (all(is.na(raster::values(ras)))) {
    return(list(pal = leaflet::colorNumeric("transparent", domain = c(0, 1)), domain = c(0, 1))) # Return a transparent palette
  }

  if (use_quantiles) {
    # Use 1st and 99th quantiles for the domain to handle outliers
    q_vals <- quantile(raster::values(ras), probs = c(0.01, 0.99), na.rm = TRUE)
    min_val <- q_vals[1]
    max_val <- q_vals[2]
  } else {
    # Use absolute min and max values for the domain to cover all values
    min_val <- raster::cellStats(ras, 'min', na.rm = TRUE)
    max_val <- raster::cellStats(ras, 'max', na.rm = TRUE)
  }

  # Handle the case where min_val and max_val are the same (e.g., raster with a single constant value)
  if (min_val == max_val) {
    min_val <- min_val - 0.01
    max_val <- max_val + 0.01
  }

  # Ensure domain is valid (e.g., not NaN from all NAs after quantile/cellStats)
  if (is.na(min_val) || is.na(max_val) || !is.finite(min_val) || !is.finite(max_val)) {
    min_val <- 0 # Fallback to a default reasonable range
    max_val <- 1
    warning("Calculated raster domain is invalid (NA/Inf), defaulting to 0-1 for palette.")
  }

  if (palette_name %in% c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")) {
    pal_func <- get(palette_name)
    color_pal <- leaflet::colorNumeric(
      palette = pal_func(256),
      domain = c(min_val, max_val),
      na.color = "transparent"
    )
  } else {
    color_pal <- leaflet::colorNumeric(
      palette = palette_name,
      domain = c(min_val, max_val),
      na.color = "transparent"
    )
  }
  return(list(pal = color_pal, domain = c(min_val, max_val)))
}

