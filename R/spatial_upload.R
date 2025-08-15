spatial_upload_ui <- function(id, country_names) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Add model materials input and outputs (rasters, shapes, etc.)."),

    # Add custom CSS for the sidebar scrollbar
    tags$head(
      tags$style(HTML("
      .sidebar {
        height: calc(100vh - 80px); /* Adjust height as needed, 80px is for title/padding */
        overflow-y: auto;
      }
    "))
    ),

    sidebarLayout(
      sidebarPanel(
        class = "sidebar", # Add a class to the sidebarPanel for CSS targeting
        h3("1. Location/Area of analysis/Extent"),

        selectInput(
          inputId = ns("selected_country"),
          label = "Select a Country:",
          choices = c("World (Global View)", country_names), # Add a global view option
          selected = "World (Global View)"
        ),

        # --- Bounding Box Group (Regrouped for space) ---
        wellPanel( # Grouping bounding box inputs
          h4("Add Custom Bounding Box Polygon"),
          fluidRow( # Put Xmin/Xmax on one line
            column(6, numericInput(ns("xmin"), "X Min (Lon):", value = -136, step = 0.1)),
            column(6, numericInput(ns("xmax"), "X Max (Lon):", value = -60, step = 0.1))
          ),
          fluidRow( # Put Ymin/Ymax on one line
            column(6, numericInput(ns("ymin"), "Y Min (Lat)::", value = 50, step = 0.1)),
            column(6, numericInput(ns("ymax"), "Y Max (Lat)::", value = 70, step = 0.1))
          ),
          actionButton(ns("add_bbox_polygon"), "Add Bounding Box Polygon")
        ),

        # --- Raster Boundary Upload Section ---
        wellPanel(
          h4("Add Raster Boundary Polygon"),
          fileInput(ns("raster_boundary_upload"), "Choose Raster to Extract Boundary (e.g., .tif, .asc)",
                    accept = c(".tif", ".tiff", ".asc", ".grd", ".nc", ".img")),
          actionButton(ns("add_raster_boundary"), "Add Raster Boundary as Polygon")
        ),
        tags$hr(),
        h3("2. Management Units (e.g., Jurisdictions, Ecoregions, etc.)"),

        # Ecoregion dropdown will be dynamically rendered here
        uiOutput(ns("ecoregion_select_ui")),
        tags$hr(), # Horizontal rule for separation

        h3("3. Species data (e.g., Occurrences, density sampling, etc.)"),
        # Shapefile Input for Detected Point Data
        fileInput(ns("point_shapefile_upload"), "Upload Detected Data (Shapefile: .shp, .shx, .dbf, .prj)",
                  multiple = TRUE,
                  accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".cpg")),
        checkboxInput(ns("show_points"), "Show Detected Points", value = FALSE),

        # NEW: Shapefile Input for Undetected Point Data
        fileInput(ns("undetected_point_shapefile_upload"), "Upload Undetected Data (Shapefile: .shp, .shx, .dbf, .prj)",
                  multiple = TRUE,
                  accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".cpg")),
        checkboxInput(ns("show_undetected_points"), "Show Undetected Points", value = FALSE),

        raster_upload_ui(ns("sample_density"), "Sampling Density"),

        raster_upload_ui(ns("predictions"), "Predictions"),

        raster_upload_ui(ns("uncertainty"), "Uncertainty"),

        raster_upload_ui(ns("extrapolation"), "Extrapolation"),

        raster_upload_ui(ns("bivariate_uncertainty"), "Bivariate Predictions Uncertainty",
                         legend_upload = TRUE),
      ),
      mainPanel(
        leaflet::leafletOutput(ns("country_map"), height = "600px")
      )
    )
  )
}

# Define the server logic for a module
spatial_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Allow files up to 50 MB
    options(shiny.maxRequestSize=50*1024^2)

    # Reactive expression for the currently selected country's geometry
    filtered_country_data <- reactive({
      if (input$selected_country == "World (Global View)") {
        return(world_countries)
      } else {
        world_countries[world_countries$name == input$selected_country, ]
      }
    })

    # Reactive expression to get ecoregion names that intersect with the current country
    # This now uses the pre-calculated lookup table for efficiency
    intersecting_ecoregion_names <- reactive({
      req(country_ecoregion_lookup) # Ensure lookup table is loaded

      country_key <- input$selected_country
      if (country_key %in% names(country_ecoregion_lookup)) {
        return(country_ecoregion_lookup[[country_key]])
      } else {
        return(character(0)) # Return empty character vector if no data found
      }
    })

    # Dynamically render the ecoregion select input based on intersecting ecoregion names
    output$ecoregion_select_ui <- renderUI({
      ecoregion_choices <- sort(intersecting_ecoregion_names())
      selectInput(
        inputId = "selected_ecoregions", # Changed ID to plural
        label = "Select Ecoregion(s):",
        choices = ecoregion_choices,
        selected = NULL, # Default to no selection for multiple select
        multiple = TRUE # Allow multiple selections
      )
    })

    # Reactive expression for the *multiple* ecoregion polygons to display
    selected_ecoregions_to_display <- reactive({
      req(input$selected_ecoregions, resolve_ecoregions) # Ensure dropdown and ecoregions data are ready

      if (!is.null(input$selected_ecoregions) && length(input$selected_ecoregions) > 0) {
        return(resolve_ecoregions[resolve_ecoregions$ECO_NAME %in% input$selected_ecoregions, ])
      } else {
        return(NULL)
      }
    })

    # NEW: Reactive for Raster Boundary Upload
    raster_boundary_data <- eventReactive(input$add_raster_boundary, {
      req(input$raster_boundary_upload)

      ras <- load_and_process_raster(input$raster_boundary_upload, "Raster Boundary")

      withProgress({
        tryCatch({
          incProgress(0.5, detail = "Calculating bounding box...")
          bbox <- as(raster::extent(ras), "SpatialPolygons")
          sp::proj4string(bbox) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs") # Ensure CRS is set
          bbox_sf <- sf::st_as_sf(bbox)

          incProgress(0.5, detail = "Done.")
          return(bbox_sf)
        }, error = function(e) {
          showNotification(paste("Error calculating bounding box:", e$message), type = "error", duration = 10)
          return(NULL)
        })
      })
    })

    map_proxy <- reactive({leaflet::leafletProxy("country_map")})

    raster_upload_server("sample_density", "Sampling Density", map_proxy)

    raster_upload_server("predictions", "Predictions", map_proxy)

    raster_upload_server("uncertainty", "Uncertainty", map_proxy)

    raster_upload_server("extrapolation", "Extrapolation", map_proxy)

    raster_upload_server("bivariate_uncertainty", "Bivariate Predictions Uncertainty",
                         map_proxy, legend_upload = TRUE)

    uploaded_points_data <- reactive({
      load_and_process_shapefile(input$point_shapefile_upload, "Detected points")
    })
    undetected_points_data <- reactive({
      load_and_process_shapefile(input$undetected_point_shapefile_upload, "Undetected points")
    })

    # Render the Leaflet map (initial setup)
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
            "Detected Density" # NEW: Add this to the initial layers control
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # Observe changes in the selected country (redraws country, updates ecoregion dropdown)
    observeEvent(input$selected_country, {
      map_data <- filtered_country_data()

      leaflet::leafletProxy("country_map") %>%
        leaflet::clearGroup("Selected Country") %>%
        leaflet::clearGroup("Selected Ecoregions") # Clear the plural group

      if (input$selected_country != "World (Global View)") {
        if (nrow(map_data) > 0) {
          bbox <- sf::st_bbox(map_data)

          leaflet::leafletProxy("country_map") %>%
            leaflet::addPolygons(
              data = map_data,
              fillColor = "blue",
              color = "black",
              weight = 1,
              fillOpacity = 0.6,
              highlightOptions = leaflet::highlightOptions(
                color = "white",
                weight = 2,
                bringToFront = TRUE
              ),
              group = "Selected Country"
            ) %>%
            leaflet::fitBounds(
              lng1 = bbox["xmin"], lat1 = bbox["ymin"],
              lng2 = bbox["xmax"], lat2 = bbox["ymax"]
            )
        }
      } else {
        leaflet::leafletProxy("country_map") %>%
          leaflet::setView(lng = 0, lat = 0, zoom = 2)
      }
    })

    # Observe changes in the selected Ecoregion dropdown (draws/clears Ecoregions)
    observeEvent(input$selected_ecoregions, { # Changed trigger to plural input ID
      ecoregions_data <- selected_ecoregions_to_display() # Use the plural reactive

      leaflet::leafletProxy("country_map") %>%
        leaflet::clearGroup("Selected Ecoregions") # Clear the plural group

      if (!is.null(ecoregions_data) && nrow(ecoregions_data) > 0) {
        bbox <- sf::st_bbox(ecoregions_data)

        leaflet::leafletProxy("country_map") %>%
          addPolygons(
            data = ecoregions_data,
            fillColor = "green",
            color = "darkgreen",
            weight = 2,
            fillOpacity = 0.5,
            group = "Selected Ecoregions", # Use the plural group
            popup = ~paste0("Ecoregion: ", ECO_NAME, "<br>",
                            "Realm: ", REALM, "<br>",
                            "Biome: ", BIOME_NAME)
          ) %>%
          leaflet::fitBounds(
            lng1 = bbox["xmin"], lat1 = bbox["ymin"],
            lng2 = bbox["xmax"], lat2 = bbox["ymax"]
          )
      }
    })

    # Observe the "Add Bounding Box Polygon" button click
    observeEvent(input$add_bbox_polygon, {
      req(input$xmin, input$xmax, input$ymin, input$ymax)

      if (input$xmin >= input$xmax || input$ymin >= input$ymax) {
        showNotification("X Max must be greater than X Min, and Y Max must be greater than Y Min.",
                         type = "error", duration = 5)
        return()
      }

      bbox_coords <- matrix(c(
        input$xmin, input$ymin,
        input$xmax, input$ymin,
        input$xmax, input$ymax,
        input$xmin, input$ymax,
        input$xmin, input$ymin
      ), ncol = 2, byrow = TRUE)

      bbox_polygon <- st_sf(
        geometry = st_sfc(st_polygon(list(bbox_coords)), crs = 4326)
      )

      leaflet::leafletProxy("country_map") %>%
        leaflet::addPolygons(
          data = bbox_polygon,
          fillColor = "red",
          color = "darkred",
          weight = 2,
          fillOpacity = 0.4,
          group = "Custom Bounding Boxes"
        ) %>%
        leaflet::fitBounds(
          lng1 = input$xmin, lat1 = input$ymin,
          lng2 = input$xmax, lat2 = input$ymax
        )
    })

    # Observe for Raster Boundary Data and add to map
    observeEvent(raster_boundary_data(), {
      boundary_sf <- raster_boundary_data()
      req(boundary_sf)

      leaflet::leafletProxy("country_map") %>%
        leaflet::addPolygons(
          data = boundary_sf,
          fillColor = "orange",
          color = "darkorange",
          weight = 2,
          fillOpacity = 0.3,
          group = "Raster Boundaries",
          popup = "Uploaded Raster Boundary"
        ) %>%
        leaflet::fitBounds(
          lng1 = sf::st_bbox(boundary_sf)["xmin"], lat1 = sf::st_bbox(boundary_sf)["ymin"],
          lng2 = sf::st_bbox(boundary_sf)["xmax"], lat2 = sf::st_bbox(boundary_sf)["ymax"]
        )
    })

    # Observe for Uploaded Detected Shapefile Point Data and add to map
    observeEvent(c(uploaded_points_data(), input$show_points), {
      points_sf <- uploaded_points_data()
      show_points <- input$show_points

      leaflet::leafletProxy("country_map") %>%
        leaflet::clearGroup("Detected Points") # Clear this specific group

      if (!is.null(points_sf) && show_points) {
        leaflet::leafletProxy("country_map") %>%
          leaflet::addCircleMarkers(
            data = points_sf,
            radius = 3,
            color = "purple",
            fillColor = "purple",
            fillOpacity = 0.8,
            stroke = FALSE,
            group = "Detected Points",
            # Create a generic popup from all properties if available
            popup = ~{
              if (length(names(points_sf)) > 0) {
                popup_html <- "<b>Attributes:</b><br>"
                # Exclude the geometry column from the popup
                for (col_name in names(points_sf)[!names(points_sf) %in% c(attr(points_sf, "sf_column"))]) {
                  popup_html <- paste0(popup_html, "<b>", col_name, ":</b> ", points_sf[[col_name]], "<br>")
                }
                popup_html
              } else {
                "Point data"
              }
            }
          )
      }
    })

    # Observe for Uploaded Undetected Shapefile Point Data and add to map
    observeEvent(c(undetected_points_data(), input$show_undetected_points), {
      points_sf <- undetected_points_data()
      show_points <- input$show_undetected_points

      leaflet::leafletProxy("country_map") %>%
        leaflet::clearGroup("Undetected Points") # Clear this specific group

      if (!is.null(points_sf) && show_points) {
        leaflet::leafletProxy("country_map") %>%
          leaflet::addCircleMarkers(
            data = points_sf,
            radius = 3,
            color = "red", # Choose a different color for "Undetected"
            fillColor = "red",
            fillOpacity = 0.8,
            stroke = FALSE,
            group = "Undetected Points",
            popup = ~{
              if (length(names(points_sf)) > 0) {
                popup_html <- "<b>Attributes:</b><br>"
                for (col_name in names(points_sf)[!names(points_sf) %in% c(attr(points_sf, "sf_column"))]) {
                  popup_html <- paste0(popup_html, "<b>", col_name, ":</b> ", points_sf[[col_name]], "<br>")
                }
                popup_html
              } else {
                "Undetected Point data"
              }
            }
          )
      }
    })

  }) # end server
}


# Helper funs
load_and_process_shapefile <- function(file_input, layer_type_name){
  # Require .shp and .dbf files, and optionally .shx and .prj
  req(file_input)

  # Get the datapath of the .shp file
  # Ensure all parts of the shapefile are present and extract the shp file
  shp_file_info <- file_input[grep(".shp$", file_input$name), ]
  dbf_file_info <- file_input[grep(".dbf$", file_input$name), ]
  shx_file_info <- file_input[grep(".shx$", file_input$name), ]
  prj_file_info <- file_input[grep(".prj$", file_input$name), ] # Optional but good to have

  if (nrow(shp_file_info) == 0 || nrow(dbf_file_info) == 0) {
    showNotification("A shapefile requires at least .shp and .dbf components.", type = "error", duration = 5)
    return(NULL)
  }

  # Create a temporary directory to store the shapefile components
  temp_dir <- tempdir()
  file.copy(file_input$datapath,
            file.path(temp_dir, file_input$name),
            overwrite = TRUE)

  # Path to the .shp file within the temporary directory
  shp_path_in_temp <- file.path(temp_dir, shp_file_info$name)

  withProgress(message = paste("Loading Shapefile Point Data:"), value = 0, {
    incProgress(0.1, detail = "Reading shapefile...")
    points_sf <- NULL
    tryCatch({
      points_sf <- sf::st_read(shp_path_in_temp, quiet = TRUE)
      incProgress(0.3, detail = "Checking geometry type...")

      # Ensure it's point data
      if (!any(sf::st_geometry_type(points_sf) %in% c("POINT", "MULTIPOINT"))) {
        showNotification("Uploaded shapefile does not contain point geometries. Please upload a point shapefile.", type = "error", duration = 10)
        return(NULL)
      }

      incProgress(0.3, detail = "Checking projection and reprojecting if necessary...")
      # Reproject to WGS84 if not already
      if (sf::st_crs(points_sf)$epsg != 4326) {
        message("Reprojecting uploaded shapefile to WGS84 (EPSG:4326)...")
        points_sf <- sf::st_transform(points_sf, 4326)
      }

      incProgress(0.2, detail = "Done.")
      return(points_sf)
    }, error = function(e) {
      showNotification(paste("Error loading or processing shapefile:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
}

spatial_upload_test <- function() {

  # --- Data Preparation (runs once when the app starts) ---
  # Get world country boundaries data
  world_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Get a list of country names for the dropdown
  country_names <- sort(world_countries$name)

  # Ecoregions (Dinerstein et al. 2017).
  ecoregions_path <- here::here("inst/app/www/datasets_solutions/Ecoregions2017/Ecoregions2017.shp")
  lookup_path <- "prepare_data/country_ecoregion_lookup.rds"

  # Initialize resolve_ecoregions and lookup table
  resolve_ecoregions <- NULL
  country_ecoregion_lookup <- NULL

  # Check if the shapefile exists before attempting to read it
  if (file.exists(ecoregions_path)) {
    resolve_ecoregions <- sf::st_read(ecoregions_path, quiet = TRUE)
    if (sf::st_crs(resolve_ecoregions)$epsg != 4326) {
      resolve_ecoregions <- sf::st_transform(resolve_ecoregions, 4326)
    }
    # Ensure the ECO_NAME column exists for filtering later
    if (!("ECO_NAME" %in% names(resolve_ecoregions))) {
      warning("Column 'ECO_NAME' not found in Ecoregions shapefile. Filtering by ecoregion will not work.")
      resolve_ecoregions <- NULL # Set to NULL if critical column is missing
    }
  } else {
    warning("RESOLVE Ecoregions shapefile not found at ", ecoregions_path,
            ". Ecoregion functionality will be limited.")
  }

  # Load the pre-calculated lookup table
  if (file.exists(lookup_path)) {
    country_ecoregion_lookup <- readRDS(lookup_path)
  } else {
    warning("Country-ecoregion lookup table not found at ", lookup_path,
            ". Please run 'prepare_data.R' first.")
  }


  ui <- fluidPage(
    spatial_upload_ui("test1", country_names)
  )

  server <- function(input, output, session) {
    spatial_upload_server("test1")
  }

  shinyApp(ui, server)
}
