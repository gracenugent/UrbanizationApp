library(shiny)
library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(bslib)

# NLCD land cover class labels
nlcd_classes <- tibble(
  value = c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52,
            71, 81, 82, 90, 95),
  class = c(
    "Open Water",
    "Developed Open Space",
    "Developed Low Intensity",
    "Developed Medium Intensity",
    "Developed High Intensity",
    "Barren",
    "Deciduous Forest",
    "Evergreen Forest",
    "Mixed Forest",
    "Shrub/Scrub",
    "Grassland/Herbaceous",
    "Pasture/Hay",
    "Cultivated Crops",
    "Woody Wetlands",
    "Emergent Herbaceous Wetlands"
  )
)

# Colors for each NLCD land cover class
nlcd_colors <- c(
  "Open Water" = "#466b9f",
  "Developed Open Space" = "#dec5c5",
  "Developed Low Intensity" = "#d99282",
  "Developed Medium Intensity" = "#eb0000",
  "Developed High Intensity" = "#ab0000",
  "Barren" = "#b3ac9f",
  "Deciduous Forest" = "#68ab5f",
  "Evergreen Forest" = "#1c5f2c",
  "Mixed Forest" = "#b5c58f",
  "Shrub/Scrub" = "#ccb879",
  "Grassland/Herbaceous" = "#dfdfc2",
  "Pasture/Hay" = "#dcd939",
  "Cultivated Crops" = "#ab6c28",
  "Woody Wetlands" = "#b8d9eb",
  "Emergent Herbaceous Wetlands" = "#6c9fb8"
)

get_urban_score <- function(rastObject, lat, lon, buffer_m = 1000, resolution_factor = 1) {
  
  nlcd <- rastObject
  
  if (resolution_factor > 1) {
    nlcd <- aggregate(nlcd, fact = resolution_factor, fun = "modal", na.rm = TRUE)
  }
  
  usercoords <- data.frame(lon = lon, lat = lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs(nlcd))
  
  userbuffer <- st_buffer(usercoords, dist = buffer_m)
  userbuffer_vect <- vect(userbuffer)
  
  nlcd_crop <- crop(nlcd, userbuffer_vect)
  nlcd_mask <- mask(nlcd_crop, userbuffer_vect)
  
  extracted <- terra::extract(nlcd, userbuffer_vect, ID = FALSE)
  
  if (nrow(extracted) == 0) {
    counts <- tibble()
    urban_score <- 0
  } else {
    
    value_raw <- extracted[[1]]
    
    value_number <- suppressWarnings(
      as.numeric(gsub("[^0-9]", "", as.character(value_raw)))
    )
    
    value_from_class <- nlcd_classes$value[
      match(as.character(value_raw), nlcd_classes$class)
    ]
    
    value <- ifelse(is.na(value_number), value_from_class, value_number)
    
    cell_values <- tibble(value = value) %>%
      filter(!is.na(value))
    
    # NLCD developed classes are treated as urban
    counts <- cell_values %>%
      count(value, name = "pixels") %>%
      left_join(nlcd_classes, by = "value") %>%
      mutate(
        class = if_else(is.na(class), paste("Unknown class", value), class),
        urban_group = if_else(value %in% c(21, 22, 23, 24), "Urban", "Non-urban"),
        proportion = pixels / sum(pixels),
        percent = proportion * 100
      )
    
    urban_score <- counts %>%
      filter(urban_group == "Urban") %>%
      summarize(score = sum(proportion, na.rm = TRUE)) %>%
      pull(score)
    
    if (length(urban_score) == 0 || is.na(urban_score)) {
      urban_score <- 0
    }
  }
  
  list(
    table = counts,
    score = urban_score,
    raster_crop = nlcd_mask,
    buffer = userbuffer
  )
}

# Load local NLCD raster
nlcd_raster <- rast("WA_NLCD_2019_30m.tif")

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c7a4b",
    base_font = font_google("Lato")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f7f2;
      }
      .title-panel {
        background-color: #2c7a4b;
        color: white;
        padding: 18px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: #ffffff;
        padding: 18px;
        border-radius: 10px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.12);
      }
      .output-card {
        background-color: #ffffff;
        padding: 18px;
        border-radius: 10px;
        margin-bottom: 18px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.10);
      }
      .btn {
        background-color: #2c7a4b;
        border-color: #2c7a4b;
        color: white;
        font-weight: bold;
      }
    "))
  ),
  
  div(class = "title-panel",
      h2("Urbanization Score Calculator"),
      p("Calculate the proportion of urban land cover around a selected coordinate.")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      class = "sidebar",
      
      h4("Coordinate Input"),
      numericInput("lat", "Latitude:", value = 48.75),
      numericInput("lon", "Longitude:", value = -122.48),
      
      h4("Analysis Settings"),
      numericInput("buffer", "Buffer Distance (in meters):", value = 1000, min = 30),
      
      selectInput(
        "resolution",
        "Analysis Resolution:",
        choices = list(
          "30 m" = 1,
          "60 m" = 2,
          "90 m" = 3,
          "150 m" = 5,
          "300 m" = 10
        ),
        selected = 1
      ),
      
      actionButton("run", "Calculate Urbanization Score")
    ),
    
    mainPanel(
      div(class = "output-card",
          h3("Urbanization Score"),
          textOutput("score")
      ),
      
      div(class = "output-card",
          h3("Map of Selected Buffer"),
          plotOutput("map_plot", height = "500px")
      ),
      
      div(class = "output-card",
          h3("Land Cover Composition"),
          plotOutput("landcover_plot", height = "500px")
      ),
      
      div(class = "output-card",
          h3("NLCD Land Cover Breakdown"),
          tableOutput("landcover_table")
      )
    )
  )
)

server <- function(input, output) {
  
  results <- eventReactive(input$run, {
    get_urban_score(
      rastObject = nlcd_raster,
      lat = input$lat,
      lon = input$lon,
      buffer_m = input$buffer,
      resolution_factor = as.numeric(input$resolution)
    )
  })
  
  output$score <- renderText({
    req(results())
    
    paste0(
      "Urbanization score = ",
      round(results()$score, 3),
      " meaning ",
      round(results()$score * 100, 1),
      "% of the buffer is developed/urban land cover."
    )
  })
  
  output$map_plot <- renderPlot({
    req(results())
    
    map_df <- as.data.frame(results()$raster_crop, xy = TRUE, na.rm = TRUE)
    names(map_df) <- c("x", "y", "value")
    
    map_df <- map_df %>%
      mutate(
        value = suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(value))))
      ) %>%
      left_join(nlcd_classes, by = "value") %>%
      filter(!is.na(class))
    
    ggplot() +
      geom_raster(
        data = map_df,
        aes(x = x, y = y, fill = class)
      ) +
      geom_sf(
        data = results()$buffer,
        fill = NA,
        color = "black",
        linewidth = 1.2
      ) +
      scale_fill_manual(
        values = nlcd_colors,
        name = "Land Cover Type"
      ) +
      labs(
        title = "NLCD Land Cover Within Selected Buffer"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  })
  
  output$landcover_table <- renderTable({
    req(results())
    
    results()$table %>%
      select(class, urban_group, pixels, proportion) %>%
      mutate(
        proportion = round(proportion, 3)
      ) %>%
      arrange(desc(proportion)) %>%
      rename(
        "Land Cover Type" = class,
        "Urban Category" = urban_group,
        "Pixel Count" = pixels,
        "Proportion of Buffer" = proportion
      )
  })
  
  output$landcover_plot <- renderPlot({
    req(results())
    req(nrow(results()$table) > 0)
    
    results()$table %>%
      ggplot(
        aes(
          x = reorder(class, -percent),
          y = percent,
          fill = class
        )
      ) +
      geom_col() +
      scale_fill_manual(values = nlcd_colors, guide = "none") +
      labs(
        x = "Land Cover Type",
        y = "Percent of Buffer",
        title = "Land Cover Composition"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

shinyApp(ui, server)