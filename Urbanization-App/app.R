library(shiny)
library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyterra)

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
    urban_summary <- tibble()
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
    
    urban_summary <- counts %>%
      group_by(urban_group) %>%
      summarize(
        pixels = sum(pixels),
        proportion = sum(proportion),
        percent = sum(percent),
        .groups = "drop"
      )
  }
  
  list(
    table = counts,
    urban_summary = urban_summary,
    score = urban_score,
    raster_crop = nlcd_mask,
    buffer = userbuffer
  )
}

WA30m <- rast("../WA_NLCD_2019_30m.tif")
rastObject <- WA30m

ui <- fluidPage(
  
  titlePanel("Urbanization Score Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Coordinate Input"),
      numericInput("lat", "Latitude", value = 48.75),
      numericInput("lon", "Longitude", value = -122.48),
      
      h4("Analysis Settings"),
      numericInput("buffer", "Buffer distance around point, in meters", value = 1000, min = 30),
      
      selectInput(
        "resolution",
        "Analysis resolution",
        choices = list(
          "30 m original" = 1,
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
      h3("Urbanization Score"),
      textOutput("score"),
      
      h3("Map of Selected Buffer"),
      plotOutput("map_plot", height = "500px"),
      
      h3("Urban vs Non-Urban Summary"),
      tableOutput("urban_table"),
      
      h3("NLCD Land Cover Breakdown"),
      tableOutput("landcover_table"),
      
      h3("Land Cover Composition"),
      plotOutput("landcover_plot", height = "500px")
    )
  )
)

server <- function(input, output) {
  
  results <- eventReactive(input$run, {
    get_urban_score(
      rastObject = rastObject,
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
    
    ggplot() +
      geom_spatraster(data = results()$raster_crop) +
      geom_sf(data = results()$buffer, fill = NA, color = "red", linewidth = 1) +
      theme_minimal()
  })
  
  output$urban_table <- renderTable({
    req(results())
    results()$urban_summary %>%
      mutate(
        proportion = round(proportion, 3),
        percent = round(percent, 1)
      )
  })
  
  output$landcover_table <- renderTable({
    req(results())
    results()$table %>%
      select(value, class, urban_group, pixels, proportion, percent) %>%
      mutate(
        proportion = round(proportion, 3),
        percent = round(percent, 1)
      ) %>%
      arrange(desc(percent))
  })
  
  output$landcover_plot <- renderPlot({
    req(results())
    req(nrow(results()$table) > 0)
    
    results()$table %>%
      ggplot(aes(x = reorder(class, percent), y = percent, fill = urban_group)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Land cover class",
        y = "Percent of buffer",
        title = "NLCD Land Cover Composition",
        fill = "Group"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
