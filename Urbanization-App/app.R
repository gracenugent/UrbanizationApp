library(shiny) # Build the app
library(terra) # Work with raster data
library(dplyr) # Clean and summarize data
library(ggplot2) # Make plots
library(sf) # Work with spatial data
library(tidyterra) # Plot terra rasters with ggplot

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
  
  nlcd <- rastObject # Use raster passed into function
  
  if (resolution_factor > 1) {
    nlcd <- aggregate(
      nlcd,
      fact = resolution_factor,
      fun = "modal",
      na.rm = TRUE
    )
  }
  
  usercoords <- data.frame(ID = 1, lat = lat, lon = lon) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(st_crs(crs(nlcd)))
  
  userbuffer <- usercoords %>% 
    st_buffer(dist = buffer_m)
  
  userbuffer_vect <- vect(userbuffer)
  
  nlcd_crop <- crop(nlcd, userbuffer_vect)
  nlcd_mask <- mask(nlcd_crop, userbuffer_vect)
  
  cell_values <- as.data.frame(nlcd_mask, xy = TRUE, na.rm = TRUE) # Convert raster cells to a data frame
  
  names(cell_values) <- c("x_cell", "y_cell", "value") # Rename columns
  
  cell_values <- cell_values %>%
    mutate(
      value = as.numeric(gsub("[^0-9]", "", as.character(value))) # Pull numeric NLCD code from factor/label
    ) %>%
    filter(!is.na(value)) # Remove missing values
  
  counts <- cell_values %>%
    count(value, name = "pixels") %>% # Count pixels in each NLCD class
    left_join(nlcd_classes, by = "value") %>% # Add readable NLCD class names
    mutate(
      urban_group = if_else(
        value %in% c(21, 22, 23, 24), # Developed classes
        "Urban",
        "Non-urban"
      )
    )
  
  urban_summary <- counts %>%
    group_by(urban_group) %>%
    summarize(
      pixels = sum(pixels, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      proportion = pixels / sum(pixels),
      percent = proportion * 100
    )
  
  urban_score <- urban_summary %>%
    filter(urban_group == "Urban") %>%
    pull(proportion)
  
  if (length(urban_score) == 0) {
    urban_score <- 0
  }
  
  list(
    table = counts,
    urban_summary = urban_summary,
    score = urban_score,
    cells = cell_values,
    raster_crop = nlcd_mask,
    buffer = userbuffer
  )
}

WA30m <- rast("WA_NLCD_2019_30m.tif")

rastObject <- WA30m

ui <- fluidPage(
  
  titlePanel("Urbanization Score Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Coordinate Input"),
      
      numericInput("lat", "Latitude", value = 48.75),
      numericInput("lon", "Longitude", value = -122.48),
      
      h4("Analysis Settings"),
      
      numericInput(
        "buffer",
        "Buffer distance around point, in meters",
        value = 1000,
        min = 30
      ),
      
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
      
      br(),
      
      h3("Map of Selected Buffer"),
      plotOutput("map_plot", height = "500px"),
      
      br(),
      
      h3("Urban vs Non-Urban Summary"),
      tableOutput("urban_table"),
      
      br(),
      
      h3("NLCD Land Cover Breakdown"),
      tableOutput("landcover_table"),
      
      br(),
      
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
    paste0(
      "Urbanization score = ",
      round(results()$score, 3),
      " meaning ",
      round(results()$score * 100, 1),
      "% of the buffer is urban land cover."
    )
  })
  
  output$map_plot <- renderPlot({
    ggplot() +
      geom_spatraster(data = results()$raster_crop) +
      geom_sf(data = results()$buffer, fill = NA, color = "red") +
      labs(
        title = "NLCD Land Cover Within Selected Buffer",
        x = "X coordinate",
        y = "Y coordinate"
      ) +
      theme_minimal()
  })
  
  output$urban_table <- renderTable({
    results()$urban_summary %>%
      mutate(
        proportion = round(proportion, 3),
        percent = round(percent, 1)
      )
  })
  
  output$landcover_table <- renderTable({
    results()$table %>%
      mutate(
        proportion = pixels / sum(pixels),
        percent = proportion * 100
      ) %>%
      select(value, class, urban_group, pixels, proportion, percent) %>%
      mutate(
        proportion = round(proportion, 3),
        percent = round(percent, 1)
      ) %>%
      arrange(desc(proportion))
  })
  
  output$landcover_plot <- renderPlot({
    results()$table %>%
      mutate(
        proportion = pixels / sum(pixels)
      ) %>%
      filter(!is.na(class)) %>%
      ggplot(aes(x = reorder(class, proportion), y = proportion, fill = urban_group)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Land cover type",
        y = "Proportion of buffer",
        title = "Proportion of Land Cover Types in Selected Buffer"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  output$landcover_plot <- renderPlot({
  results()$table %>%
    mutate(
      proportion = pixels / sum(pixels)
    ) %>%
    filter(!is.na(class)) %>%
    ggplot(aes(x = reorder(class, proportion), y = proportion, fill = urban_group)) +
    geom_col() +
    coord_flip() +
    labs(
      x = "Land cover type",
      y = "Proportion of buffer",
      title = "Proportion of Land Cover Types in Selected Buffer"
    ) +
    theme_minimal(base_size = 14)
})
  
  output$landcover_plot <- renderPlot({
    results()$table %>%
      mutate(percent = pixels / sum(pixels) * 100) %>%
      filter(!is.na(class)) %>%
      ggplot(aes(x = reorder(class, percent), y = percent, fill = urban_group)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Land cover class",
        y = "Percent of buffer",
        title = "NLCD Land Cover Composition"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
