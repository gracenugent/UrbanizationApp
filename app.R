library(shiny) # Build the app
library(terra) # Work with raster data
library(dplyr) # Clean and summarize data
library(ggplot2) # Make plots
library(sf)
library(tidyterra)

WA90m <- rast("NLCD_90m.tif")
plot(WA90m)

lat <- 45 # from user
lon <- -122 # from user
# make that into a SF object
usercoords <- data.frame(ID = 1, lat = lat,lon = lon) %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326) %>%
  st_transform(crs = crs(WA90m))
usercoords


ggplot() + geom_spatraster(data = WA90m) + geom_sf(data = usercoords)

nlcd_classes <- tibble( # Lookup table for NLCD codes
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

haversine_m <- function(lat1, lon1, lat2, lon2) { # Calculates distance between lat/long points
  
  R <- 6371000 # Earth radius in meters
  
  lat1 <- lat1 * pi / 180 # Convert first latitude to radians
  lon1 <- lon1 * pi / 180 # Convert first longitude to radians
  lat2 <- lat2 * pi / 180 # Convert raster-cell latitude to radians
  lon2 <- lon2 * pi / 180 # Convert raster-cell longitude to radians
  
  dlat <- lat2 - lat1 # Difference in latitude
  dlon <- lon2 - lon1 # Difference in longitude
  
  a <- sin(dlat / 2)^2 + # Haversine formula part 1
    cos(lat1) * cos(lat2) * sin(dlon / 2)^2 # Haversine formula part 2
  
  c <- 2 * atan2(sqrt(a), sqrt(1 - a)) # Haversine formula part 3
  
  R * c # Return distance in meters
}

get_urban_score <- function(rastObject,lat, lon, buffer_m = 1000, resolution_factor = 10) { # Main calculation function
  
  nlcd <- rastObject # Load NLCD raster
  
  if (resolution_factor > 1) { # Check if user wants coarser resolution
    nlcd <- aggregate( # Make raster coarser
      nlcd, # Raster to aggregate
      fact = resolution_factor, # Aggregation factor
      fun = "modal", # Use most common land-cover class
      na.rm = TRUE # Ignore missing values
    )
  }

  userbuffer <- usercoords %>% st_buffer(buffer_m)
  

  nlcd_crop <- crop(nlcd_ll, userbuffer) # Crop raster to small rough search area
  
  cell_values <- as.data.frame(nlcd_crop, xy = TRUE, na.rm = TRUE) # Get raster cell coordinates and values
  
  names(cell_values) <- c("lon_cell", "lat_cell", "value") # Rename columns
  
  cell_values <- cell_values %>% # Start cell data cleanup
    mutate(
      distance_m = haversine_m( # Calculate distance from input point
        lat1 = lat, # User latitude
        lon1 = lon, # User longitude
        lat2 = lat_cell, # Raster cell latitude
        lon2 = lon_cell # Raster cell longitude
      )
    ) %>%
    filter(distance_m <= buffer_m) # Keep only cells inside buffer distance
  
  counts <- cell_values %>% # Start counting land-cover classes
    count(value, name = "pixels") %>% # Count pixels by NLCD value
    left_join(nlcd_classes, by = "value") %>% # Add land-cover class labels
    mutate(
      urban_group = if_else( # Classify each class as urban or non-urban
        value %in% c(21, 22, 23, 24), # Developed NLCD classes
        "Urban", # Urban label
        "Non-urban" # Non-urban label
      )
    )
  
  urban_summary <- counts %>% # Summarize urban vs non-urban
    group_by(urban_group) %>% # Group by urban category
    summarize(
      pixels = sum(pixels, na.rm = TRUE), # Add pixels in each group
      .groups = "drop" # Drop grouping
    ) %>%
    mutate(
      proportion = pixels / sum(pixels), # Calculate proportion
      percent = proportion * 100 # Convert to percent
    )
  
  urban_score <- urban_summary %>% # Extract urban score
    filter(urban_group == "Urban") %>% # Keep urban row
    pull(proportion) # Pull proportion as number
  
  if (length(urban_score) == 0) { # If no urban pixels are found
    urban_score <- 0 # Set score to zero
  }
  
  list( # Return outputs
    table = counts, # Full land-cover table
    urban_summary = urban_summary, # Urban/non-urban table
    score = urban_score, # Urbanization score
    cells = cell_values # Cell-level data for optional plotting/checking
  )
}


ui <- fluidPage( # Create app page
  
  titlePanel("Urbanization Score Calculator"), # App title
  
  sidebarLayout( # Create sidebar/main layout
    
    sidebarPanel( # Sidebar for inputs
      
      h4("Coordinate Input"), # Coordinate section heading
      
      numericInput( # Latitude input
        "lat", # Input name
        "Latitude", # Label shown in app
        value = 48.75 # Default latitude
      ),
      
      numericInput( # Longitude input
        "lon", # Input name
        "Longitude", # Label shown in app
        value = -122.48 # Default longitude
      ),
      
      h4("Analysis Settings"), # Settings section heading
      
      numericInput( # Buffer input
        "buffer", # Input name
        "Buffer distance around point, in meters", # Label shown in app
        value = 1000, # Default buffer
        min = 30 # Minimum buffer
      ),
      
      selectInput( # Resolution input
        "resolution", # Input name
        "Coarsen NLCD raster for faster testing", # Label shown in app
        choices = c( # Dropdown choices
          "300 m, fastest test" = 10, # 30 m x 10
          "150 m" = 5, # 30 m x 5
          "90 m" = 3, # 30 m x 3
          "60 m" = 2, # 30 m x 2
          "30 m original, slowest" = 1 # Original NLCD
        ),
        selected = 10 # Default to coarse/fast
      ),
      
      actionButton( # Run button
        "run", # Button input name
        "Calculate Urbanization Score" # Button label
      )
    ),
    
    mainPanel( # Main area for outputs
      
      h3("Urbanization Score"), # Score heading
      textOutput("score"), # Score text output
      
      br(), # Space
      
      h3("Urban vs Non-Urban Summary"), # Summary heading
      tableOutput("urban_table"), # Urban/non-urban table
      
      br(), # Space
      
      h3("NLCD Land Cover Breakdown"), # Land-cover heading
      tableOutput("landcover_table"), # Full land-cover table
      
      br(), # Space
      
      plotOutput("landcover_plot", height = "500px") # Land-cover plot
    )
  )
)

server <- function(input, output) { # Server runs the calculations
  
  results <- eventReactive(input$run, { # Only run when button is clicked
    
    get_urban_score( # Run urbanization function
      lat = input$lat, # Use latitude input
      lon = input$lon, # Use longitude input
      buffer_m = input$buffer, # Use buffer input
      resolution_factor = as.numeric(input$resolution) # Use resolution input
    )
  })
  
  output$score <- renderText({ # Create score text
    
    paste0( # Paste sentence together
      "Urbanization score = ", # Label
      round(results()$score, 3), # Rounded proportion
      " meaning ", # Explanation
      round(results()$score * 100, 1), # Percent urban
      "% of the buffer is urban land cover." # Final text
    )
  })
  
  output$urban_table <- renderTable({ # Create urban summary table
    
    results()$urban_summary %>% # Use summary table
      mutate(
        proportion = round(proportion, 3), # Round proportion
        percent = round(percent, 1) # Round percent
      )
  })
  
  output$landcover_table <- renderTable({ # Create land-cover table
    
    results()$table %>% # Use full table
      mutate(percent = round(pixels / sum(pixels) * 100, 1)) %>% # Add percent
      select(class, urban_group, pixels, percent) %>% # Keep useful columns
      arrange(desc(percent)) # Sort largest to smallest
  })
  
  output$landcover_plot <- renderPlot({ # Create land-cover plot
    
    results()$table %>% # Use full table
      mutate(percent = pixels / sum(pixels) * 100) %>% # Add percent
      filter(!is.na(class)) %>% # Remove unlabeled classes
      ggplot(aes(x = reorder(class, percent), y = percent, fill = urban_group)) + # Set plot variables
      geom_col() + # Make bar plot
      coord_flip() + # Flip bars sideways
      labs(
        x = "Land cover class", # X label
        y = "Percent of buffer", # Y label
        title = "NLCD Land Cover Composition" # Plot title
      ) +
      theme_minimal(base_size = 14) # Clean theme
  })
}

shinyApp(ui, server) # Run app