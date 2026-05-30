library(shiny) # Build the app
library(terra) # Work with raster data
library(dplyr) # Clean and summarize data
library(ggplot2) # Make plots
library(sf) # Work with spatial vector data
library(tidyterra) # Plot terra rasters with ggplot

# Labeling classes of cover type
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

get_urban_score <- function(rastObject, lat, lon, buffer_m = 1000, resolution_factor = 10) {
  
  nlcd <- rastObject # Load NLCD raster
  
  if (resolution_factor > 1) {
    nlcd <- aggregate(
      nlcd,
      fact = resolution_factor,
      fun = "modal",
      na.rm = TRUE
    )
  }
  
  # Make coords into an sf object
  usercoords <- data.frame(ID = 1, lat = lat, lon = lon) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = crs(nlcd))
  
  userbuffer <- usercoords %>% 
    st_buffer(dist = buffer_m)
  
  userbuffer_vect <- vect(userbuffer) # Convert sf buffer to terra vector
  
  nlcd_crop <- crop(nlcd, userbuffer_vect) # Crop raster to buffer extent
  
  nlcd_mask <- mask(nlcd_crop, userbuffer_vect) # Keep only cells inside buffer
  
  cell_values <- as.data.frame(nlcd_mask, xy = TRUE, na.rm = TRUE)
  
  names(cell_values) <- c("x_cell", "y_cell", "value")
  
  counts <- cell_values %>%
    count(value, name = "pixels") %>%
    left_join(nlcd_classes, by = "value") %>%
    mutate(
      urban_group = if_else(
        value %in% c(21, 22, 23, 24),
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

WA90m <- rast("NLCD_90m.tif")

test_results <- get_urban_score(
  rastObject = WA90m,
  lat = 47,
  lon = -120,
  resolution_factor = 10,
  buffer_m = 10000
)

test_results$score
test_results$urban_summary
test_results$table

ggplot() +
  geom_spatraster(data = test_results$raster_crop) +
  geom_sf(data = test_results$buffer, fill = NA, color = "red")

