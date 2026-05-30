library(terra)
library(tigris)
library(FedData)
library(sf)

waBnd <- states(cb = TRUE) |> subset(STUSPS == "WA")
waBnd
plot(waBnd)
# NLCD <- get_nlcd(
#   template = waBnd,
#   year = 2019,
#   label = "WA State"
# )
# NLCD
nlcd <- rast("Annual_NLCD_LndCov_2024_CU_C1V1.tif")
nlcd
nlcdWastate <- crop(nlcd, waBnd)

crs(waBnd)
crs(nlcd)
plot(nlcdWastate)

waBnd <- waBnd %>% st_transform(crs = crs(nlcd))

nlcdWastate90m <- aggregate(nlcdWastate, fact = 3)

writeRaster(x = nlcdWastate, filename = "WA_NLCD_2019_30m.tif")
writeRaster(x = nlcdWastate90m, filename = "NLCD_90m.tif")