# Preprocessing: Redlining
# By Hannah De los Santos
# Originated on: 1/8/2020

# load data and libraries ----

# load all global variables + data
source(file.path(system.file("R", package = "holcmapr"),
                 "redlining_global_var.R"), local = T)

# load all the functions for going through values
source(file.path(system.file("R", package = "holcmapr"),
                 "redlining_map_functions.R"))

library(tidycensus)
library(sf)

data_folder <- system.file("extdata", package = "holcmapr")
assign("data_folder", data_folder, envir = topenv())

holc_dat <- sf::st_read(
  file.path(data_folder, "mappinginequality.json"))
holc_dat <- holc_dat[!is.na(holc_dat$grade) & holc_dat$grade != "",]
# label neighborhoods that have no labels
for (cts in unique(paste0(holc_dat$city, "/", holc_dat$state))){
  city <- strsplit(cts, "/")[[1]][1]
  st <- strsplit(cts, "/")[[1]][2]

  holc_sub <- holc_dat[
    holc_dat$city == city & holc_dat$state == st
    ,]

  if (any(" " %in% holc_sub$label)){
    holc_sub$label[holc_sub$label == " "] <- paste0("ZZ", 1:sum(holc_sub$label == " "))
  }

  holc_dat[
    holc_dat$city == city & holc_dat$state == st
    , "label"] <- holc_sub$label

}
# will result in holc_dat
assign("holc_dat", holc_dat, envir = topenv())


# load us cities data (for mapping to counties for easy census download)
us_cities <- read.csv(file.path(data_folder, "All_US_cities.csv"))
# also load specific city mappings
specific_us_cities <- read.csv(file.path(data_folder, "Specific_US_cities.csv"))

assign("us_cities", us_cities, envir = topenv())
assign("specific_us_cities", specific_us_cities, envir = topenv())

# load centroids
centr_pop <- read.csv(
  file.path(
    data_folder, "CenPop2020_Mean_TR.csv"
  ),
  colClasses = c(rep("character",3), rep("numeric",3))
)
centr_pop$GEOID <- paste0(
  centr_pop$STATEFP,
  centr_pop$COUNTYFP,
  centr_pop$TRACTCE
)
assign("centr_pop", centr_pop, envir = topenv())

# run ----

# INITIAL:: ~1hr
# # we're going to go through every redlined city and pick up their information
all_cities <- unique(paste0(holc_dat$city, "/", holc_dat$state))

# before ~ 20 min
# after: 4 hours
# # also load the initial run
# load(file.path(data_folder, "Redlining_Full_Preprocess_Info.RData"))

start <- Sys.time()
redlining_info <- list()
for (cts in all_cities){
  print(paste0("[", Sys.time(), "] ", cts))

  city <- strsplit(cts, "/")[[1]][1]
  st <- strsplit(cts, "/")[[1]][2]

  # counties ----
  print(paste0("[", Sys.time(), "] Getting counties..."))

  redlining_info[[cts]][["counties"]] <-
    get_city_county(city, st)

  # census tract ----
  print(paste0("[", Sys.time(), "] Getting census tracts..."))

  redlining_info[[cts]][["ct"]] <- ct <-
    get_census_dat(city, st)
  # ADD TIGRIS INFO
  tct <- get_tigris_tract_info(city, st)
  rownames(tct) <- tct$GEOID

  redlining_info[[cts]][["ct"]] <- cbind(
    redlining_info[[cts]][["ct"]],
    tct[as.character(redlining_info[[cts]][["ct"]]$GEOID),
        c("ALAND", "AWATER")]
  )

  # census block ----
  print(paste0("[", Sys.time(), "] Getting census blocks..."))

  # get census block data, but cut it down -- we don't need all of it
  cb <- get_census_block(city, st)

  holc_sub <- holc_dat[
    holc_dat$city == city & holc_dat$state == st
    ,]
  holc_sub <- st_transform(holc_sub, st_crs(ct))

  # preallocate dataframe to store all the tract areas and such
  ct_city <- st_crop(ct, st_bbox(holc_sub))
  ct_city <- ct[ct$GEOID %in% ct_city$GEOID,]

  # check that holc is valid
  holc_valid <- suppressWarnings(st_is_valid(holc_sub))
  if (!all(holc_valid)){
    # we need to reproject and fix it
    proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"
    ct_city <- st_transform(ct_city, st_crs(proj))
    holc_sub <- st_transform(holc_sub, st_crs(proj))
    # then add a 0 width buffer
    holc_sub <- st_buffer(holc_sub, 0)
  }

  ct_city_inter <- st_intersection(ct_city, holc_sub)

  # subset to only the tracts we know intersect with a holc grade
  cb_city <- cb[cb$tractFIPS %in% unique(ct_city_inter$GEOID),]

  if (!all(holc_valid)){
    cb_city <- st_transform(cb_city, st_crs(proj))
  }

  redlining_info[[cts]][["cb"]] <- cb_city

  # get census areas and populations ----
  print(paste0("[", Sys.time(), "] Getting census areas and populations..."))

  redlining_info[[cts]][["c_area_pop"]] <-
    tryCatch({
      suppressWarnings(calc_census_area_pop(city, st, ct, cb))
    },
    error = function(e){
      paste("error: census areas and populations", e)
    }
    )

  # get holc area populations ----

  print(paste0("[", Sys.time(), "] Getting HOLC area populations..."))

  redlining_info[[cts]][["holc_pop"]] <-
    tryCatch({
      calc_holc_pop(city, st, ct, cb)
    },
    error = function(e){
      paste("error: holc area populations", e)
    }
    )

  rm(cb)
}
end <- Sys.time()
print(paste0("Total time: ", end - start))

# total: 4 hours

# check for errors
lapply(redlining_info, function(x){head(x$c_area_pop[,1])})
lapply(redlining_info, function(x){head(x$holc_pop)})

# save ----

save(list = c("redlining_info"),
     file = file.path(data_folder, "Redlining_Full_Preprocess_Info.RData"))


write.csv(
  st_drop_geometry(holc_dat[!duplicated(paste0(holc_dat$city, "/", holc_dat$state)),
                            c("city", "state")]),
  file.path(data_folder, "Included_Redlined_Cities.csv"),
  row.names = F, na = "")

# break up the file for easier access ----

load(file.path(data_folder, "Redlining_Full_Preprocess_Info.RData"))
# do not include in the final update

for (cts in 1:length(redlining_info)){
  redlining_city <- redlining_info[[cts]]
  city_name <- names(redlining_info[cts])
  # rename to save as file
  city_name <- gsub("/", "_", city_name)
  # remove spaces
  city_name <- gsub(" ", "", city_name)

  save(list = c("redlining_city"),
       file = file.path(
         data_folder, "Redlining",
         paste0(city_name, "_Preprocess.RData")
       ))
}
