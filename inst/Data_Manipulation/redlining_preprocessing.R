# Preprocessing: Redlining
# By Hannah De los Santos
# Originated on: 1/8/2020

# load data and libraries ----

# load all global variables + data
source("redlining_global_var.R", local = T)

# load all the functions for going through values
source("redlining_functions.R")

# run ----

# INITIAL:: ~1hr
# # we're going to go through every redlined city and pick up their information
# all_cities <- unique(paste0(holc_dat@data$city, "/", holc_dat@data$state))

# SUBSEQUENT::1 ~11min
# we need to fix the cities that got messed up
# all_cities <- c(
#   "Union Co./NJ",
#   "Roanoke/VA",
#   "Pittsburgh/PA",
#   "Toledo/OH",
#   "Harrisburg/PA",
#   "Manchester/NH",
#   "Detroit/MI",
#   "Shreveport/LA",
#   "Covington/KY",
#   "Peoria/IL",
#   "Chicago/IL",
#   "Augusta/GA",
#   "East St. Louis/IL",
#   "Waterbury/CT",
#   "Denver/CO",
#   "Sacramento/CA"
# )
# SUBSEQUENT::2 ~1min
# we need to fix the cities that got messed up
# all_cities <- c(
#   "Norfolk/VA"
# )
# SUBSEQUENT::3 ~1min
# albany ny needs to be specifically dealt with -- seems fine?
# all_cities <- c(
#   "Albany/NY"
# )
# SUBSEQENT::4 ~2min
# we need to add tigris info
# all_cities <- unique(paste0(holc_dat@data$city, "/", holc_dat@data$state))

# UPDATE: 8/18/2021 -- fixing certain cities # 10.6 min
# all_cities <- c(
#   "Oakland/CA",
#   "San Diego/CA",
#   "Atlanta/GA",
#   "Dubuque/IA",
#   "Sioux City/IA",
#   "Aurora/IL",
#   "Evansville/IN",
#   "Brookline/MA",
#   "Everett/MA",
#   "Holyoke Chicopee/MA",
#   "Malden/MA",
#   "Melrose/MA",
#   "Milton/MA",
#   "Revere/MA",
#   "Saugus/MA",
#   "Winthrop/MA",
#   "Essex Co./NJ",
#   "Lower Westchester Co./NY",
#   "Queens/NY",
#   "Utica/NY",
#   "Youngstown/OH",
#   "Pawtucket & Central Falls/RI",
#   "Newport News/VA",
#   "Milwaukee Co./WI",
#   "Bronx/NY"
# )

# also load the initial run
load(file.path(data_folder, "Redlining_Full_Preprocess_Info.RData"))

start <- Sys.time()
# redlining_info <- list()
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
  rownames(tct) <- tct$GEOID10
  
  redlining_info[[cts]][["ct"]] <- cbind(
    redlining_info[[cts]][["ct"]],
    tct[as.character(redlining_info[[cts]][["ct"]]$GEOID), 
        c("ALAND10", "AWATER10")]
  )

  # census block ----
  print(paste0("[", Sys.time(), "] Getting census blocks..."))

  # get census block data, but cut it down -- we don't need all of it
  cb <- get_census_block(city, st)

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)

  # preallocate dataframe to store all the tract areas and such
  ct_city <- crop(ct, bbox(holc_sub))
  ct_city <- ct[ct@data$GEOID %in% ct_city@data$GEOID,]

  # check that holc is valid
  holc_valid <- suppressWarnings(gIsValid(holc_sub))
  if (!holc_valid){
    # we need to reproject and fix it
    proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"
    ct_city <- spTransform(ct_city, CRS(proj))
    holc_sub <- spTransform(holc_sub, CRS(proj))
    # then add a 0 width buffer
    holc_sub <- gBuffer(holc_sub, byid=TRUE, width=0)
  }

  ct_city_inter <- intersect(ct_city, holc_sub)

  # subset to only the tracts we know intersect with a holc grade
  cb_city <- cb[cb@data$tractFIPS %in% unique(ct_city_inter$GEOID),]

  if (!holc_valid){
    cb_city <- spTransform(cb_city, CRS(proj))
  }

  redlining_info[[cts]][["cb"]] <- cb_city

  # get census areas and populations ----
  print(paste0("[", Sys.time(), "] Getting census areas and populations..."))

  redlining_info[[cts]][["c_area_pop"]] <-
    tryCatch({
      calc_census_area_pop(city, st, ct, cb)
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

# check for errors
# lapply(redlining_info, function(x){head(x$c_area_pop[,1])})
# lapply(redlining_info, function(x){head(x$holc_pop)})

# save ----

save(list = c("redlining_info"), 
     file = file.path(data_folder, "Redlining_Full_Preprocess_Info.RData"))

# break up the file for easier access ----

load(file.path(data_folder, "Redlining_Full_Preprocess_Info.RData"))

for (cts in 1:length(redlining_info)){
  redlining_city <- redlining_info[[cts]]
  city_name <- names(redlining_info[cts])
  # rename to save as file
  city_name <- gsub("/", "_", city_name)
  
  save(list = c("redlining_city"), 
       file = file.path(
         data_folder, "Redlining", "Redlining_Cities",
         paste0("Redlining_", city_name, "_Preprocess_Info.RData")
       ))
}