# Redlining Support Functions
# By Hannah De los Santos
# Originated on: 2/8/2020

# Note: for ease of reading other scripts.

# overall functions ----

#' function to get the mode of a vector
#' @keywords internal
#' @noRd
Mode <- function(x){
  return(names(table(x))[order(table(x), decreasing = T)][1])
}

#' function to get the county for a given city and state
#' NOTE: need to use numbers
#' @keywords internal
#' @noRd
get_city_county <- function(city,st){
  if (city %in% specific_us_cities$place.name){
    ct_codes <- unique(
      specific_us_cities$county.code[
        specific_us_cities$state.code == st &
          specific_us_cities$place.name == city
      ]
    )
  } else {
    ct_codes <- unique(
      us_cities$county.code[
        us_cities$state.code == st & us_cities$place.name == city
      ]
    )

  }

  return(ct_codes)
}

#' function to get the census tract data for a given city and state
#' @importFrom methods as
#' @keywords internal
#' @noRd
get_census_dat <- function(city,st){
  # cities in two states
  specific_cities <- c(
    "Augusta", "Greater Kansas City", "Dubuque",
    "Evansville", "Pawtucket & Central Falls"
  )

  if (!city %in% specific_cities){
    init_ct <- get_decennial(
      geography = "tract",  state = st, county = get_city_county(city, st),
      year = 2010, geometry = T, variables = c("P001001", "H001001"),
      output = "wide"
    )
  } else {
    # cities in two states
    spec_st <-
      unique(specific_us_cities$st[specific_us_cities$place.name == city])

    init_ct1 <- get_decennial(
      geography = "tract",  state = spec_st[1],
      county = get_city_county(city, spec_st[1]),
      year = 2010, geometry = T, variables = c("P001001", "H001001"),
      output = "wide"
    )
    init_ct2 <- get_decennial(
      geography = "tract",  state = spec_st[2],
      county = get_city_county(city, spec_st[2]),
      year = 2010, geometry = T, variables = c("P001001", "H001001"),
      output = "wide"
    )

    init_ct <- rbind(init_ct1, init_ct2)
  }

  # remove empty geometries since spatial types can't handle them
  ct <- as(
    init_ct[!st_is_empty(init_ct),],
    "Spatial"
  )

  return(ct)
}

#' function to get the census block data for a given city and state
#' @importFrom methods as
#' @keywords internal
#' @noRd
get_census_block <- function(city,st){
  if (!city %in% c("Augusta", "Greater Kansas City")){
    init_cb <- get_decennial(
      geography = "block",  state = st, county = get_city_county(city, st),
      year = 2010, geometry = T, variables = "P001001"
    )
  } else {
    if (city == "Greater Kansas City"){
      # kansas city is in two states
      init_cb1 <- get_decennial(
        geography = "block",  state = "MO",
        county = get_city_county(city, "MO"),
        year = 2010, geometry = T, variables = "P001001"
      )
      init_cb2 <- get_decennial(
        geography = "block",  state = "KS",
        county = get_city_county(city, "KS"),
        year = 2010, geometry = T, variables = "P001001"
      )
    } else {
      # augusta is in two states
      init_cb1 <- get_decennial(
        geography = "block",  state = "GA",
        county = get_city_county(city, "GA"),
        year = 2010, geometry = T, variables = "P001001"
      )
      init_cb2 <- get_decennial(
        geography = "block",  state = "SC",
        county = get_city_county(city, "SC"),
        year = 2010, geometry = T, variables = "P001001"
      )
    }
    init_cb <- rbind(init_cb1, init_cb2)
  }

  cb <- as(
    init_cb[!st_is_empty(init_cb),],
    "Spatial"
  )

  # add the corresponding tract geoid
  cb@data$tractFIPS <- substring(cb@data$GEOID, 1, 11)

  return(cb)
}

# function to get land and water estimates for tigris tracts
get_tigris_tract_info <- function(city, st){
  tct <-tigris::tracts(
    st, county = get_city_county(city, st), year = 2010, cb = F
  )

  # keep the land, water, and geoid
  tct <- data.frame(tct[, c("GEOID10", "ALAND10", "AWATER10")])

  return(tct)
}

#' function to get the area/pop of the census tracts (aiming for efficiency)
#' @importFrom sp CRS
#' @keywords internal
#' @noRd
calc_census_area_pop <- function(city, st, ct, cb){
  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)

  # preallocate dataframe to store all the tract areas and such
  ct_city <- crop(ct, bbox(holc_sub))
  ct_city <- ct[ct@data$GEOID %in% ct_city@data$GEOID,]

  km_div <- 1000*1000 # see ?area for spatialpolygon object

  intr_df <- data.frame(
    "GEOID" = ct_city@data$GEOID,
    "A_area" = rep(0, nrow(ct_city@data)),
    "B_area" = rep(0, nrow(ct_city@data)),
    "C_area" = rep(0, nrow(ct_city@data)),
    "D_area" = rep(0, nrow(ct_city@data)),
    "not_graded_area" = rep(0, nrow(ct_city@data)),
    "total_area" = area(ct_city)/km_div
  )

  # the idea is that intr_df should be populated already if we're adding
  # population
  # subset census blocks to only specific things
  # intersect for ease

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

  # add the census block areas
  cb_city@data$area <- area(cb_city)/(1000*1000) # area in km^2

  # preallocate population metrics
  intr_df <- cbind(
    intr_df,
    data.frame(
      "A_pop" = rep(0, nrow(ct_city@data)),
      "B_pop" = rep(0, nrow(ct_city@data)),
      "C_pop" = rep(0, nrow(ct_city@data)),
      "D_pop" = rep(0, nrow(ct_city@data)),
      "not_graded_pop" = rep(0, nrow(ct_city@data)),
      "total_pop" = rep(0, nrow(ct_city@data))
    )
  )

  # go through all the city data
  for (p in 1:nrow(ct_city@data)){
    tryCatch({
      # assign by area
      # if (!add_pop){
      tmp <- intersect(ct_city[p,], holc_sub)
      for (i in names(holc_points)){
        logi <- tmp@data$holc_grade == i
        if (sum(logi) > 0){
          intr_df[p, paste0(i,"_area")] <- sum(area(tmp[logi,])/km_div)
        }
      }
      # } else {
      # assign by block population
      tmp2 <- intersect(cb_city[cb_city$tractFIPS %in% ct_city$GEOID[p],],holc_sub)
      # add total population
      intr_df$total_pop[p] <- sum(
        cb_city@data$value[cb_city$tractFIPS %in% ct_city$GEOID[p]]
      )

      for (i in names(holc_points)){
        logi <- tmp2@data$holc_grade == i
        if (sum(logi) > 0){
          # what fraction of the block is covered, has that amount of people
          intr_df[p, paste0(i,"_pop")] <- sum(
            (area(tmp2[logi,])/km_div)/tmp2@data$area[logi]*
              tmp2@data$value[logi]
          )
        }
      }
      # }
    },
    error = function(cond){
      # print(paste(p, "no intersection"))
    },
    warning = function(cond){
      # print(paste(p, "no intersection"))
    })
  }

  # add the amounts graded

  # now compute the amount that is not graded
  sum_graded_area <- rowSums(intr_df[,paste0(names(holc_points), "_area")])
  intr_df$not_graded_area <-
    intr_df$total_area - sum_graded_area

  # also compute population not graded
  sum_graded_pop <- rowSums(intr_df[,paste0(names(holc_points), "_pop")])
  intr_df$not_graded_pop <-
    intr_df$total_pop - sum_graded_pop

  return(intr_df)
}

#' function to generate tract assignments based on different methods
#' @importFrom sp CRS
#' @keywords internal
#' @noRd
test_assignment <- function(city, st, ct, cb,
                            in_methods, c_area_pop){
  # subset holc data to the city and state
  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)
  orig_holc_sub <- holc_sub

  # preallocate dataframe to store all the tract areas and such
  ct_city <- crop(ct, bbox(holc_sub))
  ct_city <- ct[ct@data$GEOID %in% ct_city@data$GEOID,]

  km_div <- 1000*1000 # see ?area for spatialpolygon object
  intr_df <- c_area_pop

  if (any(grepl("_pop", in_methods))){
    # intersect for ease
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

    # add the census block areas
    cb_city@data$area <- area(cb_city)/(1000*1000) # area in km^2
  }

  # unweighted centroids ----

  if ("unw_centroid" %in% in_methods){
    unw_centr_pt <- gCentroid(
      ct_city[,],
      byid = T)

    # find out which points fall in holc areas and their grades
    unw_overlap <- over(unw_centr_pt, holc_sub)
    intr_df$unw_centroid <- unw_overlap$holc_grade
    intr_df$unw_centroid_long <- unw_centr_pt@coords[,1]
    intr_df$unw_centroid_lat <- unw_centr_pt@coords[,2]
  }

  # weighted centroids ----
  # note: not available through tidycensus, so I'm not making them available
  # for all states -- just example states
  if ("w_centroid" %in% in_methods){
    # load the data
    # centr_pop <- read.csv(
    #   file.path(
    #     data_folder,"CenPop2010_Means", paste0("CenPop2010_Mean_",st,".csv")
    #   ),
    #   colClasses = c(rep("character",3), rep("numeric",3))
    # )
    # centr_pop$GEOID <- paste0(
    #   centr_pop$STATEFP,
    #   centr_pop$COUNTYFP,
    #   centr_pop$TRACTCE
    # )
    centr_pop_city <- centr_pop[centr_pop$GEOID %in% ct_city@data$GEOID,]

    w_pts <- SpatialPoints(centr_pop_city[,c("LONGITUDE", "LATITUDE")],
                           proj4string = ct@proj4string)
    w_overlap <- over(w_pts, orig_holc_sub)
    rownames(intr_df) <- intr_df$GEOID
    intr_df[as.character(centr_pop_city$GEOID), "w_centroid"] <-
      w_overlap$holc_grade
    intr_df[as.character(centr_pop_city$GEOID), "w_centroid_long"] <-
      w_pts@coords[,1]
    intr_df[as.character(centr_pop_city$GEOID), "w_centroid_lat"] <-
      w_pts@coords[,2]
  }
  # continuous-based measures ----

  if (any(grepl("_pop", in_methods)) |
      any(grepl("_area", in_methods)) |
      any(grepl("weighted", in_methods)) |
      any(grepl("plurality", in_methods)) |
      any(grepl("crossney", in_methods)) |
      any(grepl("ncrc", in_methods)) |
      any(grepl("krieger", in_methods)) |
      any(c("li", "lynch", "lane", "lee", "mujahid") %in% in_methods)
  ){
    # area based continuous ----

    # now compute the amount that is not graded
    sum_graded_area <- rowSums(intr_df[,paste0(names(holc_points), "_area")])
    intr_df$not_graded_area <-
      intr_df$total_area - sum_graded_area

    # first compute weighted score, relative to actual score (ignoring uncategorized)
    if (any(grepl("prop_area", in_methods))){
      intr_df$prop_area <-
        rowSums(sweep(intr_df[,paste0(names(holc_points), "_area")], MARGIN = 2, holc_points, `*`))/sum_graded_area
    }

    # then compute weighted score, w/uncategorized (avg)
    if (any(grepl("weighted_score_w_not_gr", in_methods))){
      holc_points_w_not_graded <- c(holc_points,2.5)
      names(holc_points_w_not_graded) <- c(names(holc_points), "not_graded")

      intr_df$weighted_score_w_not_gr <-
        rowSums(
          sweep(intr_df[,paste0(names(holc_points_w_not_graded), "_area")],
                MARGIN = 2,
                holc_points_w_not_graded,
                `*`))/
        intr_df$total_area
    }

    # then compute score winner takes all (ignoring uncategorized)
    if (any(grepl("plurality_area", in_methods))){
      intr_df$plurality_area <-
        names(holc_points)[
          apply(intr_df[,paste0(names(holc_points), "_area")], 1, which.max)
        ]
    }

    # compute rounding proportion of area
    if (any(grepl("round_area", in_methods))){
      intr_df$round_area <-
        round(rowSums(sweep(intr_df[,paste0(names(holc_points), "_area")], MARGIN = 2, holc_points, `*`))/sum_graded_area)

      # map to the discrete names
      intr_df$round_area <- names(holc_points)[intr_df$round_area]
    }

    # highest graded area, 10% threshold
    if ("li" %in% in_methods){
      intr_df$li <-
        apply(intr_df[,paste0(names(holc_points), "_area")], 1, function(x){
          names(holc_points)[x > 0][1]
        })

      # li has an additional threshold of 10%
      intr_df$li[sum_graded_area/intr_df$total_area < .1] <- NA
    }

    if ("crossney" %in% in_methods){
      # proportion area
      intr_df$crossney <- rowSums(sweep(intr_df[,paste0(names(holc_points), "_area")], MARGIN = 2, holc_points, `*`))/sum_graded_area
      # filter out < 250 people or | < 250 households (adjusted for 2020)
      res_df <- data.frame(
        "GEOID" = ct@data$GEOID,
        "housing_units" = ct@data$H001001,
        "pop" = ct@data$P001001
      )
      rownames(res_df) <- res_df$GEOID

      pop_avail <- res_df[as.character(intr_df$GEOID), "pop"]
      hu_avail <- res_df[as.character(intr_df$GEOID), "housing_units"]

      intr_df$crossney[pop_avail < 250 | hu_avail < 250] <- NA
    }

    if ("ncrc" %in% in_methods){
      # prop area, 20% threshold
      intr_df$ncrc <- rowSums(sweep(intr_df[,paste0(names(holc_points), "_area")], MARGIN = 2, holc_points, `*`))/sum_graded_area

      intr_df$ncrc[sum_graded_area/intr_df$total_area < .2] <- NA
    }

    if ("krieger" %in% in_methods){
      # majority rules, 50% threshold
      intr_df$krieger <-
        names(holc_points)[
          apply(intr_df[,paste0(names(holc_points), "_area")], 1, which.max)
        ]

      # calculate the how much the winning area takes up (subracting water)
      # get water
      water <- ct@data$AWATER10/km_div
      names(water) <- ct@data$GEOID
      area_prop <-
        sapply(1:nrow(intr_df), function(x){
          intr_df[x,paste0(intr_df$krieger[x], "_area")]/(
            intr_df$total_area[x] - water[as.character(intr_df$GEOID[x])])
        })

      # winning area needs at least 50%
      intr_df$krieger[area_prop < .5] <- NA
    }

    if ("lynch" %in% in_methods){
      # total proportion of area, 50% threshold
      intr_df$lynch <-
        rowSums(sweep(
          intr_df[,paste0(names(holc_points), "_area")],
          MARGIN = 2,
          holc_points,
          `*`))/intr_df$total_area

      # needs at least 50%
      intr_df$lynch[sum_graded_area/intr_df$total_area < .5] <- NA
    }

    if ("lane" %in% in_methods){
      # plurality area, 0% threshold
      intr_df$lane <- names(holc_points)[
        apply(intr_df[,paste0(names(holc_points), "_area")], 1, which.max)
      ]

      # needs at least 0%
      intr_df$lane[sum_graded_area == 0] <- NA
    }

    if ("lee" %in% in_methods){
      # plurality area, 50% threshold
      intr_df$lee <- names(holc_points)[
        apply(intr_df[,paste0(names(holc_points), "_area")], 1, which.max)
      ]

      # needs at least 50%
      intr_df$lee[sum_graded_area/intr_df$total_area < .5] <- NA
    }


    if ("mujahid" %in% in_methods){
      # round prop area, 0% threshold
      intr_df$mujahid <-
        round(rowSums(sweep(intr_df[,paste0(names(holc_points), "_area")], MARGIN = 2, holc_points, `*`))/sum_graded_area)

      # map to the discrete names
      intr_df$mujahid <- names(holc_points)[intr_df$mujahid]

      # needs at least 0%
      intr_df$mujahid[sum_graded_area == 0] <- NA
    }

    # if there's no overlap, make them NA
    to_rectify <- c("prop_area",
                    "weighted_score_w_not_gr",
                    "plurality_area",
                    "round_area")
    to_rectify <- to_rectify[to_rectify %in% in_methods]

    intr_df[sum_graded_area == 0, to_rectify] <- NA

    # population based continuous ----

    if (any(grepl("_pop", in_methods))){
      # also compute population not graded
      sum_graded_pop <- rowSums(intr_df[,paste0(names(holc_points), "_pop")])
      intr_df$not_graded_pop <-
        intr_df$total_pop - sum_graded_pop

      # first compute weighted score, relative to actual score
      # (ignoring uncategorized)
      if (any(grepl("prop_pop", in_methods))){
        intr_df$prop_pop <-
          rowSums(sweep(intr_df[,paste0(names(holc_points), "_pop")], MARGIN = 2, holc_points, `*`))/sum_graded_pop
      }

      if (any(grepl("weighted_score_w_not_gr_pop", in_methods))){
        # then compute weighted score, w/uncategorized (avg)
        holc_points_w_not_graded <- c(holc_points,2.5)
        names(holc_points_w_not_graded) <- c(names(holc_points), "not_graded")

        intr_df$weighted_score_w_not_gr_pop <-
          rowSums(
            sweep(intr_df[,paste0(names(holc_points_w_not_graded), "_pop")],
                  MARGIN = 2,
                  holc_points_w_not_graded,
                  `*`))/
          intr_df$total_pop
      }

      if (any(grepl("plurality_pop", in_methods))){
        # then compute score winner takes all (ignoring uncategorized)
        intr_df$plurality_pop <-
          names(holc_points)[
            apply(intr_df[,paste0(names(holc_points), "_pop")], 1, which.max)
          ]
      }

      # compute rounding proportion of area
      if (any(grepl("round_pop", in_methods))){
        intr_df$round_pop <-
          round(rowSums(sweep(intr_df[,paste0(names(holc_points), "_pop")], MARGIN = 2, holc_points, `*`))/sum_graded_pop)

        # map to the discrete names
        intr_df$round_pop <- names(holc_points)[intr_df$round_pop]
      }

      to_rectify <- c("prop_pop",
                      "weighted_score_w_not_gr_pop",
                      "plurality_pop",
                      "round_pop")
      to_rectify <- to_rectify[to_rectify %in% in_methods]

      intr_df[sum_graded_pop == 0, to_rectify] <- NA
    }

  }
  return(intr_df)
}

#' function to threshold non-centroid measures
#' @param intr_df: df with all the base methods
#' @param in_methods: methods that we want to calculate thresholds for, thresholds in
#'   name of method (name is in format type_contribution_amtcutoff)
#' @keywords internal
#' @noRd
add_threshold <- function(intr_df, in_methods){
  # calculate the amount of area/population graded
  sum_graded_area <- intr_df$total_area - intr_df$not_graded_area
  sum_graded_pop <- intr_df$total_pop - intr_df$not_graded_pop

  ms <- in_methods[grepl("thr", in_methods)]
  if (length(ms) > 0){
    for (m in ms){
      spl_name <- strsplit(m, "_")[[1]]

      # add base amount
      intr_df[, m] <-  intr_df[,paste(spl_name[1:2], collapse = "_")]

      thr_amt <- as.numeric(gsub("thr","", spl_name[3]))/100
      if (grepl("area", m)){
        intr_df[sum_graded_area/intr_df$total_area < thr_amt, m] <- NA
      } else {
        # population
        pop_sub <- intr_df$total_pop != 0
        intr_df[pop_sub,][
          sum_graded_pop[pop_sub]/intr_df$total_pop[pop_sub] < thr_amt, m] <- NA
      }
    }
  }

  return(intr_df)
}

#' function to get the width of a city plot, in pixels, based on a ratio of the
#' longitude to latitude (for correct aspect ratios when plotting)
#' @keywords internal
#' @noRd
get_width <- function(city, st, height){
  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  bb <- bbox(holc_sub)

  diff_y <- diff(bb[2,])
  diff_x <- diff(bb[1,])

  return(ceiling(diff_x/diff_y*height))
}

#' function to figure out threshold based on density plot
#' @keywords internal
#' @noRd
automatic_threshold <- function(intr_df, type = "area"){
  # amount graded in each tract
  amt_graded <-
    if (type == "area"){
      (intr_df$total_area - intr_df$not_graded_area)/intr_df$total_area
    } else {
      (intr_df$total_pop - intr_df$not_graded_pop)/intr_df$total_pop
    }
  amt_graded[is.na(amt_graded)] <- 0
  # think about this
  amt_graded <- amt_graded[amt_graded != 0]

  dens_grade <- density(amt_graded)
  x_val <- dens_grade$x
  y_val <- dens_grade$y[x_val >= 0 & x_val <= 1]
  x_val <- x_val[x_val >= 0 & x_val <= 1]

  lead_y <- dplyr::lead(y_val)
  lag_y <- dplyr::lag(y_val)

  # local minima happens when both surrounding y values are greater
  # we're going to choose the first
  thr <- x_val[which(lag_y > y_val & lead_y > y_val)][1]
  if (is.na(thr)){
    thr <- 0
  }

  return(thr)
}

#' assess percent of holc area represented for a given method (cn)
#' include penalty for ungraded area
#' @importFrom sp CRS
#' @keywords internal
#' @noRd
assess_holc_coverage_area <- function(city, st, ct, intr_df, cn,
                                      add_penalty = T, pen_wt = .5,
                                      add_opacity = F){
  if (cn %in% c("w_centroid", "unw_centroid", "crossney", "krieger", "ncrc",
                "li", "lynch", "lane", "lee", "mujahid")){
    add_opacity <- F
  }

  # add opacity depending on the amount graded
  if (add_opacity){
    intr_df$frac_graded <-
      if (!grepl("_pop", cn)){
        (intr_df$total_area - intr_df$not_graded_area)/intr_df$total_area
      } else {
        (intr_df$total_pop - intr_df$not_graded_pop)/intr_df$total_pop
      }

    # accounting for 0/0
    intr_df$frac_graded[is.na(intr_df$frac_graded)] <- 0
  }

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)
  orig_holc_sub <- holc_sub

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

  if (cn == "unw_centroid"){
    # calculate tract centers
    unw_centr_pt <- gCentroid(
      ct_city[,],
      byid = T)

    # find out which points fall in holc areas and their grades
    unw_overlap <- over(unw_centr_pt, holc_sub)

    # now get the areas that overlap
    total_coverage <- sapply(1:nrow(unw_overlap), function(x){
      if (!is.na(unw_overlap$holc_id[x])){
        tmp <- intersect(
          ct_city[x,],
          holc_sub[holc_sub@data$neighborho == unw_overlap$neighborho[x],])

        if (!is.null(tmp)){
          return(area(tmp))
        } else {
          return(0)
        }
      } else {
        return(0)
      }
    })

    if (!add_penalty){
      perc_coverage <- sum(total_coverage)/sum(area(holc_sub))
    } else {
      # get the penalty
      rownames(intr_df) <- intr_df$GEOID
      penalty <-
        intr_df[ct_city@data$GEOID, "not_graded_area"] * pen_wt
      penalty[total_coverage == 0] <- 0

      perc_coverage <-
        # good
        sum(total_coverage)/sum(area(holc_sub)) -
        # penalty -- don't county tracts that weren't included
        sum(penalty)/sum(intr_df$total_area[penalty != 0])
    }
  } else if (cn == "w_centroid") {
    # load the data
    # centr_pop <- read.csv(
    #   file.path(
    #     data_folder,"CenPop2010_Means", paste0("CenPop2010_Mean_",st,".csv")
    #   ),
    #   colClasses = c(rep("character",3), rep("numeric",3))
    # )
    # centr_pop$GEOID <- paste0(
    #   centr_pop$STATEFP,
    #   centr_pop$COUNTYFP,
    #   centr_pop$TRACTCE
    # )
    centr_pop_city <- centr_pop[centr_pop$GEOID %in% ct_city@data$GEOID,]

    w_pts <- SpatialPoints(centr_pop_city[,c("LONGITUDE", "LATITUDE")],
                           proj4string = ct@proj4string)
    w_overlap <- over(w_pts, orig_holc_sub)

    # now get the areas that overlap
    total_coverage <- sapply(1:nrow(w_overlap), function(x){
      if (!is.na(w_overlap$holc_id[x])){
        tmp <- intersect(
          ct_city[ct_city@data$GEOID == centr_pop_city$GEOID[x],],
          holc_sub[holc_sub@data$neighborho == w_overlap$neighborho[x],])

        return(area(tmp))
      } else {
        return(0)
      }
    })

    if (!add_penalty){
      perc_coverage <- sum(total_coverage)/sum(area(holc_sub))
    } else {
      # get the penalty
      rownames(intr_df) <- intr_df$GEOID
      penalty <-
        intr_df[as.character(centr_pop_city$GEOID), "not_graded_area"] * pen_wt
      penalty[total_coverage == 0] <- 0

      perc_coverage <-
        # good
        sum(total_coverage)/sum(area(holc_sub)) -
        # penalty -- don't county tracts that weren't included
        sum(penalty)/sum(intr_df$total_area[penalty != 0])
    }

  } else if (grepl("plurality_", cn) | cn %in% c("krieger", "li", "lane", "lee")){
    # plurality rules chooses the largest area
    sum_graded_area <- sapply(1:nrow(intr_df), function(x){
      if (!is.na(intr_df[x,cn])){
        return(intr_df[x, paste0(intr_df[x,cn], "_area")])
      } else {
        0
      }
    })
    # only include not graded area -- no need to add another penalty for not
    # including area
    penalty <- intr_df$not_graded_area * pen_wt
    penalty[is.na(intr_df[, cn])] <- 0

    # if adding weights, we want to add that as well
    wts <-
      if (add_opacity){
        intr_df$frac_graded/max(intr_df$frac_graded, na.rm = T)
      } else {1}

    if (!add_penalty){
      perc_coverage <- sum(sum_graded_area*wts)/sum(area(holc_sub)/1000/1000)
    } else {
      perc_coverage <-
        # good
        sum(sum_graded_area*wts)/sum(area(holc_sub)/1000/1000) -
        # penalty -- don't county tracts that weren't included
        sum(penalty*wts)/sum(intr_df$total_area[penalty != 0])
    }
  } else {
    # rounded also counts as continuous

    # otherwise it's continuous, taking into account all the values that cross its pass(minus the thresholding)
    sum_graded_area <- rowSums(intr_df[,paste0(names(holc_points), "_area")])
    # also, add a penalty for including ungraded areas
    penalty <- intr_df$not_graded_area * pen_wt
    sum_graded_area[is.na(intr_df[, cn])] <- penalty[is.na(intr_df[, cn])] <- 0

    # if adding weights, we want to add that as well
    wts <-
      if (add_opacity){
        intr_df$frac_graded/max(intr_df$frac_graded, na.rm = T)
      } else {1}

    if (!add_penalty){
      perc_coverage <- sum(sum_graded_area*wts)/sum(area(holc_sub)/1000/1000)
    } else {
      perc_coverage <-
        # good
        sum(sum_graded_area*wts)/sum(area(holc_sub)/1000/1000) -
        # penalty -- don't county tracts that weren't included
        sum(penalty*wts)/sum(intr_df$total_area[penalty != 0])
    }
  }

  return(perc_coverage)
}

#' calculate population in each holc polygon
#' @importFrom sp CRS
#' @keywords internal
#' @noRd
calc_holc_pop <- function(city, st, ct, cb){
  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)

  # preallocate dataframe to store all the tract area/pop and such
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

  # intersect for ease
  ct_city_inter <- intersect(ct_city, holc_sub)

  # subset to only the tracts we know intersect with a holc grade
  cb_city <- cb[cb@data$tractFIPS %in% unique(ct_city_inter$GEOID),]

  if (!holc_valid){
    cb_city <- spTransform(cb_city, CRS(proj))
  }

  # add the census block areas
  cb_city@data$area <- area(cb_city)/(1000*1000) # area in km^2

  # testing

  all_pop <-
    # sapply
    sapply(1:nrow(holc_sub), function(i){
      # only intersect with tracts where we know it will be
      holc_pop <- intersect(
        holc_sub[i,],
        cb_city[cb_city$tractFIPS %in%
                  ct_city_inter$GEOID[ct_city_inter$holc_id ==
                                        holc_sub$holc_id[i]],])

      given_pop <- sum(
        (area(holc_pop)/1000/1000)/
          holc_pop@data[,"area"]*
          holc_pop@data[,"value"]
      )

      return(given_pop)
    })
  # # sapply
  # sapply(1:nrow(holc_sub), function(i){
  #   holc_pop <- intersect(holc_sub[i,], ct_city)
  #
  #   given_pop <- sum(
  #     (area(holc_pop)/1000/1000)/
  #       holc_pop@data[,"area"]*
  #       holc_pop@data[,"value"]
  #   )
  #
  #   return(given_pop)
  # })

  names(all_pop) <- holc_sub$neighborho

  return(all_pop)
}

#' assess percent of holc pop represented for a given method (cn)
#'
#' @importFrom sp CRS
#' @keywords internal
#' @noRd
assess_holc_coverage_pop <- function(city, st, ct, cb, intr_df, all_pop, cn,
                                     add_penalty = T, pen_wt = .5,
                                     add_opacity = F){
  if (cn %in% c("w_centroid", "unw_centroid", "crossney", "krieger", "ncrc",
                "li", "lynch", "lane", "lee", "mujahid")){
    add_opacity <- F
  }

  # add opacity depending on the amount graded
  if (add_opacity){
    intr_df$frac_graded <-
      if (!grepl("_pop", cn)){
        (intr_df$total_area - intr_df$not_graded_area)/intr_df$total_area
      } else {
        (intr_df$total_pop - intr_df$not_graded_pop)/intr_df$total_pop
      }

    # accounting for 0/0
    intr_df$frac_graded[is.na(intr_df$frac_graded)] <- 0
  }

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)
  orig_holc_sub <- holc_sub

  # preallocate dataframe to store all the tract area/pop and such
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

  if (cn == "unw_centroid"){
    # calculate tract centers
    unw_centr_pt <- gCentroid(
      ct_city[,],
      byid = T)

    # find out which points fall in holc areas and their grades
    unw_overlap <- over(unw_centr_pt, holc_sub)

    # now get the populations that overlap
    total_coverage <- sapply(1:nrow(unw_overlap), function(x){
      if (!is.na(unw_overlap$holc_id[x])){
        tmp <- intersect(
          ct_city[x,],
          holc_sub[holc_sub@data$neighborho == unw_overlap$neighborho[x],])


        if (!is.null(tmp)){
          return(
            area(tmp)/
              area(holc_sub[holc_sub@data$neighborho == unw_overlap$neighborho[x],])*
              all_pop[as.character(tmp@data$neighborho[1])])
        } else {
          return(0)
        }

      } else {
        return(0)
      }
    })

    if (!add_penalty){
      perc_coverage <- sum(total_coverage)/sum(all_pop)
    } else {
      # get the penalty
      rownames(intr_df) <- intr_df$GEOID
      penalty <-
        intr_df[ct_city@data$GEOID, "not_graded_pop"] * pen_wt
      penalty[total_coverage == 0] <- 0

      perc_coverage <-
        # good
        sum(total_coverage)/sum(all_pop) -
        # penalty -- don't county tracts that weren't included
        sum(penalty)/sum(intr_df$total_pop[penalty != 0])
    }
  } else if (cn == "w_centroid") {
    # load the data
    # centr_pop <- read.csv(
    #   file.path(
    #     data_folder,"CenPop2010_Means", paste0("CenPop2010_Mean_",st,".csv")
    #   ),
    #   colClasses = c(rep("character",3), rep("numeric",3))
    # )
    # centr_pop$GEOID <- paste0(
    #   centr_pop$STATEFP,
    #   centr_pop$COUNTYFP,
    #   centr_pop$TRACTCE
    # )
    centr_pop_city <- centr_pop[centr_pop$GEOID %in% ct_city@data$GEOID,]

    w_pts <- SpatialPoints(centr_pop_city[,c("LONGITUDE", "LATITUDE")],
                           proj4string = ct@proj4string)
    w_overlap <- over(w_pts, orig_holc_sub)

    # now get the areas that overlap
    total_coverage <- sum(sapply(1:nrow(w_overlap), function(x){
      if (!is.na(w_overlap$holc_id[x])){
        tmp <- intersect(
          ct_city[ct_city@data$GEOID == centr_pop_city$GEOID[x],],
          holc_sub[holc_sub@data$neighborho == w_overlap$neighborho[x],]
        )

        return(
          area(tmp)/
            area(holc_sub[holc_sub@data$neighborho == w_overlap$neighborho[x],])*
            all_pop[as.character(tmp@data$neighborho[1])])
      } else {
        return(0)
      }
    }))

    if (!add_penalty){
      perc_coverage <- sum(total_coverage)/sum(all_pop)
    } else {
      # get the penalty
      rownames(intr_df) <- intr_df$GEOID
      penalty <-
        intr_df[as.character(centr_pop_city$GEOID), "not_graded_pop"] * pen_wt
      penalty[total_coverage == 0] <- 0

      perc_coverage <-
        # good
        sum(total_coverage)/sum(all_pop) -
        # penalty -- don't county tracts that weren't included
        sum(penalty)/sum(intr_df$total_pop[penalty != 0])
    }
  } else if (grepl("plurality_", cn) | cn %in% c("krieger", "li", "lane", "lee")){
    # plurality rules chooses the largest population
    sum_graded_pop <- sapply(1:nrow(intr_df), function(x){
      if (!is.na(intr_df[x,cn])){
        return(intr_df[x, paste0(intr_df[x,cn], "_pop")])
      } else {
        0
      }
    })
    # only include not graded pop -- no need to add another penalty for not
    # including area
    penalty <- intr_df$not_graded_pop * pen_wt
    penalty[is.na(intr_df[, cn])] <- 0

    # if adding weights, we want to add that as well
    wts <- if (add_opacity){
      intr_df$frac_graded/max(intr_df$frac_graded, na.rm = T)
    } else {1}

    if (!add_penalty){
      perc_coverage <- sum(sum_graded_pop*wts)/sum(all_pop)
    } else {
      perc_coverage <-
        # good
        sum(sum_graded_pop*wts)/sum(all_pop) -
        # penalty -- don't count tracts that weren't included
        sum(penalty*wts)/sum(intr_df$total_pop[penalty != 0])
    }
  } else {
    # otherwise it's continuous, taking into account all the values that cross its pass(minus the thresholding)
    sum_graded_pop <- rowSums(intr_df[,paste0(names(holc_points), "_pop")])
    # also, add a penalty for including ungraded areas
    penalty <- intr_df$not_graded_pop * pen_wt
    sum_graded_pop[is.na(intr_df[, cn])] <- penalty[is.na(intr_df[, cn])] <- 0

    # if adding weights, we want to add that as well
    wts <-
      if (add_opacity){
        intr_df$frac_graded/max(intr_df$frac_graded, na.rm = T)
      } else {1}

    if (!add_penalty){
      perc_coverage <- sum(sum_graded_pop*wts)/sum(all_pop)
    } else {
      perc_coverage <-
        # good
        sum(sum_graded_pop*wts)/sum(all_pop) -
        # penalty -- don't county tracts that weren't included
        sum(penalty*wts)/sum(intr_df$total_pop[penalty != 0])
    }
  }

  return(perc_coverage)
}



#' function to calculate a linear model, returns the summary
#' outcome: asthma, mental health, physical health, life expectancy
#' @importFrom stats complete.cases lm
#' @keywords internal
#' @noRd
run_lm <- function(cn, intr_df, outcome = "asthma", add_weights = F){
  if (cn %in% c("w_centroid", "unw_centroid", "crossney", "krieger", "ncrc",
                "li", "lynch", "lane", "lee", "mujahid")){
    add_weights <- F
  }

  weights <- NULL

  if (all(is.na(intr_df[,cn]))){
    return(NA)
  }

  # add the outcome to the dataframe
  # if asthma or mental health, add to data
  intr_df$outcome <-
    if (outcome == "asthma"){
      places_df[intr_df$GEOID, "CASTHMA_CrudePrev"]
    } else if (outcome == "mental health"){
      places_df[intr_df$GEOID, "MHLTH_CrudePrev"]
    } else if (outcome == "physical health"){
      places_df[intr_df$GEOID, "PHLTH_CrudePrev"]
    } else if (outcome == "life expectancy"){
      le_df[intr_df$GEOID, "estimate"]
    }

  # first, get the variable
  m_res <- intr_df[,cn]
  if (is.character(m_res)){
    # convert to numeric
    m_res <- holc_points[m_res]
  }

  # get the outcome
  outcome <- intr_df$outcome[!is.na(m_res)]
  # get weights -- weights should be higher because we're minimizing
  wts <-
    if (add_weights){
      if (grepl("pop", cn)){
        if (!grepl("plurality_", cn) | !grepl("round_", cn)){
          1 - (intr_df$not_graded_pop[!is.na(m_res)]/
                 intr_df$total_pop[!is.na(m_res)])
        } else {
          unlist(sapply(1:nrow(intr_df), function(x){
            intr_df[x,paste0(intr_df[x,cn], "_pop")]/(
              intr_df$total_pop[x])
          }))
        }
      } else {
        if (!grepl("plurality_", cn) | !grepl("round_", cn)){
          1 - (intr_df$not_graded_area[!is.na(m_res)]/
                 intr_df$total_area[!is.na(m_res)])
        } else {
          unlist(sapply(1:nrow(intr_df), function(x){
            intr_df[x,paste0(intr_df[x,cn], "_area")]/(
              intr_df$total_area[x])
          }))
        }
      }
    } else {
      rep(1, length(outcome))
    }
  # remove NAs
  m_res <- m_res[!is.na(m_res)]

  # compute linear model
  ldf <- data.frame("outcome" = outcome,
                    "m_res" = m_res,
                    "weights" = wts)
  ldf <- ldf[complete.cases(ldf),]

  if (nrow(ldf) > 0){
    lin_mod <-
      if (add_weights){
        lm(outcome ~ m_res,
           data = ldf,
           weights = weights)
      } else {
        lm(outcome ~ m_res,
           data = ldf,)
      }

    # add prediction
    ldf$pred <- suppressWarnings(predict(lin_mod, ldf))

    # create annotation data frame for R sq note
    lsum <- summary(lin_mod) # linear model summary

    return(list("ldf" = ldf, "lsum" = lsum))
  } else {
    return(NA)
  }
}
