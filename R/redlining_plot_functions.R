# Redlining Plotting Functions
# By Hannah De los Santos
# Originated on: 10/5/22


# plotting functions ----

#' function to plot HOLC areas for a given city and state
#' @keywords internal
#' @noRd
plot_HOLC_area <- function(city, st){
  long <- lat <- group <- holc_grade <- NULL

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]

  hd_plot <- tidy(holc_sub, region = "neighborho")
  hd_plot <- base::merge(hd_plot, holc_sub@data, by.x = "id", by.y = "neighborho", all.x = T)

  ggplot()+
    geom_polygon(data = hd_plot, aes(long, lat, group = group, fill = holc_grade),
                 alpha = .7)+
    geom_path(data = hd_plot, aes(long, lat, group = group))+
    scale_fill_manual("HOLC Grades", values = holc_colors)+
    theme_bw()+
    coord_cartesian(xlim = bbox(holc_sub)[1,],
                    ylim = bbox(holc_sub)[2,],
                    expand = F)+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("HOLC Grades",
            subtitle = paste0(city, ", ", st))+
    theme(legend.position = "bottom",
          legend.direction = "horizontal")+
    NULL

}

#' function to plot HOLC areas with their census map overlay
#' @keywords internal
#' @noRd
plot_census_map_overlay <- function(city, st, ct){
  long <- lat <- group <- holc_grade <- NULL

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]

  hd_plot <- tidy(holc_sub, region = "neighborho")
  hd_plot <- base::merge(hd_plot, holc_sub@data, by.x = "id", by.y = "neighborho", all.x = T)

  ct_city <- crop(ct, bbox(holc_sub))
  ct_plot <- tidy(ct_city, region = "GEOID")
  ct_plot <- base::merge(ct_plot, ct_city@data, by.x = "id", by.y = "GEOID",
                         all.x = T)

  ggplot()+
    geom_polygon(data = hd_plot,
                 aes(long, lat, group = group, fill = holc_grade),
                 alpha = .7)+
    geom_path(data = ct_plot, aes(long, lat, group = group), alpha = .7)+
    scale_fill_manual("HOLC Grade", values = holc_colors)+
    theme_bw()+
    coord_quickmap(xlim = bbox(holc_sub)[1,],
                   ylim = bbox(holc_sub)[2,],
                   expand = F)+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Census Tracts with HOLC Grade Overlay",
            subtitle = paste0(city, ", ", st))+
    theme(legend.position = "bottom",
          legend.direction = "horizontal")+
    NULL
}

# DO NOT PLOT -- TAKES A LONG WHILE
#' @keywords internal
#' @noRd
plot_census_block_pop <- function(cb, city, st){
  long <- lat <- group <- value <- NULL

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  cb_city <- crop(cb, bbox(holc_sub))
  cb_plot <- tidy(cb_city, region = "GEOID")
  cb_plot <- base::merge(cb_plot, cb_city@data, by.x = "id", by.y = "GEOID",
                         all.x = T)

  ggplot()+
    geom_polygon(data = cb_plot,
                 aes(long, lat, group = group, fill = (value)),
                 alpha = .7)+
    geom_path(data = cb_plot, aes(long, lat, group = group), alpha = .7)+
    theme_bw()+
    scale_fill_gradient(low = "#cccccc",
                        high = "#ba0000")+
    coord_cartesian(xlim = bbox(holc_sub)[1,],
                    ylim = bbox(holc_sub)[2,],
                    expand = F)+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Census Tracts with HOLC Grade Overlay",
            subtitle = paste0(city, ", ", st))+
    theme(legend.position = "bottom",
          legend.direction = "horizontal")+
    NULL
}

#' function to plot tracts with their assignment based on a specific method (cn)
#' @importFrom stats cor cov.wt complete.cases setNames
#' @keywords internal
#' @noRd
plot_assignment <- function(city, st, ct, cn, intr_df,
                            add_outcome = F, which_outcome = "le"){
  long <- lat <- group <- unw_centroid_long <- unw_centroid_lat <-
    outcome <- NULL

  if (all(is.na(intr_df[,cn]))){
    return(ggplot()+theme_bw())
  }

  add_opacity <- grepl("_wt", cn)

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

  # if adding a health outcome, add to the data
  if (add_outcome){
    intr_df$outcome <- if (which_outcome == "mh"){
      places_df[intr_df$GEOID, "MHLTH_CrudePrev"]
    } else if (which_outcome == "ph"){
      places_df[intr_df$GEOID, "PHLTH_CrudePrev"]
    } else {
      le_df[intr_df$GEOID, "estimate"]
    }
  }

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)

  ct_city <- crop(ct, bbox(holc_sub))
  ct_plot <- tidy(ct_city, region = "GEOID")
  ct_plot <- base::merge(ct_plot, ct_city@data, by.x = "id", by.y = "GEOID",
                         all.x = T)

  # we want to add them as points, so we need the centroids
  if (add_outcome){
    unw_centr_pt <- gCentroid(
      ct_city[,],
      byid = T)

    # find out which points fall in holc areas and their grades
    intr_df$unw_centroid_long <- unw_centr_pt@coords[,1]
    intr_df$unw_centroid_lat <- unw_centr_pt@coords[,2]
  }

  ct_class <- base::merge(ct_plot,
                          intr_df,
                          by.x = "id",
                          by.y = "GEOID",
                          all.x = T,
                          sort = F)
  # merge messes up order for some reason
  ct_class <- ct_class[order(ct_class$id,ct_class$order),]

  # plot classification by unweighted centroid
  p <- ggplot()

  if (add_opacity){
    p <- p +
      geom_polygon(data = ct_class,
                   aes_string("long", "lat", group = "group", fill = cn,
                              alpha = "frac_graded"))+
      if (!grepl("_pop", cn)){
        scale_alpha("Frac. Area Graded", range = c(0,1))
      } else {
        scale_alpha("Frac. Pop. Graded", range = c(0,1))
      }
  } else {
    p <- p +
      geom_polygon(data = ct_class,
                   aes_string("long", "lat", group = "group", fill = cn),
                   alpha = .7)
  }

  p <- p +
    geom_path(data = ct_plot,
              aes(long, lat, group = group))+
    # scale_fill_manual(values = holc_colors)+
    theme_bw()+
    coord_quickmap(xlim = bbox(holc_sub)[1,],
                   ylim = bbox(holc_sub)[2,],
                   expand = F)+
    xlab("Longitude")+
    ylab("Latitude")+
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.spacing.y = unit(.05, 'cm'))+
    NULL



  # fill accordingly
  if (typeof(intr_df[,cn]) == "character"){
    p <- p +
      scale_fill_manual("HOLC Grade",
                        values = holc_colors,
                        na.translate = F)
  } else if (cn == "lynch"){
    # lynch has a special color spectrum, since it kind of includes
    # ungraded areas
    p <- p +
      scale_fill_gradientn("HOLC Grade",
                           colors = holc_colors_lynch,
                           labels = names(holc_colors_lynch),
                           breaks = c(.5, 1:4),
                           values = rescale(c(.5, 1:4)),
                           limits = c(.5,4),
                           na.value = "transparent")
  } else {
    p <- p +
      scale_fill_gradientn("HOLC Grade",
                           colors = holc_colors,
                           labels = c("A","B","C","D"),
                           breaks = c(1:4),
                           limits = c(1,4),
                           na.value = "transparent")
  }

  # if it's a centroid, we want to plot those as well
  if (grepl("centroid", cn)){
    p <- p +
      geom_point(data = intr_df,
                 aes_string(paste0(cn,"_long"), paste0(cn,"_lat")),
                 color = "black")
  }

  # only add these on redlined points
  if (add_outcome){
    prev <- intr_df[!is.na(intr_df[,cn]), "outcome"]
    pt_lim <- c(floor(min(prev)), ceiling(max(prev)))
    formal_name <- setNames(names(outcome_map), outcome_map)[which_outcome]

    data_df <- intr_df[!is.na(intr_df[,cn]),]
    if (add_opacity){
      data_df <- data_df[intr_df[!is.na(intr_df[,cn]),"frac_graded"] != 0, ]
    }

    p <- p +
      geom_point(
        data = intr_df[!is.na(intr_df[,cn]),],
        aes(unw_centroid_long, unw_centroid_lat,
            size = outcome, color = outcome)
      )+
      scale_size_continuous(formal_name,
                            limits = pt_lim,
                            range = c(1,7))+
      scale_color_gradient(formal_name,
                           limits = pt_lim,
                           low = "#FFFFFF",
                           high = "purple",
                           na.value = NA
      )+
      guides(color=guide_legend(), size = guide_legend())

    cor_val <- if (is.character(intr_df[,cn])){
      holc_points[intr_df[,cn]]
    } else {
      intr_df[,cn]
    }

    if (!add_opacity){
      outcome_cor <- signif(cor(
        cor_val[!is.na(cor_val)],
        intr_df[!is.na(intr_df[,cn]), "outcome"],
        use = "complete.obs"
      ),4)
    } else {
      cov_df <- data.frame(
        intr_df$GEOID[!is.na(intr_df[,cn])],
        cor_val[!is.na(cor_val)],
        intr_df[!is.na(intr_df[,cn]), "outcome"]
      )
      cov_df <- cov_df[complete.cases(cov_df),]
      cov_wts <- intr_df[intr_df$GEOID %in% cov_df[,1],"frac_graded"]
      cov_df <- cov_df[,-1]

      outcome_cor <- signif(cov.wt(
        cov_df,
        wt = cov_wts,
        cor = T,
      )$cor[1,2], 4)
    }
  }


  # add the correct subtitle
  p <- p +
    ggtitle(methods_abbrev_analysis[cn],
            subtitle = paste(
              ifelse(add_outcome, paste0(formal_name,
                                         " Correlation: ", outcome_cor), "")
            ))

  return(p)
}

#' function to plot density plot of amounts graded
#' @keywords internal
#' @noRd
plot_graded_distributions <- function(city, st, intr_df, plt_area = T){
  graded <- NULL

  amt_graded <-
    if (plt_area){
      (intr_df$total_area - intr_df$not_graded_area)/intr_df$total_area
    } else {
      (intr_df$total_pop - intr_df$not_graded_pop)/intr_df$total_pop
    }
  amt_graded[is.na(amt_graded)] <- 0
  # think about this
  amt_graded <- amt_graded[amt_graded != 0]

  # updating colors and labels
  xlabel <-
    if (plt_area){"Fraction Area Graded"} else {"Fraction Population Graded"}
  plt_col <-
    if (plt_area){"#4287f5"} else {"#f54242"}

  ggplot(data.frame("graded" = amt_graded),
         aes(graded))+
    geom_density(color = plt_col, fill = plt_col, alpha = .5)+
    theme_bw()+
    scale_y_continuous(expand = expansion(mult =  c(0,0.05)))+
    scale_x_continuous(expand = expansion(mult =  c(0,0)))+
    xlab(xlabel)+
    ggtitle(xlabel,
            subtitle = paste0(city, ", ", st))+
    NULL
}

#' function to plot scatter plot of amounts graded (or holc grades for cn1 and 2)
#' @importFrom stats cor complete.cases
#' @keywords internal
#' @noRd
plot_graded_scatter <- function(city, st, intr_df,
                                cn1 = "", cn2 = "", plt_grade = T){
  Area <- Population <- NULL

  if (plt_grade){
    pop_graded <-
      (intr_df$total_pop - intr_df$not_graded_pop)/intr_df$total_pop
    area_graded <-
      (intr_df$total_area - intr_df$not_graded_area)/intr_df$total_area
  } else {
    holc_df <- intr_df[,c(cn1, cn2)]
    for (nc in 1:ncol(holc_df)){
      if (typeof(holc_df[,nc]) == "character"){
        holc_df[,nc] <- holc_points[holc_df[,nc]]
      }
    }

    pop_graded <- holc_df[, cn1]
    area_graded <- holc_df[, cn2]
  }

  s_title <- if(plt_grade){"Amount Graded"} else {"HOLC Grade"}

  s_df <- data.frame("Area" = area_graded, "Population" = pop_graded)
  # do not compute where both are 0 -- those were never graded anyway
  non_zero <- !(pop_graded == 0 & area_graded == 0)
  non_zero[is.na(non_zero)] <- F

  grade_rmse <- signif(
    sqrt(mean((area_graded[non_zero] - pop_graded[non_zero])^2)),
    4
  )

  p <- ggplot(s_df[complete.cases(s_df),], aes(Area, Population))+
    geom_abline(slope = 1, intercept = 0, color = "#9c9c9c")+
    geom_point(color = "#9C659C")+
    theme_bw()+
    ggtitle(paste0("Root Mean Squared Error (RMSE): ", grade_rmse),
            subtitle = paste0(city, ", ", st))+
    NULL

  if (!plt_grade){
    p <- p +
      ylab(methods_abbrev[cn1])+
      xlab(methods_abbrev[cn2])
  }

  return(p)
}

#' function to plot correlation between methods
#' @importFrom stats cor
#' @keywords internal
#' @noRd
plot_method_correlation <- function(intr_df, in_methods){
  Method.1 <- Method.2 <- Correlation <- NULL

  # get only the final HOLC grades
  holc_df <- intr_df[,in_methods]
  for (nc in 1:ncol(holc_df)){
    if (typeof(holc_df[,nc]) == "character"){
      holc_df[,nc] <- holc_points[holc_df[,nc]]
    }
  }

  corr_df <- cor(holc_df, use = "pairwise.complete.obs")
  corr_df[lower.tri(corr_df)] <- NA

  corr_df <- melt(corr_df)
  colnames(corr_df) <- c("Method.1", "Method.2", "Correlation")
  corr_df$Method.1 <- methods_abbrev[corr_df$Method.1]
  corr_df$Method.2 <- methods_abbrev[corr_df$Method.2]
  corr_df$Correlation <- signif(corr_df$Correlation, 3)

  # make factors for nice ordering
  corr_df$Method.1 <- factor(corr_df$Method.1, levels = unique(corr_df$Method.1))
  corr_df$Method.2 <- factor(corr_df$Method.2, levels = unique(corr_df$Method.2))

  # create correlation heat map
  p <- ggplot(corr_df, aes(Method.1, Method.2,
                           fill = Correlation,
                           label = Correlation))+
    geom_tile()+
    geom_text()+
    scale_fill_gradient2(
      breaks = c(-1,0,1),
      limits = c(-1,1),
      low = "#0571b0", mid = "#f7f7f7", high = "#ca0020")+
    theme_bw()+
    scale_x_discrete(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    ggtitle("Method Correlation")+
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = .5),
          legend.position = "bottom",
          legend.direction = "horizontal",
          text = element_text(size = 15))+
    NULL

  return(p)
}

#' function to plot a bar graph displaying coverage in area and population
#' @keywords internal
#' @noRd
plot_holc_coverage <- function(city, st, ct, cb, intr_df, holc_pop, in_methods,
                               add_penalty = T, pen_wt = .5,
                               add_opacity = F){
  variable <- value <- Type <- NULL

  holc_cover <- data.frame(matrix(0, nrow = 2, ncol = 1+length(in_methods)))
  colnames(holc_cover) <- c("Type", in_methods)
  holc_cover[,1] <- c("Area", "Population")

  for (m in in_methods){
    holc_cover[1,m] <-
      signif(assess_holc_coverage_area(city, st, ct, intr_df, m,
                                       add_penalty, pen_wt, add_opacity), 4)*100
    holc_cover[2,m] <- signif(assess_holc_coverage_pop(
      city, st, ct, cb, intr_df, holc_pop, m,
      add_penalty, pen_wt, add_opacity
    ), 4)*100
  }
  colnames(holc_cover)[-1] <- methods_abbrev[in_methods]

  hc_m <- melt(holc_cover, id.vars = "Type")

  ggplot(hc_m, aes(variable, value, fill = Type, group = Type,
                   label = paste0(value, "%")))+
    geom_text(position = position_dodge(.9), aes(y = value+3))+
    geom_bar(position = position_dodge(.9), stat = "identity", alpha = .8)+
    theme_bw()+
    coord_flip()+
    scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0)))+
    scale_fill_manual(
      values = c("Area" = "#4287f5", "Population" = "#f54242")
    )+
    xlab("Percent Coverage")+
    ylab("Methods")+
    ggtitle("Percent HOLC Region Covered for Specified Methods and Thresholds")+
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "horizontal")+
    NULL
}

#' function to plot holc coverage by threshold, for either area or population
#' defaults: tests autogenerated, 20%, and 50%
#' @importFrom colorspace scale_color_discrete_qualitative
#' @keywords internal
#' @noRd
plot_holc_coverage_thr <- function(
    city, st, ct, cb, intr_df_orig, holc_pop, in_methods,
    type_cover = "area",
    add_penalty = T, pen_wt = .5,
    add_opacity = F
){

  Threshold <- value <- variable <- Type <- NULL

  thr_names <- c("Auto", "pt2", "pt5")

  # get the automatic threshold
  auto_thr_area <- automatic_threshold(intr_df_orig, type = "area")
  auto_thr_pop <- automatic_threshold(intr_df_orig, type = "pop")

  # put in vector for easy cycling
  thr_amts_area <- c(auto_thr_area, .2, .5)
  thr_amts_pop <- c(auto_thr_pop, .2, .5)

  # get the thresholds and assign them to the intr_df
  thr_l <- list()
  for (t in 1:length(thr_names)){
    thr_l[[thr_names[t]]] <- add_threshold(
      intr_df_orig, in_methods
    )
  }

  holc_cover <- data.frame(matrix(0, nrow = length(thr_names), ncol = 1+length(in_methods)))
  colnames(holc_cover) <- c("Type", in_methods)
  rownames(holc_cover) <- holc_cover[,1] <- thr_names

  for (m in in_methods){
    for (t in thr_names){
      holc_cover[t,m] <-
        if (type_cover == "area"){
          signif(assess_holc_coverage_area(city, st, ct, thr_l[[t]], m,
                                           add_penalty, pen_wt, add_opacity), 4)*100
        } else {
          signif(assess_holc_coverage_pop(
            city, st, ct, cb, thr_l[[t]], holc_pop, m,
            add_penalty, pen_wt, add_opacity
          ), 4)*100
        }
    }
  }
  colnames(holc_cover)[-1] <- methods_abbrev[in_methods]

  # reshape for ggplot
  hc_m <- melt(holc_cover, id.vars = "Type")

  # get nice colors
  col_vect <-
    if (type_cover == "area"){
      c("#B3CFFB", "#8EB7F9", "#689FF7")
    } else {
      c("#FBB3B3", "#F98E8E", "#F76868")
    }
  names(col_vect) <- thr_names

  type_to_name <- c(
    "area" = "Area",
    "pop" = "Population"
  )

  ggplot(hc_m, aes(variable, value, fill = Type, group = Type,
                   label = paste0(value, "%")))+
    geom_text(position = position_dodge(.9), aes(y = value+3))+
    geom_bar(position = position_dodge(.9), stat = "identity", alpha = .8)+
    theme_bw()+
    scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0)))+
    scale_fill_manual(
      labels = c(
        paste0(
          "Area: ", signif(thr_amts_area[1], 2),
          if (any(grepl("_pop", in_methods))){
            paste0(", Pop.: ", signif(thr_amts_pop[1], 2))
          }
        ),
        .2,
        .5
      ),
      values = col_vect
    )+
    ylab("Percent Coverage")+
    xlab("Methods")+
    labs(title = paste0("Percent HOLC ", type_to_name[type_cover]," Covered for Various Thresholds"),
         caption = "Different automatic thresholds are applied for area and population measures.")+
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "horizontal")+
    NULL
}

#' function to plot holc coverage by various thresholds by method,
#' @importFrom colorspace scale_color_discrete_qualitative
#' @keywords internal
#' @noRd
plot_holc_coverage_cont_thr <- function(
    city, st, ct, cb, intr_df_orig, holc_pop, cn
){

  Threshold <- value <- variable <- Type <- NULL

  # find coverage for various thresholds (more continuous)
  all_thr <- seq(.05, .95, by = .05)
  cover_area <- cover_pop <- c()
  for (t in all_thr){
    thr_intr_df <- add_threshold(intr_df_orig, cn)

    cover_area[length(cover_area)+1] <-
      assess_holc_coverage_area(city, st, ct, thr_intr_df, cn)
    cover_pop[length(cover_pop)+1] <- assess_holc_coverage_pop(
      city, st, ct, cb, thr_intr_df, holc_pop, cn
    )
  }

  # add lines for estimated thresholds
  # get the automatic threshold
  auto_thr_area <- automatic_threshold(intr_df_orig, type = "area")
  auto_thr_pop <- automatic_threshold(intr_df_orig, type = "pop")

  # put in vector for easy cycling
  thr_amts_area <- c(auto_thr_area, auto_thr_pop, .2, .5)

  # get everything in a df
  thr_df <- data.frame(
    "Threshold" = all_thr,
    "Area" = cover_area,
    "Population" = cover_pop
  )
  thr_m <- melt(thr_df, id.vars = "Threshold")

  # also get a df for the exact thresholds
  thr_line_df <- data.frame(
    "Type" = c(
      paste0("Auto Area: ", signif(thr_amts_area[1],2)),
      paste0("Auto Pop.: ", signif(thr_amts_area[2],2)),
      ".2",
      ".5"),
    "value" = thr_amts_area
  )

  ggplot()+
    geom_point(data = thr_m, aes(Threshold, value, fill = variable), color = "transparent", shape = 21)+
    geom_vline(data = thr_line_df, aes(xintercept = value, color = Type, linetype = Type))+
    theme_bw()+
    scale_fill_manual("", values = c("Area" = "#4287f5", "Population" = "#f54242"))+
    scale_color_discrete_qualitative(name = "", palette = "Cold", nmax = 5)+
    scale_linetype("")+
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "vertical")+
    ylab("Percent HOLC Covered")+
    ggtitle(methods_abbrev[cn])+
    NULL

  # STOP HERE
}

#' function to plot the differences in methods
#' @keywords internal
#' @noRd
plot_assignment_diff <- function(city, st, ct, cn1, cn2, intr_df){
  long <- lat <- group <- diff_cn <- NULL

  if (all(is.na(intr_df[,cn2])) | all(is.na(intr_df[,cn2]))){
    return(ggplot()+theme_bw())
  }

  holc_sub <- holc_dat[
    holc_dat@data$city == city & holc_dat@data$state == st &
      holc_dat@data$holc_grade != "E"
    ,]
  holc_sub <- spTransform(holc_sub, CRSobj = ct@proj4string)

  ct_city <- crop(ct, bbox(holc_sub))
  ct_plot <- tidy(ct_city, region = "GEOID")
  ct_plot <- base::merge(ct_plot, ct_city@data, by.x = "id", by.y = "GEOID",
                         all.x = T)


  # convert assignment types
  if (is.character(intr_df[, cn1])){
    intr_df[, cn1] <- as.numeric(holc_points[intr_df[, cn1]])
  }
  if (is.character(intr_df[, cn2])){
    intr_df[, cn2] <- as.numeric(holc_points[intr_df[, cn2]])
  }
  # convert nas to 0 if at least 1 is not NA
  intr_df[is.na(intr_df[, cn1]) & !is.na(intr_df[, cn2]), cn1] <- 0
  intr_df[is.na(intr_df[, cn2]) & !is.na(intr_df[, cn1]), cn2] <- 0
  # calculate difference
  intr_df$diff_cn <- intr_df[, cn1] - intr_df[, cn2]

  ct_class <- base::merge(ct_plot,
                          intr_df,
                          by.x = "id",
                          by.y = "GEOID",
                          all.x = T,
                          sort = F)
  # merge messes up order for some reason
  ct_class <- ct_class[order(ct_class$id,ct_class$order),]

  # plot classification by unweighted centroid
  p <- ggplot() +
    geom_polygon(data = ct_class,
                 aes_string("long", "lat", group = "group", fill = "diff_cn"),
                 alpha = .7) +
    geom_path(data = ct_class,
              aes(long, lat, group = group, color = diff_cn))+
    # scale_fill_manual(values = holc_colors)+
    theme_bw()+
    coord_quickmap(xlim = bbox(holc_sub)[1,],
                   ylim = bbox(holc_sub)[2,],
                   expand = F)+
    xlab("Longitude")+
    ylab("Latitude")+
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(1.5, "cm"))+
    scale_fill_gradientn("Difference",
                         colors = c("#5e3c99", "#999999", "#e66101"),
                         labels = c("More Redlined M2", "-2","0",
                                    "2", "More Redlined M1"),
                         breaks = c(-4, -2, 0, 2, 4),
                         limits = c(-4,4),
                         na.value = "transparent")+
    scale_color_gradientn("Difference",
                          colors = c("#5e3c99", "#999999", "#e66101"),
                          labels = c("More Redlined M2", "-2","0",
                                     "2", "More Redlined M1"),
                          breaks = c(-4, -2, 0, 2, 4),
                          limits = c(-4,4),
                          na.value = "black")+
    ggtitle(paste0(methods_abbrev[cn1], " (M1) minus \n",
                   methods_abbrev[cn2], " (M2)"))+
    NULL

  return(p)
}

#' function to compute and plot a linear model for a given method
#' inputs:
#'  @param  cn: method to plot linear model for
#'  @param  intr_df: data frame with all methods of interest and associated census tract
#'     values
#'  @param  which_outcome: character for health outcome of interest, "le" (life
#'    expectancy), "mh" (mental health), or "ph" (physical health)
#' @importFrom stats pf
#' @keywords internal
#' @noRd
plot_lin_mod <- function(cn, intr_df, which_outcome = "le"){
  m_res <- pred <- outcome <- xpos <- ypos <- hjustvar <-
    vjustvar <- annotateText <- NULL

  # figure out if weights are used in the method
  add_weights <- grepl("_wt", cn)

  # map outcome to function values
  outcome_to_lm <- c(
    "le" = "life expectancy",
    "mh" = "mental health",
    "ph" = "physical health"
  )
  outcome_to_title <- c(
    "le" = "Life Expectancy (years)",
    "mh" = "Mental Health Prevalence (%)",
    "ph" = "Physical Health Prevalence (%)"
  )

  # create annotation data frame for R sq note
  l_mod <- run_lm(cn, intr_df, outcome = outcome_to_lm[which_outcome],
                  add_weights = add_weights)
  lsum <- l_mod$lsum
  ldf <- l_mod$ldf

  # fstatistic pvalue
  fpval <- pf(lsum$fstatistic[1],lsum$fstatistic[2],lsum$fstatistic[3],lower.tail=FALSE)
  annotations <- data.frame(
    xpos = c(-Inf, -Inf, -Inf),
    ypos =  c(Inf, Inf, Inf),
    annotateText = c(
      paste0('"Adj. "*R^2 == ', signif(lsum$adj.r.squared,2)),
      paste0('"HOLC Grade "*italic(p)*"-value"=="', signif(lsum$coefficients[2,4], 4),'"'),
      paste0('"F-Statistic "*italic(p)*"-value"=="', signif(fpval, 4),'"')
    ),
    hjustvar = c(-.09, -.03, -.035) ,
    vjustvar = c(1.25, 2.75, 3.75))

  # plot result
  p <- ggplot(ldf)+
    geom_line(aes(m_res, pred), size = 1)+
    geom_point(aes(m_res, outcome, color = m_res))+
    geom_text(data = annotations,
              aes(x = xpos, y = ypos,
                  hjust = hjustvar, vjust = vjustvar,
                  label = annotateText), parse = T)+
    scale_color_gradientn("HOLC Grade",
                          colors = holc_colors,
                          labels = c("A","B","C","D"),
                          breaks = c(1:4),
                          limits = c(1,4),
                          na.value = "transparent")+
    theme_bw()+
    ylab(outcome_to_title[which_outcome])+
    xlab("HOLC Grade")+
    ggtitle(paste0(methods_abbrev[cn]))+
    theme(legend.position = "bottom",
          legend.direction = "horizontal")

  return(p)
}

#' function to compute and return linear model summary for a given method
#' inputs:
#'  @param  cn: method to plot linear model for
#'  @param  intr_df: data frame with all methods of interest and associated census tract
#'     values
#'  @param  which_outcome: character for health outcome of interest, "le" (life
#'    expectancy), "mh" (mental health), or "ph" (physical health)
#' @importFrom stats lm
#' @keywords internal
#' @noRd
out_lin_mod_summary <- function(cn, intr_df, which_outcome = "le"){
  # add the outcome to the dataframe
  intr_df$outcome <- if (which_outcome == "mh"){
    places_df[intr_df$GEOID, "MHLTH_CrudePrev"]
  } else if (which_outcome == "ph"){
    places_df[intr_df$GEOID, "PHLTH_CrudePrev"]
  } else {
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
  # remove NAs
  m_res <- m_res[!is.na(m_res)]

  # compute linear model
  ldf <- data.frame("outcome" = outcome, "m_res" = m_res)
  lin_mod <- lm(outcome ~ m_res,
                data = ldf)

  return(summary(lin_mod))
}

# city attributes ----

#' function to plot dominant percentage density
#' @param type "Area" or "Population"
#' @keywords internal
#' @noRd
plot_dom_perc_dens <- function(dom_perc_vect, type = "Area"){
  if (length(dom_perc_vect) == 0){
    return(ggplot()+theme_bw())
  }

  dom_perc <- NULL

  frac.m <- data.frame(
    "dom_perc"  = dom_perc_vect,
    "type" = type
  )
  col_map <- c(
    "Area" = "#4287f5",
    "Population" = "#f54242"
  )

  p <- ggplot(frac.m, aes(dom_perc, fill = type, color = type))+
    geom_density(alpha = .7)+
    theme_bw()+
    scale_fill_manual(values = col_map)+
    scale_color_manual(values = col_map)+
    xlab("Dominant Grade Percentage")+
    ggtitle(paste0("Dominant Grade Percentage: ", type))+
    theme(plot.title = element_text(hjust = .5),
          legend.position = "none")+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = expansion(mult = c(0,.1)))

  return(p)
}

#' function to plot dominant percentage class bar plot
#' @param type "Area" or "Population"
#' @importFrom grDevices colorRampPalette
#' @keywords internal
#' @noRd
plot_dom_perc_class <- function(dom_perc_vect, type = "Area"){
  if (length(dom_perc_vect) == 0){
    return(ggplot()+theme_bw())
  }

  class.m <- data.frame(
    "class" = dom_perc_vect,
    "type" = type
  )
  class.m <- class.m[complete.cases(class.m),]

  col_map <- list(
    "Area" = c("#d3e3fd", "#073e97"),
    "Population" = c("#fdd3d3", "#960808")
  )

  pal <- rev(colorRampPalette(col_map[[type]])(length(mixed_class)))
  names(pal) <- names(mixed_class)

  p <- ggplot(class.m, aes(factor(class, levels = rev(names(mixed_class))),
                      fill = class, color = class))+
    geom_bar(alpha = .7)+
    theme_bw()+
    coord_flip()+
    scale_y_continuous(expand = expansion(mult =  c(0,0.05)))+
    scale_fill_manual("Class", values = pal)+
    scale_color_manual("Class", values = pal)+
    xlab("Mixed Class")+
    ylab("Number of Tracts")+
    ggtitle(paste0("Mixed Class: ", type))+
    # ggtitle(title)+
    theme(plot.title = element_text(hjust = .5),
          text = element_text(size = 15),
          legend.position = "none",
          legend.direction = "horizontal")



  return(p)
}
