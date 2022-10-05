# Redlining Functions
# By Hannah De los Santos
# Originated on: 2/8/2020

# Note: for ease of reading other scripts.

# plotting functions ----

# function to plot HOLC areas for a given city and state
plot_HOLC_area <- function(city, st){
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

# function to plot HOLC areas with their census map overlay
plot_census_map_overlay <- function(city, st, ct){
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
plot_census_block_pop <- function(cb){
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

# function to plot tracts with their assignment based on a specific method (cn)
plot_assignment <- function(city, st, ct, cn, intr_df,
                            add_asthma = F, add_mental_health = F){
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
  
  # if asthma or mental health, add to data
  if (add_asthma){
    intr_df$asthma <- places_df[intr_df$GEOID, "CASTHMA_CrudePrev"]
  }
  if (add_mental_health){
    intr_df$mental_health <- places_df[intr_df$GEOID, "MHLTH_CrudePrev"]
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
  if (add_asthma | add_mental_health){
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
  if (add_asthma){
    prev <- intr_df[!is.na(intr_df[,cn]), "asthma"]
    pt_lim <- c(floor(min(prev)), ceiling(max(prev)))
    
    p <- p +
      geom_point(
        data = intr_df[!is.na(intr_df[,cn]),], 
        aes(unw_centroid_long, unw_centroid_lat,
            size = asthma, color = asthma)
      )+
      scale_size_continuous("Asthma",
                            limits = pt_lim,
                            range = c(1,7))+
      scale_color_gradient("Asthma",
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
      asthma_cor <- signif(cor(
        cor_val[!is.na(cor_val)], 
        intr_df[!is.na(intr_df[,cn]), "asthma"], 
        use = "complete.obs"
      ),4)
    } else {
      asthma_cor <- signif(cov.wt(
        data.frame(
          cor_val[!is.na(cor_val)], 
          intr_df[!is.na(intr_df[,cn]), "asthma"]
        ),
        wt = intr_df[!is.na(intr_df[,cn]),"frac_graded"],
        cor = T,
      )$cor[1,2], 4)
    }
  }
  
  if (add_mental_health){
    prev <- intr_df[!is.na(intr_df[,cn]), "mental_health"]
    pt_lim <- c(floor(min(prev)), ceiling(max(prev)))
    
    p <- p +
      geom_point(
        data = intr_df[!is.na(intr_df[,cn]),], 
        aes(unw_centroid_long, unw_centroid_lat,
            size = mental_health, color = mental_health)
      )+
      scale_size_continuous("Mental Health",
                            limits = pt_lim,
                            range = c(1,7))+
      scale_color_gradient("Mental Health",
                           limits = pt_lim,
                           low = "#FFFFFF",
                           high = "blue",
                           na.value = NA
      )+
      guides(color=guide_legend(), size = guide_legend())
    
    cor_val <- if (is.character(intr_df[,cn])){
      holc_points[intr_df[,cn]]
    } else {
      intr_df[,cn]
    }
    
    if (!add_opacity){
      mental_health_cor <- signif(cor(
        cor_val[!is.na(cor_val)], 
        intr_df[!is.na(intr_df[,cn]), "mental_health"], 
        use = "complete.obs"
      ), 4)
    } else {
      mental_health_cor <- signif(cov.wt(
        data.frame(
          cor_val[!is.na(cor_val)], 
          intr_df[!is.na(intr_df[,cn]), "mental_health"]
        ),
        wt = intr_df[!is.na(intr_df[,cn]),"frac_graded"],
        cor = T,
      )$cor[1,2], 4)
    }
  }
  
  
  # add the correct subtitle
  p <- p +
    ggtitle(methods_abbrev_analysis[cn],
            subtitle = paste(
              ifelse(add_asthma, paste0("Asthma Cor.: ", asthma_cor), ""),
              ifelse(add_mental_health, 
                     paste0("Mental Health Cor.: ", mental_health_cor),
                     "")
            ))
  
  return(p)
}

# function to plot density plot of amounts graded
plot_graded_distributions <- function(city, st, intr_df, plt_area = T){
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

# function to plot scatter plot of amounts graded (or holc grades for cn1 and 2)
plot_graded_scatter <- function(city, st, intr_df, 
                                cn1 = "", cn2 = "", plt_grade = T){
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
  grade_cor <- signif(
    cor(pop_graded, area_graded, method = "pearson", use = "complete.obs"),
    4
  )
  
  p <- ggplot(s_df[complete.cases(s_df),], aes(Area, Population))+
    geom_abline(slope = 1, intercept = 0, color = "#9c9c9c")+
    geom_point(color = "#9C659C")+
    theme_bw()+
    ggtitle(paste0("Corr.: ", grade_cor), 
            subtitle = paste0(city, ", ", st))+
    NULL
  
  if (!plt_grade){
    p <- p +
      ylab(methods_abbrev[cn1])+
      xlab(methods_abbrev[cn2])
  }
  
  return(p)
}

# function to plot correlation between methods
plot_method_correlation <- function(intr_df, in_methods){
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

# function to plot a bar graph displaying coverage in area and population
plot_holc_coverage <- function(city, st, ct, cb, intr_df, holc_pop, in_methods,
                               add_penalty = T, pen_wt = .5,
                               add_opacity = F){
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
    scale_y_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0)))+
    scale_fill_manual(
      values = c("Area" = "#4287f5", "Population" = "#f54242")
    )+
    ylab("Percent Coverage")+
    xlab("Methods")+
    ggtitle("Percent HOLC Region Covered for Specified Methods and Thresholds")+
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "horizontal")+
    NULL
}

# function to plot holc coverage by threshold, for either area or population
# defaults: tests autogenerated, 20%, and 50%
plot_holc_coverage_thr <- function(
  city, st, ct, cb, intr_df_orig, holc_pop, in_methods,
  type_cover = "area",
  add_penalty = T, pen_wt = .5,
  add_opacity = F
){
  
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
      intr_df_orig, thr_area = thr_amts_area[t], thr_pop = thr_amts_pop[t]
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

# function to plot holc coverage by various thresholds by method,
plot_holc_coverage_cont_thr <- function(
  city, st, ct, cb, intr_df_orig, holc_pop, cn
){
  
  # find coverage for various thresholds (more continuous)
  all_thr <- seq(.05, .95, by = .05)
  cover_area <- cover_pop <- c()
  for (t in all_thr){
    thr_intr_df <- add_threshold(intr_df_orig, thr_area = t)
    
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

# function to plot the differences in methods 
plot_assignment_diff <- function(city, st, ct, cn1, cn2, intr_df){
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

# function to compute and plot a linear model for a given method
plot_lin_mod <- function(cn, intr_df, lin_asthma = T, add_weights = F){
  
  # create annotation data frame for R sq note
  l_mod <- if (lin_asthma){
    run_lm(cn, intr_df, outcome = "asthma", add_weights = add_weights)
  } else {
    run_lm(cn, intr_df, outcome = "mental health", add_weights = add_weights)
  }
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
    ylab(ifelse(lin_asthma, "Asthma Prevalence", "Mental Health Prevalence"))+
    xlab("HOLC Grade")+
    ggtitle(paste0(methods_abbrev[cn]))+
    theme(legend.position = "bottom",
          legend.direction = "horizontal")
  
  return(p)
}

# function to compute and return linear model summary for a given method
out_lin_mod_summary <- function(cn, intr_df, lin_asthma = T){
  # add the outcome to the dataframe
  # if asthma or mental health, add to data
  intr_df$outcome <- 
    if (lin_asthma){
      places_df[intr_df$GEOID, "CASTHMA_CrudePrev"]
    } else {
      places_df[intr_df$GEOID, "MHLTH_CrudePrev"]
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

# overall functions ----

# function to time things
timing <- function(x){
  start <- Sys.time()
  v <- x
  end <- Sys.time()
  
  print(end-start)
  return(v)
}

# function to get the mode of a vector
Mode <- function(x){
  return(names(table(x))[order(table(x), decreasing = T)][1])
}

# function to get the county for a given city and state
# NOTE: need to use numbers
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

# function to get the census tract data for a given city and state
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

# function to get the census block data for a given city and state
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

# function to get the area/pop of the census tracts (aiming for efficiency)
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

# function to generate tract assignments based on different methods
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

# function to threshold non-centroid measures
# intr_df: df with all the base methods
# in_methods: methods that we want to calculate thresholds for, thresholds in
#   name of method (name is in format type_contribution_amtcutoff)
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

# function to get the width of a city plot, in pixels, based on a ratio of the
# longitude to latitude (for correct aspect ratios when plotting)
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

# function to figure out threshold based on density plot
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

# assess percent of holc area represented for a given method (cn)
# include penalty for ungraded area
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

# calculate population in each holc polygon
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

# assess percent of holc pop represented for a given method (cn)
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



# function to calculate a linear model, returns the summary
# outcome: asthma, mental health, physical health, life expectancy
run_lm <- function(cn, intr_df, outcome = "asthma", add_weights = F){
  if (cn %in% c("w_centroid", "unw_centroid", "crossney", "krieger", "ncrc",
                "li", "lynch", "lane", "lee", "mujahid")){
    add_weights <- F
  }
  
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
