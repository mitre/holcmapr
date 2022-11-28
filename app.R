# Exploring Redlining Mapping Methods
# By Hannah De los Santos
# Originated on: 10/16/2020

# Note: Built as a shiny app for ease of use by researchers and other folks.

# load libraries and data ----

library(MITREShiny)
library(rhandsontable)
library(stringr)
library(shinyWidgets)

library(rgdal)
library(broom)
library(ggplot2)
library(raster)
library(rgeos)
library(tidycensus)
library(sf)
library(shiny)
library(reshape2)
library(grid)
library(gridExtra)
library(colorspace)
library(mapproj)
library(tigris)

# load all global variables + data
source("redlining_global_var.R", local = T)

# functions to run code
source("redlining_map_functions.R", local = T)
source("redlining_plot_functions.R", local = T)

# we're going to load each and then cache it
city_cache <- c()
redlining_info <- list()

# preallocate the base methods data frame for input
methods_df <- data.frame(
  Type = c("Proportion of", "Centroid"), 
  Contribution = c("Area", "Population"), 
  Cutoff = c("Threshold", "Centroid"), 
  Cutoff.Amount = c("20%", "N/A"),
  stringsAsFactors = F
)

# testing ----
# pretty_out <- list()
# pretty_out$city <- city <- "Savannah"
# pretty_out$st <- st <- "GA"
# city_load <- paste0(city, "_", st)
# load(file.path(
#   data_folder, "Redlining", "Redlining_Cities",
#   paste0("Redlining_", city_load, "_Preprocess_Info.RData")
# ))
# ct <- redlining_city$ct
# cb <- redlining_city$cb
# 
# in_methods <- function(){return(c("unw_centroid", "w_centroid", "prop_area_20thr","krieger","plurality_area_10thr"))}
# c_area_pop <- redlining_city$c_area_pop
# 
# intr_df <- intr_df_orig <-  test_assignment(city, st, ct, cb,
#                                             in_methods = in_methods(), c_area_pop)
# 
# thr_intr_df <- add_threshold(intr_df_orig, in_methods())
# 
# 
# start <- Sys.time()
# holc_pop <- redlining_city$holc_pop
# end <- Sys.time()
# 
# start <- Sys.time()
# cn <- "prop_area"
# all_thr <- seq(.05, .95, by = .05)
# cover_area <- cover_pop <- c()
# for (t in all_thr){
#   thr_intr_df <- add_threshold(intr_df_orig, thr_area = t)
#   cover_area[length(cover_area)+1] <-
#     assess_holc_coverage_area(city, st, ct, thr_intr_df, cn)
#   cover_pop[length(cover_pop)+1] <- assess_holc_coverage_pop(
#     city, st, ct, cb, thr_intr_df, holc_pop, cn
#   )
# }
# end <- Sys.time()



# shiny app to collect results ----

# UI ----

ui <- MITREnavbarPage(
  "Exploring Redlining Methods",
  # explore tab ----
  tabPanel(
    "Explore",
    sidebarLayout(
      # sidebar with choices ----
      sidebarPanel(
        width = 4,
        selectInput(
          "city_state", 
          "Choose the city you want to visualize:",
          choices = unique(paste0(holc_dat$city,", ", holc_dat$state))
        ),
        HTML("<p><b>Choose the mapping methods you would like to compare: </b>(Examples appear below. To add more methods, right click to add a row. For more information choosing Type, Contribution, and Cutoff, see the about page.)</p>"),
        rHandsontableOutput("methods_tab"),
        HTML("<p></p>"),
        selectInput(
          "paper_methods",
          "Choose any previously published mapping methods you would like to compare:",
          choices = paper_avail,
          multiple = T
        ),
        materialSwitch(
          "add_outcome",
          HTML("<b>Overlay health outcome in map plots?</b>"),
          value = F,
          status = "primary",
        ),
        selectInput(
          "which_outcome",
          "Which health outcome would you like to observe in overlays and linear models?",
          choices = c(
            "Life Expectancy" = "le",
            "Physical Health" = "pe",
            "Mental Health" = "mh"
          ),
          selected = "le"
        ),
        checkboxInput(
          "add_penalty", 
          HTML("<b>Add weighted penalty for including ungraded areas?</b>"),
          value = T
        ),
        numericInput(
          "pen_wt", 
          "Penalty weight (between 0 and 1):", 
          value = .5, 
          step = .1,
          min = 0),
        actionButton("upd", "Update!"),
        downloadButton("download_dat", "Download Mapping Data (.csv)")
      ),
      # main panel ----
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel(
            "Map Comparison",
            uiOutput("city_title1"),
            HTML("<center>"),
            uiOutput("assignment_plots"),
            HTML("</center>")
          ),
          tabPanel(
            "HOLC Grade Correlation",
            uiOutput("city_title2"),
            fluidRow(
              column(
                width = 4,
                plotOutput("all_m_holc_corr", height = 500)
              ),
              column(
                width = 8,
                plotOutput("m_holc_scatter", height = 800)
              )
            )
          ),
          tabPanel(
            "HOLC Area/Population Distributions",
            uiOutput("city_title3"),
            fluidRow(
              column(
                width = 4,
                plotOutput("area_graded_dens")
              ),
              column(
                width = 4,
                plotOutput("pop_graded_dens")
              ),
              column(
                width = 4,
                plotOutput("graded_scatter")
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotOutput("thr_scatter_methods")
              )
            )
          ),
          tabPanel(
            "HOLC Grade Coverage",
            uiOutput("city_title4"),
            plotOutput("holc_coverage_curr_methods"),
            plotOutput("holc_coverage_thr_area"),
            plotOutput("holc_coverage_thr_pop")
          ),
          tabPanel(
            "Map Differences",
            uiOutput("city_title5"),
            HTML("<center>"),
            HTML("Plots below show mapped differences between the selected methods, M1 and M2. White tracts indicate where both methods did not assign a grade; grey indicates no difference between the method grades; orange indicates a higher grade (more redlined) for M1; purple indicates a higher grade (more redlined) for M2. If the method did not assign a grade to a given tract while the other method did, the method without a grade was assigned a 0 value.<p>"),
            uiOutput("map_method_diff"),
            HTML("</center>")
          ),
          tabPanel(
            "Linear Models",
            uiOutput("city_title6"),
            HTML("<center><h3>Predicted Linear Models</h3></center>"),
            uiOutput("lin_mod_ui")
            
          )
        )
      )
    )
  ),
  # about tab ----
  tabPanel(
    "About",
    fluidRow(
      column(width = 4),
      column(
        width = 4,
        h2(HTML(
          "<center>Welcome to the Exploring Redlining Methods App!</center><p>"
        )),
        HTML(
          "This app is still in development! It is a product of the MIP \"Case study for using health equity framework\". For more information on this project and other applications developed for this project, please see our project page <a href = 'https://info.mitre.org/projects/01MSRH21-BA'>here</a>.<p><p>
          
          For any questions, comments, or suggestions, please contact the Co-PI for this project, Hannah De los Santos (<a href = 'mailto:hdelossantos@mitre.org'>hdelossantos@mitre.org</a>)."
        )
      ),
      column(width = 4)
    )
  )
)

# SERVER ----

server <- function(input, output, session) {
  # preallocate reactive values ----
  
  # allocations
  intr_df <- reactiveValues(
    "orig" = data.frame(),
    "thr" = data.frame()
  )
  # census blocks/tracts
  census <- reactiveValues(
    "cb" = data.frame(),
    "ct" = data.frame(),
    "counties" = c()
  )
  c_area_pop <- reactiveVal(data.frame())
  holc_pop <- reactiveVal(c()) # holc area population
  in_methods <- reactiveVal(c())
  # necessary pretty outputs
  pretty_out <- reactiveValues(
    "title" = "",
    "city" = "",
    "st" = ""
  )
  # overall threshold
  over_thr <- reactiveValues(
    "area" = "",
    "pop" = ""
  )
  # opacity
  map_opacity <- reactiveValues(
    "incl" = F,
    "opacity_area" = T 
  )
  # add places
  places_out <- reactiveValues(
    "add_outcome" = F,
    "which_outcome" = "le"
  )
  # penalties for holc coverage
  penalty <- reactiveValues(
    add_penalty = T,
    pen_wt = .5
  )
  
  # render sidebar output ----
  
  output$methods_tab <- renderRHandsontable({
    if (!is.null(input$methods_tab)) {
      DF <- hot_to_r(input$methods_tab)
    } else {
      DF <- methods_df
    }
    
    rhandsontable(DF, rowHeaders = NULL, overflow = "visible") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(col = "Type", type = "dropdown", source = names(type_avail)) %>%
      hot_col(col = "Contribution", type = "dropdown", 
              source = names(contribution_avail)) %>%
      hot_col(col = "Cutoff", type = "dropdown", 
              source = names(cutoff_avail)) %>%
      hot_col(col = "Cutoff.Amount", type = "dropdown", 
              source = names(cutoff_num_avail)) %>%
      hot_context_menu(allowRowEdit = T, allowColEdit = FALSE)
  })
  
  output$download_dat <- downloadHandler(
    filename = function() {
      paste0("Redlining_Map_Results_", pretty_out$city, "_", pretty_out$st, "_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      print("hi")
      write.csv(intr_df$thr, file, row.names = FALSE)
    }
  )
  
  # update on button press ----
  
  # watch when a new city is chosen and the button is pressed
  observeEvent(input$upd, isolate({
    withProgress(message = "Calculating!", {
      # deal with the title and allocate inputs
      pretty_out$title <- input$city_state
      
      if (input$city_state != "Stamford, Darien, and New Canaan, CT"){
        city_state_vect <- strsplit(input$city_state, ", ")[[1]]
        pretty_out$city <- city <- city_state_vect[1]
        pretty_out$st <- st <- city_state_vect[2]
      } else { # commas mess everything up
        pretty_out$city <- city <- "Stamford, Darien, and New Canaan"
        pretty_out$st <- st <- "CT"
      }
      
      # map the methods in from table and specific papers
      hot <- isolate(input$methods_tab)
      m_df <- hot_to_r(hot)
      
      # weighting has no number
      m_df$Cutoff.Amount[m_df$Cutoff == "Weighting"] <- "N/A"
      
      # convert to methods
      ms <- paste0(
        type_avail[m_df$Type], "_",
        contribution_avail[m_df$Contribution], "_",
        cutoff_num_avail[m_df$Cutoff.Amount],
        cutoff_avail[m_df$Cutoff]
      )
      ms[grepl("centroid", ms) & grepl("area", ms)] <- "unw_centroid"
      ms[grepl("centroid", ms) & grepl("pop", ms)] <- "w_centroid"
      
      # now, map to names the ones that match specific methods
      ms[ms %in% names(specific_method_map)] <- specific_method_map[ms %in% names(specific_method_map)]
      
      in_methods(c(ms, input$paper_methods))
      
      places_out$add_outcome <- input$add_outcome
      places_out$which_outcome <- input$which_outcome
      
      penalty$add_penalty <- input$add_penalty
      penalty$pen_wt <- input$pen_wt
      
      incProgress(amount = .2, message = "Getting census tract data")
      
      # check if the data is already loaded (adding more methods, for example)
      city_load <- paste0(city, "_", st)
      if (!city_load %in% city_cache){
        load(file.path(
          data_folder, "Redlining", "Redlining_Cities",
          paste0("Redlining_", city_load, "_Preprocess_Info.RData")
        ))
        
        redlining_info[[city_load]] <<- redlining_city
        city_cache <<- c(city_cache, city_load)
      } else {
        redlining_city <- redlining_info[[city_load]]
      }
      
      census$ct <- redlining_city$ct
      census$counties <- redlining_city$counties
      
      incProgress(amount = .2, message = "Getting census block data")
      
      census$cb <- redlining_city$cb
      c_area_pop(redlining_city$c_area_pop)
      holc_pop(redlining_city$holc_pop)
      
      incProgress(amount = .2, message = "Getting HOLC assignments")
      
      # this gets the base HOLC assignments (no thresholds)
      intr_df$orig <- intr_df$thr <-
        test_assignment(
          pretty_out$city, pretty_out$st, census$ct, census$cb,
          in_methods(), c_area_pop()
        )
      
      print(head(intr_df$thr))
      
      incProgress(amount = .2, message = "Adding thresholds")
      
      intr_df$thr <-
        add_threshold(intr_df$orig, in_methods())
      
      # add the weighting methods, if any -- weights are added in plotting
      if (any(grepl("_wt", in_methods()))){
        ms <- in_methods()[grepl("_wt", in_methods())]
        
        for (m in ms){
          spl_name <- strsplit(m, "_")[[1]]
          
          # add base amount
          intr_df$thr[, m] <-  intr_df$thr[,paste(spl_name[1:2], collapse = "_")]
        }
      }
    })
  }))
  
  # render UI ----
  
  ht <- 300 
  
  output$city_title1 <- output$city_title2 <- 
    output$city_title3 <- output$city_title4 <-
    output$city_title5 <- output$city_title6 <-
    renderUI({
      HTML(paste0("<h3><center>", pretty_out$title, "</center></h3>"))
    })
  
  # generate all the assignment plot UIs
  output$assignment_plots <- renderUI({
    # need at least 1 method
    if (length(in_methods()) > 0){
      tot_plots <- length(in_methods())
      num_row <- ceiling(tot_plots/2)
      num_col <- 3
      
      all_plot_names <- c("census_holc_overlay", in_methods())
      # insert column names
      if (length(all_plot_names) > 3){
        ins_ind <- seq(4, num_row*num_col, by = num_col-1)
        val <- c(all_plot_names, rep("", length(ins_ind)))
        id <- c(seq_along(all_plot_names), ins_ind - .5)
        all_plot_names <- val[order(id)]
      }
      
      plot_output_list <- lapply(1:(length(all_plot_names)), function(pc){
        if (pc == 1){
          column(4, plotOutput(all_plot_names[pc], 
                               height = ht+133))
        } else {
          if ((pc - 1) %% num_col == 0){
            column(4)
          } else {
            column(4, plotOutput(paste0("plot", all_plot_names[pc]), 
                                 height = ht+133))
          }
        }
      })
      
      # now put all the columns in rows
      row_list <- lapply(1:num_row, function(r){
        if (num_col * r > length(plot_output_list)){
          fluidRow(plot_output_list[(num_col*(r-1)+1):length(plot_output_list)])
        } else {
          fluidRow(plot_output_list[(num_col*(r-1)+1):(num_col*r)])
        }
      })
      
      # do.call(tagList, plot_output_list)
      fluidPage(
        row_list
      )
    } else {
      fluidPage()
    }
  })
  
  # generate all the map difference plot UIs
  output$map_method_diff <- renderUI({
    # need at least 2 method
    if (length(in_methods()) > 1){
      poss <- combn(in_methods(), 2)
      tot_plots <- ncol(poss)
      num_row <- ceiling(tot_plots/3)
      num_col <- 3
      
      all_plot_names <- c(paste0("mp", "m", poss[1,], "m", poss[2,]))
      
      plot_output_list <- lapply(1:(length(all_plot_names)), function(pc){
        column(4, plotOutput(all_plot_names[pc], height = ht+133))
      })
      
      # now put all the columns in rows
      row_list <- lapply(1:num_row, function(r){
        if (num_col * r > length(plot_output_list)){
          fluidRow(plot_output_list[(num_col*(r-1)+1):length(plot_output_list)])
        } else {
          fluidRow(plot_output_list[(num_col*(r-1)+1):(num_col*r)])
        }
      })
      
      # do.call(tagList, plot_output_list)
      fluidPage(
        row_list
      )
    } else {
      fluidPage()
    }
  })
  
  # generate all the linear model plot/summary UIs
  output$lin_mod_ui <- renderUI({
    tot_plots <- length(in_methods())
    num_col <- 3
    num_row <- ceiling(tot_plots/num_col)
    
    all_plot_names <- paste0("lin_plot_", in_methods())
    all_summary_names <- paste0("lin_sum_", in_methods())
    
    plot_output_list <- lapply(1:(length(all_plot_names)), function(pc){
      column(
        4, 
        plotOutput(all_plot_names[pc], height = ht+133)#,
        # fluidRow(
        #   style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
        #   verbatimTextOutput(all_summary_names[pc])
        # )
        )
    })
    
    # now put all the columns in rows
    row_list <- lapply(1:num_row, function(r){
      if (num_col * r > length(plot_output_list)){
        fluidRow(plot_output_list[(num_col*(r-1)+1):length(plot_output_list)])
      } else {
        fluidRow(plot_output_list[(num_col*(r-1)+1):(num_col*r)])
      }
    })
    
    # do.call(tagList, plot_output_list)
    fluidPage(
      row_list
    )
  })
  
  # plot map comparison ----
  
  # census area overlaid on HOLC grades
  output$census_holc_overlay <- renderPlot({
    if (nrow(census$ct) > 0){
      plot_census_map_overlay(pretty_out$city, pretty_out$st, census$ct)
    } else {
      ggplot()
    }
  })
  
  # plot all the assignment methods
  for (pc in methods_avail_analysis){
    local({
      my_pc <- pc
      output[[paste0("plot", my_pc)]] <- renderPlot({
        withProgress(message = "Plotting maps", detail = my_pc, {
          if (my_pc %in% in_methods()){
            plot_assignment(
              pretty_out$city, pretty_out$st, census$ct, my_pc, intr_df$thr,
              add_outcome = places_out$add_outcome, 
              which_outcome = places_out$which_outcome
            )
          } else {
            ggplot()+theme_bw()
          }
          
        })
      })
    })
  }
  
  # plot correlation metrics ----
  
  output$all_m_holc_corr <- renderPlot({
    plot_method_correlation(intr_df$thr, in_methods())
  })
  
  output$m_holc_scatter <- renderPlot({
    poss <- combn(1:length(in_methods()), 2)
    p_list <- lapply(1:ncol(poss), function(x){
      plot_graded_scatter(pretty_out$city, pretty_out$st, intr_df$thr, 
                          in_methods()[poss[1,x]], in_methods()[poss[2,x]], F)
    })
    
    grid.arrange(grobs = p_list, nrow = ceiling(ncol(poss)/4), top = textGrob("HOLC Grades", gp=gpar(fontface = "bold")))
  })
  
  # plot area/pop distributions ----
  
  # plot the grading plot
  output$area_graded_dens <- renderPlot({
    plot_graded_distributions(pretty_out$city, pretty_out$st, intr_df$thr, T)
  })
  
  output$pop_graded_dens <- renderPlot({
    plot_graded_distributions(pretty_out$city, pretty_out$st, intr_df$thr, F)
  })
  
  output$graded_scatter <- renderPlot({
    plot_graded_scatter(pretty_out$city, pretty_out$st, intr_df$thr, T)
  })
  
  # we're not doing these for now
  output$thr_scatter_methods <- renderPlot({
    # withProgress(message = "Making threshold scatter plots", {
    #   m_valid <- in_methods()[!grepl("centroid", in_methods())]
    #   
    #   plist <- lapply(m_valid, function(m){
    #     incProgress(detail = m)
    #     
    #     plot_holc_coverage_cont_thr(
    #       pretty_out$city, pretty_out$st,
    #       census$ct, census$cb, intr_df$thr, 
    #       holc_pop(), m
    #     )
    #   })
    # })
    # 
    # if (length(m_valid) > 0){
    #   grid.arrange(
    #     grobs = plist, 
    #     nrow = 1, 
    #     top = textGrob("Percent HOLC Area/Population Covered for Various Thresholds, for Methods not Including Centroids", gp=gpar(fontface = "bold"))
    #   )
    # } else {
    #   grid.arrange(
    #     grobs = list(ggplot()+theme_bw()), 
    #     nrow = 1, 
    #     top = textGrob("Percent HOLC Area/Population Covered for Various Thresholds, for Methods not Including Centroids", gp=gpar(fontface = "bold"))
    #   )
    # }
  })
  
  # plot coverage metrics ----
  
  output$holc_coverage_curr_methods <- renderPlot({
    withProgress(message = "Making HOLC coverage plots for current methods", {
      plot_holc_coverage(
        pretty_out$city, pretty_out$st, 
        census$ct, census$cb, intr_df$thr, 
        holc_pop(), in_methods(),
        penalty$add_penalty, penalty$pen_wt, add_opacity = map_opacity$incl
      )
    })
  })
  
  output$holc_coverage_thr_area <- renderPlot({
    withProgress(message = "Making HOLC coverage plots for thresholds based on area", {
      plot_holc_coverage_thr(
        pretty_out$city, pretty_out$st, 
        census$ct, census$cb, intr_df$orig, holc_pop(), in_methods(),
        type_cover = "area",
        penalty$add_penalty, penalty$pen_wt, add_opacity = map_opacity$incl
      )
    })
  })
  
  output$holc_coverage_thr_pop <- renderPlot({
    withProgress(message = "Making HOLC coverage plots for thresholds based on population", {
      plot_holc_coverage_thr(
        pretty_out$city, pretty_out$st, 
        census$ct, census$cb, intr_df$orig, holc_pop(), in_methods(),
        type_cover = "pop",
        penalty$add_penalty, penalty$pen_wt, add_opacity = map_opacity$incl
      )
    })
  })
  

  # plot map differences ----
  
  # plot all the assignment methods
  # for (pc1 in methods_avail){
  #   for (pc2 in methods_avail){
  #     local({
  #       my_pc1 <- pc1
  #       my_pc2 <- pc2
  #       output[[paste0("mp", "m", my_pc1, "m", my_pc2)]] <- renderPlot({
  #         withProgress(message = "Plotting maps", 
  #                      detail = paste(my_pc1, "-", my_pc2), {
  #           if (my_pc1 %in% in_methods() & my_pc2 %in% in_methods()){
  #             plot_assignment_diff(
  #               pretty_out$city, pretty_out$st, census$ct, 
  #               my_pc1, my_pc2, intr_df$thr
  #             )
  #           } else {
  #             ggplot()+theme_bw()
  #           }
  #           
  #         })
  #       })
  #     })
  #   }
  # }
  
  # plot linear models ----
  
  # plot all the linear model methods (only some will be rendered)
  for (pc in methods_avail_analysis){
      local({
        my_pc <- pc
        # linear models
        output[[paste0("lin_plot_", pc)]] <- renderPlot({
          withProgress(
            message = "Plotting linear models", 
            detail = paste(my_pc), {
              if (my_pc %in% in_methods()){
                plot_lin_mod(my_pc, intr_df$thr, places_out$which_outcome)
              } else {
                ggplot()+theme_bw()
              }
            })
        })
        
        # not using summary output for now
        # # summary output
        # output[[paste0("lin_sum_", pc)]] <- renderPrint({
        #   withProgress(
        #     message = "Creating linear model summaries", 
        #     detail = paste(my_pc), {
        #       if (my_pc %in% in_methods()){
        #         out_lin_mod_summary(my_pc, intr_df$thr, places_out$which_outcome)
        #       } else {
        #         print("")
        #       }
        #     })
        # })
      })
    
  }
}

# RUN ----

shinyApp(ui, server)
