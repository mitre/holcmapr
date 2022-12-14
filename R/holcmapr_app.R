# Exploring Redlining Mapping Methods
# By Hannah De los Santos
# Originated on: 10/16/2020

# Note: Built as a shiny app for ease of use by researchers and other folks.

#' Function to run redlining mapping methodology comparison app.
#'
#' @import shiny broom ggplot2 tidycensus sf reshape2 tigris stringr rhandsontable rgeos raster rgdal
#' @importFrom shinyWidgets materialSwitch
#' @importFrom grid textGrob gpar
#' @importFrom gridExtra grid.arrange
#' @importFrom scales rescale
#' @importFrom sp spTransform SpatialPoints over
#' @importFrom utils combn write.csv
#' @export
#'
#' @examples
#' if (interactive()){
#'   run_holcmapr()
#' }
run_holcmapr <- function(){

  # load data ----

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

  addResourcePath("app_www", system.file("app_www", package = "holcmapr"))

  # UI ----

  ui <- MITREnavbarPage(
    "holcmapr: Compare Redlining Mapping Methods",
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
              "Physical Health" = "ph",
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
            min = 0,
            max = 1
            ),
          actionButton("upd", "Update!"),
          downloadButton("download_dat", "Download Mapping Data (.csv)")
        ),
        # main panel ----
        mainPanel(
          width = 8,
          tabsetPanel(
            tabPanel(
              "City Attributes",
              # HTML("<h3><center>City Attributes: How well do original HOLC neighborhoods match up with current day census boundaries?</center></h3>"),
              uiOutput("city_title2"),
              # add short description
              fluidRow(
                column(
                  width = 4,
                  fluidRow(plotOutput("city_overlay"))
                ),
                column(
                  width = 8,
                  tabsetPanel(
                    tabPanel(
                      "Dominant Grade Percentage",
                      HTML("<h5><i><center><p>Dominant Grade Percentage: how much area or population does the dominant grade take up?</i></center></h5></p>"),
                      fluidRow(
                        column(
                          width = 6,
                          plotOutput("dom_perc_area", height = 300)
                        ),
                        column(
                          width = 6,
                          plotOutput("dom_perc_pop", height = 300)
                        )
                      ),
                      fluidRow(
                        column(
                          width = 6,
                          plotOutput("dom_perc_area_class", height = 300)
                        ),
                        column(
                          width = 6,
                          plotOutput("dom_perc_pop_class", height = 300)
                        )
                      )
                    ),
                    tabPanel(
                      "Area/Population Correlation",
                      HTML("<h5><i><center><p>Area/Population Correlation: How much does a graded area correspond to the currently graded population?</i></center></h5></p>"),
                      fluidRow(
                        plotOutput("graded_scatter", height = 300)
                      ),
                      fluidRow(
                        column(
                          width = 6,
                          plotOutput("area_graded_dens", height = 300)
                        ),
                        column(
                          width = 6,
                          plotOutput("pop_graded_dens", height = 300)
                        )
                      )

                    ),
                    tabPanel(
                      "Summary Table",
                      HTML("<p></p>"),
                      dataTableOutput("city_stats")
                    )
                  )
                )
              ),
            ),
            tabPanel(
              "Map Comparison",
              uiOutput("city_title1"),
              HTML("<center>"),
              uiOutput("assignment_plots"),
              HTML("</center>")
            ),
            tabPanel(
              "HOLC Grade Coverage",
              uiOutput("city_title4"),
              plotOutput("holc_coverage_curr_methods")
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
        column(width = 2),
        column(
          width = 8,
          h2(HTML(
            "<center><p>Welcome to holcmapr!</center></p>"
          )),
          HTML(
            'holcmapr is an R package that provides a Shiny application for implementing and comparing methods of mapping Home Owners\' Loan Corporation (HOLC) redlining map neighborhoods to present-day census tracts for all redlined cities. To learn more about redlining and look through the original HOLC maps, please see the <a href = "https://dsl.richmond.edu/panorama/redlining/" target = "_blank">Mapping Inequality website</a>.<p><p>

            <p>holcmapr compares published methods and logical extensions of
published methods by 3 aspects: method type, contribution, and cutoff in
the following workflow:</p>
<div class="figure">
<center>
<img src="app_www/figures/fig1_flow.png" alt />
</center>
</div>
<p>These can be customized for comparison for census tracts within
holcmapr\'s main application.</p>
<p>Published methods include:</p>
<ul>
<li><p>Population-weighted centroids (<a href="https://pubmed.ncbi.nlm.nih.gov/31999951/">Nardone, et al.
2020</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/35504083/">Mitchell,
et al. 2022</a>)</p></li>
<li><p>Proportion of area, filtered by people and housing units (<a href="https://www.tandfonline.com/doi/abs/10.1080/10511482.2005.9521555">Crossney
and Bartelt 2005</a>)</p></li>
<li><p>Proportion of area, 20% threshold (<a href="https://ncrc.org/holc-health/">NCRC 2020</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/35798451/">Motairek, et al.
2022</a>)</p></li>
<li><p>Plurality of area, 50% threshold, remove water areas (<a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7287548/">Krieger, et
al. 2020</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/32219369/">Krieger, et
al. 2020</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/35603845/">Wright, et
al. 2022</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/35286901/">Li and
Yuan 2022</a>)</p></li>
<li><p>Unweighted area centroids (<a href="https://www.tandfonline.com/doi/full/10.1080/01944363.2020.1759127">Wilson
2020</a>, <a href="https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2791603">Erikson,
et al. 2022</a>, <a href="https://link.springer.com/article/10.1007/s10460-022-10340-3">Shaker,
et al. 2022</a>)</p></li>
<li><p>Highest graded area (<a href="https://pubmed.ncbi.nlm.nih.gov/34178163/">Li and Yuan
2020</a>)</p></li>
<li><p>Total proportion of area, 50% threshold (<a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8099638/">Lynch, et
al. 2021</a>)</p></li>
<li><p>Plurality of area, 0% threshold (<a href="https://pubs.acs.org/doi/full/10.1021/acs.estlett.1c01012">Lane,
et al. 2022</a>)</p></li>
<li><p>Plurality of area, 50% threshold (<a href="https://pubmed.ncbi.nlm.nih.gov/33102679/">Lee, et al.
2022</a>)</p></li>
<li><p>Rounded proportion of area, 0% threshold (<a href="https://www.pnas.org/doi/abs/10.1073/pnas.2110986118">Mujahid, et
al. 2022</a>)</p></li>
<li><p>Proportion of area, 0% threshold (<a href="https://pubmed.ncbi.nlm.nih.gov/35639415/">Linde, et al.
2022</a>)</p></li>
</ul>


          For any questions, comments, or suggestions, please contact the Principal Investigator for this project, Hannah De los Santos (<a href = "mailto:hdelossantos@mitre.org" target = "_blank">hdelossantos@mitre.org</a>).'
          )
        ),
        column(width = 2)
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

    # city attributes
    city_attr <- reactiveValues(
      "dom_perc_area" = c(),
      "dom_perc_pop" = c(),
      "dom_perc_area_class" = c(),
      "dom_perc_pop_class" = c()
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
        ms[ms %in% names(specific_method_map)] <- specific_method_map[names(specific_method_map) %in% ms]

        # filter out methods that do not exist
        ms <- ms[ms %in% unname(methods_avail_analysis)]

        in_methods(unique(c(ms, input$paper_methods)))

        places_out$add_outcome <- input$add_outcome
        places_out$which_outcome <- input$which_outcome

        penalty$add_penalty <- input$add_penalty
        penalty$pen_wt <- input$pen_wt

        incProgress(amount = .2, message = "Getting census tract data")

        # check if the data is already loaded (adding more methods, for example)
        city_load <- paste0(city, "_", st)
        if (!city_load %in% city_cache){
          load(file.path(
            data_folder, "Redlining",
            paste0(gsub(" ", "", city_load), "_Preprocess.RData")
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

        incProgress(amount = .2, message = "Calculating city attributes")

        # calculate dominant percentage values
        dom_perc_res <- calc_dom_perc(intr_df$thr, type = "exact")
        dom_perc_class <- calc_dom_perc(intr_df$thr, type = "class")

        city_attr$dom_perc_area <- dom_perc_res$area
        city_attr$dom_perc_pop <- dom_perc_res$pop
        city_attr$dom_perc_area_class <- dom_perc_class$area
        city_attr$dom_perc_pop_class <- dom_perc_class$pop

        intr_df_main <<- intr_df$thr
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

    # plot city attributes ----

    # table of statistics
    output$city_stats <- renderDataTable({
      if (nrow(intr_df$thr) > 0){
        # calculate rmse
        pop_graded <-
          (intr_df$thr$total_pop - intr_df$thr$not_graded_pop)/intr_df$thr$total_pop
        area_graded <-
          (intr_df$thr$total_area - intr_df$thr$not_graded_area)/
          intr_df$thr$total_area

        # do not compute where both are 0 -- those were never graded anyway
        non_zero <- !(pop_graded == 0 & area_graded == 0)
        non_zero[is.na(non_zero)] <- F
        grade_rmse <- signif(
          sqrt(mean((area_graded[non_zero] - pop_graded[non_zero])^2)),
          4
        )

        map_class <- function(x){
          as.character(base::cut(x, mixed_class,
                                 names(mixed_class)[-1], include.lowest = T))
        }

        res_df <- data.frame(
          "Metric" = c("Fraction Graded RMSE",
                       "Avg. Fraction Graded",
                       "Avg. Dominant Grade Percentage",
                       "Avg. Mixed Class"),
          "Area" = c(grade_rmse,
                     signif(mean(area_graded[area_graded != 0], na.rm = T), 4),
                     signif(mean(city_attr$dom_perc_area[area_graded != 0],
                                 na.rm = T), 4),
                     map_class(mean(city_attr$dom_perc_area[area_graded != 0],
                                    na.rm = T))
          ),
          "Population" = c(grade_rmse,
                           signif(mean(pop_graded[pop_graded != 0], na.rm = T), 4),
                           signif(mean(city_attr$dom_perc_pop[pop_graded != 0],
                                na.rm = T), 4),
                           map_class(mean(city_attr$dom_perc_pop[pop_graded != 0],
                                          na.rm = T))
          )
        )

        res_df
      }

    })

    # plot "exact" density plots
    output$dom_perc_area <- renderPlot({
      plot_dom_perc_dens(city_attr$dom_perc_area, "Area")
    })
    output$dom_perc_pop <- renderPlot({
      plot_dom_perc_dens(city_attr$dom_perc_pop, "Population")
    })

    # plot "mixed class" bar plots
    output$dom_perc_area_class <- renderPlot({
      plot_dom_perc_class(city_attr$dom_perc_area_class, "Area")
    })
    output$dom_perc_pop_class <- renderPlot({
      plot_dom_perc_class(city_attr$dom_perc_area_class, "Population")
    })

    # plot area/pop distributions

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

    # plot map comparison ----

    # census area overlaid on HOLC grades
    output$census_holc_overlay <- output$city_overlay <- renderPlot({
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
}
