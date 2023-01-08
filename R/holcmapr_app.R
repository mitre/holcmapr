# Exploring Redlining Mapping Methods
# By Hannah De los Santos
# Originated on: 10/16/2020

# Note: Built as a shiny app for ease of use by researchers and other folks.

#' Function to run redlining mapping methodology comparison app.
#'
#' @import shiny broom ggplot2 tidycensus sf reshape2 tigris stringr rhandsontable rgeos raster rgdal shinyBS
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
          bsCollapse(
            multiple = T,
            open = c("Select Methods", "Update and Export"),
            bsCollapsePanel(
              "Select Methods",
              p(HTML('<b>holcmapr</b> lets you implement and compare methods of mapping Home Owners\' Loan Corporation (HOLC) redlining map neighborhoods to present-day census tracts for all redlined cities. To learn more about this application and how to use it, please see the <b>About</b> tab.')),
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
              )
            ),
            bsCollapsePanel(
              "Advanced Options",
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
              )
            ),
            bsCollapsePanel(
              "Update and Export",
              actionButton("upd", "Update!"),
              downloadButton("download_dat", "Download Mapping Data (.csv)")
            )
          )
        ),
        # main panel ----
        mainPanel(
          width = 8,
          uiOutput("city_title2"),
          tabsetPanel(
            tabPanel(
              "City Attributes",
              # HTML("<h3><center>City Attributes: How well do original HOLC neighborhoods match up with current day census boundaries?</center></h3>"),
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
              "Method Map Comparison",
              HTML("<center>"),
              uiOutput("assignment_plots"),
              HTML("</center>")
            ),
            tabPanel(
              "HOLC Grade Coverage",
              plotOutput("holc_coverage_curr_methods", height = 750)
            ),
            tabPanel(
              "Linear Models",
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
      tabsetPanel(
        tabPanel(
          "Welcome",
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              h3(HTML(
                "<center><p>Welcome to holcmapr!</center></p>"
              )),
              hr(),
              HTML(
                'holcmapr is an R package that provides a Shiny application for implementing and comparing methods of mapping Home Owners\' Loan Corporation (HOLC) redlining map neighborhoods to present-day census tracts for all redlined cities. To learn more about redlining and look through the original HOLC maps, please see the <a href = "https://dsl.richmond.edu/panorama/redlining/" target = "_blank">Mapping Inequality website</a>.<p><p>',

                HTML("<p>In order to analyze the relationship between health outcomes and redlining, researchers need to put redlining neighborhoods and evaluation areas (such as census tracts) in the same unit. However, redlining neighborhoods <b>do not always match</b> with census tract areas, depending on city, as we can see below:</p>"),

                HTML('<p><img src="app_www/figures/sfig_redlining_maps_NY.png"></p>'),

                HTML("<p>In A, we have Manhattan, NY, with the original HOLC map on the left and the the redlined neighborhoods superimposed on the census areas on the right. As you can see, they match up pretty well -- those city blocks are still city blocks, not much as changed. But if we move over to a different part of New York City in B, with Staten Island, NY, we can see those neighborhoods really don't match up well anymore. As such, depending on the city you choose, the method you use to rectify these differences really matters.</p>"),

                HTML("<p>That's where <b>holcmapr</b> can help! This app lets you <b>compare methods</b> for geographically mapping those redlining areas <b>for every redlined city in the US</b>. You can <b>build methods from scratch</b>, based on their attributes, or choose from <b>previously published methods</b> (see the  \"Building/Choosing Methods\" tab). You can then compare those mapping methods across several <b>different metrics</b> (see the  \"How to Comapre Methods\" tab), including how much of the redlined area is retained by the method and how well methods perform in univariate linear models. Once you choose a method, you can <b>download the redlining values</b> for every method in your selected city for your future work (see the  \"How to Export Methods\" tab).</p>"),

                HTML("<p>If you use this work in your research, don't forget to <b>cite us</b> (see the \"Contact and Citation\" tab). If you have any questions, please feel free to reach out.</p>"),

                HTML("<p><b>We hope you enjoy comparing redlining mapping methods!</b></p>")

              )
            ),
            column(width = 2)
          )
        ),
        tabPanel(
          "Building/Choosing Comparison Methods",
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              h3("Building/Choosing Comparison Methods"),
              hr(),
              HTML(
                "<p>So you've chosen your redlined city of interest. How can you choose your methods? <b>holcmapr</b> lets you compare methods by building them from scratch and/or choosing from previously published, implemented methods.</p>"
              ),
              HTML('<p><b>holcmapr</b> categorizes compares published methods and logical extensions of published methods by 3 aspects: method type, contribution, and cutoff in the following workflow:</p>'),
              HTML('<p><img src="app_www/figures/fig1_flow.png" height = "400"></p>'),
              HTML('<p>Choosing mapping methods for comparison uses the following order: </p>'),
              HTML(
                '<ol>
            <li><b>Select Geographic Boundaries:</b> In this case, we\'re choosing census tracts.</li>
            <li><b>Select Method Type:</b> Method type refers to whether the method uses discrete or continuous grading. Discrete grading picks one of out of all applicable   HOLC grades and uses that grade for the whole area, while in continuous grading, geographic areas receive an average of the multiple applicable HOLC grades.</li>
            <li><b>Select Method Contribution:</b> Method type refers to whether the method uses area or population to assign grading.</li>
            <li><b>Select Method Cutoff:</b> Method cutoffs refers to whether the method uses thresholding (requiring some minimum amount of the area/population to be initially graded), weighting (applying grading based on the fraction of area or population represented in the area), or centroid (assigning the grade for the area in which the centroid falls).</li>
            </ol>'),
            HTML('<p>Enter those choices in the table on the sidebar in the main app to compare them (right click on a row to add more methods). Once you\'ve gone through the flow chart and entered those attributes for your methods, you\'ll have the methods below:</p>'),
            HTML(
              '<ul>
            <li><b>Proportion of Area:</b> Continuous area method. Assigns points to each grade based on a 4 point scale: A as 1, B as 2, C as 3, and D as 4. The final grade is weighted proportionally by fraction area graded, resulting in a graded value between 1 and 4.</li>
            <li><b>Proportion of Population:</b> Continuous population method. Assigns points to each grade based on a 4 point scale: A as 1, B as 2, C as 3, and D as 4. To weight each grade, it uses the population of the census blocks that fall within each tract via area-based weighting and summation. The final grade is weighted proportionally by the fraction population within each area graded, resulting in a graded value between 1 and 4.</li>
            <li><b>Plurality of Area:</b> Discrete area method. Assigns the final grade as the grade with the largest amount of tract area.</li>
            <li><b>Rounded Proportion of Area:</b> Discrete area method. Takes the result of the proportion of area method and rounds the result to the nearest whole point, mapping these to the original four grades.</li>
            <li><b>Unweighted centroids:</b> Discrete area method. Assign the final grade as the area where the census tract centroid falls into.</li>
            <li><b>Plurality of Population:</b> Discrete population method. Assigns the final grade as the grade with the largest amount of tract population, assigned by census blocks.</li>
            <li><b>Rounded Proportion of Population:</b> Discrete population method. Takes the result of the proportion of population method and rounds the result to the nearest whole point, mapping these to the original four grades.</li>
            <li><b>Population weighted centroids:</b> Discrete population method. Assign the final grade as the area where the population-weighted census tract centroid falls into.</li>
            <li><b>...with X% thresholding:</b> Threshold cutoff method. Requires a minimum percentage of the area or population to be initially graded. Thresholds vary from 0% to 50%, with a 10% step.</li>
            <li><b>...with weighting:</b> Threshold cutoff method. Uses final grades proportionally to the fraction of area or population represented in the tract, adding corresponding weights to models and analysis.</li>
            </ul>'),
            HTML('
            <p>The previously published methods these "from scratch" methodologies are based on are also available for comparison. (If you build a method from scratch that exactly matches a previously published method, the previously published method will be displayed.) Published methods include:</p>
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
            </ul>'
            )
            )
          )

        ),
        tabPanel(
          "How to Compare Methods",
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              h3("How to Compare Methods"),
              hr(),
              HTML("<p>Now that you've decided on your methods, how can you compare and choose between them in <b>holcmapr</b>? <b>holcmapr</b> lets you understand the <b>mapping baseline</b> in your city, <b>compare maps for chosen methods</b>, compare redlined <b>neighborhood coverage</b>, and compare the results of <b>linear health associations</b>.</p>"),

              p(h4("City Attributes")),
              p("Before we even understand which methods do well against each other, we need understand to the baseline of how well those HOLC neighborhoods match with current day census tracts. When we go to this tab, we'll see the following attributes to evaluate it:"),
              HTML(
                "<ul>
                <li><b>Census Tracts with HOLC Grade Overlay:</b> a map of census tracts with original HOLC neighborhoods overlaid.</li>
                <li><b>Dominant Grade Percentage:</b> These graphs let us answer the question, \"How much area or population does the dominant grade take up?\", or \"Are census tracts mostly graded with one grade?\" High values of the dominant grade percentage for each tract mean that this tract is not very mixed, and only has one grade -- the HOLC neiborhood and that tract line up well. These values are displayed in density graphs. We also bin these into qualitative labels for easy interpretation in bar graphs:  [0, 1] as Not Graded; (1, 25] as Very Mixed; (25, 50] as Mixed; (50, 75] as Moderately Mixed; (75, 99] as Not Very Mixed; (99, 100] as Not Mixed.</li>
                <li><b>Area/Population Correlation:</b> These graphs let us answer the question, \"How much does a graded area correspond to the currently graded population??\", or \"Can I use area and population interchangeably in my method choice?\" The top graph displays fraction of each tract's area that is graded versus each tract's population. This is also summarized by the Root Mean Squared Error (RMSE). Lower values of RMSE indicate that they line up perfectly, while higher values indicate that they do not line up at all. Distributions of the fraction of area and population graded within all census tracts for the chosen city are displayed below.</li>
                <li><b>Summary Table:</b> This summary table places all the high level statistics to answer these questions in one table: the fraction graded RMSE, the average fraction graded, the average dominant percentage, and the average mixed class.</li>
                </ul>"
              ),
              p("With all that information, we can now understand how well those HOLC neighborhoods match up to the census tracts, giving us a basis of comparison for our chosen methods."),

              p(h4("Method Map Comparison")),
              p("Now that we understand what we're working with in terms of mapping in our city, we can look at what the census tracts our methods have chosen and how they've graded them in their maps. To the left, you'll see the census tracts with a HOLC grade overlay as a baseline. To the right, you'll see the maps for all the methods you've chosen. Continuous methods will show a continuous color scale between the grades for all tracts. Discete methods will show a set color for each grade. If weighting is chosen, opacity will be added to the map for the fraction of area or population in that tract."),

              p(h4("HOLC Grade Coverage")),
              p(HTML("Now we'll start comparing our methods with different metrics. The first being <b>neighborhood coverage</b>. Neighborhood coverage lets us understand how much of the originally HOLC graded area is included in final analyses by the different redlining methods. We estimate this coverage based on area and population by calculating how much of the originally graded area or population is included by each method’s final set of census tracts. We then add a penalty for ungraded area of tracts to discourage methods that include any tract with graded neighborhood overlap (e.g., area edges pulled from different sources not exactly lining up due to projection effects). The penalty is set to 0.5 by default (and can be adjusted in the advanced options to between [0, 1]). For example, if a method includes a tract that is half graded and does not take any tracts with the remaining half of the graded neighborhood, its area neighborhood coverage would be 50% (graded area) – 50% (ungraded area) * 0.5 (penalty) = 25%.")),
              p("Neighborhood coverage is displayed in a comparison bar graph for both area and population."),

              p(h4("Linear Models")),
              p("Our last set of metrics compares how much method choice influences associations with health related outcomes. To do this, we perform a univariate linear model for each selected method and the health outcome. Health outcomes are life expectancy, physical health, and mental health. In this tab, a scatterplot with linear model fit is displayed for each method. In each of these plots, we can see the resulting R^2 and p-value for the fit. Note that these fits are only for the purpose of comparing methods; this is not a fully adjusted model and should not be used to extrapolate the relationship between redlining and your selected health outcome.")
            )
          )
        ),
        # how to export ----
        tabPanel(
          "How to Export Methods",
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              h3("How to Export Methods"),
              hr(),
              p("Once you've decided the methods you're comparing, you can export out grades for every census tract for every method selected in your city. Clicking \"Download Mapping Data (.csv)\" will download a CSV with the following columns:"),
              HTML(
                "<ul>
            <li><b>GEOID:</b> Census Tract ID</li>
            <li><b>(A,B,C,D)_area:</b> (A,B,C,D) graded area within census tract, in km^2</li>
            <li><b>not_graded_area:</b> Area not assigned to any grade within census tract, in km^2</li>
            <li><b>total_area:</b> Total area in census tract</li>
            <li><b>(A,B,C,D)_pop:</b> (A,B,C,D) graded population within census tract</li>
            <li><b>not_graded_pop:</b> Population not assigned to any grade within census tract</li>
            <li><b>total_pop:</b> Total population in census tract</li>
            <li><b>(built method: [type]_[contribution]_[amount of threshold, if used][cutoff]):</b> Methods built from scratch, that were not previously published. Results for a previously published method. Discrete methods have grades from A to D, continuous methods have grades from 1 to 4 or 0 to 4 (for Lynch, et al.). Not graded tracts appear as empty values.
            <ul>
              <li><b>Type:</b> prop = Proportion of. plurality = Plurality of. round = Rounded Proportion of.</li>
              <li><b>Contibution:</b> area = Area. pop = Population.</li>
              <li><b>Cutoff:</b> thr = Threshold. wt = Weighting.</li>
            </ul></li>
            <li><b>(centroid method: w_centroid, unw_centroid):</b> Centroid method results, if chosen. w_centroid = Population weighted centroid. unw_centroid = Area centroid. Also adds columns for latitude (lat) and longitude (long) of each centroid. Grades appear from A to D.</li>
            <li><b>(previously published method: crossney, ncrc, krieger, li, lynch, lane, lee, mujahid):</b> Results for a previously published method. Discrete methods have grades from A to D, continuous methods have grades from 1 to 4 or 0 to 4 (for Lynch, et al.). Not graded tracts appear as empty values.</li>
            </ul>"
              )
            )
          )

        ),
        tabPanel(
          "Contact and Citation",
          fluidRow(
            column(width = 2),
            column(
              width = 8,
              h3("Contact and Citation"),
              hr(),
              p("If you're using `holcmapr` or any of its content, please cite us and let us know you're using it at opensource@mitre.org. To see citation information for holcmapr, enter the following in the console window:"),
              p(code(
                'citation("holcmapr")'
              )),
              HTML(
                '<p>The data sources used in this package are:</p>
<ul>
<li><a href="https://dsl.richmond.edu/panorama/redlining/">Mapping Inequality</a>: Redlining Spatial Files</li>
<li><a href="https://download.geonames.org/export/zip/">GeoNames Postal Code Files</a>: Mapping cities to counties</li>
<li><a href="https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.2010.html">US Census Bureau Centers of Population (2010)</a>: Population centers for census tracts, for population-weighted centroid method</li>
<li><a href="https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html">USALEEP Life Expectancy</a>: Life expectancy census tract estimates</li>
<li><a href="https://www.cdc.gov/places">CDC Places</a>: Mental health and physical health census tract estimates</li>
</ul>
'
              ),

              p('For any questions, comments, or suggestions, please contact the maintainer for this package, Hannah De los Santos at hdelossantos@mitre.org.')
            )
          )
        )
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

    # render welcome modal ----

    observe({
      showModal(modalDialog(
        easyClose = T,
        title = HTML("<center><b>Welcome to {holcmapr}!</b></center>"),
        fluidRow(
          column(
            width = 12,
            HTML("<p><b>holcmapr</b> is an R package that provides a Shiny application for implementing and comparing methods of mapping Home Owners' Loan Corporation (HOLC) redlining map neighborhoods to present-day census tracts for all redlined cities. To learn more about redlining and look through the original HOLC maps, please see the <a href ='https://dsl.richmond.edu/panorama/redlining/' target = '_blank'>Mapping Inequality website</a>.</p>"),
            HTML("<p>In order to analyze the relationship between health outcomes and redlining, researchers need to put redlining neighborhoods and evaluation areas (such as census tracts) in the same unit. However, redlining neighborhoods <b>do not always match</b> with census tract areas, depending on city.</p>"),
            # HTML('<p><img src="app_www/figures/" height = "300"></p>'),
            img(src = file.path("app_www","figures", "sfig_redlining_maps_NY.png"), align = "center", width = "100%"),
            HTML("<p>This app lets you <b>compare methods</b> for geographically mapping those redlining areas <b>for every redlined city in the US</b>. You can <b>build methods from scratch</b>, based on their attributes, or choose from <b>previously published methods</b>. You can then compare those mapping methods across several <b>different metrics</b>, including how much of the redlined area is retained by the method and how well methods perform in univariate linear models. Once you choose a method, you can <b>download the redlining values</b> for every method in your selected city for your future work.</p>"),
            HTML("<p>For more information on what these methods are, <b>check out the tooltips</b> throughout this application and the <b>About page</b> for more detailed information. If you use this work in your research, don't forget to cite us.</p>"),
            HTML("<p><b>We hope you enjoy comparing redlining mapping methods!</b></p>")
          )
        )

      ))
    })

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
      plot_dom_perc_class(city_attr$dom_perc_pop_class, "Population")
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
