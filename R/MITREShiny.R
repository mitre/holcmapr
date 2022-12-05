# MITREShiny functions
# By Hannah De los Santos, Karen Jiang, and Sarah Ober

# internal ----

#' Add MITRE Copyright to bottom of the shiny app
#'
#' by calling the function as the last argument in the ui section of the shiny app.
#'
#' @return sticky footer at the bottom left of the application window with the MITRE Corporation copyright and year.
#' @importFrom shiny fluidPage navbarPage
#' @author Sarah Ober
#' @author Karen Jiang
#' @author Hannah De los Santos
#' @keywords internal
#' @noRd
#'
MITREfooter <- function(){
  HTML(paste0(
    "<span class = 'copyright-footer'>&copy; ",
    format(Sys.Date(), "%Y"),
    ", The MITRE Corporation</span>"
  ))
}

#' Create a page with MITRE theme styling on top level navigation bar
#'
#' MITRE theme styling includes hyperlinked logo to \url{mitre.org} on blue background following \href{https://comm.mitre.org/strategiccommunications/our-brand/#boilerplate}{MITRE Brand guidelines}.
#'
#' @inheritParams shiny::navbarPage
#' @param ... Additional arguments from \code{\link[shiny]{navbarPage}}
#'
#' @return MITRE logo and styling applied to navigation bar at the top of the app. Logo contains link that redirects to MITRE homepage.
#'
#' @import shiny
#' @author Sarah Ober
#' @author Karen Jiang
#' @author Hannah De los Santos
#' @keywords internal
#' @noRd
#'
#' @seealso \code{\link[shiny]{navbarPage}}
#' @examples
#' if (interactive()){
#'   ui <- MITREnavbarPage(
#'      title = "MITRE Shiny App",
#'      tabPanel(
#'        "Tab 1"
#'      )
#'   )
#'
#'   shinyApp(ui = ui, server = function(input, output, session){})
#' }
MITREnavbarPage <- function(title, ..., windowTitle = title){

  # get MITRE logo and css files from system files
  logo <- system.file(file.path("app_www", "MITRE_logo.png"), package = "holcmapr")
  stylesheet <- system.file(file.path("app_www", "app.css"), package = "holcmapr")

  fluidPage(
    style = "padding:0px; margin:0px",
    navbarPage(
      title=div(
        a(
          href="https://www.mitre.org/",
          img(src= file.path("app_www", "MITRE_logo.png"), height=30),
          target="blank"
        ),
        title
      ),
      theme=file.path("app_www", "app.css"),
      windowTitle = windowTitle,
      ...
    ),
    MITREfooter()
  )
}

