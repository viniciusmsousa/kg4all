#' covid19 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_covid19_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' covid19 Server Function
#'
#' @noRd 
mod_covid19_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_covid19_ui("covid19_ui_1")
    
## To be copied in the server
# callModule(mod_covid19_server, "covid19_ui_1")
 
