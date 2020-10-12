#' covid19 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinycssloaders
#' @import dplyr
#' @import DT
mod_covid19_ui <- function(id){
  ns <- NS(id)
  # UI Parameter ------------------------------------------------------------
  loader_color <- "#3A3F44"
  
  tagList(
    sidebarLayout(
      # sidebar -----------------------------------------------------------------
      sidebarPanel = sidebarPanel(
        width = 0,
        tags$div()
        # tags$div(
        #   tags$h3("About"),
        #   "Esta aplicacao, de codigo aberto, objetiva disponibilizar analises sobre os dados do Covid-19 no Brasil, Estados e Cidades, para ajudar no monitoramento dos dados sobre os casos de ocorrencia e combater a pandemia.
        #    A aplicacao foi desenvolvida por", tags$a(href="https://www.linkedin.com/in/viniciusmsousa/","Vinicius M. de Sousa"), ", graduado em Ciencias Economicas pela ESAG/UDESC, com a colaboracao do prof. Antonio Heronaldo de Sousa do CCT/UDESC.
        #    A Reitoria da ", tags$a(href="udesc.br","UDESC "),"disponibilizou toda a infraestrutura de hospedagem da aplicação para a difusao das informacoes de monitoramento na Internet.",
        #   tags$br()
        # )
      ),
      # Main Pane ---------------------------------------------------------------
      mainPanel(
        width = 12,
        tabsetPanel(
          # triplets View -----------------------------------------------------------
          tabPanel(
            title = 'Triplets View',
            br(),
            DT::DTOutput(outputId = ns('triplets_table'))%>% 
              shinycssloaders::withSpinner(color = loader_color)
          ),
          # Document View -----------------------------------------------------------
          tabPanel(
            title = "Document View",
            br(),
            uiOutput(outputId = ns("selected_document")) %>% 
              shinycssloaders::withSpinner(color = loader_color)
          )
        )
      )
    )
  )
}
    
#' covid19 Server Function
#'
#' @noRd 
#' @import dplyr
#' @import tidyr
#' @import DT
#' 
mod_covid19_server <- function(input, output, session){
  ns <- session$ns
  # import data -------------------------------------------------------------
  load('data/kg.rda')


  # triplets View Server ----------------------------------------------------
  output$triplets_table <- DT::renderDT({
    data = kg
  })

  

  # Document View Server ----------------------------------------------------
  output$selected_document <- renderUI({
    selectInput(
      inputId = session$ns("selected_document"),
      label = h5("Select a document:"),
      choices = unique(kg$title),
      selected = F
    )
  })  
  
  
 
}
    
## To be copied in the UI
# mod_covid19_ui("covid19_ui_1")
    
## To be copied in the server
# callModule(mod_covid19_server, "covid19_ui_1")
 
