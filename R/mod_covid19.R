#' covid19 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr
#' @import tidyr
mod_covid19_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      # Brazil Monitor Sidebar --------------------------------------------------
      sidebarPanel = sidebarPanel(
        width = 3,
        tags$div(
          tags$h3("Sobre"),
          "Esta aplicacao, de codigo aberto, objetiva disponibilizar analises sobre os dados do Covid-19 no Brasil, Estados e Cidades, para ajudar no monitoramento dos dados sobre os casos de ocorrencia e combater a pandemia.
           A aplicacao foi desenvolvida por", tags$a(href="https://www.linkedin.com/in/viniciusmsousa/","Vinicius M. de Sousa"), ", graduado em Ciencias Economicas pela ESAG/UDESC, com a colaboracao do prof. Antonio Heronaldo de Sousa do CCT/UDESC.
           A Reitoria da ", tags$a(href="udesc.br","UDESC "),"disponibilizou toda a infraestrutura de hospedagem da aplicação para a difusao das informacoes de monitoramento na Internet.",
          tags$br()
        )
      ),
      
      # Brazil Monitor Main Pane Tabs -------------------------------------------
      mainPanel(
        width = 9,
        tabsetPanel(
          # State Comparison Over Time ----------------------------------------------
          tabPanel(
            title = "Comparacao entre os Estados",
            br(),
            
          )
        )
      )
    )
 
  )
}
    
#' covid19 Server Function
#'
#' @noRd 
mod_covid19_server <- function(input, output, session){
  ns <- session$ns
  # import data -------------------------------------------------------------
  triplets <- read.csv(
    file = 'dev\\datasets\\2020-09-22-triplets.csv',
    stringsAsFactors = F
  ) %>% 
    as_tibble() %>%
    mutate(
      same_ent=if_else(head_cui==tail_cui,T,F)
    ) %>% 
    filter(same_ent==F) %>% 
    select(-same_ent) 
  
  
  definitons <- read.csv(
    file = 'dev\\datasets\\SRDEF_definitions.csv',
    stringsAsFactors = F,
    header = F,
    sep = '|'
  ) %>% 
    as_tibble()
  
  relations <- read.csv(
    file = 'dev\\datasets\\SRSTRE1_relations.csv',
    stringsAsFactors = F,
    header = F,
    sep = '|',
    col.names = c('head_tui','relation_tui','tail_tui','n')
  ) %>% 
    as_tibble() %>% 
    select(-n)

  # transform data ----------------------------------------------------------
  triplets %>% 
    left_join(relations,by=c('head_tui','tail_tui')) %>% 
    drop_na(relation_tui) %>% 
    left_join(select(definitons,V2,V3),by=c('head_tui'='V2')) %>% 
    rename(head_type=V3) %>% 
    left_join(select(definitons,V2,V3),by=c('tail_tui'='V2')) %>%
    rename(tail_type=V3) %>% 
    left_join(select(definitons,V2,V3),by=c('relation_tui'='V2')) %>%
    rename(relation_type=V3) %>% 
    select(
      head_sha,contains('head'),contains('relation'), contains('tail')
    ) %>% 
    group_by(head_cui,relation_tui,tail_cui) %>% 
    add_count() %>% 
    ungroup() %>% 
    rename(relation_count=n) -> kg
  
  
 
}
    
## To be copied in the UI
# mod_covid19_ui("covid19_ui_1")
    
## To be copied in the server
# callModule(mod_covid19_server, "covid19_ui_1")
 
