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
#' @import visNetwork
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
          # Document View -----------------------------------------------------------
          tabPanel(
            title = "Document View",
            br(),
            uiOutput(outputId = ns("selected_document")) %>% 
              shinycssloaders::withSpinner(color = loader_color),
            br(),
            DT::DTOutput(outputId = ns('document_relations')),
            br(),
            visNetwork::visNetworkOutput(outputId = ns('network_graph'))
          ),
          # entity view ui ----------------------------------------------------------
          tabPanel(
            title = 'Entity View',
            br(),
            uiOutput(outputId = ns("selected_entity")) %>%
              shinycssloaders::withSpinner(color = loader_color),
            br(),
            visNetwork::visNetworkOutput(outputId = ns('entity_view_network_graph'))
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
#' @import visNetwork
#' 
mod_covid19_server <- function(input, output, session){
  ns <- session$ns
  # import data -------------------------------------------------------------
  load('data/kg.rda')
  # Document View Server ----------------------------------------------------
  output$selected_document <- renderUI({
    selectInput(
      inputId = session$ns("selected_document"),
      label = h5("Select a document:"),
      choices = unique(kg$title),
      selected = F,
      width = '100%'
    )
  })
  
  output$document_relations <- DT::renderDT({
    kg %>% 
      filter(
        title%in%input$selected_document
      ) %>% 
      select(title, publish_time,head_name,relation_type,tail_name,head_semantic_descrition,tail_semantic_descrition,relation_count)
  })
  
  output$network_graph <- visNetwork::renderVisNetwork({
    print(input$selected_document)
    kg %>% 
      select(
        head_cui,head_name, head_type
      ) %>%
      rename(
        cui=head_cui,
        name=head_name,
        type=head_type
      ) %>% 
      bind_rows(
        kg %>% 
          select(tail_cui,tail_name,tail_type) %>% 
          rename(
            cui=tail_cui,
            name=tail_name,
            type=tail_type
          )
      ) %>% 
      distinct() %>%
      rename(
        id=cui
      ) %>% 
      # mutate(
      #   id=row_number()
      # ) %>% 
      select(id,name,type) -> nodes
    
    
    kg %>% 
      select(head_cui,tail_cui,relation_type) %>% 
      count(head_cui,tail_cui,relation_type) %>% 
      arrange(desc(n)) %>%
      distinct() %>% 
      rename(
        from=head_cui,
        to=tail_cui,
        type=relation_type,
        count=n
      ) -> edges
    
    kg %>% 
      filter(
        title%in%input$selected_document
      ) %>% 
      select(head_cui) %>% 
      distinct() %>% 
      unlist -> cuis_from_document
    
    (
      edges %>% 
        filter(from%in%cuis_from_document) %>%
        mutate(count=count/100) %>% 
        rename(value=count,label=type)-> arestas
    )

    
    (
      nos <- nodes %>% 
        filter(id%in%c(unique(c(arestas$from,arestas$to)))) %>% 
        rename(group=type,label=name)
    )
    
    
    visNetwork(
      nos, arestas
    ) %>% 
      visGroups(
        # groupname = "Disease or Syndrome", color = "darkblue", shape = "square", 
        # shadow = list(enabled = TRUE)
      ) %>% 
      visLegend() %>% 
      visPhysics(solver = "forceAtlas2Based", 
                 forceAtlas2Based = list(gravitationalConstant = -30)) %>% 
      visIgraphLayout(layout = "layout_in_circle")
    
  })
  
 


  # Entity View -------------------------------------------------------------
  output$selected_entity <- renderUI({
    selectInput(
      inputId = session$ns("selected_entity"),
      label = h5("Select a document:"),
      choices = unique(kg$head_name),
      selected = F,
      width = '100%'
    )
  })
  
  output$entity_view_network_graph <- visNetwork::renderVisNetwork({

    kg %>%
      select(
        head_cui,head_name, head_type
      ) %>%
      rename(
        cui=head_cui,
        name=head_name,
        type=head_type
      ) %>%
      bind_rows(
        kg %>%
          select(tail_cui,tail_name,tail_type) %>%
          rename(
            cui=tail_cui,
            name=tail_name,
            type=tail_type
          )
      ) %>%
      distinct() %>%
      rename(
        id=cui
      ) %>%
      # mutate(
      #   id=row_number()
      # ) %>%
      select(id,name,type) -> nodes


    kg %>%
      select(head_cui,tail_cui,relation_type) %>%
      count(head_cui,tail_cui,relation_type) %>%
      arrange(desc(n)) %>%
      distinct() %>%
      rename(
        from=head_cui,
        to=tail_cui,
        type=relation_type,
        count=n
      ) -> edges

  cui_filter <- kg %>% 
    filter(head_name%in%input$selected_entity) %>% 
    select(head_cui) %>% 
    distinct() %>% 
    unlist()

    (
      edges %>%
        filter(from%in%cui_filter) %>%
        mutate(count=count/100) %>%
        rename(value=count,label=type)-> arestas
    )


    (
      nos <- nodes %>%
        filter(id%in%c(unique(c(arestas$from,arestas$to)))) %>%
        rename(group=type,label=name)
    )


    visNetwork(
      nos, arestas
    ) %>%
      visGroups(
        # groupname = "Disease or Syndrome", color = "darkblue", shape = "square",
        # shadow = list(enabled = TRUE)
      ) %>%
      visLegend() %>%
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -30)) %>%
      visIgraphLayout(layout = "layout_in_circle")




  })

  
  }
    
## To be copied in the UI
# mod_covid19_ui("covid19_ui_1")
    
## To be copied in the server
# callModule(mod_covid19_server, "covid19_ui_1")
 
