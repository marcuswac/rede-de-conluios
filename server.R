#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr, warn.conflicts =  FALSE, quietly = TRUE,verbose = FALSE)
library(htmlwidgets, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shiny, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

source("R/carrega_dados.R")

coparticipacoes <- suppressMessages(carrega_dados_coparticipacoes())
inidoneas <- suppressMessages(carrega_dados_inidoneas_pb())
participantes_stats <- carrega_dados_participantes_stats_com_cnae() %>%
  mutate(idoneidade = if_else(nu_cpfcnpj %in% inidoneas$nu_cpfcnpj, "inidônea",
                              "regular"),
         secao_cnae = if_else(!is.na(DescricaoSecao), DescricaoSecao,
                             "INDEFINIDO")) %>%
  select(1:4, idoneidade, secao_cnae)

function(input, output, session) {

  filtra_dados <- reactive({
    min_frequency <- input$min_frequency
    empresa <- input$empresa
    participantes_filt <- participantes_stats

    # Filtra frequencia minima de coparticipacoes
    coparticipacoes_filt <- coparticipacoes %>%
      mutate(p1 = as.character(p1), p2 = as.character(p2)) %>%
      filter(frequency >= min_frequency)

    # Encontra empresa por CNPJ ou nome
    if (!is.null(input$empresa) && input$empresa != "") {
      participantes_filt <- participantes_filt %>%
        filter(grepl(empresa, str_c(nu_cpfcnpj, toupper(nome)), fixed = TRUE))
    } else {
      # Encontra empresas por seção do CNAE
      if (!is.null(input$secao_cnae) && input$secao_cnae != "") {
        participantes_filt <- participantes_filt %>%
          filter(secao_cnae %in% input$secao_cnae)
      }
      # Encontra empresas inidoneas, se nao tiver filtro por CNPJ ou nome
      if (!is.null(input$filt_inidoneas) && input$filt_inidoneas) {
        participantes_filt <- participantes_filt %>%
          filter(idoneidade != "regular")
      }
    }
    
    # aplica filtro de empresas
    if (nrow(participantes_filt) > 0) {
      coparticipacoes_filt <- coparticipacoes_filt %>%
        filter(p1 %in% participantes_filt$nu_cpfcnpj |
               p2 %in% participantes_filt$nu_cpfcnpj)
    }

    if (nrow(coparticipacoes_filt) > 0) {
      cnpjs_filt <- c(coparticipacoes_filt$p1, coparticipacoes_filt$p2) %>%
        unique() %>%
        sort()
      participantes_filt <- participantes_stats %>%
        filter(nu_cpfcnpj %in% cnpjs_filt)
    }

    participantes_nodes <- participantes_filt %>%
      ungroup() %>%
      mutate(group = ifelse(prop_vencedoras < .25, "perdeu muito", "normal"),
             node_id = paste(nome, " (", nu_cpfcnpj, ") Venceu ",
                             round(prop_vencedoras * n_licitacoes),
                             " de ", n_licitacoes, sep = ""),
            node_size = 30 * prop_vencedoras) %>%
      arrange(desc(idoneidade)) %>%
      as.data.frame()
    
    cnpjs_filt <- participantes_nodes$nu_cpfcnpj

    coparticipacoes_links <- coparticipacoes_filt %>%
      rowwise() %>%
      transmute(source = which(p1 == cnpjs_filt) - 1,
                target = which(p2 == cnpjs_filt) - 1,
                value = frequency / min_frequency)

    # se empresas filtradas nao possuem frequencia minima de coparticipacao,
    # mostrar apenas as empresas filtradas, sem as arestas
    if (nrow(coparticipacoes_links) == 0) {
      coparticipacoes_links <- data.frame(source = 0, target = 0, value = 0)
    } else {
      coparticipacoes_links <- as.data.frame(coparticipacoes_links)
    }

    return(list(participantes_nodes = participantes_nodes,
                coparticipacoes_links = coparticipacoes_links))
  })

  updateSelectizeInput(session, "empresa", choices = participantes_stats,
                       server = TRUE)
  
  secoes_cnae <- participantes_stats$secao_cnae %>% unique() %>% sort()
  updateSelectizeInput(session, "secao_cnae", choices = secoes_cnae,
                       server = TRUE)

  visualiza_conluios <- reactive({
    dados <- filtra_dados()
    forceNetwork(Links = dados$coparticipacoes_links,
                 Nodes = dados$participantes_nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "node_id",
                 Nodesize = "node_size", Group = "idoneidade", legend = TRUE,
                 zoom = TRUE, opacity = 0.8, fontSize = 12,
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                 charge = -20)
  })

  output$conluios_plot <- renderForceNetwork(visualiza_conluios())
}
