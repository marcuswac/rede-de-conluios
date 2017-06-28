#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(htmlwidgets, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shiny, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(DT, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

source("R/carrega_dados.R")

coparticipacoes <- suppressMessages(carrega_dados_coparticipacoes())
inidoneas <- suppressMessages(carrega_dados_inidoneas_pb())
participantes_stats <- carrega_dados_participantes_stats_com_cnae() %>%
  mutate(idoneidade = if_else(nu_cpfcnpj %in% inidoneas$nu_cpfcnpj, "inidônea",
                              "regular"),
         secao_cnae = if_else(!is.na(DescricaoSecao), DescricaoSecao,
                             "INDEFINIDA")) %>%
  select(1:4, idoneidade, secao_cnae)

function(input, output, session) {

  reactive_values <- reactiveValues(participante_cnpj = NULL)

  filtra_dados <- reactive({
    min_frequency <- input$min_frequency
    nome_cnpj <- input$empresa_filt
    participantes_filt <- participantes_stats

    # Filtra frequencia minima de coparticipacoes
    coparticipacoes_filt <- coparticipacoes %>%
      mutate(p1 = as.character(p1), p2 = as.character(p2)) %>%
      filter(frequency >= min_frequency)

    # Encontra empresa por CNPJ ou nome
    if (!is.null(nome_cnpj) && nome_cnpj != "") {
      participantes_filt <- participantes_filt %>%
        filter(grepl(nome_cnpj, str_c(nu_cpfcnpj, toupper(nome)), fixed = TRUE))
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
      mutate(prop_vencedoras = n_vencedora / n_licitacoes,
             group = ifelse(prop_vencedoras < .25, "perdeu muito", "normal"),
             node_id = paste(nome, " (", nu_cpfcnpj, ") Venceu ", n_vencedora,
                             " de ", n_licitacoes, sep = ""),
            node_size = 50 * prop_vencedoras) %>%
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

  updateSelectizeInput(session, "empresa_filt", choices = participantes_stats,
                       server = TRUE)

  secoes_cnae <- participantes_stats$secao_cnae %>% unique() %>% sort()
  updateSelectizeInput(session, "secao_cnae", choices = secoes_cnae,
                       server = TRUE)

  visualiza_conluios <- reactive({
    dados <- filtra_dados()
    notify_node_clicked <- 'Shiny.onInputChange("node_clicked", d.name)'

    forceNetwork(Links = dados$coparticipacoes_links,
                 Nodes = dados$participantes_nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "nome",
                 Nodesize = "node_size", Group = "idoneidade", legend = TRUE,
                 zoom = TRUE, opacity = 0.8, fontSize = 12,
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                 charge = -15, clickAction = notify_node_clicked,
                 linkDistance = 50)
  })

  output$conluios_plot <- renderForceNetwork(visualiza_conluios())

  observeEvent(input$node_clicked, {
    participante_nome <- input$node_clicked
    participante <- participantes_stats %>%
      filter(nome == participante_nome)
    reactive_values$participante_cnpj <- participante$nu_cpfcnpj

    updateSelectizeInput(session, "empresa_filt",
                         selected = participante,
                         choices = participantes_stats, server = TRUE)
    updateTabsetPanel(session, "conluios_tabs", selected = "conluios_info_tab")
  })

  observeEvent(input$empresa_filt, {
    reactive_values$participante_cnpj <- input$empresa_filt
  })

  output$participante_info <- renderUI({
    participante_cnpj <- reactive_values$participante_cnpj
    if (is.null(participante_cnpj) || participante_cnpj == "") {
      return()
    }
    participante <- participantes_stats %>%
      filter(nu_cpfcnpj == participante_cnpj)

    div(id = "div_cnpj_info",
        h4("Informações do participante"),
        p(),
        p(strong("Nome: "), participante$nome),
        p(strong("CNPJ: "), participante_cnpj),
        p(strong("Quantidade de licitações: "), participante$n_licitacoes),
        p(strong("Quantidade de vitórias: "), participante$n_vencedora),
        p(strong("Idoneidade: "), participante$idoneidade),
        p(strong("Atividade econômica: "), participante$secao_cnae),
        hr(),
        h4("Tabela de coparticipações"))
  })

  get_coparticipantes <- function(participante_cnpj) {
    if (is.null(participante_cnpj) || participante_cnpj == "") {
      return()
    }
    participante <- participantes_stats %>%
      filter(nu_cpfcnpj == participante_cnpj)

    coparticipacoes_filt <- coparticipacoes %>%
      filter(p1 == participante_cnpj | p2 == participante_cnpj) %>%
      transmute(nu_cpfcnpj = ifelse(p1 != participante_cnpj, p1, p2),
                n_coparticipacoes = frequency)

    if (nrow(coparticipacoes_filt) == 0) {
      return()
    }

    coparticipantes <- coparticipacoes_filt %>%
      left_join(participantes_stats, by = "nu_cpfcnpj") %>%
      arrange(desc(n_coparticipacoes)) %>%
      select("Nome" = nome,
             "CNPJ" = nu_cpfcnpj,
             "Qtd. coparticipacoes" = n_coparticipacoes,
             "Qtd. licitacoes" = n_licitacoes,
             "Qtd. vitorias" = n_vencedora,
             "Idoneidade" = idoneidade,
             "Atividade economica" = secao_cnae)

    coparticipantes
  }

  output$participante_table <- DT::renderDataTable(
    DT::datatable(get_coparticipantes(reactive_values$participante_cnpj),
                  options = list( pageLength = 20))
  )
}
