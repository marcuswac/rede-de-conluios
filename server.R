#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DT, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(dplyr, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(htmlwidgets, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shiny, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

source("R/carrega_dados.R")

coparticipacoes <- suppressMessages(carrega_dados_coparticipacoes())
participantes_stats <- carrega_dados_participantes_stats_com_cnae() %>%
  filter(nu_cpfcnpj %in% coparticipacoes$p1 |
           nu_cpfcnpj %in% coparticipacoes$p2) %>%
  left_join(carrega_dados_inidoneas_pb(), by = "nu_cpfcnpj") %>%
  mutate(idoneidade = if_else(!is.na(tipo_sancao), "inidônea",
                              "regular"),
         tipo_sancao = if_else(!is.na(tipo_sancao), tipo_sancao, "Nada consta"),
         secao_cnae = if_else(!is.na(DescricaoSecao), DescricaoSecao,
                             "INDEFINIDA"),
         subclasse_cnae = if_else(!is.na(DescricaoSubclasse),
                                  DescricaoSubclasse, "INDEFINIDA")) %>%
  select(nu_cpfcnpj, nome, n_licitacoes, n_vencedora, tipo_sancao, idoneidade,
         secao_cnae, subclasse_cnae)
secoes_cnae <- participantes_stats$secao_cnae %>% unique() %>% sort()

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
        filter(nome_cnpj == nu_cpfcnpj)
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

    CAT_VITORIAS <- c("intermediária", "venceu pouco (< 25%)",
                      "venceu muito (> 75%)")

    participantes_nodes <- participantes_filt %>%
      ungroup() %>%
      mutate(prop_vencedoras = n_vencedora / n_licitacoes,
             cat_vitorias = factor(
               ifelse(prop_vencedoras < .25, CAT_VITORIAS[2],
                      if_else(prop_vencedoras > .75, CAT_VITORIAS[3],
                              CAT_VITORIAS[1])), levels = CAT_VITORIAS),
             node_id = paste(nome, " (CNPJ: ", nu_cpfcnpj, ")", sep = ""),
             node_size = 20 * prop_vencedoras,
             idoneidade = factor(idoneidade,
                                 levels = c("regular", "inidônea"))) %>%
      arrange_(input$node_group) %>%
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

  get_coparticipantes <- function(participante_cnpj) {
    if (is.null(participante_cnpj) || participante_cnpj == "") {
      return(data.frame())
    }
    participante <- participantes_stats %>%
      filter(nu_cpfcnpj == participante_cnpj)

    coparticipacoes_filt <- coparticipacoes %>%
      filter(p1 == participante_cnpj | p2 == participante_cnpj) %>%
      transmute(nu_cpfcnpj = ifelse(p1 != participante_cnpj, p1, p2),
                n_coparticipacoes = frequency)

    if (nrow(coparticipacoes_filt) == 0) {
      return(data.frame())
    }

    coparticipacoes <- coparticipacoes_filt %>%
      left_join(participantes_stats, by = "nu_cpfcnpj") %>%
      arrange(desc(n_coparticipacoes)) %>%
      ungroup() %>%
      select(nome, nu_cpfcnpj, n_coparticipacoes, n_licitacoes, n_vencedora,
             tipo_sancao)
    coparticipacoes
  }

  visualiza_conluios <- reactive({
    dados <- filtra_dados()
    grupo_node <- if_else(!is.na(input$node_group) && input$node_group != "",
                         input$node_group, "idoneidade")

    # O nonce força a mudança de estado quando o mesmo nó é clicado de novo
    notify_node_clicked <- "Shiny.onInputChange('node_clicked', {
                              name: d.name,
                              '.nonce': Math.random()
                            })"

    forceNetwork(Links = dados$coparticipacoes_links,
                 Nodes = dados$participantes_nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "node_id",
                 Nodesize = "node_size", Group = grupo_node, legend = TRUE,
                 zoom = TRUE, opacity = 1, fontSize = 12,
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                 charge = -15, clickAction = notify_node_clicked,
                 linkDistance = 50, opacityNoHover = 0.3)
  }) %>%
    debounce(1000) # adiciona delay para re-gerar grafico


  updateSelectizeInput(session, "empresa_filt", choices = participantes_stats,
                       server = TRUE)

  updateSelectizeInput(session, "secao_cnae", choices = secoes_cnae,
                       server = TRUE)

  observeEvent(input$empresa_filt, {
    reactive_values$participante_cnpj <- input$empresa_filt
  })

  observeEvent(input$reset_input, {
    updateSelectizeInput(session, "empresa_filt", selected = "")
    updateSelectizeInput(session, "secao_cnae", selected = "")
    updateSliderInput(session, "min_frequency", value = 50)
    updateCheckboxInput(session, "filt_inidoneas", value = FALSE)
    updateSelectInput(session, "node_group", selected = "idoneidade")
  })

  observeEvent(input$node_clicked, {
    print(paste("Clicked", input$node_clicked$name))
    participante_cnpj <- input$node_clicked$name %>%
      str_split(fixed("(CNPJ: "), simplify = TRUE) %>%
    getElement(2) %>%
    str_split(fixed(")"), simplify = TRUE) %>%
    getElement(1)

    participante <- participantes_stats %>%
      filter(nu_cpfcnpj == participante_cnpj)

    reactive_values$participante_cnpj <- participante_cnpj

    updateSelectizeInput(session, "empresa_filt",
                         selected = participante,
                         choices = participantes_stats, server = TRUE)
    updateTabItems(session, "tabs", selected = "info_tab")
  })

  output$conluios_plot <- renderForceNetwork({
    visualiza_conluios()
  })


  output$participante_info <- renderUI({
    participante_cnpj <- reactive_values$participante_cnpj
    if (is.null(participante_cnpj) || participante_cnpj == "") {
      return(div(id = "div_cnpj_info",
                 p("Escolha uma empresa usando o filtro de CNPJ ou nome ao lado,
                    ou clicando em um nó do gráfico.")))
    }
    participante <- participantes_stats %>%
      filter(nu_cpfcnpj == participante_cnpj)

    div(id = "div_cnpj_info",
        h3(strong("Informações do participante")),
        p(),
        p(strong("Nome: "), participante$nome),
        p(strong("CNPJ: "), participante_cnpj),
        p(strong("Vitórias em licitações: "),
          participante$n_vencedora, " de ",
          participante$n_licitacoes, " participações (",
          round(100 * participante$n_vencedora / participante$n_licitacoes, 2),
          "% )"),
        p(strong("Inidoneidade: "), participante$tipo_sancao),
        p(strong("Atividade econômica primária: "),
          participante$subclasse_cnae),
        hr(),
        h4(strong("Tabela de coparticipações")))
  })

  output$participante_table_ui <- renderUI({
    if (!is.na(input$empresa_filt) && input$empresa_filt != "") {
      DT::dataTableOutput("participante_table")
    } else {
      return("")
    }
  })

  output$participante_table <- DT::renderDataTable(
    DT::datatable(
      get_coparticipantes(reactive_values$participante_cnpj),
      options = list(pageLength = 20,
                     lengthMenu = c(20, 50, 100, 500, 1000),
                     language = list(
                       url = "http://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"),
                     "dom" = 'T<"clear">lBfrtip',
                     buttons = list('copy', 'csv', 'excel')
      ),
      extensions = "Buttons",
      colnames = c("Nome" = "nome",
                   "CNPJ" = "nu_cpfcnpj",
                   "Qtd. coparticipacoes" = "n_coparticipacoes",
                   "Qtd. licitacoes" = "n_licitacoes",
                   "Qtd. vitorias" = "n_vencedora",
                   "Inidoneidade" = "tipo_sancao"
      )
    )
  )
  
  observeEvent(input$print, {
    js$winprint()
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    cnpj <- query[["cnpj"]]
    if (!is.null(cnpj)) {
      print(cnpj)
      updateTabItems(session, "tabs", selected = "info_tab")
      updateSelectizeInput(session, "empresa_filt",
                           selected = cnpj,
                           choices = participantes_stats, server = TRUE)
    }
  })
}
