library(htmlwidgets, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(networkD3, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shiny, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shinyBS, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shinythemes, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE,
        verbose = FALSE)
library(DT, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Rede de conluios"),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Gráfico", tabName = "graph_tab", icon = icon("spinner")),
        menuItem("Informações", tabName = "info_tab",
                 icon = icon("info-circle")),
        menuItem("Filtros:", icon = icon("filter"))
      ),
     selectizeInput("empresa_filt", "CNPJ ou nome do participante:",
                    choice = "",
                    options = list(maxOptions = 50,
                                   valueField = "nu_cpfcnpj",
                                   labelField = "nome",
                                   searchField = c("nome", "nu_cpfcnpj"),
                                   render = I("{
                                              option: function(item, escape) {
                                                return '<div>' +
                                                  escape(item.nu_cpfcnpj) +
                                                  ' - ' +
                                                  escape(item.nome) + '</div>';
                                              }
                                              }"))),
      selectizeInput("secao_cnae",
                     "Atividade econômica do participante (seção CNAE):",
                     choices = "", multiple = TRUE),
      selectizeInput("node_group",
                     "Agrupar (colorir) empresas por:",
                     choices = c("Idoneidade" = "idoneidade",
                                 "Atividade economica" = "secao_cnae",
                                 "Vitorias em licitacoes" = "cat_vitorias"),
                     selected = "idoneidade"),
      sliderInput("min_frequency", "Frequência mínima de coparticipação:",
                  min = 5, max = 100, value = 40, ticks = TRUE),
      checkboxInput("filt_inidoneas", label = "Empresas inidôneas",
                    value = FALSE),
      actionButton("reset_input", "Limpar filtros")
    ),
    dashboardBody(
      tabItems(
        tabItem("graph_tab",
          h4(strong("Coparticipação de empresas em licitações na Paraíba"),
             bsButton("q1", label = "", icon = icon("question"),
                      style = "primary", size = "extra-small")),
          bsPopover("q1", "<strong>Descrição do gráfico</strong>",
                    "<p>Cada nó (círculo) representa uma empresa. As arestas\\
                     ligam empresas que participaram das mesmas licitações,\\
                     com a frequênca mínima escolhida.\\
                     Arestas mais grossas indicam maior coparticipação.\\
                     Empresas inidôneas (CEIS) estão em laranja. Clique\\
                     em um nó para obter mais informações da empresa.\\
                     <p>Use o mouse para navegar no gráfico e\\
                     controlar o zoom (scroll).</p>"),
          forceNetworkOutput("conluios_plot", width = "100%",
                              height = "800px")
        ),
        tabItem("info_tab",
                 uiOutput("participante_info"),
                 DT::dataTableOutput("participante_table")
        )
        )
    )
  )
)
