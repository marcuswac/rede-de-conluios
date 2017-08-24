library(DT, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(htmlwidgets, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(networkD3, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shiny, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shinyBS, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shinydashboard, warn.conflicts = FALSE, quietly = TRUE,
        verbose = FALSE)
library(shinyjs, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(shinythemes, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(V8, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

jsCode <- 'shinyjs.winprint = function(){
  window.print();
}'

shinyUI(
  dashboardPage(
    dashboardHeader(title = tags$a(href='/', "Rede de conluios")),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Gráfico", tabName = "graph_tab", icon = icon("spinner")),
        menuItem("Relatórios", tabName = "info_tab",
                 icon = icon("table")),
        menuItem("Filtros:", icon = icon("filter"), startExpanded = TRUE,
          selectizeInput(
            "empresa_filt", "CNPJ ou nome do participante:",
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
            checkboxGroupInput(
              "filt_checkbox", "Mostrar apenas empresas:",
              choiceNames = c("Com mesmo sócio", "Inidôneas"),
              choiceValues = c("mesmo_socio", "inidoneas")
            ),
            sliderInput(
              "min_frequency",
              "Frequência mínima de coparticipação:",
              min = 5, max = 100, value = 50, ticks = TRUE
            ),
            selectizeInput(
              "secao_cnae",
              "Atividade econômica (CNAE):", choices = "", multiple = TRUE
            ),
            selectInput(
              "node_group",
              "Agrupar (colorir) por:",
              choices = c("Idoneidade" = "idoneidade",
                          "Atividade economica" = "secao_cnae",
                          "Vitorias em licitacoes" = "cat_vitorias"),
              selected = "idoneidade"
            ),
            actionButton("reset_input", "Limpar filtros")
    ))),
    dashboardBody(
      tags$head(
        includeScript("google-analytics.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem("graph_tab",
          h4(strong("Coparticipação de empresas em licitações na Paraíba"),
             bsButton("q1", label = "", icon = icon("question"),
                      type = "toggle", style = "danger",
                      size = "extra-small")),
          bsPopover("q1", "<strong>Descrição do gráfico</strong>",
                    "<p>Cada nó (círculo) representa uma empresa. As arestas\\
                     ligam empresas que participaram das mesmas licitações,\\
                     com a frequência mínima escolhida.\\
                     Arestas mais grossas indicam maior coparticipação.\\
                     Empresas inidôneas (CEIS) estão em laranja. Clique\\
                     em um nó para obter mais informações da empresa.\\
                     <p>Use o mouse para navegar no gráfico e\\
                     controlar o zoom (scroll).</p>"),
          forceNetworkOutput("conluios_plot", width = "100%",
                              height = "800px")
        ),
        tabItem("info_tab",
                useShinyjs(),
                extendShinyjs(text = jsCode),
                fluidRow(
                  column(8, uiOutput("participante_info")),
                  column(4, actionButton("print", "Imprimir"), style="padding:20px;")
                ),
                uiOutput("participante_table_ui")
        )
      )
    )
  )
)
