library(htmlwidgets, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(networkD3, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shiny, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(shinythemes, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

shinyUI(
  navbarPage(
    "# Rede de conluios",
    theme = shinytheme("cerulean"),
    tabPanel("Licitações PB",
      titlePanel("Coparticipação de empresas em licitações na Paraíba"),
      p("Cada nó representa uma empresa. As arestas ligam empresas que
         participam de uma mesma licitação. Nós mais próximos e arestas mais
         grossas indicam uma maior frequência de coparticipação.
         Nós laranja indicam empresas inidôneas no CEIS."),
      p(em("Use o mouse ou touchscreen para navegar no gráfico e controlar o
           zoom.")),
      hr(),
      sidebarPanel(width = 3,
        h3("Filtros"),
        selectizeInput("empresa", "CNPJ ou Nome do participante:", choice = "",
          width = "100%",
          options = list(maxOptions = 1000,
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
        selectizeInput(
          'secao_cnae', 'Atividade econômica do participante (CNAE):',
          choices = "", multiple = TRUE
        ),
        
        sliderInput("min_frequency", "Frequência mínima de coparticipação:",
                    min = 5, max = 100, value = 30, ticks = FALSE),
        strong("Filtrar:"),
        checkboxInput("filt_inidoneas", label = "Empresas inidôneas",
                      value = FALSE)
      ),
      mainPanel(width = 9,
        forceNetworkOutput("conluios_plot", width = "100%", height = "700px")
      )
    )
  )
)
