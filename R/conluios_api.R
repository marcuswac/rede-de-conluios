library(dplyr)
library(plumber)

source("R/carrega_dados.R")
source("R/common.R")

coparticipacoes <- suppressMessages(carrega_dados_coparticipacoes())
participantes_stats <- carrega_dados_participantes_stats_com_cnae() %>%
  filter(nu_cpfcnpj %in% coparticipacoes$nu_cpfcnpj_1 |
           nu_cpfcnpj %in% coparticipacoes$nu_cpfcnpj_2) %>%
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

get_coparticipantes <- function(participante_cnpj, participantes_stats,
                                coparticipacoes) {
  if (is.null(participante_cnpj) || participante_cnpj == "") {
    return(data.frame())
  }
  participante <- participantes_stats %>%
    filter(nu_cpfcnpj == participante_cnpj)
  
  coparticipacoes_filt <- coparticipacoes %>%
    filter(nu_cpfcnpj_1 == participante_cnpj |
             nu_cpfcnpj_2 == participante_cnpj) %>%
    transmute(nu_cpfcnpj = ifelse(nu_cpfcnpj_1 != participante_cnpj,
                                  nu_cpfcnpj_1, nu_cpfcnpj_2),
              n_coparticipacoes = frequency)
  
  if (nrow(coparticipacoes_filt) == 0) {
    return(data.frame())
  }
  
  coparticipacoes <- coparticipacoes_filt %>%
    left_join(participantes_stats, by = "nu_cpfcnpj") %>%
    arrange(desc(n_coparticipacoes)) %>%
    ungroup() %>%
    select(nome, nu_cpfcnpj, n_coparticipacoes, n_licitacoes, n_vencedora,
           n_mesmo_socio, tipo_sancao)
  coparticipacoes
}

#* Retorna coparticipacoes de empresas em licitacoes
#* @get /coparticipacoes
function(cnpj = NA) {
  if (is.na(cnpj)) {
    return(
      list(
        error = "CNPJ is missing. Usage: /coparticipacoes?cnpj=000.000.000-00"
      )
    )
  }
  
  # removing symbols
  cnpj <- str_extract_all(cnpj, "\\d", simplify =  T) %>%
    str_c(collapse = "")
      
  participante_info <- filter(participantes_stats, nu_cpfcnpj == cnpj)
  coparticipacoes <- get_coparticipantes(cnpj, participantes_stats,
                                         coparticipacoes)
  list(participante_info = participante_info,
       coparticipacoes = coparticipacoes)
}
