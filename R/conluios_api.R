library(dplyr)
library(plumber)

source("R/common.R")

coparticipacoes <- suppressMessages(carrega_dados_coparticipacoes())

participantes_stats <- carrega_dados_participantes_stats_com_cnae() %>%
  filter(nu_cpfcnpj %in% coparticipacoes$nu_cpfcnpj_1 |
           nu_cpfcnpj %in% coparticipacoes$nu_cpfcnpj_2) %>%
  left_join(carrega_dados_inidoneas_pb(), by = "nu_cpfcnpj") %>%
  mutate(idoneidade = if_else(!is.na(tipo_sancao), "inidonea",
                              "regular"),
         sancao_ceis = if_else(!is.na(tipo_sancao), tipo_sancao, "Nada consta"),
         secao_cnae = if_else(!is.na(DescricaoSecao), DescricaoSecao,
                              "INDEFINIDA"),
         subclasse_cnae = if_else(!is.na(DescricaoSubclasse),
                                  DescricaoSubclasse, "INDEFINIDA")) %>%
  select(nu_cpfcnpj, nome, n_licitacoes, n_vencedora, sancao_ceis, idoneidade,
         secao_cnae, subclasse_cnae)

socios <- carrega_dados_socios_pb() %>%
  select(nu_cpfcnpj, socio_nome, socio_nome_legal) %>%
  filter(!is.na(socio_nome))

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
      
  participante_info <- filter(participantes_stats, nu_cpfcnpj == cnpj) %>%
    left_join(socios, by = c("nu_cpfcnpj")) %>%
    mutate(socio_nome = ifelse(is.na(socio_nome_legal), socio_nome,
                               socio_nome_legal)) %>%
    group_by(nome, nu_cpfcnpj, n_licitacoes, n_vencedora, sancao_ceis) %>%
    summarise(socios_nomes = list(socio_nome))
  
  coparticipantes <- get_coparticipantes(cnpj, coparticipacoes) %>%
    left_join(participantes_stats,
              by = c("nu_cpfcnpj_coparticipante" = "nu_cpfcnpj")) %>%
    ungroup() %>%
    select(nome, nu_cpfcnpj = nu_cpfcnpj_coparticipante, n_coparticipacoes,
           n_vitorias_participante = n_vitorias_1,
           n_vitorias_coparticipante = n_vitorias_2,
           n_mesmo_socio) %>%
    arrange(desc(n_coparticipacoes))
  
  list(participante_info = participante_info,
       coparticipantes = coparticipantes)
}
