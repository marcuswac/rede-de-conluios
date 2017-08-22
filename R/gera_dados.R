library(tidyr)
library(stringr)
library(dplyr)
library(multidplyr)
library(readr)

source("R/common.R")

find_all_pairs <- function(cnpjs, vencedoras) {
  idx <- order(cnpjs)
  cnpjs_ordered <- cnpjs[idx]
  vencedoras_ordered <- vencedoras[idx]
  
  combn(length(cnpjs), 2, simplify = FALSE) %>%
    map_df(~ data_frame(nu_cpfcnpj_1 = cnpjs_ordered[.[[1]]],
                        nu_cpfcnpj_2 = cnpjs_ordered[.[[2]]],
                        vencedora_1 = vencedoras_ordered[.[[1]]],
                        vencedora_2 = vencedoras_ordered[.[[2]]]))
}

conta_mesmo_socio <- function(nu_cpfcnpj_1, nu_cpfcnpj_2, socios_list) {
  map2_int(nu_cpfcnpj_1, nu_cpfcnpj_2,
           function(x, y) {
             socios_nomes_1 <- socios_list[[x]]
             socios_nomes_2 <- socios_list[[y]]
             ifelse(is.character(socios_nomes_1) && is.character(socios_nomes_2),
                    sum(map_lgl(socios_nomes_1, ~ any(.x == socios_nomes_2))),
                    0)
           }
  )
}

gera_coparticipacoes <- function(n_cores = 4,
                                 output = "data/coparticipacoes4.csv") {

  participantes_df <- carrega_dados_participantes()
  propostas_stats <- carrega_dados_propostas() %>%
    group_by(nu_cpfcnpj, cd_ugestora, nu_licitacao, tp_licitacao) %>%
    summarise(vencedora = any(de_situacaoproposta == "Vencedora", na.rm = TRUE))
  
  socios_list <- carrega_dados_participantes_stats_com_cnae() %>%
    select(nu_cpfcnpj, nome) %>%
    left_join(carrega_dados_socios_pb()) %>%
    mutate(socio_nome = ifelse(!is.na(socio_nome_legal), socio_nome_legal,
                               ifelse(!is.na(socio_nome), socio_nome,
                                      nome))) %>%
    select(nu_cpfcnpj, socio_nome) %>%
    filter(!is.na(socio_nome))%>%
    split(.$nu_cpfcnpj) %>%
    map(~ .$socio_nome)

  cluster <- create_cluster(n_cores)
  set_default_cluster(cluster)
  cluster %>% cluster_assign_value("find_all_pairs", find_all_pairs)
  cluster %>% cluster_assign_value("conta_mesmo_socio", conta_mesmo_socio)
  cluster %>% cluster_assign_value("socios_list", socios_list)
  cluster %>% cluster_library(c("dplyr", "purrr"))

  coparticipacoes <- participantes_df %>%
    left_join(propostas_stats) %>%
    select(cd_ugestora, nu_licitacao, tp_licitacao, nu_cpfcnpj, vencedora) %>%
    group_by(cd_ugestora, nu_licitacao, tp_licitacao) %>%
    mutate(n_empresas = n()) %>%
    filter(n_empresas > 1) %>%
    partition(cd_ugestora, nu_licitacao, tp_licitacao) %>%
    do(find_all_pairs(.$nu_cpfcnpj, .$vencedora)) %>%
    collect() %>%
    group_by(nu_cpfcnpj_1, nu_cpfcnpj_2) %>%
    summarise(n_coparticipacoes = n(),
              n_vitorias_1 = sum(vencedora_1, na.rm = TRUE),
              n_vitorias_2 = sum(vencedora_2, na.rm = TRUE),
              n_mesmo_socio = conta_mesmo_socio(first(nu_cpfcnpj_1),
                                                first(nu_cpfcnpj_2),
                                                socios_list))

  if (is.character(output)) {
    write_csv(coparticipacoes, output)
  }

  return(coparticipacoes)
}

gera_participantes_stats <- function(output = "data/participantes_stats.csv") {
  participantes_df <- carrega_dados_participantes()
  propostas_df <- carrega_dados_propostas()
  licitacao_df <- carrega_dados_licitacao()

  propostas_stats <- propostas_df %>%
    group_by(nu_cpfcnpj, cd_ugestora, nu_licitacao, tp_licitacao) %>%
    mutate(vencedora = any(de_situacaoproposta == "Vencedora", na.rm = T)) %>%
    group_by(nu_cpfcnpj) %>%
    summarise(n_licitacoes = n(), n_vencedora = sum(vencedora))

  participantes_stats <- participantes_df %>%
    mutate(cd_ugestora = as.character(cd_ugestora)) %>%
    left_join(propostas_stats, by = "nu_cpfcnpj") %>%
    left_join(licitacao_df, by = c("cd_ugestora",
                                   "nu_licitacao" = "nu_Licitacao",
                                   "tp_licitacao" = "tp_Licitacao")) %>%
    group_by(nu_cpfcnpj) %>%
    summarise(nome = first(no_participante),
              n_licitacoes = first(n_licitacoes),
              n_vencedora = first(n_vencedora))

  if (is.character(output)) {
    write_csv(participantes_stats, output)
  }

  return(participantes_stats)
}

gera_participantes_stats_com_cnae <- function(
  output = "data/participantes_stats_com_cnae.csv") {

  participantes_cnae <- carrega_dados_participantes_stats() %>%
    left_join(carrega_dados_cnae(), by = c("nu_cpfcnpj" = "nrcnpj"))
  write_csv(participantes_cnae, output)

  return(participantes_cnae)
}

gera_inidoneas_pb <- function(output = "data/inidoneas_pb.csv") {
  participantes_df <- carrega_dados_participantes()

  inidoneas_df <- carrega_dados_ceis() %>%
    group_by(nu_cpfcnpj) %>%
    summarise(tipo_sancao = paste(unique(tipo_sancao), collapse = " / ")) %>%
    filter(nu_cpfcnpj %in% participantes_df$nu_cpfcnpj)

  if (is.character(output)) {
    write_csv(inidoneas_df, output)
  }

  return(inidoneas_df)
}

gera_socios_pb <- function(output = "data/cnpj_socios_licitacoes_pb.csv") {
  cnpj_info <- carrega_dados_receitaws_pb()
  
  cnpj_socios <- cnpj_info %>%
    select(cnpj, starts_with("partner_")) %>%
    gather(key, value, -cnpj) %>%
    filter(!is.na(value), value != "") %>%
    separate(key, into = c("cat", "socio_indice", "attr"), sep = "_",
             extra = "merge") %>%
    spread(attr, value) %>%
    filter(!is.na(name)) %>%
    mutate(nu_cpfcnpj = (str_extract_all(.$cnpj, "\\d") %>%
                           map_chr(paste, collapse = ""))) %>%
    select(nu_cpfcnpj, socio_indice,
           socio_nome = name,
           socio_nome_legal = legal_representative_name,
           socio_qualificacao = qualification,
           socio_qualificacao_legal = legal_representative_qualification,
           socio_pais_origem = contry_origin)
  
  if (is.character(output)) {
    write_csv(cnpj_socios, output)
  }
  
  return(cnpj_socios)
}
