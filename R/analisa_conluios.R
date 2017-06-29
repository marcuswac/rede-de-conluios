library(tidyr)
library(stringr)
library(dplyr)
library(multidplyr)
library(readr)

source("R/carrega_dados.R")

find_all_pairs <- function(x) {
  combn(sort(x), 2, simplify = FALSE) %>%
    map_df(~ data_frame(p1 = .[[1]], p2 = .[[2]]))
}

gera_coparticipacoes <- function(n_cores = 4,
                                 output = "data/coparticipacoes.csv") {

  participantes_df <- carrega_dados_participantes()

  cluster <- create_cluster(n_cores)
  set_default_cluster(cluster)
  cluster %>% cluster_assign_value("find_all_pairs", find_all_pairs)
  cluster %>% cluster_library(c("dplyr", "purrr"))

  coparticipacoes <- participantes_df %>%
    select(cd_ugestora, nu_licitacao, tp_licitacao, nu_cpfcnpj) %>%
    group_by(cd_ugestora, nu_licitacao, tp_licitacao) %>%
    mutate(n_empresas = n()) %>%
    filter(n_empresas > 1) %>%
    partition(cd_ugestora, nu_licitacao, tp_licitacao) %>%
    do(find_all_pairs(.$nu_cpfcnpj)) %>%
    collect() %>%
    group_by(p1, p2) %>%
    summarise(frequency = n())

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
    mutate(vencedora = any(de_situacaoproposta == "Vencedora")) %>%
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
