library(tidyr)
library(stringr)
library(dplyr)
library(multidplyr)
library(readr)

source("R/carrega_dados.R")

find_all_pairs <- function(x) {
  combn(x, 2, simplify = FALSE) %>%
    map_df(~ data_frame(p1 = .[[1]], p2 = .[[2]]))
}

gera_coparticipacoes <- function(participantes, n_cores = 4,
                                 output = "data/coparticipacoes.csv") {

  participantes_df <- ifelse(is.character(participantes),
                             carrega_dados_participantes(),
                             participantes)

  cluster <- create_cluster(n_cores)
  set_default_cluster(cluster)
  cluster %>% cluster_assign_value("find_all_pairs", find_all_pairs)
  cluster %>% cluster_library(c("dplyr", "purrr"))

  coparticipacoes <- participantes_df %>%
    select(cd_ugestora, nu_licitacao, nu_cpfcnpj) %>%
    group_by(cd_ugestora, nu_licitacao) %>%
    mutate(n_empresas = n()) %>%
    filter(n_empresas > 1) %>%
    partition(cd_ugestora, nu_licitacao) %>%
    do(find_all_pairs(.$nu_cpfcnpj)) %>%
    collect() %>%
    group_by(p1, p2) %>%
    summarise(frequency = n())

  if (is.character(output)) {
    write_csv(coparticipacoes, output)
  }
}

gera_participantes_stats <- function(output = "data/participantes_stats.csv") {
  participantes_df <- carrega_dados_participantes()
  propostas_df <- carrega_dados_propostas()
  licitacao_df <- carrega_dados_licitacao()

  propostas_stats <- propostas_df %>%
    group_by(nu_cpfcnpj, cd_ugestora, nu_licitacao) %>%
    mutate(vencedora = any(de_situacaoproposta == "Vencedora")) %>%
    group_by(nu_cpfcnpj) %>%
    summarise(n_licitacoes = n(), n_vencedora = sum(vencedora))

  participantes_stats <- participantes_df %>%
    mutate(cd_ugestora = as.character(cd_ugestora)) %>%
    left_join(propostas_stats, by = "nu_cpfcnpj") %>%
    left_join(licitacao_df, by = c("cd_ugestora",
                                   "nu_licitacao" = "nu_Licitacao")) %>%
    group_by(nu_cpfcnpj) %>%
    summarise(nome = first(no_participante),
              n_licitacoes = first(n_licitacoes),
              prop_vencedoras = first(n_vencedora) / n_licitacoes)

  if (is.character(output)) {
    write_csv(participantes_stats, output)
  }

  return(participantes_stats)
}

gera_inidoneas_pb <- function(output = "data/inidoneas_pb.csv") {
  participantes_df <- carrega_dados_participantes()

  inidoneas_df <- carrega_dados_ceis() %>%
    select(nu_cpfcnpj, razao_social) %>%
    filter(nu_cpfcnpj %in% participantes_df$nu_cpfcnpj) %>%
    distinct()

  if (is.character(output)) {
    write_csv(inidoneas_df, output)
  }

  return(inidoneas_df)
}
