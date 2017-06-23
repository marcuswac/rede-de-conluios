library(dplyr, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(purrr, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(readr, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)
library(stringr, warn.conflicts =  FALSE, quietly = TRUE, verbose = FALSE)

# os dados de licitacao do TCE possuem quebras de linha. tem que fazer esta
# gambiarra para carrega-los
read_csv_from_tce <- function(file_name, field_sep = "|", line_sep = "\n") {
  f <- file(file_name)
  row_list <- f %>%
    scan("character", sep = line_sep) %>%
    map(str_split, fixed(field_sep)) %>%
    map(1)
  close(f)

  col_names <- row_list[[1]]
  n_cols <- length(col_names)

  row_filt <- row_list[-1] %>% # removing header
    map(length) == n_cols
  row_filt <- c(FALSE, row_filt) # defining header row as false

  df <- as_data_frame(do.call(rbind, row_list[row_filt]))
  colnames(df) <- col_names
  return(df)
}

carrega_dados_licitacao <- function(
  file = "data/TCE-PB-SAGRES-Licitacao_Esfera_Municipal.txt") {
  return(read_csv_from_tce(file))
}

carrega_dados_propostas <- function(
  file = "data/TCE-PB-SAGRES-Propostas_Licitacao_Esfera_Municipal.txt") {
  return(read_delim(file, delim = "|"))
}

carrega_dados_participantes <- function(
  file = "data/TCE-PB-SAGRES-Participantes_Licitacao_Esfera_Municipal.txt") {
  return(read_delim(file, delim = "|"))
}

carrega_dados_coparticipacoes <- function(
  file = "data/coparticipacoes.csv") {
  return(read_csv(file))
}

carrega_dados_participantes_stats <- function(
  file = "data/participantes_stats.csv") {
  return(read_csv(file))
}

carrega_dados_tce <- function() {
  dados <- list()
  dados$licitacao <- carrega_dados_licitacao()
  dados$propostas <- carrega_dados_propostas()
  dados$participantes <- carrega_dados_participantes()
  return(dados)
}

carrega_dados_ceis <- function(file = "data/20170614_CEIS.csv") {
  dados <- read_delim("data/20170614_CEIS.csv", delim = ";")
  colnames(dados) <- c("tipo_pessoa", "nu_cpfcnpj", "nome",
                       "razao_social", "nome fantasia", "nu_processo",
                       "tipo_sancao", "data_inicio_sancao",
                       "data_final_sancao", "orgao_sancionador",
                       "uf_orgao_sancionador", "origem_info",
                       "data_publicacao", "publicacao", "detalhamento",
                       "abrangencia", "fundamentacao_legal",
                       "descricao_fundamentacao_legal",
                       "data_transito_julgado", "complemento_orgao",
                       "contato_da_origem_info")
  return(dados)
}

carrega_dados_inidoneas_pb <- function(file = "data/inidoneas_pb.csv") {
  return(read_csv(file))
}

carrega_dados_cnae <- function(file = "data/cnpj_cep.txt") {
  return(read_delim(file, "|", trim_ws = TRUE, quote = "",
                    locale = locale(encoding = "latin1")))
}

carrega_dados_participantes_stats_com_cnae <- function(
  file = "data/participantes_stats_com_cnae.csv") {
  return(read_csv(file))
}
