library(networkD3)
library(dplyr)

source("R/carrega_dados.R")

visualiza_coparticipacoes <- function(
  participantes_stats = carrega_dados_participantes_stats(),
  coparticipacoes = carrega_dados_coparticipacoes(),
  min_frequency = 50) {

  coparticipacoes_filt <- coparticipacoes %>%
  mutate(p1 = as.character(p1), p2 = as.character(p2)) %>%
  filter(frequency >= min_frequency)

  cnpjs_filt <- sort(unique(c(coparticipacoes_filt$p1,
                              coparticipacoes_filt$p2)))

  coparticipacoes_links <- coparticipacoes_filt %>%
    rowwise() %>%
    transmute(source = which(p1 == cnpjs_filt) - 1,
              target = which(p2 == cnpjs_filt) - 1,
              value = frequency / min_frequency)

  nodes <- participantes_stats %>%
    ungroup() %>%
    filter(nu_cpfcnpj %in% cnpjs_filt) %>%
    mutate(group = ifelse(prop_vencedoras < .25, "perde muito",
                          ifelse(prop_vencedoras < .75, "intermediario",
                                 "vence muito")),
           node_id = paste(nome, " (CNPJ: ", nu_cpfcnpj, " / Venceu ",
                           round(prop_vencedoras * n_licitacoes),
                           " de ", n_licitacoes, ")", sep = ""))

  plot <- forceNetwork(Links = coparticipacoes_links, Nodes = nodes,
                       Source = "source", Target = "target", Value = "value",
                       NodeID = "node_id", Nodesize = "prop_vencedoras",
                       Group = "group", legend = TRUE, zoom = TRUE)
  return(plot)
}
