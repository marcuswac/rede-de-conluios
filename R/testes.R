participantes <- participantes %>%
  mutate(is_empresa = !startsWith(nu_cpfcnpj, "000"))

participantes %>%
  group_by(de_tipolicitacao, is_empresa) %>%
  summarise(freq = n()) %>%
  mutate(prop = freq / sum(freq)) %>%
  arrange(de_tipolicitacao) %>%
  data.frame()

participantes %>%
  filter(grepl("LACRIL", no_participante, fixed = TRUE))

head(licitacoes)

participantes %>%
  filter(cd_ugestora == "201082", nu_licitacao == "000242012", tp_licitacao == 11) %>%
  data.frame()

licitacoes %>%
  filter(cd_ugestora == "201082", nu_Licitacao == "000242012", tp_Licitacao == 11) %>%
  data.frame

propostas %>%
  filter(cd_ugestora == "201082", nu_licitacao == "000242012", tp_licitacao == 11) %>%
  data.frame()





analisa_socios <- function() {
  
  cnpj_info <- read_csv("data/cnpj-info-pb.csv")
  cnpj_info_short <- cnpj_info %>%
    select(-starts_with("secondary_activity_"), -starts_with("partner_"))
  
  cnpj_socios <- cnpj_info %>%
    select(cnpj, starts_with("partner_")) %>%
    melt(id.vars = "cnpj") %>%
    cbind(colsplit(.$variable, "_", c("cat", "socio_indice", "attr"))) %>%
    dcast(cnpj + socio_indice ~ attr, value.var = "value") %>%
    filter(!is.na(name)) %>%
    rename("socio_pais_origem" = "contry_origin",
           "socio_nome_legal" = "legal_representative_name",
           "socio_qualificacao_legal" = "legal_representative_qualification",
           "socio_nome" = "name",
           "socio_qualificacao" = "qualification") %>%
    mutate(nu_cpfcnpj = (str_extract_all(.$cnpj, "\\d") %>%
                           map_chr(paste, collapse = "")))
  
  cnpj_socios_top <- cnpj_socios %>%
    group_by(socio_nome) %>%
    mutate(n_empresas = n()) %>%
    left_join(cnpj_info_short) %>%
    arrange(desc(n_empresas), socio_nome) %>%
    filter(n_empresas >= 5) %>%
    select(cnpj, name, socio_nome)
  
  coparticipacoes_mesmo_socio <- coparticipacoes %>%
    inner_join(cnpj_socios, by = c("p1" = "nu_cpfcnpj")) %>%
    inner_join(cnpj_socios, by = c("p2" = "nu_cpfcnpj"),
               suffix = c(".p1", ".p2")) %>%
    filter(!is.na(socio_nome.p1), !is.na(socio_nome.p2), 
           socio_nome.p1 == socio_nome.p2) %>%
    inner_join(cnpj_info_short, by = c("cnpj.p1" = "cnpj")) %>%
    inner_join(cnpj_info_short, by = c("cnpj.p2" = "cnpj"),
               suffix = c(".p1", ".p2")) %>%
    group_by(socio_nome.p1, socio_nome.p2) %>%
    mutate(freq = n()) %>%
    arrange(desc(freq))
  
  participantes_mesmo_socio <- participantes %>%
    inner_join(cnpj_socios, by = c("nu_cpfcnpj")) %>%
    group_by(cd_ugestora, nu_licitacao, tp_licitacao, socio_nome) %>%
    mutate(n_mesmo_socio = n()) %>%
    filter(n_mesmo_socio > 1)
  
  participantes_mesmo_socio %>%
    #filter(cd_ugestora == 201050) %>%
    ungroup() %>%
    #select(cd_ugestora, nu_licitacao, tp_licitacao, de_tipolicitacao, no_participante, socio_nome, n_mesmo_socio) %>%
    left_join(propostas) %>%
    mutate(cd_ugestora = as.character(cd_ugestora),
           tp_licitacao = as.character(tp_licitacao)) %>%
    left_join(licitacao_df, by = c("cd_ugestora",
                                   "nu_licitacao" = "nu_Licitacao",
                                   "tp_licitacao" = "tp_Licitacao")) %>%
    mutate(vl_Licitacao = as.numeric(vl_Licitacao)) %>%
    arrange(desc(vl_Licitacao), cd_ugestora, socio_nome, nu_licitacao) %>%
    data.frame() %>%
    View()
  
  filter(cnpj_socios, nu_cpfcnpj %in% c(00171428000105, 09250085000130, 22114083000182,
                                        22113946000105, 12613006000113))
  filter(participantes, nu_cpfcnpj %in% c(00171428000105, 09250085000130, 22114083000182,
                                          22113946000105, 12613006000113))
  
}

begin_time <- Sys.time()
gera_coparticipacoes()
end_time <- Sys.time()

print(end_time - begin_time)

tecnocenter_participantes <- filter(participantes, nu_cpfcnpj == "06948769000201")

participantes %>%
  inner_join(tecnocenter_participantes, by = c("cd_ugestora", "nu_licitacao", "tp_licitacao")) %>%
  View()