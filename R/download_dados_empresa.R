library("httr")
library("jsonlite")
library("readr")

source("R/carrega_dados.R")

participantes <- carrega_dados_participantes()

cnpjs <- filter(participantes, !startsWith(nu_cpfcnpj, "000"))$nu_cpfcnpj %>%
  unique() %>%
  sort()
  
output_dir <- "data/cnpj-info-json/"

last_i <- 1

for (i in seq(last_i, length(cnpjs))) {
  last_i <- i
  cnpj <- cnpjs[i]
  output_file <- paste(output_dir, cnpj, ".json", sep = "")
  print(paste("Baixando empresa", i, "para:", output_file))
  cnpj_url <- str_c("https://www.receitaws.com.br/v1/cnpj/", cnpj)
  cnpj_req <- GET(cnpj_url)
  cnpj_json <- content(cnpj_req, as = "text")
  if (http_error(cnpj_req)) {
    output_file <- paste(output_file, "_erro", sep = "")
    print(output_file)
    cnpj_info <- cnpj_json
  } else {
    cnpj_info <- fromJSON(cnpj_json)
  }
  write_json(cnpj_info, output_file)
  Sys.sleep(2)
}
