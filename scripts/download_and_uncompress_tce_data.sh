#!/bin/bash

cd data/

wget https://dados.tce.pb.gov.br/TCE-PB-SAGRES-Licitacao_Esfera_Municipal.txt.gz
gzip -d TCE-PB-SAGRES-Licitacao_Esfera_Municipal.txt.gz

wget https://dados.tce.pb.gov.br/TCE-PB-SAGRES-Participantes_Licitacao_Esfera_Municipal.txt.gz
gzip -d TCE-PB-SAGRES-Participantes_Licitacao_Esfera_Municipal.txt.gz

wget https://dados.tce.pb.gov.br/TCE-PB-SAGRES-Propostas_Licitacao_Esfera_Municipal.txt.gz
gzip -d TCE-PB-SAGRES-Propostas_Licitacao_Esfera_Municipal.txt.gz
