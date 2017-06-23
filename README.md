# Rede de conluios

Análise da coparticipação de empresas em licitações na Paraíba.

## Dependências

- [*R*](https://cran.r-project.org/) >= 3.3

### Pacotes R

Depois de ter o [*R*](https://cran.r-project.org/) instalado, você deve
instalar os pacotes do R. Vá até o diretório raiz do projeto e rode:

```
Rscript instala_pacotes.R
```

### Dados do TCE

Para atualizar os dados das licitações e gerar novamente as estatísticas
de coparticipações, você deve baixar os dados no site do TCE-PB.
Vá até o diretório raiz do projeto e rode:

```
scripts/download_and_uncompress_tce_data.sh
```

