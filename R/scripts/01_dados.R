library(readODS)
library(dplyr)
library(janitor)
library(stringr)


#### Prévia da população nos municípios ####

caminho_populacao <- "~/R/projetos/censo/2022/tabelas/previa_populacao_municipios.ods"
municipio_populacao <- read_ods(caminho_populacao, sheet = 1)

municipio_populacao <- municipio_populacao %>%
  clean_names() %>% 
  mutate(
    mun_id = paste0(cod_uf,cod_munic),
    uf = as.factor(uf),
    cod_uf = as.numeric(cod_uf),
    cod_munic = as.numeric(cod_munic),
    mun_id = as.numeric(mun_id),
    nome_do_municipio = as.factor(nome_do_municipio),
    populacao = populacao %>%
      str_remove_all("\\.") %>%          
      str_remove("\\(.*\\)") %>%         
      as.numeric()                       
  ) %>% 
  mutate(
    nome_do_municipio = case_when(
      nome_do_municipio == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
      TRUE ~ nome_do_municipio
    )
  ) %>% 
  mutate(
    nome_do_municipio = as.factor(nome_do_municipio)
  )


#### Tabela de divisão territorial brasileira 2022 ####

caminho_territorio <- "~/R/projetos/censo/2022/tabelas/variaveis_territoriais.ods"
territorio <- read_ods(caminho_territorio, sheet = 2)

territorio <- territorio %>% 
  clean_names() %>% 
  rename(rginter_code = regiao_geografica_intermediaria,
         rginter_nome = nome_regiao_geografica_intermediaria,
         rgimedi_code = regiao_geografica_imediata,
         rgimedi_nome = nome_regiao_geografica_imediata,
         meso_code = mesorregiao_geografica,
         meso_nome = nome_mesorregiao,
         micr_code = microrregiao_geografica,
         micr_nome = nome_microrregiao,
         mun_id = codigo_municipio_completo,
         mun_nome = nome_municipio
         ) %>% 
  mutate(
    nome_uf = as.factor(nome_uf),
    rginter_nome = as.factor(rginter_nome),
    rgimedi_nome = as.factor(rgimedi_nome),
    meso_code = as.numeric(meso_code),
    meso_nome = as.factor(meso_nome),
    micr_code = as.numeric(micr_code),
    micr_nome = as.factor(micr_nome),
    municipio = as.numeric(municipio),
    mun_id = as.numeric(mun_id),
    mun_nome = as.factor(mun_nome)
  )


#### Tabela com número de homens e mulheres ####

caminho_genero <- "~/R/projetos/censo/2022/tabelas/genero.ods"
genero <- read_ods(caminho_genero, sheet = 3)

genero <- genero %>% 
  clean_names() %>% 
  select(
    municipio,
    total,
    homens,
    mulheres
  ) %>% 
  tidyr::separate(
    municipio,                   # Coluna que será dividida
    into = c("municipio", "uf"), # Novos nomes das colunas
    sep = " \\(",                # Separador: espaço seguido de parêntese
    remove = TRUE                # Remove a coluna original
  ) %>% 
  mutate(
    uf = str_replace(uf, "\\)", "") # Remove o parêntese final da coluna uf
  ) %>% 
  mutate(
    municipio = as.factor(municipio),
    uf = as.factor(uf)
  )


#### Tabela com número de analfabetos ####

caminho_alfabetizados <- "~/R/projetos/censo/2022/tabelas/alfabetizados.ods"
alfabetizados <- read_ods(caminho_alfabetizados, sheet = 3)

alfabetizados <- alfabetizados %>% 
  clean_names() %>% 
  tidyr::separate(
    municipio,                   # Coluna que será dividida
    into = c("municipio", "uf"), # Novos nomes das colunas
    sep = " \\(",                # Separador: espaço seguido de parêntese
    remove = TRUE                # Remove a coluna original
  ) %>% 
  mutate(
    uf = str_replace(uf, "\\)", "") # Remove o parêntese final da coluna uf
  ) %>% 
  mutate(
    municipio = as.factor(municipio),
    uf = as.factor(uf)
  )
  

#### Tabela com domicílios particulares permanentes ####

caminho_domicilios <- "~/R/projetos/censo/2022/tabelas/domicilios.ods"
domicilios <- read_ods(caminho_domicilios, sheet = 3)

domicilios <- domicilios %>% 
  clean_names() %>% 
  tidyr::separate(
    municipio,                   # Coluna que será dividida
    into = c("municipio", "uf"), # Novos nomes das colunas
    sep = " \\(",                # Separador: espaço seguido de parêntese
    remove = TRUE                # Remove a coluna original
  ) %>% 
  mutate(
    uf = str_replace(uf, "\\)", "") # Remove o parêntese final da coluna uf
  ) %>% 
  mutate(
    municipio = as.factor(municipio),
    uf = as.factor(uf)
  )


#### Juntar as tabelas ####
# populacao, alfabetizados, domicilios, genero, territorio
cadastro <- municipio_populacao %>% 
  left_join(
    alfabetizados, by = c("nome_do_municipio" = "municipio", "uf" = "uf")
    ) %>% 
  left_join(
    domicilios, by = c("nome_do_municipio" = "municipio", "uf" = "uf")
    ) %>% 
  left_join(
    genero, by = c("nome_do_municipio" = "municipio", "uf" = "uf")
    ) %>% 
  left_join(
    territorio, by = c("mun_id")
  ) %>% 
  select(
    uf_code = cod_uf,
    uf_nome = uf,
    municipio_id = mun_id,
    municipio = nome_do_municipio,
    populacao = total.y,
    alfabetizadas,
    dom_particular = particular,
    homens = homens,
    mulheres = mulheres,
    rginter_code,
    rginter_nome,
    rgimedi_code,
    rgimedi_nome,
    meso_code,
    meso_nome,
    micro_code = micr_code,
    micro_nome = micr_nome
  )


#### Cadastro ####
saveRDS(cadastro, file = "cadastro.RDS")

