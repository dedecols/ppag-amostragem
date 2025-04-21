#### Pacotes ####
library(dplyr)
library(janitor)
library(readr)

#### Informações da base ####
# 338 fazendas produtoras de cana-de-açúcar
# area: área plantada com cana
# quant: quantidade colhida de cana
# receita e despesa: valor em reais
# regiao e classe: variáveis de contexto sobre as fazendas

fazendas <- read_delim("~/ENCE/2024.1/Amostragem/Fazendas.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

fazendas <- fazendas %>% 
  janitor::clean_names()

fazendas <- fazendas %>% mutate(count = n())
N <- nrow(fazendas)
n <- 32

# Plano 1 - estratificação por região e alocação proporcional
fazendas <- fazendas %>% 
  arrange(regiao) %>% 
  group_by(regiao) %>% 
  mutate(fpc_regiao = n())

table(fazendas$regiao)
table(fazendas$regiao,fazendas$fpc_regiao)

#### Planos 1 e 2 ####
# Alocação proporcional e ótima - variável AREA como auxiliar
fazendas_alloc12 <- fazendas %>% 
  group_by(regiao)%>%
  summarize(Nh = n(), 
            Wh = Nh/N, 
            SAREA = sum(area),        # variavel auxiliar
            Sh = sd(area),            # variavel auxiliar
            NhSh = Nh*Sh,             # variavel auxiliar
            S2hQ = var(quant),        # variável de interesse 1
            S2hR = var(receita),      # variável de interesse 2
            SQUANT = sum(quant),      # variável de interesse 1
            SRECEITA = sum(receita)   # variável de interesse 2
            ) %>%
    mutate(nh.p1 = round((n*Wh),0), 
           SNhSh=sum(NhSh),
           nh.p2 = round((n*NhSh/SNhSh),0)
           )

#### Parâmetros para quant e receita ####
# Total, Média e CV (depende da variância)
# variável 1: quant
quant_total <- sum(fazendas$quant)
quant_media <- mean(fazendas$quant)
quant_dp <- sd(fazendas$quant)
quant_cv <- (quant_dp/quant_media)*100

receita_total <- sum(fazendas$receita)
receita_media <- mean(fazendas$receita)
receita_dp <- sd(fazendas$receita)
receita_cv <- (receita_dp/receita_media)*100

resultados <- data.frame(
  Variável = c("quant", "receita"),
  Total = c(quant_total, receita_total),
  Média = c(quant_media, receita_media),
  Desvio_Padrão = c(quant_dp, receita_dp),
  CV = c(quant_cv, receita_cv)
)











