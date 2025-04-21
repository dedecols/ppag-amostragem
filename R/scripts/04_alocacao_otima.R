
# Exercício de estratificação
# Variável de interesse: quantidade pessoas alfabetizadas
# Variável auxiliar: domicílios
# Variável estratificadora: unidade da federação



#### 1. Tamanho da amostra ####
estratificar <- cadastro %>%
  group_by(uf_nome)%>%
  summarize(Nh = n(),
            Wh = Nh/nrow(cadastro),
            S_dom = sum(dom_particular),             # variável auxiliar
            Sh_dom = sd(dom_particular),             # variável auxiliar
            S_alfabetizadas = sum(alfabetizadas),    # variável de interesse
            Sh_alfabetizadas = sd(alfabetizadas),    # variável de interesse
            NhSh_dom = Nh*Sh_dom                     # usado no lab4
  )

N <- nrow(cadastro)
cv <- 0.15
V <- (cv^2*sum(estratificar$S_dom)^2)
n0 <-  N/V * sum(estratificar$Nh * estratificar$Sh_dom^2)
n <- n0/(1 + n0/N)

alocacao_otima <- estratificar %>%
  mutate(
    SNhSh = sum(NhSh_dom),
    nh = round(n*NhSh_dom/SNhSh,0)
  )

sum(alocacao_otima$nh)





#### 2. Amostra Aleatória Simples ####

library(sampling)
set.seed(1)

# Ordernar o cadastro pelo nome da UF
cadastro <- cadastro %>% 
  arrange(uf_nome)

# Unidades amostrais selecionadas por AAS
unidades_amostrais_aas <- sampling::srswor(n,N)

# Incluir informação no cadastro
cadastro <- data.frame(
  cbind(cadastro,
        unidades_amostrais_aas)
)

sum(cadastro$unidades_amostrais)

# Amostra Aleatória Simples (dplyr e base R)
amostra_aas <- cadastro %>% 
  filter(unidades_amostrais_aas == 1)

amostra_aas <- sampling::getdata(cadastro, unidades_amostrais_aas)
# amostra_aas <- cadastro[which(unidades_amostrais == 1),]
# amostra_aas <- amostra_aas %>% 
#   select(-unidades_amostrais_aas)

aas_design <- survey::svydesign(
  ids = ~1,                           
  data = amostra_aas,                 # Dados da amostra selecionada
  fpc = rep(N,nrow(amostra_aas))      # Fator de correção populacional (ajusta a variabilidade)
)





#### 3. Amostra Estratificada Simples ####

unidades_amostrais_aes_ney <- sampling::strata(cadastro,
                                                stratanames = "uf_nome",
                                                size = c(alocacao_otima$nh),
                                                method = c("srswor")
)

amostra_aes_ney <- sampling::getdata(cadastro,unidades_amostrais_aes_ney)
# amostra_aes <- cadastro[unidades_amostrais_aes$ID_unit,]

# Adicionar informação do Nh
amostra_aes_ney_modificada <- amostra_aes_ney %>%
  left_join(
    estratificar %>% select(uf_nome, Nh), 
    by = "uf_nome"
  )

aes_ney_design <- survey::svydesign(
  ids = ~1,                            # Amostragem aleatória simples
  strata = ~uf_nome,                   # Estratos definidos por uf_nome
  data = amostra_aes_ney_modificada,   # Dados da amostra selecionada
  fpc = ~Nh                            # Tamanho populacional de cada estrato
)


# Os pesos vão aumentando na AES e são iguais na AAS 
# Não entendi o que exatamente esses pesos representam
table(weights(aes_ney_design))
table(weights(aas_design))





#### 4. Estimativas ####

# Total AAS
estimativas_aas <- survey::svytotal(~alfabetizadas, aas_design)
total_aas <- as.numeric(coef(estimativas_aas))
var_total_aas <- as.numeric(survey::SE(estimativas_aas)^2)
cv_total_aas <- as.numeric(survey::SE(estimativas_aas)/coef(estimativas_aas))*100
ic_total_aas <- confint(estimativas_aas)

resultado_aas <- data.frame(
  estimador = c('Total','CV','IC (Limite Inferior)', 'IC (Limite Superior'),
  valores = c(
    format(total_aas, big.mark = ".", decimal.mark = ","),
    format(round(cv_total_aas,2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    format(ic_total_aas[1], big.mark = ".", decimal.mark = ","),
    format(ic_total_aas[2], big.mark = ".", decimal.mark = ",")
  )
)

# # Média AAS
# media_aas <- survey::svymean(~alfabetizadas, aas_design)
# var_total_aas <- as.numeric(SE(media_aas)^2)
# cv_media_aas <- as.numeric(SE(media_aas)/coef(media_aas))
# ic_media_aas <- confint(media_aas)

# Total AES 
total_aes_ney <- survey::svytotal(~alfabetizadas, aes_ney_design)
var_total_aes_ney <- as.numeric(SE(total_aes_ney)^2)
cv_total_aes_ney <- as.numeric(SE(total_aes_ney)/coef(total_aes_ney))
ic_total_aes_ney <- confint(total_aes_ney)

# Total AES 
estimativas_aes_ney <- survey::svytotal(~alfabetizadas, aes_ney_design)
total_aes_ney <- as.numeric(coef(estimativas_aes_ney))
var_total_aes_ney <- as.numeric(survey::SE(estimativas_aes_ney)^2)
cv_total_aes_ney <- as.numeric(survey::SE(estimativas_aes_ney)/coef(estimativas_aes_ney))*100
ic_total_aes_ney <- confint(estimativas_aes_ney)

resultado_aes_ney <- data.frame(
  estimador = c('Total','CV','IC (Limite Inferior)', 'IC (Limite Superior'),
  valores = c(
    format(total_aes_ney, big.mark = ".", decimal.mark = ","),
    format(round(cv_total_aes_ney,2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    format(ic_total_aes_ney[1], big.mark = ".", decimal.mark = ","),
    format(ic_total_aes_ney[2], big.mark = ".", decimal.mark = ",")
  )
)

# # Media AES
# media_aes_ney <- survey::svymean(~alfabetizadas, aes_prop_design)
# var_media_aes_ney <- as.numeric(SE(media_aes_ney)^2)
# cv_media_aes_ney <- as.numeric(SE(media_aes_ney)/coef(media_aes_ney))
# ic_media_aes_ney <- confint(media_aes_ney)






#### 5. Comparação entre os planos ####

# EPA (efeito do plano amostral - design effect)
survey::svytotal(~alfabetizadas, aes_ney_design, deff = TRUE)
survey::deff(survey::svytotal(~alfabetizadas, aes_ney_design, deff = TRUE))











