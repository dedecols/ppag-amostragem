
# Exercício de estratificação
# Variável de interesse: quantidade pessoas alfabetizadas
# Variável auxiliar: domicílios
# Variável estratificadora: unidade da federação





#### 1. Tamanho da amostra ####

# Informações para estratificação
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

# Tamanho da amostra
N <- nrow(cadastro)
cv <- 0.15
V <- (cv^2*sum(estratificar$S_dom)^2)
n0 <-  N/V * sum(estratificar$Nh * estratificar$Sh_dom^2)
n <- ceiling(n0/(1 + n0/N))

# Tamanho de cada estrato
alocacao_proporcional <- estratificar %>%
  mutate(
    nh = round(n*Wh,0)
  )

sum(alocacao_proporcional$nh) #mencionar a diferença entre o tamanho da amostra e a soma dos estratos (nh)





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

unidades_amostrais_aes_prop <- sampling::strata(cadastro,
                                stratanames = "uf_nome",
                                size = c(alocacao_proporcional$nh),
                                method = c("srswor")
                                )

amostra_aes_prop <- sampling::getdata(cadastro, unidades_amostrais_aes_prop)
# amostra_aes_prop <- cadastro[unidades_amostrais_aes$ID_unit,]

# Adicionar informação do Nh (usar no survey)
amostra_aes_prop_modificada <- amostra_aes_prop %>%
  left_join(
    estratificar %>% select(uf_nome, Nh), 
    by = "uf_nome"
    )

aes_prop_design <- survey::svydesign(
  ids = ~1,                              # Amostragem aleatória simples
  strata = ~uf_nome,                     # Estratos definidos por uf_nome
  data = amostra_aes_prop_modificada,    # Dados da amostra selecionada
  fpc = ~Nh                              # Tamanho populacional de cada estrato
)

# Os pesos vão aumentando na AES e são iguais na AAS 
# Não entendi o que exatamente esses pesos representam
table(weights(aes_prop_design))
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
estimativas_aes_prop <- survey::svytotal(~alfabetizadas, aes_prop_design)
total_aes_prop <- as.numeric(coef(estimativas_aes_prop))
var_total_aes_prop <- as.numeric(survey::SE(estimativas_aes_prop)^2)
cv_total_aes_prop <- as.numeric(survey::SE(estimativas_aes_prop)/coef(estimativas_aes_prop))*100
ic_total_aes_prop <- confint(estimativas_aes_prop)

resultado_aes_prop <- data.frame(
  estimador = c('Total','CV','IC (Limite Inferior)', 'IC (Limite Superior'),
  valores = c(
    format(total_aes_prop, big.mark = ".", decimal.mark = ","),
    format(round(cv_total_aes_prop,2), big.mark = ".", decimal.mark = ",", nsmall = 2),
    format(ic_total_aes_prop[1], big.mark = ".", decimal.mark = ","),
    format(ic_total_aes_prop[2], big.mark = ".", decimal.mark = ",")
  )
)

# # Media AES
# media_aes_prop <- survey::svymean(~alfabetizadas, aes_prop_design)
# var_media_aes_prop <- SE(media_aes_prop)^2
# cv_media_aes_prop <- as.numeric(SE(media_aes_prop)/coef(media_aes_prop))
# ic_media_aes_prop <- confint(media_aes_prop)





#### 5. Comparação entre os planos ####

# EPA (efeito do plano amostral - design effect)
# Deff < 1: plano amostral estratificado simples é mais eficiente que o plano com amostra aleatória simples
survey::svytotal(~alfabetizadas, aes_prop_design, deff = TRUE)
survey::deff(survey::svytotal(~alfabetizadas, aes_prop_design, deff = TRUE))


