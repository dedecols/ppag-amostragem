# Exercício de estratificação
# Variável de interesse: quantidade pessoas alfabetizadas
# Variável auxiliar: domicílios
# Variável estratificadora: unidade da federação

library(dplyr)

#### Parâmetros ####
# Variável de interesse: quantidade de pessoas alfabetizadas
# Total, média e coeficiente de variação
parametros <- matrix(
  data = c(sum(cadastro$alfabetizadas),
           mean(cadastro$alfabetizadas),
           sd(cadastro$alfabetizadas)/mean(cadastro$alfabetizadas)),
  nrow = 1,
  ncol = 3
  )
rownames(parametros) <- "Alfabetizados"
colnames(parametros) <- c("Total","Média","CV")

#### Estratificação por UF ####
cadastro <- cadastro %>%
  mutate(npop = as.numeric(n())) %>% 
  #arrange(uf_nome) %>% 
  #group_by(uf_nome) %>% 
  #mutate(fpc_uf = as.numeric(n())) %>%   # quantidade de municipios em cada UF
  filter(uf_code != 53)

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






