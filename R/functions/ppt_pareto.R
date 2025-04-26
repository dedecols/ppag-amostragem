library(dplyr)
library(tibble)
library(ggplto2)
library(gt)
library(gtExtras)


# ---- ORGANIZAÇÃO DO SCRIPT ---- #
# 1. Determinar tamanho da amostra (script separado)
# 2. Estimadores PPT de Pareto
# 3. Análise dos Estimadores e comparação com os Parâmetros 

# Adicionar na parte 3: tabela sobre precisão (variabilidade) e acurácia (viés)
# Falar sobre a distribuição dos estimadores, observando a variabilidade. Isso nos informa sobre a precisão deles. 
# O viés ajuda a entender sobre a acurácia, que nos informa sobre o quão distante esperamos que os estimadores estejam do valor real


#### 1. Determinar o tamanho da amostra ####
# VER DEPOIS COMO CALCULAR PARA PPT DE PARETO

n <- 1106 # (mesmo tamanho da AES)
N <- nrow(cadastro)
data <- cadastro %>% as_tibble()



#### 2. Estimadores PPT de Pareto #####
total_ppt_pareto <- function(cadastro,n,repetir){
  
  N <- nrow(cadastro)
  estimadores_pareto <- numeric(repetir)
  
  for (i in 1:repetir) {
    data <- cadastro %>% 
      dplyr::mutate(
        
        # Gera número aleatório (A_i) uniforme 
        num_aleatorio = stats::runif(N, min = 0, max = 1),
        
        # Calcula a probabilidade desejada de seleção usando a variável auxiliar
        lambda = n*populacao/sum(populacao),
        
        # Número aleatório modificado (depende de A_i e lambda_i)
        num_aleatorio_modificado = (num_aleatorio*(1-lambda))/(lambda*(1-lambda)),
        
        # Usar o peso para criar o desenho da amostra com survey
        peso_amostral = 1/lambda
      ) %>% 
      
      # Ordenar de forma crescente para selecionar a amostra depois
      dplyr::arrange(num_aleatorio_modificado)
    
    # Seleciona as primeiras 'n' unidades para compor a amostra
    amostra <- data %>% 
      dplyr::slice(1:n)
    
    desenho_ppt_pareto <- survey::svydesign(
      id = ~1,
      data = amostra,
      weights = ~peso_amostral
    )
    
    estimadores_pareto[i] <- as.numeric(
      coef(survey::svytotal(~alfabetizadas, desenho_ppt_pareto))
      )
    
  }
  
  return(estimadores_pareto)
}


totais_ppt_pareto <- total_ppt_pareto(cadastro, n = 1106, repetir = 1000)


#### 3. Análise dos estimadores e comparação com os carâmetros ####


# Visualização da distribuição dos estimadores
n <- 1106
repetir_estimadores <- 1000

ggplot2::ggplot(data = tibble::as_tibble(totais_ppt_pareto), 
                ggplot2::aes(totais_ppt_pareto)) +
  ggplot2::geom_histogram(
    color = "black", alpha = 0.4
  ) + 
  ggplot2::labs(
    title = "Distribuição das estimativas com PPT de Pareto",
    subtitle = paste0("Amostra de tamanho ", 
                      format(n, big.mark = ".", decimal.mark = ","), 
                      " e ", 
                      format(repetir_estimadores, big.mark = ".", decimal.mark = ","), 
                      " estimadores"),
    x = "Estimativa do total de alfabetizados",
    y = "Frequência"
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "plain", size = 12),
    axis.title.y = ggplot2::element_text(face = "plain", size = 12),
    axis.text = ggplot2::element_text(face = "plain", size = 11),
    plot.title = ggplot2::element_text(face = "bold", size = 15)
  ) 

resumo <- tibble::tibble(
  Estatísticas = c("Mínimo","Mediana","3ª Quartil", "Máximo", "Média"),
  Total_Estimado = c(
    totais_ppt_pareto %>% min(),
    totais_ppt_pareto %>% median(),
    totais_ppt_pareto %>% quantile(0.75),
    totais_ppt_pareto %>% max(),
    totais_ppt_pareto %>% mean())
  ) %>% 
  dplyr::mutate(
    Total_Estimado = scales::label_comma(
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 1)(Total_Estimado)
    ) %>% 
  gt() %>% 
  gtExtras::gt_theme_538() %>% 
  gt::cols_label(Total_Estimado = "Totais Estimados") %>% 
  gt::tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
resumo

valor_populacional <- cadastro %>% dplyr::pull(alfabetizadas) %>% sum()

# ggplot2::ggplot(data = tibble::as_tibble(totais_ppt_pareto), 
#                 ggplot2::aes(x = totais_ppt_pareto)) +
#   ggplot2::geom_density(fill = "lightblue", alpha = 0.4) +
#   ggplot2::geom_vline(
#     xintercept = valor_populacional, 
#     color = "red", linetype = "dashed", size = 1
#   ) +
#   ggplot2::annotate(
#     "text", x = valor_populacional, y = 0, hjust = -0.1,
#     label = "Valor Populacional",
#     color = "red", fontface = "bold", size = 4
#   ) +
#   ggplot2::labs(
#     title = "Distribuição das estimativas (PPT de Pareto)",
#     x = "Estimativa do total de alfabetizados",
#     y = "Densidade"
#   ) +
#   ggplot2::scale_x_continuous(
#     labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
#   ) +
#   ggplot2::theme_minimal()


# tibble::tibble(
#   Descrição = c("Média das Estimativas", "Valor Populacional", "Viés"),
#   Valor = c(
#     mean(totais_ppt_pareto),
#     valor_populacional,
#     mean(totais_ppt_pareto) - valor_populacional
#   )
# ) %>%
#   dplyr::mutate(
#     Valor = scales::label_comma(big.mark = ".", decimal.mark = ",")(Valor)
#   ) %>%
#   gt::gt() %>%
#   gt::tab_header(title = "Comparação: estimativa x valor real") %>%
#   gtExtras::gt_theme_538()


# Analisando a dispersão 
# tabela com o mínimo, máx, 3º quartil, média e mediana 
# adicionar uma tabela com as informações abaixo
# Desvio-padrão
totais_ppt_pareto %>% sd()

# Variância
totais_ppt_pareto %>% var()

# Coeficiente de variação
totais_ppt_pareto %>% sd()/totais_ppt_pareto %>% mean() * 100

# Viés
total_populacional <- cadastro %>% dplyr::pull(alfabetizadas) %>% sum()
vies <- mean(totais_ppt_pareto)-total_populacional
vies_relativo <- 100 * vies / valor_populacional

# Erro quadrático médio
eqm <- var(totais_ppt_pareto) + (vies)^2

sqrt(eqm)





data <- cadastro %>% 
  dplyr::mutate(
    # Gera número aleatório (A_i) uniforme 
    num_aleatorio = stats::runif(N, min = 0, max = 1),
    # Calcula a probabilidade desejada de seleção usando a variável auxiliar
    lambda = n*populacao/sum(populacao),
    # Número aleatório modificado (depende de A_i e lambda_i)
    num_aleatorio_modificado = (num_aleatorio*(1-lambda))/(lambda*(1-lambda)),
    # Usar o peso para criar o desenho da amostra com survey
    peso_amostral = 1/lambda
  ) %>% 
  # Ordenar de forma crescente para selecionar a amostra depois
  dplyr::arrange(num_aleatorio_modificado)

amostra <- data %>% 
  dplyr::slice(1:n)

desenho_ppt_pareto <- survey::svydesign(
  id = ~1,
  data = amostra,
  weights = ~peso_amostral
)

survey::svytotal(~alfabetizadas, desenho_ppt_pareto)

estimadores_pareto[i] <- as.numeric(
  coef(survey::svytotal(~alfabetizadas, desenho_ppt_pareto))
)

resultado <- survey::svytotal(~alfabetizadas, desenho_ppt_pareto)

cv_alfabetizadas <- 100 * survey::SE(resultado) / coef(resultado)[1]
cv_alfabetizadas

#





