library(dplyr)
library(tibble)
library(ggplto2)
library(gt)
library(gtExtras)
library(kableExtra)

# ---- ORGANIZAÇÃO DO SCRIPT ---- #
# 1. Determinar tamanho da amostra (script separado)
# 2. Estimadores PPT de Pareto
# 3. Análise dos Estimadores
# 4. Comparação com os Parâmetros 
# ---- ORGANIZAÇÃO DO SCRIPT ---- #


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


#### 3. Análise dos Estimadores ####

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

# minimo
# mediana
# 3 quartil
# maximo

class(totais_ppt_pareto)
str(totais_ppt_pareto)

# Coluna - Totais produzidos pela amostragem PPT de Pareto
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
  gtExtras::gt_theme_nytimes() %>% 
  cols_label(Total_Estimado = "Totais Estimados") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
resumo



#### 4. Comparação com os Parâmetros  ####

cadastro %>% pull(alfabetizadas) %>% sum()
cadastro %>% pull(alfabetizadas) %>% summary()



