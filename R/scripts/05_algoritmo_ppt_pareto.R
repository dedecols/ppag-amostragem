library(dplyr)
library(tibble)
library(ggplot2)

#### Introdução ####

n <- 1106 # (mesmo tamanho da AES)
N <- nrow(cadastro)
data <- cadastro %>% as_tibble()

# Na amostragem por PPT temos que ter uma variável auxiliar, correlacionada com a variável de interesse
# Variável de interesse: quantidade de pessoas na unidade amostral (município)
# Variável auxiliar: quantidade de pessoas alfabetizadas na unidade amostral (município)

# ATENÇÃO: pegar tabela anterior para comprovar a correlação entre a variável auxiliar e a variável de interesse - procurar no sidra ou algo assim

# Variáveis disponíveis no cadastro
# cor(data$alfabetizadas,data$populacao)
# cor(data$alfabetizadas,data$dom_particular)

# Duas variáveis possíveis: 
# X: total populacional
# X: total de domicílios 


#### Algoritmo PPT Pareto ####
# 1. Gerar N números aleatórios 
# 2. Calcular lambda (probabilidade de inclusão desevável)
# 3. Calcular o número aleatório modificado
# 4. Ordernar as unidades de forma crescente segundo o número aleatório modificado
# 5. Selecionar para a amostra as n unidades com menores valores do número aleatório modificado
set.seed(rnorm(1))
data <- data %>% 
  dplyr::mutate(
    num_aleatorio = stats::runif(N, min = 0, max = 1),
    lambda = n*populacao/sum(populacao),
    num_aleatorio_modificado = (num_aleatorio*(1-lambda))/(lambda*(1-lambda)),
    peso_amostral = 1/lambda
    ) %>% 
  dplyr::arrange(num_aleatorio_modificado)

amostra <- data %>% 
  dplyr::slice(1:n)

summary(amostra$lambda)

# Quantil (ponto)
# Quartil (intervalo)
q1 <- quantile(amostra$lambda, 0.25)
q2 <- median(amostra$lambda)
q3 <- quantile(amostra$lambda, 0.75)
# iqr <- q3-q1
# limite_inferior <- q1 - 1.5*iqr
# limite_superior <- q3 + 1.5*iqr

amostra <- amostra %>% 
  dplyr::mutate(
    quartil = dplyr::case_when(
      (lambda <= q1) ~ "Do mínimo até 25%",
      (lambda > q1) & (lambda <= q2) ~ "Entre 25% e 50%",
      (lambda > q2) & (lambda <= q3) ~ "Entre 50% e 75%",
      (lambda > q3) ~ "De 75% até o máximo"
    ),
    quartil = factor(quartil, levels = c(
      "Do mínimo até 25%", 
      "Entre 25% e 50%", 
      "Entre 50% e 75%", 
      "De 75% até o máximo"
    ))
  )

# Summary
library(skimr)

skim(amostra$lambda)

# Tabela com resumo por faixas
library(gt)
library(gtExtras)
library(gtsummary)

min <- min(amostra$lambda)
q1 <- quantile(amostra$lambda, 0.25)
q2 <- median(amostra$lambda)
q3 <- quantile(amostra$lambda, 0.75)
max <- max(amostra$lambda)

intervalos <- tibble::tibble(
  quartil = factor(c(
    "Do mínimo até 25%",
    "Entre 25% e 50%",
    "Entre 50% e 75%",
    "De 75% até o máximo"
  ), levels = c(
    "Do mínimo até 25%", "Entre 25% e 50%",
    "Entre 50% e 75%", "De 75% até o máximo"
  )),
  inferior = c(min, q1, q2, q3),
  superior = c(q1, q2, q3, max)
  )

# título de probabilidade de inclusão desejada
faixas <- amostra %>%
  dplyr::group_by(quartil) %>%
  dplyr::summarize(Count = n(), .groups = "drop") %>%
  dplyr::left_join(intervalos, by = "quartil") %>%
  dplyr::mutate(
    Intervalo = paste0("[", round(inferior, 4), ", ", round(superior, 4), "]")
  ) %>%
  dplyr::select(quartil, Intervalo, Count) %>% 
  dplyr::rename(" " = quartil,
                "Lambda Range" = Intervalo,
                "Quant." = Count)

gt_preview(faixas)


# ggplot2::ggplot(amostra, ggplot2::aes(x = lambda, fill = quartil)) +
#   ggplot2::geom_histogram(
#     color = "black",
#     linewidth = 0.3
#   ) +
#   RColorBrewer::display.brewer.all() +
#   ggplot2::labs(
#     x = "Log da Probabilidade de Inclusão Desejada",
#     y = "Frequência"
#   ) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(
#     axis.title.x = ggplot2::element_text(face = "plain", size = 12),
#     axis.title.y = ggplot2::element_text(face = "plain", size = 12)
#   )

ggplot2::ggplot(amostra, ggplot2::aes(x = lambda, fill = quartil)) +
  ggplot2::geom_histogram(
    color = "black",
    linewidth = 0.3
  ) +
  ggplot2::scale_x_log10() +
  RColorBrewer::display.brewer.all() +
  ggplot2::labs(
    x = "Log da Probabilidade de Inclusão Desejada",
    y = "Frequência"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "plain", size = 12),
    axis.title.y = ggplot2::element_text(face = "plain", size = 12)
  )




#### Estimativas ####

desenho <- survey::svydesign(
  id = ~1,
  data = amostra,
  weights = ~peso_amostral
)

survey::svytotal(~alfabetizadas, desenho)



#### Dados populacionais ####

# Parâmetros para variável de interesse: número de pessoas alfabetizadas
# total, variância

alfabetizados_resumo <- dplyr::bind_rows(
  cadastro %>% dplyr::group_by(uf_nome) %>% dplyr::summarize(Alfabetizados = sum(alfabetizadas)),
  cadastro %>% dplyr::summarize(uf_nome = "BRASIL", Alfabetizados = sum(alfabetizadas))
) %>% tibble::as_tibble()





