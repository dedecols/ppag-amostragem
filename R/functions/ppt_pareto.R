n <- 1106 # (mesmo tamanho da AES)
N <- nrow(cadastro)
data <- cadastro %>% as_tibble()

total_ppt_pareto <- function(cadastro,n,repetir){
  
  N <- nrow(cadastro)
  estimadores_pareto <- numeric(repetir)
  
  for (i in 1:repetir) {
    data <- cadastro %>% 
      dplyr::mutate(
        num_aleatorio = stats::runif(N, min = 0, max = 1),
        lambda = n*populacao/sum(populacao),
        num_aleatorio_modificado = (num_aleatorio*(1-lambda))/(lambda*(1-lambda)),
        peso_amostral = 1/lambda
      ) %>% 
      dplyr::arrange(num_aleatorio_modificado)
    
    amostra <- data %>% 
      dplyr::slice(1:n)
    
    desenho_ppt_pareto <- survey::svydesign(
      id = ~1,
      data = amostra,
      weights = ~peso_amostral
    )
    
    estimadores_pareto[i] <- as.numeric(coef(survey::svytotal(~alfabetizadas, desenho_ppt_pareto)))
    
  }
  
  return(estimadores_pareto)
}


totais_ppt_pareto <- total_ppt_pareto(cadastro, n = 1106, repetir = 1000)

ggplot2::ggplot(data = tibble::as_tibble(totais_ppt_pareto), ggplot2::aes(totais_ppt_pareto)) +
  ggplot2::geom_histogram(
    color = "black", alpha = 0.4
  ) + 
  ggplot2::labs(
    title = "Distribuição das estimativas com PPT de Pareto",
    subtitle = "Amostra de tamanho 1.106 e 1.000 estimadores",
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

sum(cadastro$alfabetizadas)

