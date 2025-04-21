library(dplyr)

# Usado para calcular o tamanho da amostra
cadastro <- cadastro %>%
  mutate(npop = as.numeric(n())) %>% 
  #arrange(uf_nome) %>% 
  #group_by(uf_nome) %>% 
  #mutate(fpc_uf = as.numeric(n())) %>%   # quantidade de municipios em cada UF
  filter(uf_code != 53) %>% 
  dplyr::arrange(uf_nome)

estratificar <- cadastro %>%
  dplyr::group_by(uf_nome)%>%
  dplyr::summarize(Nh = n(),
            Wh = Nh/nrow(cadastro),
            S_dom = sum(dom_particular),             # variável auxiliar
            Sh_dom = sd(dom_particular),             # variável auxiliar
            S_alfabetizadas = sum(alfabetizadas),    # variável de interesse
            Sh_alfabetizadas = sd(alfabetizadas),    # variável de interesse
            NhSh_dom = Nh*Sh_dom                     # usado no lab4
  )

# Tamanho da população
N <- nrow(cadastro)

# Tamanho da amostra
cv <- 0.15
V <- (cv^2*sum(estratificar$S_dom)^2)
n0 <-  N/V * sum(estratificar$Nh * estratificar$Sh_dom^2)
n <- ceiling(n0/(1 + n0/N))

comparar_estimadores <- function(cadastro,n,N,alocacao,repetir){
  
  # Vetores para armazenar os resultados
  estimadores_aas <- numeric(repetir)
  estimadores_aes <- numeric(repetir)
  
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | Tempo restante: :eta",  # Formato da barra
    total = repetir,                                    # Total de iterações
    width = 60                                          # Largura da barra
  )
  
  for (i in 1:repetir) {
    
    # Atualiza progresso a cada iteração
    pb$tick()
    
    # 1. Estimadores de um plano de Amostragem Aleatória Simples
    
    # 1.1. Seleciona AAS
    amostra_aas <- sampling::getdata(cadastro,   
                                     sampling::srswor(n,N))
    
    # 1.2. Desenho AAS
    aas_design <- survey::svydesign(
      ids = ~1,                           
      data = amostra_aas,                 # Dados da amostra selecionada
      fpc = rep(N,nrow(amostra_aas))      # Fator de correção populacional (ajusta a variabilidade)
    )
    
    # 1.3. Calcula o total e armazena no vetor
    total_aas <- as.numeric(coef(survey::svytotal(~alfabetizadas, aas_design)))    
    estimadores_aas[i] <- total_aas
    
    
    # 2. Estimadores de um plano de Amostragem Estratificada Simples
    
    if (alocacao == 'proporcional') {
      
      alocacao_proporcional <- estratificar %>% 
        dplyr::mutate(nh = round(n*Wh,0)) %>% 
        dplyr::arrange(uf_nome)
      
      # 2.1. Alocação proporcional
      # 2.1.1. Seleção da AES 
      # Usa AAS dentro dos estratos
      # Tamanho amostrais dos estratos determinados previamente 
      unidades_amostrais_aes_prop <- sampling::strata(cadastro,
                                                      stratanames = "uf_nome",
                                                      size = c(alocacao_proporcional$nh), 
                                                      method = c("srswor")
      )
      
      # 2.1.2. Extrai as unidades amostrais do cadastro
      amostra_aes_prop <- sampling::getdata(cadastro, unidades_amostrais_aes_prop)
      
      # 2.1.3. Adiciona as informações sobre o tamanho populacional do estratos
      # Usar para o desenho da amostra
      amostra_aes_prop_modificada <- amostra_aes_prop %>%
        left_join(estratificar %>% select(uf_nome, Nh), by = "uf_nome")
      
      # 2.1.4. Desenho AES com alocação proporcional
      aes_prop_design <- survey::svydesign(
        ids = ~1,                              # Amostragem aleatória simples
        strata = ~uf_nome,                     # Estratos definidos por uf_nome
        data = amostra_aes_prop_modificada,    # Dados da amostra selecionada
        fpc = ~Nh                              # Tamanho populacional de cada estrato
      )
      
      # 2.1.5. Estimadores do total AES com alocação proporcional
      estimadores_aes[i] <- as.numeric(coef(survey::svytotal(~alfabetizadas, aes_prop_design)))
      
    } else if (alocacao == 'otima') {
      
      alocacao_otima <- estratificar %>% 
        dplyr::mutate(SNhSh = sum(NhSh_dom), 
                      nh = round(n*NhSh_dom/SNhSh,0)) %>% 
        dplyr::arrange(uf_nome)
      
      # 2.2. Alocação ótima
      # 2.2.1. Seleção da AES
      unidades_amostrais_aes_oti <- sampling::strata(cadastro,
                                                     stratanames = "uf_nome",
                                                     size = c(alocacao_otima$nh),
                                                     method = c("srswor")
      )
      
      amostra_aes_oti <- sampling::getdata(cadastro,unidades_amostrais_aes_oti)
      
      amostra_aes_oti_modificada <- amostra_aes_oti %>%
        left_join(estratificar %>% select(uf_nome, Nh), by = "uf_nome")
      
      aes_oti_design <- survey::svydesign(
        ids = ~1,                            # Amostragem aleatória simples
        strata = ~uf_nome,                   # Estratos definidos por uf_nome
        data = amostra_aes_oti_modificada,   # Dados da amostra selecionada
        fpc = ~Nh                            # Tamanho populacional de cada estrato
      )
      
      estimadores_aes[i] <- as.numeric(coef(survey::svytotal(~alfabetizadas, aes_oti_design)))
      
    } else {
      stop("Erro: o parâmetro 'alocacao' deve ser preenchido com 'proporcional' ou 'otimo'.")
    }
  }
  return(list(AAS = estimadores_aas,
              AES = estimadores_aes))
}

# resultado <- comparar_estimadores(cadastro, n, N, alocacao = 'otima', repetir = 2000)
str(resultado)


# Transforma os resultados em um data frame longo
distribuicoes <- data.frame(
  Estimativa = c(resultado$AAS, resultado$AES),
  Plano = rep(c("AAS", "AES"), each = length(resultado$AAS))
)

# Criando o histograma sobreposto
grafico <- ggplot2::ggplot(distribuicoes, ggplot2::aes(x = Estimativa, 
                                            fill = Plano, 
                                            y = ..density..)) +
  ggplot2::geom_histogram(position = "identity", alpha = 0.6, bins = 30) +  
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Comparação das Distribuições dos Estimadores",
    x = "Estimativa da População Alfabetizada",
    y = "Densidade",
    fill = "Plano Amostral"
  ) +
  ggplot2::scale_fill_manual(values = c("AAS" = "#E69F00",
                                        "AES" = "#0072B2")) +
  
  ggplot2::scale_x_continuous(labels = scales::label_number(
    big.mark = ".", decimal.mark = ","
  )) +
  ggplot2::scale_y_continuous(labels = scales::label_scientific())

savedir <- 'C:/Users/a_les/Documents/R/projetos/censo/relatorio_anexos/'

ggplot2::ggsave(paste0(savedir,'comparacao_planos.jpg'), plot = grafico)
ggplot2::ggsave(paste0(savedir,'comparacao_planos.pdf'), plot = grafico)
ggplot2::ggsave(paste0(savedir,'comparacao_planos.svg'), plot = grafico)





























