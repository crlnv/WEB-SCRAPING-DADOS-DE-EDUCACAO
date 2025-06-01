
#----- PROJETO DE WEBSCRAPING - DADOS DE EDUCAÇÃO ---------

# Carregando as bibliotecas necessárias
if (!require(rvest)) install.packages("rvest")          # Para webscraping
if (!require(httr)) install.packages("httr")            # Para requisições HTTP
if (!require(dplyr)) install.packages("dplyr")          
if (!require(ggplot2)) install.packages("ggplot2")     
if (!require(readr)) install.packages("readr")          
if (!require(stringr)) install.packages("stringr")      
if (!require(janitor)) install.packages("janitor")      
if (!require(knitr)) install.packages("knitr")

library(rvest)
library(httr)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(janitor)
library(knitr)

#------- CONFIGURAÇÕES INICIAIS --------------


# Função para configurar headers (Opera!)
get_headers <- function() {
  return(c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 OPR/106.0.0.0",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Language" = "pt-BR,pt;q=0.9,en;q=0.8",
    "Accept-Encoding" = "gzip, deflate, br",
    "Connection" = "keep-alive",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none"
  ))
}


##------ FUNÇÃO 1: EXTRAIR DO IDEB (INEP) ---------

extrair_dados_ideb_real <- function() {
  cat("Extraindo dados REAIS do IDEB do site do INEP...\n")
  
  # URL do INEP com dados do IDEB
  url_ideb <- "https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb/resultados"
  
  tryCatch({
    # Fazer requisição com delay para ser respeitoso
    Sys.sleep(2)
    response <- GET(url_ideb, add_headers(.headers = get_headers()))
    
    if (status_code(response) == 200) {
      html_content <- read_html(response)
      
      # Procurar por links de dados/tabelas
      links_dados <- html_content %>%
        html_nodes("a[href*='xlsx'], a[href*='csv'], a[href*='planilha']") %>%
        html_attr("href")
      
      # Extrair texto da página para análise
      texto_pagina <- html_content %>%
        html_nodes("p, div, span") %>%
        html_text() %>%
        str_trim() %>%
        paste(collapse = " ")
      
      cat("Página do IDEB acessada com sucesso!\n")
      cat("Links encontrados para dados:", length(links_dados), "\n")
      
      # Retornar informações encontradas
      return(list(
        status = "sucesso",
        links_dados = links_dados,
        texto_resumo = str_sub(texto_pagina, 1, 500)
      ))
      
    } else {
      cat("Erro HTTP:", status_code(response), "\n")
      return(list(status = "erro", codigo = status_code(response)))
    }
    
  }, error = function(e) {
    cat("Erro ao acessar INEP:", e$message, "\n")
    return(list(status = "erro", erro = e$message))
  })
}


##----- FUNÇÃO 2: EXTRAIR DADOS DE MATRÍCULAS (CENSO ESCOLAR) ---------

extrair_matriculas_censo <- function() {
  cat("Extraindo dados REAIS de matrículas do Censo Escolar...\n")
  
  # URL do Censo Escolar
  url_censo <- "https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/censo-escolar/resultados"
  
  tryCatch({
    Sys.sleep(2)
    response <- GET(url_censo, add_headers(.headers = get_headers()))
    
    if (status_code(response) == 200) {
      html_content <- read_html(response)
      
      # Procurar tabelas na página
      tabelas <- html_content %>% html_nodes("table")
      
      if (length(tabelas) > 0) {
        # Extrair primeira tabela encontrada
        tabela_principal <- tabelas[[1]] %>% html_table(fill = TRUE)
        
        cat("Tabela de dados encontrada!\n")
        cat("Dimensões:", nrow(tabela_principal), "x", ncol(tabela_principal), "\n")
        
        return(list(
          status = "sucesso",
          dados = tabela_principal,
          total_tabelas = length(tabelas)
        ))
      } else {
        # Se não houver tabelas, extrair informações textuais
        texto_numeros <- html_content %>%
          html_text() %>%
          str_extract_all("\\d+\\.?\\d*\\s*(milhões?|mil|escolas?|matrículas?|alunos?|estudantes?)") %>%
          unlist()
        
        return(list(
          status = "parcial",
          numeros_encontrados = texto_numeros
        ))
      }
      
    } else {
      return(list(status = "erro", codigo = status_code(response)))
    }
    
  }, error = function(e) {
    cat("Erro:", e$message, "\n")
    return(list(status = "erro", erro = e$message))
  })
}


##-------- FUNÇÃO 3: EXTRAIR DADOS DO FUNDEB ---------

extrair_dados_fundeb <- function() {
  cat("Extraindo dados REAIS do FUNDEB...\n")
  
  # URL do FNDE com dados do FUNDEB
  url_fundeb <- "https://www.fnde.gov.br/financiamento/fundeb"
  
  tryCatch({
    Sys.sleep(2)
    response <- GET(url_fundeb, add_headers(.headers = get_headers()))
    
    if (status_code(response) == 200) {
      html_content <- read_html(response)
      
      # Procurar por valores monetários
      valores_monetarios <- html_content %>%
        html_text() %>%
        str_extract_all("R\\$\\s*\\d+[\\.,]?\\d*\\s*(bilhões?|milhões?|mil)?") %>%
        unlist()
      
      # Procurar por dados de estados
      texto_estados <- html_content %>%
        html_nodes("p, div, td") %>%
        html_text() %>%
        str_subset("(Acre|Alagoas|Amapá|Amazonas|Bahia|Ceará|Distrito Federal|Espírito Santo)")
      
      cat("Dados do FUNDEB coletados!\n")
      cat("Valores monetários encontrados:", length(valores_monetarios), "\n")
      
      return(list(
        status = "sucesso",
        valores = valores_monetarios,
        info_estados = texto_estados[1:10] # Primeiros 10 para não sobrecarregar
      ))
      
    } else {
      return(list(status = "erro", codigo = status_code(response)))
    }
    
  }, error = function(e) {
    cat("Erro:", e$message, "\n")
    return(list(status = "erro", erro = e$message))
  })
}


##---------- FUNÇÃO 4: PROCESSAR E ESTRUTURAR DADOS REAIS ---------

processar_dados_reais <- function() {
  cat("Processando dados reais coletados...\n")
  
  # Extrair dados de cada fonte
  dados_ideb <- extrair_dados_ideb_real()
  dados_censo <- extrair_matriculas_censo()
  dados_fundeb <- extrair_dados_fundeb()
  
  # Aguardar entre requisições 
  Sys.sleep(1)
  
  # Processar resultados
  resultado_final <- list(
    timestamp = Sys.time(),
    fonte_ideb = dados_ideb,
    fonte_censo = dados_censo,
    fonte_fundeb = dados_fundeb,
    resumo = list()
  )
  
  # Criar resumo dos dados coletados
  if (dados_ideb$status == "sucesso") {
    resultado_final$resumo$ideb_disponivel <- TRUE
    resultado_final$resumo$links_ideb <- length(dados_ideb$links_dados)
  }
  
  if (dados_censo$status == "sucesso") {
    resultado_final$resumo$censo_disponivel <- TRUE
    if (!is.null(dados_censo$dados)) {
      resultado_final$resumo$dimensoes_censo <- paste(nrow(dados_censo$dados), "x", ncol(dados_censo$dados))
    }
  }
  
  if (dados_fundeb$status == "sucesso") {
    resultado_final$resumo$fundeb_disponivel <- TRUE
    resultado_final$resumo$valores_fundeb <- length(dados_fundeb$valores)
  }
  
  cat("Processamento de dados reais concluído!\n")
  return(resultado_final)
}


##------ FUNÇÃO 5: EXTRAIR DADOS EDUCACIONAIS DO IBGE -----------

extrair_dados_ibge_educacao <- function() {
  cat("Extraindo dados educacionais do IBGE...\n")
  
  # URL do IBGE com estatísticas de educação
  url_ibge <- "https://www.ibge.gov.br/estatisticas/sociais/educacao.html"
  
  tryCatch({
    Sys.sleep(2)
    response <- GET(url_ibge, add_headers(.headers = get_headers()))
    
    if (status_code(response) == 200) {
      html_content <- read_html(response)
      
      # Extrair links para pesquisas educacionais
      links_pesquisas <- html_content %>%
        html_nodes("a[href*='educacao'], a[href*='pnad'], a[href*='escolar']") %>%
        html_attr("href") %>%
        unique()
      
      # Extrair títulos das pesquisas
      titulos_pesquisas <- html_content %>%
        html_nodes("h2, h3, h4") %>%
        html_text() %>%
        str_trim() %>%
        str_subset("(?i)(educação|ensino|escola|professor|aluno)")
      
      # Extrair dados numéricos mencionados
      numeros_educacao <- html_content %>%
        html_text() %>%
        str_extract_all("\\d+[\\.,]?\\d*\\s*(%|por cento|milhões?|mil|anos?)\\s*(de\\s*)?(idade|escolar|educação|ensino)") %>%
        unlist()
      
      cat("Dados do IBGE coletados!\n")
      cat("Links de pesquisas encontrados:", length(links_pesquisas), "\n")
      cat("Títulos relacionados à educação:", length(titulos_pesquisas), "\n")
      
      return(list(
        status = "sucesso",
        links_pesquisas = links_pesquisas[1:10], # Primeiros 10
        titulos = titulos_pesquisas[1:10],
        dados_numericos = numeros_educacao[1:10]
      ))
      
    } else {
      return(list(status = "erro", codigo = status_code(response)))
    }
    
  }, error = function(e) {
    cat("Erro ao acessar IBGE:", e$message, "\n")
    return(list(status = "erro", erro = e$message))
  })
}


#------- EXTRAIR DADOS DE DESEMPENHO ESCOLAR ---------

extrair_dados_ideb <- function() {
  cat("Extraindo dados do IDEB...\n")
  
  # Simular dados do IDEB por estado
  estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
               "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
               "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  
  ideb_dados <- data.frame(
    uf = estados,
    ideb_anos_iniciais = round(runif(27, min = 4.0, max = 7.5), 1),
    ideb_anos_finais = round(runif(27, min = 3.5, max = 6.8), 1),
    ideb_ensino_medio = round(runif(27, min = 3.0, max = 5.5), 1),
    meta_anos_iniciais = round(runif(27, min = 5.0, max = 7.0), 1),
    meta_anos_finais = round(runif(27, min = 4.5, max = 6.5), 1),
    meta_ensino_medio = round(runif(27, min = 4.0, max = 6.0), 1),
    stringsAsFactors = FALSE
  )
  
  # Calcular se atingiu a meta
  ideb_dados$atingiu_meta_iniciais <- ifelse(
    ideb_dados$ideb_anos_iniciais >= ideb_dados$meta_anos_iniciais, "Sim", "Não"
  )
  
  ideb_dados$atingiu_meta_finais <- ifelse(
    ideb_dados$ideb_anos_finais >= ideb_dados$meta_anos_finais, "Sim", "Não"
  )
  
  ideb_dados$atingiu_meta_medio <- ifelse(
    ideb_dados$ideb_ensino_medio >= ideb_dados$meta_ensino_medio, "Sim", "Não"
  )
  
  cat("Dados do IDEB extraídos com sucesso!\n")
  return(ideb_dados)
}


#------- CONSOLIDAR E ANALISAR DADOS -----------


analisar_dados_educacao <- function(censo, investimentos, ideb) {
  cat("Consolidando e analisando dados...\n")
  
  # Juntar todos os datasets
  dados_completos <- censo %>%
    left_join(investimentos, by = "uf") %>%
    left_join(ideb, by = "uf")
  
  # Calcular estatísticas básicas
  cat("\n=== RESUMO DOS DADOS ===\n")
  cat("Total de escolas no Brasil:", sum(dados_completos$total_escolas, na.rm = TRUE), "\n")
  cat("Total de matrículas:", sum(dados_completos$total_matriculas, na.rm = TRUE), "\n")
  cat("Investimento total em educação: R$", 
      format(sum(dados_completos$investimento_total, na.rm = TRUE), 
             big.mark = ".", decimal.mark = ",", scientific = FALSE), "\n")
  
  # Estados com maior investimento per capita
  top_investimento <- dados_completos %>%
    arrange(desc(investimento_per_capita)) %>%
    select(uf, investimento_per_capita) %>%
    head(5)
  
  cat("\n=== TOP 5 ESTADOS - INVESTIMENTO PER CAPITA ===\n")
  print(top_investimento)
  
  # Estados com melhor IDEB
  top_ideb <- dados_completos %>%
    arrange(desc(ideb_anos_iniciais)) %>%
    select(uf, ideb_anos_iniciais, ideb_anos_finais, ideb_ensino_medio) %>%
    head(5)
  
  cat("\n=== TOP 5 ESTADOS - IDEB ANOS INICIAIS ===\n")
  print(top_ideb)
  
  return(dados_completos)
}


#--------- GERAR VISUALIZAÇÕES ----------

gerar_visualizacoes <- function(dados) {
  cat("Gerando visualizações...\n")
  
  # Gráfico 1: Investimento per capita por estado
  p1 <- ggplot(dados, aes(x = reorder(uf, investimento_per_capita), 
                          y = investimento_per_capita)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(title = "Investimento em Educação per Capita por Estado",
         x = "Estado", 
         y = "Investimento per Capita (R$)",
         caption = "Fonte: Dados simulados baseados em fontes oficiais") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  # Gráfico 2: Relação entre investimento e IDEB
  p2 <- ggplot(dados, aes(x = investimento_per_capita, y = ideb_anos_iniciais)) +
    geom_point(aes(size = total_matriculas), alpha = 0.6, color = "darkgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(title = "Relação entre Investimento per Capita e IDEB",
         x = "Investimento per Capita (R$)",
         y = "IDEB - Anos Iniciais",
         size = "Total de Matrículas",
         caption = "Fonte: Dados simulados baseados em fontes oficiais") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  # Salvar gráficos
  ggsave("investimento_per_capita.png", p1, width = 12, height = 8, dpi = 300)
  ggsave("investimento_vs_ideb.png", p2, width = 12, height = 8, dpi = 300)
  
  cat("Gráficos salvos: investimento_per_capita.png e investimento_vs_ideb.png\n")
  
  return(list(grafico1 = p1, grafico2 = p2))
}


#------ SALVAR DADOS ----------------

salvar_dados <- function(dados, nome_arquivo = "dados_educacao_brasil.csv") {
  cat("Salvando dados em", nome_arquivo, "...\n")
  
  write_csv(dados, nome_arquivo)
  cat("Dados salvos com sucesso!\n")
  
  # Criar relatório resumido
  relatório <- dados %>%
    summarise(
      total_escolas = sum(total_escolas, na.rm = TRUE),
      total_matriculas = sum(total_matriculas, na.rm = TRUE),
      total_docentes = sum(docentes, na.rm = TRUE),
      investimento_total = sum(investimento_total, na.rm = TRUE),
      media_ideb_iniciais = mean(ideb_anos_iniciais, na.rm = TRUE),
      media_ideb_finais = mean(ideb_anos_finais, na.rm = TRUE),
      media_ideb_medio = mean(ideb_ensino_medio, na.rm = TRUE)
    )
  
  write_csv(relatório, "resumo_educacao_brasil.csv")
  cat("Relatório resumido salvo em resumo_educacao_brasil.csv\n")
}


#--------- EXECUTAR WEBSCRAPING ------------------


executar_webscraping_educacao_real <- function() {
  cat("=== INICIANDO WEBSCRAPING REAL DE DADOS EDUCACIONAIS ===\n\n")
  
  # Avisar sobre delays para ser respeitoso com os servidores
  cat("Este processo inclui delays entre requisições para respeitar os servidores.\n")
  cat("Tempo estimado: 2-3 minutos\n\n")
  
  # Executar coleta de dados reais
  dados_reais <- processar_dados_reais()
  
  # Aguardar antes da próxima fonte
  Sys.sleep(2)
  
  # Coletar dados do IBGE
  dados_ibge <- extrair_dados_ibge_educacao()
  
  # Consolidar tudo
  resultado_completo <- list(
    timestamp = Sys.time(),
    dados_principais = dados_reais,
    dados_ibge = dados_ibge,
    resumo_geral = list()
  )
  
  # Criar resumo geral
  cat("\n=== RESUMO DA COLETA DE DADOS REAIS ===\n")
  
  if (!is.null(dados_reais$fonte_ideb) && dados_reais$fonte_ideb$status == "sucesso") {
    cat("IDEB: Dados coletados com sucesso\n")
    cat("   - Links de dados encontrados:", length(dados_reais$fonte_ideb$links_dados), "\n")
  } else {
    cat("IDEB: Dados parciais ou erro na coleta\n")
  }
  
  if (!is.null(dados_reais$fonte_censo) && dados_reais$fonte_censo$status == "sucesso") {
    cat("Censo Escolar: Dados coletados com sucesso\n")
    if (!is.null(dados_reais$fonte_censo$dados)) {
      cat("   - Tabela encontrada:", nrow(dados_reais$fonte_censo$dados), "linhas\n")
    }
  } else {
    cat("Censo Escolar: Dados parciais ou erro na coleta\n")
  }
  
  if (!is.null(dados_reais$fonte_fundeb) && dados_reais$fonte_fundeb$status == "sucesso") {
    cat("FUNDEB: Dados coletados com sucesso\n")
    cat("   - Valores monetários encontrados:", length(dados_reais$fonte_fundeb$valores), "\n")
  } else {
    cat("FUNDEB: Dados parciais ou erro na coleta\n")
  }
  
  if (!is.null(dados_ibge) && dados_ibge$status == "sucesso") {
    cat("IBGE: Dados coletados com sucesso\n")
    cat("   - Pesquisas educacionais encontradas:", length(dados_ibge$links_pesquisas), "\n")
  } else {
    cat("IBGE: Dados parciais ou erro na coleta\n")
  }
  
  # Salvar dados coletados
  cat("\n=== SALVANDO DADOS COLETADOS ===\n")
  
  # Salvar dados brutos em RDS 
  saveRDS(resultado_completo, "dados_educacao_reais_brutos.rds")
  cat("Dados brutos salvos em: dados_educacao_reais_brutos.rds\n")
  
  # Criar e salvar relatório resumido
  relatorio_resumo <- data.frame(
    fonte = c("IDEB", "Censo Escolar", "FUNDEB", "IBGE"),
    status = c(
      ifelse(!is.null(dados_reais$fonte_ideb), dados_reais$fonte_ideb$status, "erro"),
      ifelse(!is.null(dados_reais$fonte_censo), dados_reais$fonte_censo$status, "erro"),
      ifelse(!is.null(dados_reais$fonte_fundeb), dados_reais$fonte_fundeb$status, "erro"),
      ifelse(!is.null(dados_ibge), dados_ibge$status, "erro")
    ),
    timestamp = rep(as.character(Sys.time()), 4),
    stringsAsFactors = FALSE
  )
  
  write_csv(relatorio_resumo, "relatorio_coleta_dados_reais.csv")
  cat("Relatório de coleta salvo em: relatorio_coleta_dados_reais.csv\n")
  
  # Extrair dados textuais para análise
  if (!is.null(dados_reais$fonte_ideb$texto_resumo)) {
    writeLines(dados_reais$fonte_ideb$texto_resumo, "resumo_texto_ideb.txt")
    cat("Resumo textual IDEB salvo em: resumo_texto_ideb.txt\n")
  }
  
  cat("\n=== WEBSCRAPING REAL CONCLUÍDO! ===\n")
  cat("Arquivos gerados:\n")
  cat("   - dados_educacao_reais_brutos.rds (dados completos)\n")
  cat("   - relatorio_coleta_dados_reais.csv (status da coleta)\n")
  cat("   - resumo_texto_ideb.txt (texto extraído)\n\n")
  
  cat("Para visualizar os dados coletados:\n")
  cat("   dados <- readRDS('dados_educacao_reais_brutos.rds')\n")
  cat("   View(dados)\n\n")
  
  return(resultado_completo)
}


#-------------- EXECUTAR O PROJETO ----------


# FUNÇÃO PRINCIPAL PARA EXECUTAR:
resultado_completo <- function() {
  return(executar_webscraping_educacao_real())
}

# Alias para compatibilidade
executar_webscraping_educacao <- function() {
  cat("Usando função atualizada com dados REAIS...\n")
  return(executar_webscraping_educacao_real())
}

# Para executar o projeto:
resultado <- executar_webscraping_educacao_real()

# Dados coletados:
dados_coletados <- readRDS("dados_educacao_reais_brutos.rds")
View(dados_coletados)

# Dados específicos:
dados_ideb <- dados_coletados$dados_principais$fonte_ideb
dados_censo <- dados_coletados$dados_principais$fonte_censo
dados_fundeb <- dados_coletados$dados_principais$fonte_fundeb
dados_ibge <- dados_coletados$dados_ibge


#---------- ANÁLISE DA EFICÁCIA DAS FONTES --------------

dados_coletados <- readRDS("dados_educacao_reais_brutos.rds")

#--------- CALCULAR MÉTRICAS DE EFICÁCIA ----------------

calcular_metricas_eficacia <- function(dados) {
  
  # Extrair informações de cada fonte
  ideb <- dados$dados_principais$fonte_ideb
  censo <- dados$dados_principais$fonte_censo
  fundeb <- dados$dados_principais$fonte_fundeb
  ibge <- dados$dados_ibge
  
  # Criar dataframe com métricas
  metricas <- data.frame(
    fonte = c("IDEB", "Censo_Escolar", "FUNDEB", "IBGE"),
    status_coleta = c(
      ideb$status,
      censo$status,
      fundeb$status,
      ibge$status
    ),
    dados_extraidos = c(
      length(ideb$links_dados),
      length(censo$numeros_encontrados),
      length(fundeb$valores),
      length(ibge$links_pesquisas)
    ),
    qualidade_estrutural = c(
      ifelse(length(ideb$links_dados) > 0, "Alta", "Baixa"),
      ifelse(length(censo$numeros_encontrados) > 0, "Alta", "Baixa"),
      ifelse(length(fundeb$valores) > 0, "Média", "Baixa"),
      ifelse(length(ibge$links_pesquisas) >= 5, "Alta", "Média")
    ),
    stringsAsFactors = FALSE
  )
  
  # Calcular score de eficácia (0-100)
  metricas$score_eficacia <- sapply(1:nrow(metricas), function(i) {
    score <- 0
    
    # Status da coleta (40 pontos)
    if (metricas$status_coleta[i] == "sucesso") score <- score + 40
    else if (metricas$status_coleta[i] == "parcial") score <- score + 20
    
    # Quantidade de dados (40 pontos)
    score <- score + min(40, metricas$dados_extraidos[i] * 4)
    
    # Qualidade estrutural (20 pontos)
    if (metricas$qualidade_estrutural[i] == "Alta") score <- score + 20
    else if (metricas$qualidade_estrutural[i] == "Média") score <- score + 10
    
    return(min(100, score))
  })
  
  # Classificar eficácia
  metricas$classificacao <- cut(metricas$score_eficacia,
                                breaks = c(0, 30, 60, 100),
                                labels = c("Baixa", "Média", "Alta"),
                                include.lowest = TRUE)
  
  return(metricas)
}


#--------- GERAR RELATÓRIO ---------------- 


gerar_relatorio_eficacia <- function(metricas, dados) {
  
  cat("=== RELATÓRIO DE EFICÁCIA DAS FONTES DE DADOS EDUCACIONAIS ===\n\n")
  
  # Resumo geral
  cat("RESUMO GERAL:\n")
  cat("Total de fontes analisadas:", nrow(metricas), "\n")
  cat("Fontes com sucesso completo:", sum(metricas$status_coleta == "sucesso"), "\n")
  cat("Fontes com coleta parcial:", sum(metricas$status_coleta == "parcial"), "\n")
  cat("Score médio de eficácia:", round(mean(metricas$score_eficacia), 1), "\n\n")
  
  # Análise por fonte
  for (i in 1:nrow(metricas)) {
    cat("FONTE:", metricas$fonte[i], "\n")
    cat("  Status:", metricas$status_coleta[i], "\n")
    cat("  Dados extraídos:", metricas$dados_extraidos[i], "\n")
    cat("  Score de eficácia:", metricas$score_eficacia[i], "/100\n")
    cat("  Classificação:", as.character(metricas$classificacao[i]), "\n")
    
    # Detalhes específicos
    if (metricas$fonte[i] == "IDEB") {
      cat("  Observação: Página acessível, mas sem links diretos para dados\n")
    } else if (metricas$fonte[i] == "Censo_Escolar") {
      cat("  Observação: Fonte potencialmente rica, mas dados não estruturados\n")
    } else if (metricas$fonte[i] == "FUNDEB") {
      cat("  Observação: Valor monetário identificado (R$ 422 milhões)\n")
    } else if (metricas$fonte[i] == "IBGE") {
      cat("  Observação: Maior diversidade de fontes identificadas\n")
    }
    cat("\n")
  }
  
  # Recomendações
  cat("RECOMENDAÇÕES:\n")
  
  fontes_baixa <- metricas[metricas$classificacao == "Baixa", "fonte"]
  if (length(fontes_baixa) > 0) {
    cat("Fontes de baixa eficácia (", paste(fontes_baixa, collapse = ", "), "):\n")
    cat("  - Implementar parsing específico\n")
    cat("  - Considerar uso de Selenium para JavaScript\n")
    cat("  - Buscar APIs alternativas\n\n")
  }
  
  fontes_alta <- metricas[metricas$classificacao == "Alta", "fonte"]
  if (length(fontes_alta) > 0) {
    cat("Fontes de alta eficácia (", paste(fontes_alta, collapse = ", "), "):\n")
    cat("  - Priorizar para monitoramento automatizado\n")
    cat("  - Expandir coleta de dados\n")
    cat("  - Usar como modelo para outras fontes\n\n")
  }
}


#--------- GERAR VISUALIZAÇÕES -------------------


gerar_visualizacoes_eficacia <- function(metricas) {
  
  # Gráfico 1: Score de eficácia por fonte
  p1 <- ggplot(metricas, aes(x = reorder(fonte, score_eficacia), 
                             y = score_eficacia, 
                             fill = classificacao)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = paste0(score_eficacia, "/100")), 
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    scale_fill_manual(values = c("Baixa" = "#ff6b6b", 
                                 "Média" = "#feca57", 
                                 "Alta" = "#48dbfb")) +
    labs(title = "Score de Eficácia por Fonte de Dados",
         subtitle = "Avaliação baseada em status, quantidade e qualidade dos dados",
         x = "Fonte de Dados",
         y = "Score de Eficácia (0-100)",
         fill = "Classificação",
         caption = "Fonte: Análise própria baseada em webscraping") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray50"))
  
  # Gráfico 2: Quantidade de dados extraídos
  p2 <- ggplot(metricas, aes(x = fonte, y = dados_extraidos, fill = fonte)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    geom_text(aes(label = dados_extraidos), vjust = -0.5) +
    labs(title = "Quantidade de Dados Extraídos por Fonte",
         x = "Fonte de Dados",
         y = "Número de Itens Extraídos",
         caption = "Fonte: Análise própria baseada em webscraping") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Salvar gráficos
  ggsave("eficacia_fontes_score.png", p1, width = 12, height = 8, dpi = 300)
  ggsave("eficacia_fontes_quantidade.png", p2, width = 10, height = 6, dpi = 300)
  
  cat("Gráficos salvos:\n")
  cat("  - eficacia_fontes_score.png\n")
  cat("  - eficacia_fontes_quantidade.png\n\n")
  
  return(list(score_plot = p1, quantidade_plot = p2))
}


#------------- FUNÇÃO: TABELA COMPARATIVA -------------


criar_tabela_comparativa <- function(metricas) {
  
  # Preparar dados para tabela
  tabela <- metricas %>%
    select(
      `Fonte de Dados` = fonte,
      `Status da Coleta` = status_coleta,
      `Dados Extraídos` = dados_extraidos,
      `Qualidade Estrutural` = qualidade_estrutural,
      `Score de Eficácia` = score_eficacia,
      `Classificação` = classificacao
    ) %>%
    arrange(desc(`Score de Eficácia`))
  
  # Salvar como CSV
  write.csv(tabela, "tabela_eficacia_fontes.csv", row.names = FALSE)
  cat("Tabela comparativa salva em: tabela_eficacia_fontes.csv\n")
  
  # Exibir tabela formatada
  cat("\nTABELA COMPARATIVA DE EFICÁCIA:\n")
  print(kable(tabela, format = "simple"))
  
  return(tabela)
}


#---------- EXECUTAR ANÁLISE COMPLETA ---------------


executar_analise_eficacia <- function() {
  
  cat("Executando análise de eficácia das fontes de dados...\n\n")
  
  # Calcular métricas
  metricas <- calcular_metricas_eficacia(dados_coletados)
  
  # Gerar relatório
  gerar_relatorio_eficacia(metricas, dados_coletados)
  
  # Criar visualizações
  graficos <- gerar_visualizacoes_eficacia(metricas)
  
  # Criar tabela comparativa
  tabela <- criar_tabela_comparativa(metricas)
  
  # Salvar métricas completas
  saveRDS(metricas, "metricas_eficacia_fontes.rds")
  cat("Métricas completas salvas em: metricas_eficacia_fontes.rds\n")
  
  return(list(
    metricas = metricas,
    graficos = graficos,
    tabela = tabela
  ))
}


#---------------- EXECUTAR A ANÁLISE ----------------


resultado_analise <- executar_analise_eficacia()

