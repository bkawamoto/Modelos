# ==============================================================================
# ANÁLISE EM DADOS DE PAINEL
# ------------------------------------------------------------------------
# Amostra: 86 países
# Período: 1990-2023
# Fonte dos dados: Penn World Table 11.0 (pwt110.xlsx) disponível em: 
# https://dataverse.nl/api/access/datafile/554105
# Obs.: É necessário salvar a PWT 11.0 na pasta do projeto ativo do R
# ==============================================================================

# ==============================================================================
# PACOTES NECESSÁRIOS
# ==============================================================================

pacotes <- c(
  "readxl", "dplyr", "tidyr", "plm", "lmtest",
  "car", "tseries", "corrplot", "ggplot2", "sandwich"
)

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}  

# ==============================================================================
# 1. BASE DE DADOS: PENN WORLD TABLE (PWT 11.0)
# ==============================================================================
df_pwt <- read_excel("pwt110.xlsx", sheet = "Data")

# ==============================================================================
# 2. SELEÇÃO E CONSTRUÇÃO DA AMOSTRA
# ==============================================================================
# Amostra - 86 países
mrw_iso3 <- c(
  "ALB", "ARG", "AUS", "AUT", "BGD", "BEL", "BLZ", "BOL", "BWA", "BRA",
  "BRN", "BGR", "KHM", "CMR", "CAN", "CHL", "CHN", "COL", "CRI", "CIV",
  "DNK", "DOM", "SLV", "SWZ", "ETH", "PHL", "FIN", "FRA", "DEU", "GRC",
  "GTM", "HTI", "HND", "HKG", "MAC", "HUN", "IND", "IRL", "ISR", "ITA",
  "JAM", "JPN", "JOR", "KEN", "LAO", "MDG", "MWI", "MYS", "MDV", "MLI",
  "MEX", "MNG", "MAR", "MMR", "NAM", "NLD", "NZL", "NIC", "NOR", "PAK",
  "PAN", "PRY", "PER", "POL", "PRT", "KOR", "ROU", "SEN", "SGP", "ZAF",
  "ESP", "LKA", "SWE", "CHE", "SYR", "THA", "TTO", "TUN", "TUR", "TZA",
  "GBR", "USA", "URY", "VNM", "ZMB", "ZWE"
)

## 2.1 Extração e construção do painel -----------------------------------------
dados_painel <- df_pwt |>
  filter(year >= 1990 & year <= 2023) |>
  filter(countrycode %in% mrw_iso3) |>
  select(country, countrycode, year, rgdpna, pop, hc, rnna) |>
  drop_na() |>
  mutate(
    ln_y = log(rgdpna / pop),  # Log do PIB per capita
    ln_k = log(rnna / pop),    # Log do Capital Físico per capita
    ln_h = log(hc)             # Log do Capital Humano
  )

## 2.3 Declaração da estrutura de painel ---------------------------------------
p_dados <- pdata.frame(dados_painel, index = c("countrycode", "year"))

# ==============================================================================
# 3. ANÁLISE PRELIMINAR DOS DADOS (ESTATÍSTICA DESCRITIVA)
# ==============================================================================

## 3.1 Matriz de correlação das variáveis do modelo ----------------------------
matriz_correlacao <- dados_painel |>
  select(ln_y, ln_k, ln_h) |>
  cor(use = "complete.obs")

cat("\n--- Matriz de Correlação (ln_y, ln_k, ln_h) ---\n")
print(round(matriz_correlacao, 4))

corrplot(matriz_correlacao,
         method      = "color",
         type        = "upper",
         addCoef.col = "white",
         number.digits = 4,
         tl.col      = "black",
         tl.srt      = 45,
         number.cex  = 0.9,
         mar         = c(0, 0, 1, 0))

## 3.2 Dispersão das variáveis ao longo do tempo -------------------------------
# ln_y - Log do PIB per capita
media_ln_y <- mean(dados_painel$ln_y, na.rm = TRUE)

grafico_pib_tempo <- ggplot(dados_painel, aes(x = year, y = ln_y)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_hline(aes(yintercept = media_ln_y, color = "Média do período"),
             linewidth = 1) +
  scale_color_manual(name = NULL, values = c("Média do período" = "firebrick")) +
  labs(title = "",
       x = "Ano", y = "ln_y (log do PIB per capita)") +
  theme_minimal() +
  theme(legend.position = "top")
print(grafico_pib_tempo)

## 3.3 Distribuição das variáveis do modelo ------------------------------------
# Histograma com curva de densidade sobreposta para cada variável em log.
# ln_y
grafico_dist_ln_y <- ggplot(dados_painel, aes(x = ln_y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", alpha = 0.6, color = "white") +
  geom_density(color = "steelblue", linewidth = 1) +
  labs(title = "",
       x = "ln_y (log do PIB per capita)", y = "Densidade") +
  theme_minimal()
print(grafico_dist_ln_y)

# ln_k
grafico_dist_ln_k <- ggplot(dados_painel, aes(x = ln_k)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "darkorange", alpha = 0.6, color = "white") +
  geom_density(color = "darkorange", linewidth = 1) +
  labs(title = "",
       x = "ln_k (log do capital físico per capita)", y = "Densidade") +
  theme_minimal()
print(grafico_dist_ln_k)

# ln_h
grafico_dist_ln_h <- ggplot(dados_painel, aes(x = ln_h)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "forestgreen", alpha = 0.6, color = "white") +
  geom_density(color = "forestgreen", linewidth = 1) +
  labs(title = "",
       x = "ln_h (log do capital humano)", y = "Densidade") +
  theme_minimal()
print(grafico_dist_ln_h)

## 3.4 Séries temporais por país -----------------------------------------------
# ln_y
media_ln_y_ano <- dados_painel |>
  group_by(year) |>
  summarise(media = mean(ln_y, na.rm = TRUE), .groups = "drop")

grafico_series_ln_y <- ggplot(dados_painel,
                              aes(x = year, y = ln_y, group = countrycode)) +
  geom_line(color = "steelblue", alpha = 0.3, linewidth = 0.4) +
  geom_line(data = media_ln_y_ano, aes(x = year, y = media, group = 1,
                                       color = "Média geral"),
            linewidth = 1.1) +
  scale_color_manual(name = NULL, values = c("Média geral" = "firebrick")) +
  labs(title = "",
       x = "Ano", y = "ln_y (Log do PIB per capita)") +
  theme_minimal() +
  theme(legend.position = "top")
print(grafico_series_ln_y)

# ln_k
media_ln_k_ano <- dados_painel |>
  group_by(year) |>
  summarise(media = mean(ln_k, na.rm = TRUE), .groups = "drop")

grafico_series_ln_k <- ggplot(dados_painel,
                              aes(x = year, y = ln_k, group = countrycode)) +
  geom_line(color = "darkorange", alpha = 0.3, linewidth = 0.4) +
  geom_line(data = media_ln_k_ano, aes(x = year, y = media, group = 1,
                                       color = "Média geral"),
            linewidth = 1.1) +
  scale_color_manual(name = NULL, values = c("Média geral" = "firebrick")) +
  labs(title = "",
       x = "Ano", y = "ln_k (Log do Capital Físico per capita)") +
  theme_minimal() +
  theme(legend.position = "top")
print(grafico_series_ln_k)

# ln_h
media_ln_h_ano <- dados_painel |>
  group_by(year) |>
  summarise(media = mean(ln_h, na.rm = TRUE), .groups = "drop")

grafico_series_ln_h <- ggplot(dados_painel,
                              aes(x = year, y = ln_h, group = countrycode)) +
  geom_line(color = "forestgreen", alpha = 0.3, linewidth = 0.4) +
  geom_line(data = media_ln_h_ano, aes(x = year, y = media, group = 1,
                                       color = "Média geral"),
            linewidth = 1.1) +
  scale_color_manual(name = NULL, values = c("Média geral" = "firebrick")) +
  labs(title = "",
       x = "Ano", y = "ln_h (Log do Capital Humano)") +
  theme_minimal() +
  theme(legend.position = "top")
print(grafico_series_ln_h)

## 3.5 Quadro de estatísticas descritivas --------------------------------------
estatisticas_descritivas <- dados_painel |>
  select(ln_y, ln_k, ln_h) |>
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor") |>
  group_by(Variavel) |>
  summarise(
    N          = n(),
    Media      = mean(Valor, na.rm = TRUE),
    Mediana    = median(Valor, na.rm = TRUE),
    DesvPadrao = sd(Valor, na.rm = TRUE),
    Variancia  = var(Valor, na.rm = TRUE),
    Minimo     = min(Valor, na.rm = TRUE),
    Maximo     = max(Valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric) & !matches("^N$"), \(x) sprintf("%.4f", x)))

cat("\n--- Quadro de Estatísticas Descritivas (variáveis do modelo) ---\n")
print(estatisticas_descritivas)

# ==============================================================================
# 4. ESTIMAÇÃO DOS MODELOS
# ==============================================================================

# A) Modelo de Dados Empilhados (Pooled OLS)
modelo_pooled <- plm(ln_y ~ ln_k + ln_h,
                     data = p_dados, model = "pooling")
summary(modelo_pooled)

# B) Modelo de Efeitos Fixos (Within, com efeitos de país e de ano)
modelo_ef <- plm(ln_y ~ ln_k + ln_h,
                 data = p_dados, model = "within", effect = "twoways")
summary(modelo_ef)

# C) Modelo de Efeitos Aleatórios (Random)
modelo_ea <- plm(ln_y ~ ln_k + ln_h,
                 data = p_dados, model = "random", effect = "twoways")
summary(modelo_ea)

# ==============================================================================
# 5. TESTES DE SELEÇÃO DE MODELOS (ÁRVORE DE DECISÃO)
# ==============================================================================
# Teste F de Chow: Pooled OLS vs. Efeitos Fixos
# H0: o modelo Pooled é adequado (não há heterogeneidade individual/temporal
#     relevante entre os países; os interceptos são iguais para todos)
# H1: o modelo de Efeitos Fixos é preferível (existem efeitos específicos
#     de país e/ou ano não capturados pelo Pooled)
teste_chow <- pFtest(modelo_ef, modelo_pooled)
print(teste_chow)

# Teste de Breusch-Pagan: Pooled OLS vs. Efeitos Aleatórios
# H0: a variância dos efeitos específicos de país é zero (Var(u_i) = 0),
#     ou seja, o modelo Pooled é preferível
# H1: existe variância significativa nos efeitos individuais, favorecendo
#     o modelo de Efeitos Aleatórios
teste_bp <- plmtest(modelo_pooled, type = "bp", effect = "twoways")
print(teste_bp)

# Teste de Hausman: Efeitos Aleatórios vs. Efeitos Fixos
# H0: o estimador de Efeitos Aleatórios é consistente e eficiente (não há
#     correlação entre os efeitos individuais e os regressores) -> EA preferível
# H1: há correlação entre os efeitos individuais e os regressores, o que
#     torna o estimador de EA inconsistente/viesado -> EF preferível
teste_hausman <- phtest(modelo_ef, modelo_ea)
print(teste_hausman)

# ==============================================================================
# 6. VALIDAÇÃO DO MODELO: TESTES DE DIAGNÓSTICO
# ==============================================================================
## 6.1 Dependência transversal (Cross-Sectional Dependence) -------------------
# H0 (comum aos dois testes abaixo): ausência de correlação contemporânea
#     entre os resíduos dos diferentes países (independência transversal)
# H1: existe correlação contemporânea entre os países (choques comuns,
#     contágio, integração econômica etc. não capturados pelo modelo)
# A) Teste CD de Pesaran:
teste_pesaran <- pcdtest(modelo_ef, model = "within", test = "cd")
print(teste_pesaran)

# B) Teste de Frees:
frees_test <- function(modelo, B = 5000, seed = 123) {
  
  if (!inherits(modelo, "plm"))
    stop("O objeto deve ser um modelo da classe 'plm'.")
  
  e   <- residuals(modelo)
  idx <- attr(e, "index")
  if (is.null(idx))
    stop("Nao foi possivel recuperar os indices do painel.")
  
  dados <- data.frame(id = idx[[1]], time = idx[[2]], e = as.numeric(e))
  
  E <- xtabs(e ~ id + time, dados)
  existe <- table(dados$id, dados$time) > 0
  E[!existe] <- NA
  
  # Só usa painel balanceado: remove países com qualquer lacuna e, em seguida,
  # anos com qualquer lacuna (sequencialmente, e não em conjunto como no original,
  # que descartava também anos inteiros por causa de um único país incompleto).
  E <- E[rowSums(is.na(E)) == 0, , drop = FALSE]
  E <- E[, colSums(is.na(E)) == 0, drop = FALSE]
  
  N  <- nrow(E)
  Tt <- ncol(E)
  if (N < 2)  stop("E necessario N >= 2.")
  if (Tt < 3) stop("E necessario T >= 3.")
  
  soma_r2 <- function(M) {
    R <- cor(t(M), method = "spearman")
    sum(R[upper.tri(R)]^2)
  }
  
  Q <- soma_r2(E)
  
  set.seed(seed)
  Q0 <- numeric(B)
  for (b in seq_len(B)) Q0[b] <- soma_r2(matrix(rnorm(N * Tt), N, Tt))
  
  pvalor <- (1 + sum(Q0 >= Q)) / (B + 1)
  
  statistic <- Q
  names(statistic) <- "Soma(r2 Spearman)"
  
  resultado <- list(
    statistic   = statistic,
    p.value     = pvalor,
    method      = paste0("Frees (Spearman), p-valor de Monte Carlo (B = ", B,
                         "; N = ", N, ", T = ", Tt, ")"),
    data.name   = "residuos do modelo",
    alternative = "dependencia transversal"
  )
  class(resultado) <- "htest"
  resultado
}

teste_frees <- frees_test(modelo_ef)
print(teste_frees)

## 6.2 Normalidade dos resíduos -------------------------------------------------
# H0 (ambos os testes): os resíduos seguem distribuição Normal
# H1: os resíduos não seguem distribuição Normal
# A) Teste de Shapiro-Wilk:
teste_shapiro <- shapiro.test(residuals(modelo_ef))
print(teste_shapiro)

# B) Teste de Jarque-Bera:
teste_jarque_bera <- jarque.bera.test(residuals(modelo_ef))
print(teste_jarque_bera)

## 6.3 Heterocedasticidade -------------------------------------------------------
# Teste de Wald Modificado para Heterocedasticidade em Painel:
# H0: homocedasticidade -> a variância do erro é igual entre todos os países
# H1: heterocedasticidade -> a variância do erro difere entre os países
wald_modificado <- function(modelo, min_Ti = 3) {
  
  residuos <- residuals(modelo)
  id <- attr(residuos, "index")[[1]]
  e  <- as.numeric(residuos)
  
  grupos <- split(e, id)
  grupos <- grupos[vapply(grupos, length, integer(1)) >= min_Ti]
  
  Ti      <- vapply(grupos, length, integer(1))
  sigma_i <- vapply(grupos, function(x) mean(x^2), numeric(1))
  V_i     <- vapply(grupos, function(x) {
    sum((x^2 - mean(x^2))^2) / (length(x) * (length(x) - 1))
  }, numeric(1))
  
  validos <- V_i > .Machine$double.eps
  if (sum(validos) < 2)
    stop("Número insuficiente de grupos válidos para realizar o teste.")
  if (length(unique(Ti)) > 1)
    warning("Painel desbalanceado: o teste é assintótico (T grande).")
  
  sigma2 <- sum(unlist(grupos)^2) / sum(Ti)
  W  <- sum((sigma_i[validos] - sigma2)^2 / V_i[validos])
  gl <- sum(validos)
  
  resultado <- list(
    statistic = c(Chisq = W),
    parameter = c(df = gl),
    p.value   = pchisq(W, df = gl, lower.tail = FALSE),
    method    = "Teste de Wald Modificado (Greene) para heterocedasticidade entre grupos",
    data.name = deparse(formula(modelo))
  )
  class(resultado) <- "htest"
  resultado
}

teste_wald <- wald_modificado(modelo_ef)
print(teste_wald)

# Teste de Breusch-Pagan para Heterocedasticidade
# H0: homocedasticidade (variância constante dos resíduos)
# H1: heterocedasticidade (ausência de variância constante dos resíduos)
mod_lm_eq <- lm(ln_y ~ ln_k + ln_h + factor(countrycode) + factor(year), data = dados_painel)
teste_bph <- bptest(mod_lm_eq, ~ ln_k + ln_h, data = dados_painel, studentize = TRUE)
print(teste_bph)

## 6.4 Multicolinearidade ---------------------------------------------------------
# Não é um teste de hipótese formal; o critério de referência da literatura
# é VIF > 5 (ou 10) para indicar multicolinearidade preocupante entre as
# variáveis explicativas.
vif_painel <- vif(lm(ln_y ~ ln_k + ln_h, data = dados_painel))
print(vif_painel)

## 6.5 Autocorrelação serial de primeira ordem -------------------------------------
# H0: ausência de autocorrelação serial de primeira ordem nos resíduos
# H1: presença de autocorrelação serial nos resíduos
teste_wooldridge <- pwartest(modelo_ef)
print(teste_wooldridge)

## 6.6 Forma funcional (Teste RESET de Ramsey) -------------------------------------
# H0: o modelo está corretamente especificado (a forma funcional é adequada,
#     sem termos não lineares relevantes omitidos)
# H1: o modelo está mal especificado (há não linearidades ou variáveis
#     relevantes omitidas que os termos polinomiais capturam)
teste_reset <- resettest(mod_lm_eq, power = 2:3, vcov = vcovHC)
print(teste_reset)

# ==============================================================================
# 7. INFERÊNCIA ESTATÍSTICA COM ERROS-PADRÃO ROBUSTOS
# ==============================================================================
# Erros-padrão robustos a heterocedasticidade, autocorrelação e dependência
# transversal (Driscoll-Kraay)
modelo_driscoll_kraay <- coeftest(modelo_ef, vcov = vcovSCC(modelo_ef))
print(modelo_driscoll_kraay)
