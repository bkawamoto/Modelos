# ==============================================================================
# ANÁLISE COM REGRESSÃO QUANTÍLICA — DINÂMICA TEMPORAL (ANO A ANO)
# ------------------------------------------------------------------------
# Amostra: 86 países
# Período: 1990-2023
# Fonte dos dados: Penn World Table 11.0 (pwt110.xlsx) disponível em: 
# https://dataverse.nl/api/access/datafile/554105
# Obs.: É necessário salvar a planilha da PWT 11.0 na pasta do projeto do R
# ==============================================================================

# ==============================================================================
# PACOTES NECESSÁRIOS
# ==============================================================================

pacotes <- c("readxl", "dplyr", "tidyr", "purrr", "ggplot2", 
             "quantreg", "knitr")

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}     

# ==============================================================================
# 1. SELEÇÃO E CONSTRUÇÃO DA AMOSTRA
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

# ==============================================================================
# 2. DEFINIÇÃO DOS ANOS E DOS QUANTIS DA AMOSTRA
# ==============================================================================

anos_amostra    <- 1990:2023
quantis_grafico <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# ==============================================================================
# 3. IMPORTAÇÃO E CONSTRUÇÃO DO PAINEL ANUAL
# ==============================================================================

dados_pwt_bruto <- read_excel("pwt110.xlsx", sheet = "Data") |>
  filter(year %in% anos_amostra) |>
  filter(countrycode %in% mrw_iso3) |>
  select(country, countrycode, year, rgdpna, rnna, pop, hc) |>
  drop_na()

dados_solow_hc <- dados_pwt_bruto |>
  mutate(
    y    = rgdpna / pop,   # PIB por trabalhador (nível anual)
    k    = rnna / pop,     # Capital físico por trabalhador (nível anual)
    h    = hc,             # Índice de capital humano por pessoa (nível anual)
    ln_y = log(y),
    ln_k = log(k),
    ln_h = log(h)
  )

# ==============================================================================
# 4. ESTIMAÇÃO DA REGRESSÃO QUANTÍLICA ANO A ANO
# ==============================================================================

estima_rq_ano <- function(ano_ref, dados) {
  
  df_ano   <- dados |> filter(year == ano_ref)
  n_paises <- n_distinct(df_ano$countrycode)
  
  mod_quant <- tryCatch(
    rq(ln_y ~ ln_k + ln_h, tau = quantis_grafico, data = df_ano),
    error = function(e) NULL
  )
  
  if (is.null(mod_quant)) {
    message("Ano ", ano_ref, ": falha ao estimar a regressão quantílica (N = ",
            n_paises, " países). Ano excluído da análise.")
    return(NULL)
  }
  
  sum_quant <- tryCatch(
    summary(mod_quant, se = "boot", R = 1000),
    error = function(e) {
      message("Ano ", ano_ref, ": bootstrap (se='boot') falhou; usando se='nid'.")
      summary(mod_quant, se = "nid")
    }
  )
  
  list(ano = ano_ref, n_paises = n_paises, sum_quant = sum_quant)
}

set.seed(42)

resultados_anuais <- purrr::map(anos_amostra, estima_rq_ano, dados = dados_solow_hc)
names(resultados_anuais) <- as.character(anos_amostra)
resultados_anuais <- purrr::compact(resultados_anuais)

# ==============================================================================
# 5. RESULTADOS POR ANO (REGRESSÃO QUANTÍLICA)
# ==============================================================================

for (a in names(resultados_anuais)) {
  res <- resultados_anuais[[a]]
  
  cat("\n\n==============================\n")
  cat("===", a, "(N =", res$n_paises, "países) ===\n")
  cat("==============================\n")
  
  cat("\n--- Regressão Quantílica (corte transversal do ano) ---\n")
  print(res$sum_quant)
}

# ==============================================================================
# 6. EXTRAÇÃO DOS COEFICIENTES DE ln_k E ln_h, ANO A ANO
# ==============================================================================

extrai_coef_rq_ano <- function(res) {
  purrr::map_dfr(res$sum_quant, function(s) {
    coefs <- as.data.frame(s$coefficients)
    coefs$termo <- rownames(coefs)
    colnames(coefs) <- c("estimativa", "erro_padrao", "t_valor", "p_valor", "termo")
    coefs$tau   <- s$tau
    coefs$lower <- coefs$estimativa - 1.96 * coefs$erro_padrao
    coefs$upper <- coefs$estimativa + 1.96 * coefs$erro_padrao
    coefs
  }) |>
    filter(termo %in% c("ln_k", "ln_h")) |>
    mutate(ano = res$ano)
}

df_coef_rq_anual <- purrr::map_dfr(resultados_anuais, extrai_coef_rq_ano)

rotula_termo <- function(termo) {
  case_when(
    termo == "ln_k" ~ "Capital Físico (ln_k)",
    termo == "ln_h" ~ "Capital Humano (ln_h)",
    TRUE ~ as.character(termo)
  )
}

df_coef_rq_anual <- df_coef_rq_anual |>
  mutate(
    termo_label   = rotula_termo(termo),
    quantil_label = factor(paste0("q", round(tau * 100)),
                           levels = paste0("q", round(quantis_grafico * 100))),
    significancia = factor(
      ifelse(p_valor < 0.05, "Significativo (p<0,05)", "Não significativo"),
      levels = c("Significativo (p<0,05)", "Não significativo")
    )
  )

# ==============================================================================
# 7. GRÁFICOS POR VARIÁVEL E POR QUANTIL: TRAJETÓRIA ANUAL DOS COEFICIENTES
# ==============================================================================

variaveis_grafico <- list(
  list(termo = "ln_k", titulo = "Capital F\u00EDsico (ln_k)", cor = "#D95F02"),
  list(termo = "ln_h", titulo = "Capital Humano (ln_h)",      cor = "#1B9E77")
)

gera_grafico_var_quantil <- function(termo_selecionado, titulo_variavel, cor_variavel, tau_alvo) {
  
  # Série anual: apenas esta variável, neste quantil.
  dados_anual <- df_coef_rq_anual |>
    filter(termo == termo_selecionado, abs(tau - tau_alvo) < 1e-8)
  
  subtitulo_bruto <- paste0(
    "O = coeficiente significativo | X = coeficiente n\u00E3o significativo (p<0,05) | ",
    "Faixa colorida = IC 95% bootstrap da s\u00E9rie anual"
  )
  
  ggplot(dados_anual, aes(x = ano, y = estimativa)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dotted") +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = cor_variavel, alpha = 0.20, color = NA) +
    geom_line(color = cor_variavel, linewidth = 1) +
    geom_point(
      aes(shape = significancia, color = significancia, fill = significancia),
      size = 2.4, stroke = 1
    ) +
    scale_shape_manual(
      name = "Signific\u00E2ncia (RQ anual)",
      values = c("Significativo (p<0,05)" = 21, "N\u00E3o significativo" = 4)
    ) +
    scale_color_manual(
      name = "Signific\u00E2ncia (RQ anual)",
      values = c("Significativo (p<0,05)" = cor_variavel, "N\u00E3o significativo" = "firebrick")
    ) +
    scale_fill_manual(
      name = "Signific\u00E2ncia (RQ anual)",
      values = c("Significativo (p<0,05)" = "white", "N\u00E3o significativo" = "transparent")
    ) +
    scale_x_continuous(breaks = seq(min(anos_amostra), max(anos_amostra), by = 2)) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x            = element_text(angle = 45, hjust = 1),
      panel.grid.minor       = element_blank(),
      plot.title.position    = "plot",
      plot.caption.position  = "plot",
      plot.title    = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, color = "gray20", lineheight = 1.15,
                                   margin = margin(b = 10)),
      plot.caption  = element_text(size = 8, color = "gray30", hjust = 0,
                                   margin = margin(t = 8)),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
      legend.position = "bottom"
    ) +
    labs(
      subtitle = paste(strwrap(subtitulo_bruto, width = 100), collapse = "\n"),
      x = "Ano", y = "Estimativa do coeficiente",
      caption = paste0(
        "Elabora\u00E7\u00E3o pr\u00F3pria: rq(ln_y ~ ln_k + ln_h) por ano, tau = ", tau_alvo, "."
      )
    )
}

graficos_var_quantil <- list()
for (v in variaveis_grafico) {
  for (tau_alvo in quantis_grafico) {
    chave <- paste0(v$termo, "_q", round(tau_alvo * 100))
    graficos_var_quantil[[chave]] <- gera_grafico_var_quantil(
      termo_selecionado = v$termo,
      titulo_variavel   = v$titulo,
      cor_variavel      = v$cor,
      tau_alvo          = tau_alvo
    )
  }
}

for (chave in names(graficos_var_quantil)) {
  print(graficos_var_quantil[[chave]])
}

