# ==============================================================================
# ANÁLISE COM REGRESSÃO QUANTÍLICA
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
             "quantreg")

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
ordem_periodos  <- as.character(anos_amostra)
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
    y       = rgdpna / pop,   # PIB por trabalhador (nível anual)
    k       = rnna / pop,     # Capital físico por trabalhador (nível anual)
    h       = hc,             # Índice de capital humano por pessoa (nível anual)
    ln_y    = log(y),
    ln_k    = log(k),
    ln_h    = log(h),
    periodo = factor(as.character(year), levels = ordem_periodos)
  )

periodo_referencia <- levels(dados_solow_hc$periodo)[1]

ordem_paises <- sort(unique(dados_solow_hc$countrycode))
dados_solow_hc <- dados_solow_hc |>
  mutate(pais = factor(countrycode, levels = ordem_paises))

pais_referencia <- levels(dados_solow_hc$pais)[1]

# ==============================================================================
# PARTE I — HETEROGENEIDADE AO LONGO DA DISTRIBUIÇÃO
# ==============================================================================
# ==============================================================================
# 4. REGRESSÃO QUANTÍLICA AGRUPADA COM DUMMIES ANUAIS E DE PAÍS
# ==============================================================================

mod_quant_pooled <- rq(
  ln_y ~ ln_k + ln_h + periodo + pais,
  tau  = quantis_grafico,
  data = dados_solow_hc
)

set.seed(42)

sum_quant_pooled <- tryCatch({
  summary(
    mod_quant_pooled, se = "boot", bsmethod = "cluster",
    cluster = dados_solow_hc$countrycode, R = 1000
  )
}, error = function(e) {
  message(
    "Bootstrap por cluster (país) não suportado pela versão do pacote ",
    "quantreg instalada; usando bootstrap padrão (xy), que assume ",
    "observações independentes entre anos dentro do mesmo país. ",
    "Os erros-padrão e testes de Wald abaixo podem estar subestimados ",
    "em relação a uma versão com cluster."
  )
  summary(mod_quant_pooled, se = "boot", R = 1000)
})

cat("\n--- Regressão Quantílica Agrupada (dummies anuais + dummies de país) ---\n")
print(sum_quant_pooled)

# MQO agrupado de referência (com as mesmas dummies de ano e de país)
mod_mqo_painel <- lm(ln_y ~ ln_k + ln_h + periodo + pais, data = dados_solow_hc)
summary(mod_mqo_painel)

# ==============================================================================
# 5. TESTES DE WALD (CONJUNTO E INDIVIDUAL POR PARES DE QUANTIS)
# ==============================================================================

quant_10 <- rq(
  ln_y ~ ln_k + ln_h + periodo + pais,
  tau  = 0.10,
  data = dados_solow_hc
)

quant_25 <- rq(
  ln_y ~ ln_k + ln_h + periodo + pais,
  tau  = 0.25,
  data = dados_solow_hc
)

quant_50 <- rq(
  ln_y ~ ln_k + ln_h + periodo + pais,
  tau  = 0.50,
  data = dados_solow_hc
)

quant_75 <- rq(
  ln_y ~ ln_k + ln_h + periodo + pais,
  tau  = 0.75,
  data = dados_solow_hc
)

quant_90 <- rq(
  ln_y ~ ln_k + ln_h + periodo + pais,
  tau  = 0.90,
  data = dados_solow_hc
)

wald_conj_10_25 <- anova.rq(quant_10, quant_25, test = "Wald", joint = TRUE)
print(wald_conj_10_25)
wald_conj_10_50 <- anova.rq(quant_10, quant_50, test = "Wald", joint = TRUE)
print(wald_conj_10_50)
wald_conj_10_75 <- anova.rq(quant_10, quant_75, test = "Wald", joint = TRUE)
print(wald_conj_10_75)
wald_conj_10_90 <- anova.rq(quant_10, quant_90, test = "Wald", joint = TRUE)
print(wald_conj_10_90)
wald_conj_25_50 <- anova.rq(quant_25, quant_50, test = "Wald", joint = TRUE)
print(wald_conj_25_50)
wald_conj_25_75 <- anova.rq(quant_25, quant_75, test = "Wald", joint = TRUE)
print(wald_conj_25_75)
wald_conj_25_90 <- anova.rq(quant_25, quant_90, test = "Wald", joint = TRUE)
print(wald_conj_25_90)
wald_conj_50_75 <- anova.rq(quant_50, quant_75, test = "Wald", joint = TRUE)
print(wald_conj_50_75)
wald_conj_50_90 <- anova.rq(quant_50, quant_90, test = "Wald", joint = TRUE)
print(wald_conj_50_90)
wald_conj_75_90 <- anova.rq(quant_75, quant_90, test = "Wald", joint = TRUE)
print(wald_conj_75_90)

wald_10_25 <- anova.rq(quant_10, quant_25, test = "Wald", joint = FALSE)
print(wald_10_25)
wald_10_50 <- anova.rq(quant_10, quant_50, test = "Wald", joint = FALSE)
print(wald_10_50)
wald_10_75 <- anova.rq(quant_10, quant_75, test = "Wald", joint = FALSE)
print(wald_10_75)
wald_10_90 <- anova.rq(quant_10, quant_90, test = "Wald", joint = FALSE)
print(wald_10_90)
wald_25_50 <- anova.rq(quant_25, quant_50, test = "Wald", joint = FALSE)
print(wald_25_50)
wald_25_75 <- anova.rq(quant_25, quant_75, test = "Wald", joint = FALSE)
print(wald_25_75)
wald_25_90 <- anova.rq(quant_25, quant_90, test = "Wald", joint = FALSE)
print(wald_25_90)
wald_50_75 <- anova.rq(quant_50, quant_75, test = "Wald", joint = FALSE)
print(wald_50_75)
wald_50_90 <- anova.rq(quant_50, quant_90, test = "Wald", joint = FALSE)
print(wald_50_90)
wald_75_90 <- anova.rq(quant_75, quant_90, test = "Wald", joint = FALSE)
print(wald_75_90)

# ==============================================================================
# 6. TRAJETÓRIA DOS COEFICIENTES DE ln_k E ln_h AO LONGO DOS QUANTIS
# ==============================================================================

extrai_coef <- function(sum_quant) {
  
  df_rq <- purrr::map_dfr(sum_quant, function(s) {
    coefs <- as.data.frame(s$coefficients)
    coefs$termo <- rownames(coefs)
    colnames(coefs) <- c("estimativa", "erro_padrao", "t_valor", "p_valor", "termo")
    coefs$tau <- s$tau
    coefs$lower <- coefs$estimativa - 1.96 * coefs$erro_padrao
    coefs$upper <- coefs$estimativa + 1.96 * coefs$erro_padrao
    coefs$signif <- ifelse(coefs$p_valor < 0.05, "Significativo (5%)", "Não significativo")
    coefs
  })
  
  df_rq |>
    mutate(
      termo_label = case_when(
        termo == "(Intercept)" ~ "Intercepto",
        termo == "ln_k" ~ "Capital Físico (ln_k)",
        termo == "ln_h" ~ "Capital Humano (ln_h)",
        grepl("^pais", termo) ~ paste0("País: ", sub("^pais", "", termo)),
        grepl("^periodo", termo) ~ paste0("Ano: ", sub("^periodo", "", termo)),
        TRUE ~ as.character(termo)
      )
    )
}

df_coef <- extrai_coef(sum_quant_pooled)

termos_principais <- c("Intercepto", "Capital Físico (ln_k)", "Capital Humano (ln_h)")
df_principais <- df_coef |>
  filter(termo_label %in% termos_principais) |>
  mutate(termo_label = factor(termo_label, levels = termos_principais))

grafico_coef_principais <- ggplot(df_principais, aes(x = tau, y = estimativa)) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4, linetype = "dotted") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1F77B4", alpha = 0.20) +
  geom_line(color = "#1F77B4", linewidth = 1) +
  geom_point(
    aes(shape = signif, color = signif, fill = signif),
    size = 3, stroke = 1
  ) +
  scale_shape_manual(name = "Significância",
                     values = c("Significativo (5%)" = 21, "Não significativo" = 4)) +
  scale_color_manual(name = "Significância",
                     values = c("Significativo (5%)" = "#1F77B4", "Não significativo" = "firebrick")) +
  scale_fill_manual(name = "Significância",
                    values = c("Significativo (5%)" = "white", "Não significativo" = "transparent")) +
  facet_wrap(~ termo_label, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = quantis_grafico, labels = paste0("q", round(quantis_grafico * 100))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "gray92"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.4),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 9.5, color = "gray20", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray30", hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    # title = "Trajetória dos Coeficientes da Regressão Quantílica Agrupada — 1990-2023",
    subtitle = "Faixa azul = IC 95% (bootstrap) da RQ | Controla por dummies anuais e de país",
    x = "Quantil (tau)", y = "Estimativa do Coeficiente",
    caption = "Elaboração própria a partir de rq(ln_y ~ ln_k + ln_h + periodo + pais) e summary(se='boot')."
  )

print(grafico_coef_principais)
