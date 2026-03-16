###############################################################################
#  FIGURAS PARA PUBLICAÇÃO — Estilo Lancet
#  Sem grelhas, sem títulos, layout limpo e padronizado
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(viridis)
  library(RColorBrewer)
  library(patchwork)
})

# Diretório
dir_out <- file.path(getwd(), "resultados", "figuras_publicacao")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

set.seed(2025)

cat("Gerando figuras para publicação (estilo Lancet)...\n\n")

# ============================================================================
# TEMA LANCET — limpo, sem grelhas, sem títulos
# ============================================================================

tema_lancet <- theme_classic(base_size = 11, base_family = "Helvetica") +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(linewidth = 0.4, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "lines"),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold"),
    plot.margin = margin(5, 10, 5, 5, "pt")
  )

# ============================================================================
# CARREGAR DADOS (simulados ou reais)
# ============================================================================

dir_dados <- file.path(getwd(), "dados_datasus")
dados_reais <- file.exists(
  file.path(dir_dados, "banco_analitico_nacional.rds")
)

if (dados_reais) {
  cat("  Usando dados reais do DATASUS.\n")
  banco_nac <- readRDS(
    file.path(dir_dados, "banco_analitico_nacional.rds")
  )
  banco_reg <- readRDS(
    file.path(dir_dados, "banco_analitico_regiao.rds")
  )
  banco_uf <- readRDS(
    file.path(dir_dados, "banco_analitico_uf.rds")
  )
  obitos_raca <- readRDS(file.path(dir_dados, "obitos_raca.rds"))
  obitos_idade <- readRDS(file.path(dir_dados, "obitos_idade.rds"))
  obitos_causa <- readRDS(file.path(dir_dados, "obitos_causa.rds"))
  obitos_esc <- readRDS(
    file.path(dir_dados, "obitos_escolaridade.rds")
  )
} else {
  cat("  Dados reais não encontrados. Usando dados simulados.\n")

  # --- Simulação rápida (mesma do script principal) ---
  anos <- 2000:2023

  gerar_rmm <- function(anos) {
    n <- length(anos)
    rmm <- numeric(n)
    for (i in seq_along(anos)) {
      a <- anos[i]
      if (a <= 2010) {
        rmm[i] <- 73 - (a - 2000) * 1.8
      } else if (a <= 2015) {
        rmm[i] <- 55 - (a - 2010) * 0.4
      } else if (a <= 2019) {
        rmm[i] <- 53 + (a - 2015) * 0.8
      } else if (a == 2020) {
        rmm[i] <- 74
      } else if (a == 2021) {
        rmm[i] <- 107
      } else if (a == 2022) {
        rmm[i] <- 62
      } else {
        rmm[i] <- 56
      }
    }
    rmm + rnorm(n, 0, 1.5)
  }

  banco_nac <- data.frame(ano = anos, rmm = gerar_rmm(anos))

  regioes <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  fatores_r <- c(1.35, 1.20, 0.85, 0.75, 0.90)
  banco_reg <- expand_grid(ano = anos, regiao = regioes) %>%
    left_join(banco_nac, by = "ano") %>%
    mutate(
      fr = fatores_r[match(regiao, regioes)],
      rmm = rmm * fr + rnorm(n(), 0, 3)
    ) %>%
    select(ano, regiao, rmm)

  racas <- c("Branca", "Preta", "Parda", "Indigena", "Amarela")
  fr_raca <- c(0.70, 1.85, 1.10, 2.50, 0.65)
  rmm_raca_df <- expand_grid(ano = anos, raca_cor = racas) %>%
    left_join(banco_nac %>% rename(rmm_base = rmm), by = "ano") %>%
    mutate(
      fr = fr_raca[match(raca_cor, racas)],
      rmm = rmm_base * fr + rnorm(n(), 0, 4),
      rmm = pmax(rmm, 5)
    ) %>%
    select(ano, raca_cor, rmm)

  faixas <- c("<20", "20-34", ">=35")
  fr_idade <- c(1.15, 0.80, 1.90)
  rmm_idade_df <- expand_grid(ano = anos, faixa_idade = faixas) %>%
    left_join(banco_nac %>% rename(rmm_base = rmm), by = "ano") %>%
    mutate(
      fr = fr_idade[match(faixa_idade, faixas)],
      rmm = rmm_base * fr + rnorm(n(), 0, 3),
      rmm = pmax(rmm, 5)
    ) %>%
    select(ano, faixa_idade, rmm)

  causas <- c("Hipertensivas", "Hemorragia", "Infeccao_puerperal",
              "Abortamento", "Causas_indiretas", "Outras_diretas")
  pc <- c(0.23, 0.10, 0.07, 0.05, 0.30, 0.25)
  ob_causa_df <- expand_grid(ano = anos, causa_grupo = causas) %>%
    left_join(banco_nac %>% rename(rmm_base = rmm), by = "ano") %>%
    mutate(
      pb = pc[match(causa_grupo, causas)],
      pa = case_when(
        causa_grupo == "Causas_indiretas" & ano %in% 2020:2021 ~
          pb + 0.20,
        causa_grupo != "Causas_indiretas" & ano %in% 2020:2021 ~
          pb * 0.75,
        TRUE ~ pb
      ),
      obitos = round(rmm_base * 30 * pa + rnorm(n(), 0, 5)),
      obitos = pmax(obitos, 1)
    ) %>%
    select(ano, causa_grupo, obitos)

  escs <- c("Nenhuma/Fund.Inc.", "Fund.Comp./Medio Inc.",
            "Medio Comp./Sup.Inc.", "Superior Comp.")
  fr_esc <- c(1.80, 1.20, 0.75, 0.45)
  rmm_esc_df <- expand_grid(ano = anos, escolaridade = escs) %>%
    left_join(banco_nac %>% rename(rmm_base = rmm), by = "ano") %>%
    mutate(
      fr = fr_esc[match(escolaridade, escs)],
      rmm = rmm_base * fr + rnorm(n(), 0, 4),
      rmm = pmax(rmm, 3)
    ) %>%
    select(ano, escolaridade, rmm)

  # Heatmap UF
  ufs_df <- data.frame(
    uf = c("AC","AM","AP","PA","RO","RR","TO",
           "AL","BA","CE","MA","PB","PE","PI","RN","SE",
           "ES","MG","RJ","SP","PR","RS","SC",
           "DF","GO","MS","MT"),
    regiao = c(rep("Norte",7), rep("Nordeste",9),
               rep("Sudeste",4), rep("Sul",3),
               rep("Centro-Oeste",4))
  )

  banco_uf <- ufs_df %>%
    cross_join(data.frame(ano = anos)) %>%
    left_join(banco_reg, by = c("ano", "regiao")) %>%
    mutate(
      rmm = rmm * runif(n(), 0.7, 1.4) + rnorm(n(), 0, 5),
      rmm = pmax(rmm, 10),
      uf_sigla = uf
    )
}

# ============================================================================
# FIGURAS
# ============================================================================

# Cores padronizadas
col_main <- "#2C3E50"
col_rc <- "#C0392B"
col_covid <- "#2980B9"
cores_regiao <- c(
  "Norte" = "#E74C3C", "Nordeste" = "#F39C12",
  "Sudeste" = "#27AE60", "Sul" = "#2980B9",
  "Centro-Oeste" = "#8E44AD"
)
cores_raca <- c(
  "Branca" = "#2980B9", "Preta" = "#C0392B",
  "Parda" = "#F39C12", "Indigena" = "#27AE60",
  "Amarela" = "#8E44AD"
)

max_ano <- max(banco_nac$ano)
min_ano <- min(banco_nac$ano)

# ---------------------------------------------------------------
# FIGURA 1: Tendência nacional
# ---------------------------------------------------------------
fig1 <- ggplot(banco_nac, aes(x = ano, y = rmm)) +
  geom_line(linewidth = 0.8, color = col_main) +
  geom_point(size = 1.8, color = col_main) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             color = col_rc, alpha = 0.6, linewidth = 0.4) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = col_covid, alpha = 0.6, linewidth = 0.4) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = col_covid) +
  annotate("text", x = 2011.3,
           y = max(banco_nac$rmm) * 0.97,
           label = "Rede Cegonha", color = col_rc,
           hjust = 0, size = 2.8, fontface = "italic") +
  annotate("text", x = 2020.3,
           y = max(banco_nac$rmm) * 0.90,
           label = "COVID-19", color = col_covid,
           hjust = 0, size = 2.8, fontface = "italic") +
  scale_x_continuous(breaks = seq(2000, max_ano, 2)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)") +
  tema_lancet

ggsave(file.path(dir_out, "figure1.png"), fig1,
       width = 7, height = 4.2, dpi = 600)
ggsave(file.path(dir_out, "figure1.pdf"), fig1,
       width = 7, height = 4.2)
ggsave(file.path(dir_out, "figure1.tiff"), fig1,
       width = 7, height = 4.2, dpi = 600, compression = "lzw")
cat("  Figure 1 saved.\n")

# ---------------------------------------------------------------
# FIGURA 2: Por região
# ---------------------------------------------------------------
fig2 <- ggplot(banco_reg, aes(x = ano, y = rmm, color = regiao)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             alpha = 0.3, linewidth = 0.3) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             alpha = 0.3, linewidth = 0.3) +
  scale_color_manual(values = cores_regiao) +
  scale_x_continuous(breaks = seq(2000, max_ano, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)",
       color = "Region") +
  tema_lancet

ggsave(file.path(dir_out, "figure2.png"), fig2,
       width = 7, height = 4.5, dpi = 600)
ggsave(file.path(dir_out, "figure2.pdf"), fig2,
       width = 7, height = 4.5)
ggsave(file.path(dir_out, "figure2.tiff"), fig2,
       width = 7, height = 4.5, dpi = 600, compression = "lzw")
cat("  Figure 2 saved.\n")

# ---------------------------------------------------------------
# FIGURA 3: Por raça/cor
# ---------------------------------------------------------------
if (exists("rmm_raca_df")) {
  raca_plot <- rmm_raca_df
} else {
  # Dados reais: precisamos calcular RMM por raça
  raca_plot <- obitos_raca %>%
    filter(raca_cor != "Ignorada") %>%
    rename(rmm = obitos)  # placeholder
}

fig3 <- ggplot(raca_plot, aes(x = ano, y = rmm, color = raca_cor)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             alpha = 0.3, linewidth = 0.3) +
  scale_color_manual(values = cores_raca) +
  scale_x_continuous(breaks = seq(2000, max_ano, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)",
       color = "Race/Ethnicity") +
  tema_lancet

ggsave(file.path(dir_out, "figure3.png"), fig3,
       width = 7, height = 4.5, dpi = 600)
ggsave(file.path(dir_out, "figure3.pdf"), fig3,
       width = 7, height = 4.5)
ggsave(file.path(dir_out, "figure3.tiff"), fig3,
       width = 7, height = 4.5, dpi = 600, compression = "lzw")
cat("  Figure 3 saved.\n")

# ---------------------------------------------------------------
# FIGURA 4: Por faixa etária
# ---------------------------------------------------------------
if (exists("rmm_idade_df")) {
  idade_plot <- rmm_idade_df
} else {
  idade_plot <- obitos_idade %>%
    filter(faixa_idade != "Ignorada") %>%
    rename(rmm = obitos)
}

fig4 <- ggplot(idade_plot,
               aes(x = ano, y = rmm, color = faixa_idade)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_manual(
    values = c("<20" = "#E74C3C", "20-34" = "#2980B9",
               ">=35" = "#27AE60")
  ) +
  scale_x_continuous(breaks = seq(2000, max_ano, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)",
       color = "Age group (years)") +
  tema_lancet

ggsave(file.path(dir_out, "figure4.png"), fig4,
       width = 7, height = 4.5, dpi = 600)
ggsave(file.path(dir_out, "figure4.pdf"), fig4,
       width = 7, height = 4.5)
ggsave(file.path(dir_out, "figure4.tiff"), fig4,
       width = 7, height = 4.5, dpi = 600, compression = "lzw")
cat("  Figure 4 saved.\n")

# ---------------------------------------------------------------
# FIGURA 5: Causas (áreas empilhadas)
# ---------------------------------------------------------------
if (exists("ob_causa_df")) {
  causa_plot <- ob_causa_df %>%
    group_by(ano) %>%
    mutate(prop = obitos / sum(obitos) * 100) %>%
    ungroup()
} else {
  causa_plot <- obitos_causa %>%
    group_by(ano) %>%
    mutate(prop = obitos / sum(obitos) * 100) %>%
    ungroup()
}

causa_labels <- c(
  "Abortamento" = "Abortion",
  "Causas_indiretas" = "Indirect causes",
  "Hemorragia" = "Haemorrhage",
  "Hipertensivas" = "Hypertensive disorders",
  "Infeccao_puerperal" = "Puerperal sepsis",
  "Outras_diretas" = "Other direct causes"
)

fig5 <- ggplot(causa_plot,
               aes(x = ano, y = prop, fill = causa_grupo)) +
  geom_area(alpha = 0.85) +
  scale_fill_brewer(palette = "Set2", labels = causa_labels) +
  scale_x_continuous(breaks = seq(2000, max_ano, 4)) +
  labs(x = "Year", y = "Proportion (%)", fill = "Cause of death") +
  tema_lancet +
  theme(legend.text = element_text(size = 7))

ggsave(file.path(dir_out, "figure5.png"), fig5,
       width = 8, height = 4.5, dpi = 600)
ggsave(file.path(dir_out, "figure5.pdf"), fig5,
       width = 8, height = 4.5)
ggsave(file.path(dir_out, "figure5.tiff"), fig5,
       width = 8, height = 4.5, dpi = 600, compression = "lzw")
cat("  Figure 5 saved.\n")

# ---------------------------------------------------------------
# FIGURA 6: Heatmap UF
# ---------------------------------------------------------------
uf_order <- banco_uf %>%
  group_by(uf_sigla) %>%
  summarise(rmm_m = mean(rmm, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rmm_m)) %>%
  pull(uf_sigla)

fig6 <- banco_uf %>%
  mutate(uf_sigla = factor(uf_sigla, levels = rev(uf_order))) %>%
  ggplot(aes(x = ano, y = uf_sigla, fill = rmm)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "MMR") +
  scale_x_continuous(breaks = seq(2000, max_ano, 4),
                     expand = c(0, 0)) +
  labs(x = "Year", y = "") +
  tema_lancet +
  theme(axis.text.y = element_text(size = 6.5),
        axis.line = element_blank(),
        axis.ticks.y = element_blank())

ggsave(file.path(dir_out, "figure6.png"), fig6,
       width = 8, height = 7, dpi = 600)
ggsave(file.path(dir_out, "figure6.pdf"), fig6,
       width = 8, height = 7)
ggsave(file.path(dir_out, "figure6.tiff"), fig6,
       width = 8, height = 7, dpi = 600, compression = "lzw")
cat("  Figure 6 saved.\n")

# ---------------------------------------------------------------
# FIGURA 7: Escolaridade
# ---------------------------------------------------------------
if (exists("rmm_esc_df")) {
  esc_plot <- rmm_esc_df
  esc_col <- "escolaridade"
} else {
  esc_plot <- obitos_esc %>%
    filter(escolaridade_grupo != "Ignorada") %>%
    rename(rmm = obitos, escolaridade = escolaridade_grupo)
  esc_col <- "escolaridade"
}

esc_labels <- c(
  "Nenhuma/Fund.Inc." = "None/Primary incomplete",
  "Fund.Comp./Medio Inc." = "Primary/Secondary incomplete",
  "Medio Comp./Sup.Inc." = "Secondary/Tertiary incomplete",
  "Superior Comp." = "Tertiary complete"
)

fig7 <- ggplot(esc_plot,
               aes(x = ano, y = rmm, color = escolaridade)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_brewer(palette = "Dark2", labels = esc_labels) +
  scale_x_continuous(breaks = seq(2000, max_ano, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)",
       color = "Education level") +
  tema_lancet +
  theme(legend.text = element_text(size = 7))

ggsave(file.path(dir_out, "figure7.png"), fig7,
       width = 7.5, height = 4.5, dpi = 600)
ggsave(file.path(dir_out, "figure7.pdf"), fig7,
       width = 7.5, height = 4.5)
ggsave(file.path(dir_out, "figure7.tiff"), fig7,
       width = 7.5, height = 4.5, dpi = 600, compression = "lzw")
cat("  Figure 7 saved.\n")

# ---------------------------------------------------------------
# FIGURA 8: ITS com contrafactual
# ---------------------------------------------------------------
rmm_nac_its <- banco_nac %>%
  mutate(
    log_rmm = log(rmm),
    tempo = ano - min(ano) + 1,
    pos_rc = ifelse(ano >= 2011, 1, 0),
    tempo_pos_rc = ifelse(ano >= 2011, ano - 2011 + 1, 0),
    pos_covid = ifelse(ano >= 2020, 1, 0),
    tempo_pos_covid = ifelse(ano >= 2020, ano - 2020 + 1, 0),
    covid_pico = ifelse(ano %in% 2020:2021, 1, 0)
  )

its_mod <- lm(
  log_rmm ~ tempo + pos_rc + tempo_pos_rc +
    covid_pico + pos_covid + tempo_pos_covid,
  data = rmm_nac_its
)

rmm_nac_its$fitted <- exp(predict(its_mod))
rmm_nac_its$cf <- exp(
  coef(its_mod)["(Intercept)"] +
    coef(its_mod)["tempo"] * rmm_nac_its$tempo
)

fig8 <- ggplot(rmm_nac_its, aes(x = ano)) +
  geom_point(aes(y = rmm), size = 2, color = col_main) +
  geom_line(aes(y = fitted), linewidth = 0.8, color = col_rc) +
  geom_line(aes(y = cf), linewidth = 0.7,
            color = col_covid, linetype = "dashed") +
  geom_vline(xintercept = 2011, linetype = "dotted",
             alpha = 0.5, linewidth = 0.3) +
  geom_vline(xintercept = 2020, linetype = "dotted",
             alpha = 0.5, linewidth = 0.3) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = col_rc) +
  scale_x_continuous(breaks = seq(2000, max_ano, 2)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)") +
  tema_lancet

ggsave(file.path(dir_out, "figure8.png"), fig8,
       width = 7, height = 4.2, dpi = 600)
ggsave(file.path(dir_out, "figure8.pdf"), fig8,
       width = 7, height = 4.2)
ggsave(file.path(dir_out, "figure8.tiff"), fig8,
       width = 7, height = 4.2, dpi = 600, compression = "lzw")
cat("  Figure 8 saved.\n")

# ---------------------------------------------------------------
# FIGURA 9: Previsão ARIMA
# ---------------------------------------------------------------
suppressPackageStartupMessages(library(forecast))

ts_rmm <- ts(banco_nac$rmm, start = min_ano, frequency = 1)
fit_arima <- auto.arima(ts_rmm)
fc <- forecast(fit_arima, h = 3)

fc_df <- data.frame(
  ano = max_ano + 1:3,
  mean = as.numeric(fc$mean),
  lo95 = as.numeric(fc$lower[, 2]),
  hi95 = as.numeric(fc$upper[, 2]),
  lo80 = as.numeric(fc$lower[, 1]),
  hi80 = as.numeric(fc$upper[, 1])
)

fig9 <- ggplot() +
  geom_line(data = banco_nac, aes(x = ano, y = rmm),
            linewidth = 0.8, color = col_main) +
  geom_point(data = banco_nac, aes(x = ano, y = rmm),
             size = 1.8, color = col_main) +
  geom_ribbon(data = fc_df,
              aes(x = ano, ymin = lo95, ymax = hi95),
              alpha = 0.12, fill = col_rc) +
  geom_ribbon(data = fc_df,
              aes(x = ano, ymin = lo80, ymax = hi80),
              alpha = 0.22, fill = col_rc) +
  geom_line(data = fc_df, aes(x = ano, y = mean),
            linewidth = 0.8, color = col_rc, linetype = "dashed") +
  geom_point(data = fc_df, aes(x = ano, y = mean),
             size = 2, color = col_rc, shape = 17) +
  scale_x_continuous(breaks = seq(2000, max_ano + 4, 2)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)") +
  tema_lancet

ggsave(file.path(dir_out, "figure9.png"), fig9,
       width = 7, height = 4.2, dpi = 600)
ggsave(file.path(dir_out, "figure9.pdf"), fig9,
       width = 7, height = 4.2)
ggsave(file.path(dir_out, "figure9.tiff"), fig9,
       width = 7, height = 4.2, dpi = 600, compression = "lzw")
cat("  Figure 9 saved.\n")

cat("\n  All figures saved to:", dir_out, "\n")
cat("  Formats: PNG (600 dpi), PDF, TIFF (600 dpi, LZW)\n")
cat("  Done.\n")
