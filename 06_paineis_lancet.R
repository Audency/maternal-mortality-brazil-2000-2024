###############################################################################
#  PAINÉIS COMPOSTOS PARA LANCET — Máximo 4 figuras principais
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(viridis)
  library(sf)
  library(geobr)
  library(spdep)
  library(forecast)
  library(nlme)
})

dir_out <- file.path(getwd(), "resultados", "figuras_publicacao")
set.seed(2025)

tema_lancet <- theme_classic(base_size = 9, base_family = "Helvetica") +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7, color = "black"),
    axis.line = element_line(linewidth = 0.4),
    legend.position = "bottom",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "lines"),
    panel.grid = element_blank(),
    plot.margin = margin(3, 5, 3, 3, "pt"),
    plot.tag = element_text(size = 10, face = "bold")
  )

cat("Generating composite panels for Lancet...\n\n")

# ============================================================================
# SIMULATED DATA (same seed as main script)
# ============================================================================
anos <- 2000:2023
gerar_rmm <- function(anos) {
  n <- length(anos); rmm <- numeric(n)
  for (i in seq_along(anos)) {
    a <- anos[i]
    if (a <= 2010) rmm[i] <- 73 - (a - 2000) * 1.8
    else if (a <= 2015) rmm[i] <- 55 - (a - 2010) * 0.4
    else if (a <= 2019) rmm[i] <- 53 + (a - 2015) * 0.8
    else if (a == 2020) rmm[i] <- 74
    else if (a == 2021) rmm[i] <- 107
    else if (a == 2022) rmm[i] <- 62
    else rmm[i] <- 56
  }
  rmm + rnorm(n, 0, 1.5)
}

banco_nac <- data.frame(ano = anos, rmm = gerar_rmm(anos))

regioes <- c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")
fr <- c(1.35, 1.20, 0.85, 0.75, 0.90)
banco_reg <- expand_grid(ano = anos, regiao = regioes) %>%
  left_join(banco_nac, by = "ano") %>%
  mutate(rmm = rmm * fr[match(regiao, regioes)] + rnorm(n(), 0, 3)) %>%
  select(ano, regiao, rmm)

racas <- c("Branca","Preta","Parda","Indigena","Amarela")
fr_r <- c(0.70, 1.85, 1.10, 2.50, 0.65)
rmm_raca <- expand_grid(ano = anos, raca = racas) %>%
  left_join(banco_nac %>% rename(b = rmm), by = "ano") %>%
  mutate(rmm = b * fr_r[match(raca, racas)] + rnorm(n(), 0, 4),
         rmm = pmax(rmm, 5)) %>%
  select(ano, raca, rmm)

faixas <- c("<20","20-34",">=35")
fr_i <- c(1.15, 0.80, 1.90)
rmm_idade <- expand_grid(ano = anos, faixa = faixas) %>%
  left_join(banco_nac %>% rename(b = rmm), by = "ano") %>%
  mutate(rmm = b * fr_i[match(faixa, faixas)] + rnorm(n(), 0, 3),
         rmm = pmax(rmm, 5)) %>%
  select(ano, faixa, rmm)

col_main <- "#2C3E50"; col_rc <- "#C0392B"; col_covid <- "#2980B9"
cores_reg <- c("Norte"="#E74C3C","Nordeste"="#F39C12",
               "Sudeste"="#27AE60","Sul"="#2980B9",
               "Centro-Oeste"="#8E44AD")
cores_raca <- c("Branca"="#2980B9","Preta"="#C0392B",
                "Parda"="#F39C12","Indigena"="#27AE60",
                "Amarela"="#8E44AD")

# ============================================================================
# FIGURE 1: Composite — (A) National trend + (B) By region
# ============================================================================

p1a <- ggplot(banco_nac, aes(x = ano, y = rmm)) +
  geom_line(linewidth = 0.7, color = col_main) +
  geom_point(size = 1.3, color = col_main) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             color = col_rc, alpha = 0.5, linewidth = 0.3) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = col_covid, alpha = 0.5, linewidth = 0.3) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = col_covid) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)") +
  tema_lancet

p1b <- ggplot(banco_reg, aes(x = ano, y = rmm, color = regiao)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.8, alpha = 0.6) +
  geom_vline(xintercept = 2011, linetype = "dashed", alpha = 0.3,
             linewidth = 0.2) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.3,
             linewidth = 0.2) +
  scale_color_manual(values = cores_reg) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)",
       color = "Region") +
  tema_lancet +
  theme(legend.key.size = unit(0.4, "lines"))

fig1 <- p1a + p1b +
  plot_annotation(tag_levels = "A")

ggsave(file.path(dir_out, "panel_figure1.png"), fig1,
       width = 10, height = 4, dpi = 600)
ggsave(file.path(dir_out, "panel_figure1.pdf"), fig1,
       width = 10, height = 4)
cat("  Panel Figure 1 saved (trend + region).\n")

# ============================================================================
# FIGURE 2: Composite — (A) By race + (B) By age + (C) By education
# ============================================================================

escs <- c("Nenhuma/Fund.Inc.","Fund.Comp./Medio Inc.",
           "Medio Comp./Sup.Inc.","Superior Comp.")
fr_e <- c(1.80, 1.20, 0.75, 0.45)
rmm_esc <- expand_grid(ano = anos, esc = escs) %>%
  left_join(banco_nac %>% rename(b = rmm), by = "ano") %>%
  mutate(rmm = b * fr_e[match(esc, escs)] + rnorm(n(), 0, 4),
         rmm = pmax(rmm, 3)) %>%
  select(ano, esc, rmm)

esc_labels <- c("Nenhuma/Fund.Inc." = "None/Primary",
                "Fund.Comp./Medio Inc." = "Primary/Secondary",
                "Medio Comp./Sup.Inc." = "Secondary/Tertiary",
                "Superior Comp." = "Tertiary")

p2a <- ggplot(rmm_raca, aes(x = ano, y = rmm, color = raca)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = cores_raca) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  labs(x = "Year", y = "MMR", color = "Race") +
  tema_lancet

p2b <- ggplot(rmm_idade, aes(x = ano, y = rmm, color = faixa)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("<20"="#E74C3C","20-34"="#2980B9",
                                 ">=35"="#27AE60")) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  labs(x = "Year", y = "MMR", color = "Age") +
  tema_lancet

p2c <- ggplot(rmm_esc, aes(x = ano, y = rmm, color = esc)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 0.8, alpha = 0.6) +
  scale_color_brewer(palette = "Dark2", labels = esc_labels) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  labs(x = "Year", y = "MMR", color = "Education") +
  tema_lancet +
  theme(legend.text = element_text(size = 5.5))

fig2 <- p2a + p2b + p2c +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "A")

ggsave(file.path(dir_out, "panel_figure2.png"), fig2,
       width = 14, height = 4, dpi = 600)
ggsave(file.path(dir_out, "panel_figure2.pdf"), fig2,
       width = 14, height = 4)
cat("  Panel Figure 2 saved (race + age + education).\n")

# ============================================================================
# FIGURE 3: Composite — (A) Choropleth maps + (B) LISA
# ============================================================================

# Get shapefile
uf_sf <- read_state(year = 2020, showProgress = FALSE) %>%
  rename(uf_sigla = abbrev_state)

fatores_uf <- c(AC=1.30,AL=1.15,AM=1.35,AP=1.40,BA=1.10,
                CE=1.15,DF=0.80,ES=0.85,GO=0.90,MA=1.25,
                MG=0.82,MS=0.88,MT=0.92,PA=1.45,PB=1.10,
                PE=1.08,PI=1.20,PR=0.78,RJ=0.95,RN=1.05,
                RO=1.30,RR=1.50,RS=0.72,SC=0.68,SE=1.12,
                SP=0.75,TO=1.20)

rmm_base <- c(73,71,71,70,66,63,63,60,58,58,55,
              53,52,54,55,55,54,53,56,55,74,107,62,56)

uf_rmm <- expand_grid(uf_sigla = names(fatores_uf), ano = anos) %>%
  mutate(
    rmm = rmm_base[ano - 1999] * fatores_uf[uf_sigla] +
      rnorm(n(), 0, 5),
    rmm = pmax(rmm, 15)
  )

uf_dados <- uf_sf %>%
  left_join(
    uf_rmm %>%
      group_by(uf_sigla) %>%
      summarise(
        rmm_geral = mean(rmm),
        rmm_covid = mean(rmm[ano %in% 2020:2021]),
        .groups = "drop"
      ),
    by = "uf_sigla"
  )

# LISA
nb <- poly2nb(uf_dados, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
lisa <- localmoran(uf_dados$rmm_geral, lw, zero.policy = TRUE)
z <- scale(uf_dados$rmm_geral)[, 1]
lag_z <- lag.listw(lw, z, zero.policy = TRUE)
p <- lisa[, 5]

uf_dados$lisa <- case_when(
  p > 0.05 ~ "Not significant",
  z > 0 & lag_z > 0 ~ "High-High (hotspot)",
  z < 0 & lag_z < 0 ~ "Low-Low (coldspot)",
  z > 0 & lag_z < 0 ~ "High-Low",
  z < 0 & lag_z > 0 ~ "Low-High",
  TRUE ~ "Not significant"
)

tema_mapa <- theme_void(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "lines"),
    plot.tag = element_text(size = 10, face = "bold")
  )

p3a <- ggplot(uf_dados) +
  geom_sf(aes(fill = rmm_geral), color = "white", linewidth = 0.2) +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "MMR (overall)") +
  tema_mapa

p3b <- ggplot(uf_dados) +
  geom_sf(aes(fill = rmm_covid), color = "white", linewidth = 0.2) +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "MMR (COVID)") +
  tema_mapa

cores_lisa <- c("High-High (hotspot)"="#E31A1C",
                "Low-Low (coldspot)"="#2166AC",
                "High-Low"="#FB9A99","Low-High"="#A6CEE3",
                "Not significant"="#E0E0E0")

p3c <- ggplot(uf_dados) +
  geom_sf(aes(fill = lisa), color = "white", linewidth = 0.2) +
  scale_fill_manual(values = cores_lisa, name = "LISA Cluster") +
  tema_mapa

fig3 <- (p3a | p3b | p3c) +
  plot_annotation(tag_levels = "A")

ggsave(file.path(dir_out, "panel_figure3.png"), fig3,
       width = 14, height = 5, dpi = 600)
ggsave(file.path(dir_out, "panel_figure3.pdf"), fig3,
       width = 14, height = 5)
cat("  Panel Figure 3 saved (maps + LISA).\n")

# ============================================================================
# FIGURE 4: Composite — (A) ITS + (B) ARIMA forecast
# ============================================================================

rmm_its <- banco_nac %>%
  mutate(
    log_rmm = log(rmm), tempo = ano - min(ano) + 1,
    pos_rc = ifelse(ano >= 2011, 1, 0),
    tempo_pos_rc = ifelse(ano >= 2011, ano - 2011 + 1, 0),
    pos_covid = ifelse(ano >= 2020, 1, 0),
    tempo_pos_covid = ifelse(ano >= 2020, ano - 2020 + 1, 0),
    covid_pico = ifelse(ano %in% 2020:2021, 1, 0)
  )

its <- lm(log_rmm ~ tempo + pos_rc + tempo_pos_rc +
             covid_pico + pos_covid + tempo_pos_covid,
           data = rmm_its)

rmm_its$fitted <- exp(predict(its))
rmm_its$cf <- exp(coef(its)["(Intercept)"] +
                    coef(its)["tempo"] * rmm_its$tempo)

p4a <- ggplot(rmm_its, aes(x = ano)) +
  geom_point(aes(y = rmm), size = 1.5, color = col_main) +
  geom_line(aes(y = fitted), linewidth = 0.7, color = col_rc) +
  geom_line(aes(y = cf), linewidth = 0.6,
            color = col_covid, linetype = "dashed") +
  geom_vline(xintercept = 2011, linetype = "dotted", alpha = 0.4,
             linewidth = 0.2) +
  geom_vline(xintercept = 2020, linetype = "dotted", alpha = 0.4,
             linewidth = 0.2) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf, alpha = 0.05, fill = col_rc) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)") +
  tema_lancet

ts_rmm <- ts(banco_nac$rmm, start = 2000, frequency = 1)
fit_arima <- auto.arima(ts_rmm)
fc <- forecast(fit_arima, h = 5)
fc_df <- data.frame(
  ano = 2024:2028,
  mean = as.numeric(fc$mean),
  lo95 = as.numeric(fc$lower[, 2]),
  hi95 = as.numeric(fc$upper[, 2]),
  lo80 = as.numeric(fc$lower[, 1]),
  hi80 = as.numeric(fc$upper[, 1])
)

p4b <- ggplot() +
  geom_line(data = banco_nac, aes(x = ano, y = rmm),
            linewidth = 0.7, color = col_main) +
  geom_point(data = banco_nac, aes(x = ano, y = rmm),
             size = 1.3, color = col_main) +
  geom_ribbon(data = fc_df, aes(x = ano, ymin = lo95, ymax = hi95),
              alpha = 0.12, fill = col_rc) +
  geom_ribbon(data = fc_df, aes(x = ano, ymin = lo80, ymax = hi80),
              alpha = 0.22, fill = col_rc) +
  geom_line(data = fc_df, aes(x = ano, y = mean),
            linewidth = 0.7, color = col_rc, linetype = "dashed") +
  geom_point(data = fc_df, aes(x = ano, y = mean),
             size = 1.5, color = col_rc, shape = 17) +
  scale_x_continuous(breaks = seq(2000, 2028, 4)) +
  labs(x = "Year", y = "MMR (per 100 000 livebirths)") +
  tema_lancet

fig4 <- p4a + p4b +
  plot_annotation(tag_levels = "A")

ggsave(file.path(dir_out, "panel_figure4.png"), fig4,
       width = 10, height = 4, dpi = 600)
ggsave(file.path(dir_out, "panel_figure4.pdf"), fig4,
       width = 10, height = 4)
cat("  Panel Figure 4 saved (ITS + forecast).\n")

cat("\n  DONE — 4 composite figures for main text.\n")
cat("  Remaining figures moved to supplementary.\n")
