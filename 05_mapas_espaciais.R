###############################################################################
#  MAPAS ESPACIAIS — Clusters e Hotspots de Mortalidade Materna
#  Mapas coropléticos, Moran's I global e local (LISA)
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(geobr)
  library(spdep)
  library(viridis)
  library(RColorBrewer)
  library(patchwork)
})

dir_out <- file.path(getwd(), "resultados", "figuras_publicacao")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

tema_mapa <- theme_void(base_size = 10) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(2, 2, 2, 2, "pt")
  )

set.seed(2025)

cat("============================================================\n")
cat("  MAPAS ESPACIAIS — Clusters e Hotspots\n")
cat("============================================================\n\n")

# ============================================================================
# 1. DADOS GEOGRÁFICOS
# ============================================================================

cat("1. Baixando shapefile das UFs...\n")
uf_sf <- read_state(year = 2020, showProgress = FALSE)
uf_sf <- uf_sf %>%
  rename(uf_sigla = abbrev_state, uf_nome = name_state,
         regiao = name_region)

# ============================================================================
# 2. DADOS DE RMM (simulados ou reais)
# ============================================================================

cat("2. Preparando dados...\n")

dir_dados <- file.path(getwd(), "dados_datasus")
dados_reais <- file.exists(
  file.path(dir_dados, "banco_analitico_uf.rds")
)

if (dados_reais) {
  cat("   Usando dados reais.\n")
  banco_uf <- readRDS(file.path(dir_dados, "banco_analitico_uf.rds"))
  rmm_uf <- banco_uf %>%
    select(ano, uf_sigla, rmm)
} else {
  cat("   Usando dados simulados.\n")
  ufs_df <- data.frame(
    uf_sigla = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
                 "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
                 "RO","RR","RS","SC","SE","SP","TO"),
    regiao = c("Norte","Nordeste","Norte","Norte","Nordeste","Nordeste",
               "Centro-Oeste","Sudeste","Centro-Oeste","Nordeste",
               "Sudeste","Centro-Oeste","Centro-Oeste","Norte",
               "Nordeste","Nordeste","Nordeste","Sul","Sudeste",
               "Nordeste","Norte","Norte","Sul","Sul","Nordeste",
               "Sudeste","Norte")
  )

  fatores_uf <- c(
    AC = 1.30, AL = 1.15, AM = 1.35, AP = 1.40, BA = 1.10,
    CE = 1.15, DF = 0.80, ES = 0.85, GO = 0.90, MA = 1.25,
    MG = 0.82, MS = 0.88, MT = 0.92, PA = 1.45, PB = 1.10,
    PE = 1.08, PI = 1.20, PR = 0.78, RJ = 0.95, RN = 1.05,
    RO = 1.30, RR = 1.50, RS = 0.72, SC = 0.68, SE = 1.12,
    SP = 0.75, TO = 1.20
  )

  rmm_nac_base <- c(
    73, 71, 71, 70, 66, 63, 63, 60, 58, 58, 55,
    53, 52, 54, 55, 55, 54, 53, 56, 55,
    74, 107, 62, 56
  )

  rmm_uf <- expand_grid(
    uf_sigla = ufs_df$uf_sigla,
    ano = 2000:2023
  ) %>%
    mutate(
      idx = ano - 1999,
      rmm_base = rmm_nac_base[idx],
      fator = fatores_uf[uf_sigla],
      rmm = rmm_base * fator + rnorm(n(), 0, 5),
      rmm = pmax(rmm, 15)
    ) %>%
    select(ano, uf_sigla, rmm)
}

# ============================================================================
# 3. CALCULAR RMM POR PERÍODO
# ============================================================================

cat("3. Calculando RMM por período...\n")

rmm_periodos <- rmm_uf %>%
  mutate(periodo = case_when(
    ano <= 2010 ~ "2000-2010",
    ano <= 2019 ~ "2011-2019",
    ano <= 2021 ~ "2020-2021",
    TRUE ~ "2022-2023"
  )) %>%
  group_by(uf_sigla, periodo) %>%
  summarise(rmm_media = mean(rmm, na.rm = TRUE), .groups = "drop")

rmm_geral <- rmm_uf %>%
  group_by(uf_sigla) %>%
  summarise(
    rmm_media = mean(rmm, na.rm = TRUE),
    rmm_pre = mean(rmm[ano <= 2010], na.rm = TRUE),
    rmm_pos = mean(rmm[ano >= 2011 & ano <= 2019], na.rm = TRUE),
    rmm_covid = mean(rmm[ano %in% 2020:2021], na.rm = TRUE),
    rmm_recente = mean(rmm[ano >= 2022], na.rm = TRUE),
    variacao_pct = (rmm_covid - rmm_pos) / rmm_pos * 100,
    .groups = "drop"
  )

# Juntar com shapefile
uf_dados <- uf_sf %>%
  left_join(rmm_geral, by = "uf_sigla")

# ============================================================================
# 4. MAPAS COROPLÉTICOS
# ============================================================================

cat("4. Gerando mapas coropléticos...\n")

# --- Mapa A: RMM média geral ---
mapa_a <- ggplot(uf_dados) +
  geom_sf(aes(fill = rmm_media), color = "white", linewidth = 0.3) +
  scale_fill_viridis(
    option = "magma", direction = -1,
    name = "MMR",
    breaks = seq(40, 120, 20)
  ) +
  tema_mapa

# --- Mapa B: RMM pré-Rede Cegonha ---
mapa_b <- ggplot(uf_dados) +
  geom_sf(aes(fill = rmm_pre), color = "white", linewidth = 0.3) +
  scale_fill_viridis(
    option = "magma", direction = -1,
    name = "MMR",
    breaks = seq(40, 120, 20)
  ) +
  tema_mapa

# --- Mapa C: RMM COVID ---
mapa_c <- ggplot(uf_dados) +
  geom_sf(aes(fill = rmm_covid), color = "white", linewidth = 0.3) +
  scale_fill_viridis(
    option = "magma", direction = -1,
    name = "MMR",
    breaks = seq(40, 180, 20)
  ) +
  tema_mapa

# --- Mapa D: Variação % (COVID vs pré-COVID) ---
mapa_d <- ggplot(uf_dados) +
  geom_sf(aes(fill = variacao_pct), color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#2980B9", mid = "#F7DC6F", high = "#C0392B",
    midpoint = 60,
    name = "% change",
    breaks = seq(0, 150, 30)
  ) +
  tema_mapa

# Painel de 4 mapas
fig_mapas <- (mapa_b + mapa_c) / (mapa_a + mapa_d) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 12, face = "bold")
    )
  )

ggsave(file.path(dir_out, "figure13_maps.png"), fig_mapas,
       width = 12, height = 10, dpi = 600)
ggsave(file.path(dir_out, "figure13_maps.pdf"), fig_mapas,
       width = 12, height = 10)
cat("   Figura 13 (mapas coropléticos) salva.\n")

# ============================================================================
# 5. ANÁLISE ESPACIAL — MORAN'S I E LISA
# ============================================================================

cat("5. Análise espacial (Moran's I e LISA)...\n")

# Criar vizinhança (queen contiguity)
nb <- poly2nb(uf_dados, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# --- 5.1 Moran's I global ---
moran_geral <- moran.test(uf_dados$rmm_media, lw,
                           zero.policy = TRUE)
moran_covid <- moran.test(uf_dados$rmm_covid, lw,
                           zero.policy = TRUE)

cat(sprintf("   Moran's I (RMM geral): %.4f, p = %.4f\n",
            moran_geral$estimate[1], moran_geral$p.value))
cat(sprintf("   Moran's I (RMM COVID): %.4f, p = %.4f\n",
            moran_covid$estimate[1], moran_covid$p.value))

# --- 5.2 LISA (Local Moran's I) ---
lisa_geral <- localmoran(uf_dados$rmm_media, lw,
                          zero.policy = TRUE)
lisa_covid <- localmoran(uf_dados$rmm_covid, lw,
                          zero.policy = TRUE)

# Classificar clusters LISA
classify_lisa <- function(x, lisa_result, lw) {
  z <- scale(x)[, 1]
  lag_z <- lag.listw(lw, z, zero.policy = TRUE)
  p <- lisa_result[, 5]  # p-value

  cluster <- case_when(
    p > 0.05 ~ "Not significant",
    z > 0 & lag_z > 0 ~ "High-High (hotspot)",
    z < 0 & lag_z < 0 ~ "Low-Low (coldspot)",
    z > 0 & lag_z < 0 ~ "High-Low (outlier)",
    z < 0 & lag_z > 0 ~ "Low-High (outlier)",
    TRUE ~ "Not significant"
  )
  cluster
}

uf_dados$lisa_geral <- classify_lisa(
  uf_dados$rmm_media, lisa_geral, lw
)
uf_dados$lisa_covid <- classify_lisa(
  uf_dados$rmm_covid, lisa_covid, lw
)

# Lisa p-values
uf_dados$lisa_p_geral <- lisa_geral[, 5]
uf_dados$lisa_p_covid <- lisa_covid[, 5]

cat("\n   LISA clusters (RMM geral):\n")
print(table(uf_dados$lisa_geral))
cat("\n   LISA clusters (RMM COVID):\n")
print(table(uf_dados$lisa_covid))

# --- 5.3 Mapas LISA ---
cores_lisa <- c(
  "High-High (hotspot)" = "#E31A1C",
  "Low-Low (coldspot)" = "#2166AC",
  "High-Low (outlier)" = "#FB9A99",
  "Low-High (outlier)" = "#A6CEE3",
  "Not significant" = "#E0E0E0"
)

mapa_lisa_geral <- ggplot(uf_dados) +
  geom_sf(aes(fill = lisa_geral), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "LISA Cluster") +
  tema_mapa

mapa_lisa_covid <- ggplot(uf_dados) +
  geom_sf(aes(fill = lisa_covid), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "LISA Cluster") +
  tema_mapa

fig_lisa <- mapa_lisa_geral + mapa_lisa_covid +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 12, face = "bold")
    )
  )

ggsave(file.path(dir_out, "figure14_lisa.png"), fig_lisa,
       width = 14, height = 6, dpi = 600)
ggsave(file.path(dir_out, "figure14_lisa.pdf"), fig_lisa,
       width = 14, height = 6)
cat("   Figura 14 (LISA clusters) salva.\n")

# ============================================================================
# 6. MAPA POR PERÍODO (4 painéis temporais)
# ============================================================================

cat("6. Mapa temporal por período...\n")

rmm_per_sf <- uf_sf %>%
  left_join(rmm_periodos, by = "uf_sigla")

fig_temporal <- ggplot(rmm_per_sf) +
  geom_sf(aes(fill = rmm_media), color = "white", linewidth = 0.2) +
  facet_wrap(~ periodo, ncol = 2) +
  scale_fill_viridis(
    option = "magma", direction = -1,
    name = "MMR\n(per 100 000\nlivebirths)",
    breaks = seq(30, 180, 30)
  ) +
  tema_mapa +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.key.height = unit(1.5, "cm")
  )

ggsave(file.path(dir_out, "figure15_maps_temporal.png"),
       fig_temporal, width = 10, height = 9, dpi = 600)
ggsave(file.path(dir_out, "figure15_maps_temporal.pdf"),
       fig_temporal, width = 10, height = 9)
cat("   Figura 15 (mapas temporais) salva.\n")

# ============================================================================
# 7. EXPORTAR RESULTADOS
# ============================================================================

cat("\n7. Exportando resultados espaciais...\n")

tab_espacial <- uf_dados %>%
  st_drop_geometry() %>%
  select(uf_sigla, uf_nome, regiao,
         rmm_media, rmm_pre, rmm_pos, rmm_covid, rmm_recente,
         variacao_pct, lisa_geral, lisa_covid,
         lisa_p_geral, lisa_p_covid) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

writexl::write_xlsx(
  list(
    "Spatial_Results" = tab_espacial,
    "Moran_Tests" = tibble(
      Test = c("Global Moran I (overall)", "Global Moran I (COVID)"),
      Statistic = round(c(moran_geral$estimate[1],
                           moran_covid$estimate[1]), 4),
      p_value = round(c(moran_geral$p.value,
                         moran_covid$p.value), 4)
    )
  ),
  file.path(getwd(), "resultados", "tabelas",
            "resultados_espaciais.xlsx")
)

cat("   Resultados exportados.\n")

cat("\n============================================================\n")
cat("  MAPAS ESPACIAIS — CONCLUÍDO\n")
cat("============================================================\n")
cat("  Figure 13: Choropleth maps (4 panels)\n")
cat("  Figure 14: LISA cluster maps (hotspots/coldspots)\n")
cat("  Figure 15: Temporal maps by period\n")
cat(sprintf("  Moran's I (overall): %.4f (p=%.4f)\n",
            moran_geral$estimate[1], moran_geral$p.value))
cat(sprintf("  Moran's I (COVID):   %.4f (p=%.4f)\n",
            moran_covid$estimate[1], moran_covid$p.value))
cat("============================================================\n")
