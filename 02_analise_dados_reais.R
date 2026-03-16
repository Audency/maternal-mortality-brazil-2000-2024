###############################################################################
#  ANÁLISE DE MORTALIDADE MATERNA — DADOS REAIS DATASUS
#  Com padronização de taxas e validação com a literatura
#  Período: 2000-2024
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lmtest)
  library(sandwich)
  library(nlme)
  library(forecast)
  library(broom)
  library(gt)
  library(patchwork)
  library(scales)
  library(viridis)
  library(RColorBrewer)
  library(writexl)
})

# --- Funções auxiliares ---

# Prais-Winsten via GLS (AR1)
pw_fit <- function(formula, data) {
  gls(formula, data = data, correlation = corAR1(), method = "ML")
}

pw_apc <- function(mod, var = "tempo") {
  beta <- coef(mod)[var]
  se <- sqrt(vcov(mod)[var, var])
  ci_low <- beta - 1.96 * se
  ci_high <- beta + 1.96 * se
  apc <- (exp(beta) - 1) * 100
  apc_low <- (exp(ci_low) - 1) * 100
  apc_high <- (exp(ci_high) - 1) * 100
  p_val <- 2 * (1 - pnorm(abs(beta / se)))
  rho <- as.numeric(
    coef(mod$modelStruct$corStruct, unconstrained = FALSE)
  )
  list(
    APC = apc, CI_low = apc_low, CI_high = apc_high,
    p = p_val, rho = rho, beta = beta, se = se
  )
}

# Padronização direta por idade
padronizar_taxa <- function(obitos_por_grupo, nv_por_grupo,
                            pop_padrao_prop) {
  # obitos_por_grupo e nv_por_grupo: vetores por faixa etária

  # pop_padrao_prop: proporção da população padrão em cada faixa
  taxas_especificas <- obitos_por_grupo / nv_por_grupo * 100000
  taxa_padronizada <- sum(taxas_especificas * pop_padrao_prop,
                          na.rm = TRUE)
  taxa_padronizada
}

# Diretórios
dir_dados <- file.path(getwd(), "dados_datasus")
dir_out <- file.path(getwd(), "resultados")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_out, "figuras"), showWarnings = FALSE)
dir.create(file.path(dir_out, "tabelas"), showWarnings = FALSE)
dir.create(file.path(dir_out, "suplementar"), showWarnings = FALSE)

set.seed(2025)

cat("============================================================\n")
cat("  ANÁLISE DE MORTALIDADE MATERNA — DADOS REAIS\n")
cat("  Com padronização e validação literária\n")
cat("============================================================\n\n")

# ============================================================================
# 1. CARREGAR DADOS
# ============================================================================

cat("1. Carregando dados...\n")

# Verificar se dados reais existem
dados_reais <- file.exists(file.path(dir_dados, "sim_obitos_maternos_2000_2024.rds"))

if (dados_reais) {
  cat("   Dados reais do DATASUS encontrados!\n")

  sim <- readRDS(file.path(dir_dados, "sim_obitos_maternos_2000_2024.rds"))
  sinasc <- readRDS(file.path(dir_dados, "sinasc_nascidos_vivos_2000_2024.rds"))

  banco_nacional <- readRDS(file.path(dir_dados, "banco_analitico_nacional.rds"))
  banco_regiao <- readRDS(file.path(dir_dados, "banco_analitico_regiao.rds"))
  banco_uf <- readRDS(file.path(dir_dados, "banco_analitico_uf.rds"))
  obitos_raca <- readRDS(file.path(dir_dados, "obitos_raca.rds"))
  obitos_idade <- readRDS(file.path(dir_dados, "obitos_idade.rds"))
  obitos_escolaridade <- readRDS(file.path(dir_dados, "obitos_escolaridade.rds"))
  obitos_causa <- readRDS(file.path(dir_dados, "obitos_causa.rds"))

  fonte_dados <- "SIM/SINASC — DATASUS/Ministério da Saúde"

} else {
  cat("   Dados reais não encontrados. Usando dados simulados.\n")
  cat("   Execute primeiro: Rscript 01_download_datasus.R\n\n")

  # Usar dados simulados (fallback)
  source("analise_mortalidade_materna.R")
  stop("Execute o download primeiro ou use o script de simulação.")
}

cat(sprintf("   Período: %d - %d\n",
            min(banco_nacional$ano), max(banco_nacional$ano)))
cat(sprintf("   Óbitos maternos: %s\n",
            format(sum(banco_nacional$obitos_maternos), big.mark = ".")))
cat(sprintf("   Nascidos vivos: %s\n",
            format(sum(banco_nacional$nascidos_vivos), big.mark = ".")))

# ============================================================================
# 2. PADRONIZAÇÃO DAS TAXAS POR IDADE
# ============================================================================

cat("\n2. Padronização das taxas por idade (método direto)...\n")

# --- 2.1 Extrair óbitos e NV por faixa etária e ano ---
# Definir faixas etárias da mãe

# Processar dados de idade a partir dos microdados
sim_idade <- sim %>%
  mutate(
    obito_materno_oficial = grepl("^O", CAUSABAS) &
      !grepl("^O9[67]", CAUSABAS),
    idade_num = as.numeric(IDADE),
    faixa_idade = case_when(
      idade_num < 20 ~ "<20",
      idade_num >= 20 & idade_num <= 34 ~ "20-34",
      idade_num >= 35 ~ ">=35",
      TRUE ~ "Ignorada"
    )
  ) %>%
  filter(obito_materno_oficial, faixa_idade != "Ignorada")

sinasc_idade <- sinasc %>%
  mutate(
    idade_mae_num = as.numeric(IDADEMAE),
    faixa_idade = case_when(
      idade_mae_num < 20 ~ "<20",
      idade_mae_num >= 20 & idade_mae_num <= 34 ~ "20-34",
      idade_mae_num >= 35 ~ ">=35",
      TRUE ~ "Ignorada"
    )
  ) %>%
  filter(faixa_idade != "Ignorada")

# Óbitos por ano e faixa etária
ob_idade_ano <- sim_idade %>%
  group_by(ano_obito, faixa_idade) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  rename(ano = ano_obito)

# NV por ano e faixa etária
nv_idade_ano <- sinasc_idade %>%
  group_by(ano_nasc, faixa_idade) %>%
  summarise(nv = n(), .groups = "drop") %>%
  rename(ano = ano_nasc)

# --- 2.2 População padrão (proporção de NV por faixa etária) ---
# Usar a distribuição média de NV como população padrão
pop_padrao <- nv_idade_ano %>%
  group_by(faixa_idade) %>%
  summarise(nv_total = sum(nv), .groups = "drop") %>%
  mutate(prop = nv_total / sum(nv_total))

cat("   População padrão (proporção de NV por faixa etária):\n")
for (i in seq_len(nrow(pop_padrao))) {
  cat(sprintf("     %s: %.3f\n",
              pop_padrao$faixa_idade[i], pop_padrao$prop[i]))
}

# --- 2.3 Calcular taxas padronizadas por ano ---
taxas_padronizadas <- ob_idade_ano %>%
  left_join(nv_idade_ano, by = c("ano", "faixa_idade")) %>%
  mutate(taxa_especifica = obitos / nv * 100000) %>%
  left_join(pop_padrao %>% select(faixa_idade, prop),
            by = "faixa_idade") %>%
  group_by(ano) %>%
  summarise(
    rmm_padronizada = sum(taxa_especifica * prop, na.rm = TRUE),
    .groups = "drop"
  )

# Juntar com RMM bruta
banco_nacional_pad <- banco_nacional %>%
  left_join(taxas_padronizadas, by = "ano")

cat("\n   Comparação RMM bruta vs padronizada:\n")
print(banco_nacional_pad %>%
        select(ano, rmm, rmm_padronizada) %>%
        mutate(rmm = round(rmm, 1),
               rmm_padronizada = round(rmm_padronizada, 1),
               diferenca_pct = round(
                 (rmm_padronizada - rmm) / rmm * 100, 1
               )))

# ============================================================================
# 3. VALIDAÇÃO COM A LITERATURA
# ============================================================================

cat("\n3. Validação com valores da literatura...\n")

# Valores de referência da literatura
# WHO/UNICEF/UNFPA/World Bank 2023 report
referencia_who <- data.frame(
  ano = c(2000, 2005, 2010, 2015, 2017, 2020),
  rmm_who = c(66, 63, 60, 60, 60, 72),
  fonte = rep("WHO/UNICEF/UNFPA 2023", 6)
)

# GBD 2019/2021 estimates
referencia_gbd <- data.frame(
  ano = c(2000, 2005, 2010, 2015, 2019),
  rmm_gbd = c(73, 67, 62, 58, 57),
  fonte = rep("GBD 2019", 5)
)

# MS Brasil (oficial)
referencia_ms <- data.frame(
  ano = c(2000, 2005, 2010, 2015, 2019, 2020, 2021, 2022),
  rmm_ms = c(73.3, 64.8, 60.1, 57.6, 55.3, 74.7, 107.5, 56.0),
  fonte = rep("MS/SVS Brasil", 8)
)

# Comparar dados observados com referência
validacao <- banco_nacional_pad %>%
  select(ano, rmm) %>%
  left_join(referencia_who, by = "ano") %>%
  left_join(referencia_gbd, by = "ano") %>%
  left_join(referencia_ms %>% select(ano, rmm_ms), by = "ano") %>%
  filter(!is.na(rmm_who) | !is.na(rmm_gbd) | !is.na(rmm_ms)) %>%
  mutate(
    rmm = round(rmm, 1),
    dif_who = round(rmm - rmm_who, 1),
    dif_gbd = round(rmm - rmm_gbd, 1),
    dif_ms = round(rmm - rmm_ms, 1)
  )

cat("\n   Comparação com a literatura:\n")
print(validacao %>%
        select(ano, rmm, rmm_who, dif_who, rmm_gbd, dif_gbd,
               rmm_ms, dif_ms))

# Verificar inconsistências
inconsistencias <- validacao %>%
  mutate(
    flag_who = abs(dif_who) > 15,
    flag_gbd = abs(dif_gbd) > 15,
    flag_ms = abs(dif_ms) > 10
  ) %>%
  filter(flag_who | flag_gbd | flag_ms)

if (nrow(inconsistencias) > 0) {
  cat("\n   ATENCAO: Inconsistências detectadas:\n")
  for (i in seq_len(nrow(inconsistencias))) {
    r <- inconsistencias[i, ]
    cat(sprintf("     Ano %d: RMM=%.1f", r$ano, r$rmm))
    if (!is.na(r$flag_who) && r$flag_who) {
      cat(sprintf(" | WHO=%.0f (dif=%.1f)", r$rmm_who, r$dif_who))
    }
    if (!is.na(r$flag_gbd) && r$flag_gbd) {
      cat(sprintf(" | GBD=%.0f (dif=%.1f)", r$rmm_gbd, r$dif_gbd))
    }
    if (!is.na(r$flag_ms) && r$flag_ms) {
      cat(sprintf(" | MS=%.1f (dif=%.1f)", r$rmm_ms, r$dif_ms))
    }
    cat("\n")
  }
  cat("\n   Nota: Diferenças podem dever-se a:\n")
  cat("   - Ajustes por sub-registro aplicados pelo MS/WHO\n")
  cat("   - Inclusão/exclusão de O96-O97 (mortes tardias)\n")
  cat("   - Diferentes fontes de NV no denominador\n")
} else {
  cat("\n   Dados consistentes com a literatura.\n")
}

# ============================================================================
# 4. TABELAS DESCRITIVAS COM DADOS REAIS
# ============================================================================

cat("\n4. Construindo tabelas descritivas...\n")

tema_artigo <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# --- Tabela 1: Descritiva por período ---
tab1 <- banco_nacional_pad %>%
  mutate(periodo = cut(
    ano,
    breaks = c(1999, 2010, 2019, 2021, 2025),
    labels = c("Pré-Rede Cegonha (2000-2010)",
               "Pós-Rede Cegonha (2011-2019)",
               "COVID-19 (2020-2021)",
               "Pós-pandemia (2022+)")
  )) %>%
  group_by(periodo) %>%
  summarise(
    n_anos = n(),
    obitos_total = sum(obitos_maternos),
    nv_total = sum(nascidos_vivos),
    rmm_media = round(mean(rmm, na.rm = TRUE), 1),
    rmm_dp = round(sd(rmm, na.rm = TRUE), 1),
    rmm_min = round(min(rmm, na.rm = TRUE), 1),
    rmm_max = round(max(rmm, na.rm = TRUE), 1),
    rmm_pad_media = round(mean(rmm_padronizada, na.rm = TRUE), 1),
    .groups = "drop"
  )

tab1_gt <- tab1 %>%
  gt() %>%
  tab_header(
    title = "Tabela 1. Mortalidade materna por período",
    subtitle = paste("Brasil, dados reais —", fonte_dados)
  ) %>%
  cols_label(
    periodo = "Período",
    n_anos = "Anos",
    obitos_total = "Óbitos",
    nv_total = "NV",
    rmm_media = "RMM média",
    rmm_dp = "DP",
    rmm_min = "Min",
    rmm_max = "Máx",
    rmm_pad_media = "RMM padronizada"
  ) %>%
  fmt_number(columns = c(obitos_total, nv_total), decimals = 0,
             sep_mark = ".") %>%
  tab_spanner(
    label = "RMM bruta (por 100.000 NV)",
    columns = c(rmm_media, rmm_dp, rmm_min, rmm_max)
  ) %>%
  tab_source_note(
    paste("Fonte:", fonte_dados,
          "| RMM padronizada por idade (método direto).")
  )

gtsave(tab1_gt, file.path(dir_out, "tabelas",
                           "tabela1_descritiva_real.html"))

# --- Tabela 2: Por região ---
tab2 <- banco_regiao %>%
  mutate(periodo = cut(
    ano,
    breaks = c(1999, 2010, 2019, 2021, 2025),
    labels = c("2000-2010", "2011-2019", "2020-2021", "2022+")
  )) %>%
  group_by(regiao, periodo) %>%
  summarise(rmm_media = round(mean(rmm, na.rm = TRUE), 1),
            .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = rmm_media)

tab2_gt <- tab2 %>%
  gt() %>%
  tab_header(
    title = "Tabela 2. RMM média por região e período",
    subtitle = "Brasil (por 100.000 NV)"
  ) %>%
  cols_label(regiao = "Região") %>%
  tab_source_note(paste("Fonte:", fonte_dados))

gtsave(tab2_gt, file.path(dir_out, "tabelas",
                           "tabela2_regiao_real.html"))

# --- Tabela 3: Por raça/cor ---
# Precisamos do total de NV por raça/cor para calcular RMM
nv_raca_ano <- sinasc %>%
  mutate(
    raca_cor = case_when(
      RACACOR == "1" ~ "Branca",
      RACACOR == "2" ~ "Preta",
      RACACOR == "3" ~ "Amarela",
      RACACOR == "4" ~ "Parda",
      RACACOR == "5" ~ "Indigena",
      TRUE ~ "Ignorada"
    )
  ) %>%
  filter(raca_cor != "Ignorada") %>%
  group_by(ano_nasc, raca_cor) %>%
  summarise(nv = n(), .groups = "drop") %>%
  rename(ano = ano_nasc)

rmm_raca <- obitos_raca %>%
  filter(raca_cor != "Ignorada") %>%
  left_join(nv_raca_ano, by = c("ano", "raca_cor")) %>%
  mutate(rmm = obitos / nv * 100000)

tab3 <- rmm_raca %>%
  mutate(periodo = cut(
    ano,
    breaks = c(1999, 2010, 2019, 2021, 2025),
    labels = c("2000-2010", "2011-2019", "2020-2021", "2022+")
  )) %>%
  group_by(raca_cor, periodo) %>%
  summarise(rmm_media = round(mean(rmm, na.rm = TRUE), 1),
            .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = rmm_media)

tab3_gt <- tab3 %>%
  gt() %>%
  tab_header(
    title = "Tabela 3. RMM por raça/cor e período",
    subtitle = "Brasil (por 100.000 NV)"
  ) %>%
  cols_label(raca_cor = "Raça/Cor") %>%
  tab_source_note(paste("Fonte:", fonte_dados))

gtsave(tab3_gt, file.path(dir_out, "tabelas",
                           "tabela3_raca_real.html"))

# --- Tabela 4: Por faixa etária ---
nv_idade <- nv_idade_ano

rmm_idade <- ob_idade_ano %>%
  left_join(nv_idade, by = c("ano", "faixa_idade")) %>%
  mutate(rmm = obitos / nv * 100000)

tab4 <- rmm_idade %>%
  mutate(periodo = cut(
    ano,
    breaks = c(1999, 2010, 2019, 2021, 2025),
    labels = c("2000-2010", "2011-2019", "2020-2021", "2022+")
  )) %>%
  group_by(faixa_idade, periodo) %>%
  summarise(rmm_media = round(mean(rmm, na.rm = TRUE), 1),
            .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = rmm_media)

tab4_gt <- tab4 %>%
  gt() %>%
  tab_header(
    title = "Tabela 4. RMM por faixa etária e período",
    subtitle = "Brasil (por 100.000 NV)"
  ) %>%
  cols_label(faixa_idade = "Faixa Etária") %>%
  tab_source_note(paste("Fonte:", fonte_dados))

gtsave(tab4_gt, file.path(dir_out, "tabelas",
                           "tabela4_idade_real.html"))

cat("   Tabelas 1-4 salvas.\n")

# ============================================================================
# 5. FIGURAS COM DADOS REAIS
# ============================================================================

cat("\n5. Gerando figuras com dados reais...\n")

# --- Fig 1: Tendência nacional (bruta + padronizada) ---
fig1 <- ggplot(banco_nacional_pad, aes(x = ano)) +
  geom_line(aes(y = rmm, color = "RMM bruta"), linewidth = 1) +
  geom_point(aes(y = rmm, color = "RMM bruta"), size = 2) +
  geom_line(aes(y = rmm_padronizada, color = "RMM padronizada"),
            linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = rmm_padronizada, color = "RMM padronizada"),
             size = 1.5, shape = 17) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             color = "#E74C3C", alpha = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = "#3498DB", alpha = 0.5) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "#3498DB") +
  annotate("text", x = 2011.5,
           y = max(banco_nacional_pad$rmm, na.rm = TRUE) * 0.95,
           label = "Rede Cegonha", color = "#E74C3C",
           hjust = 0, size = 3.2) +
  annotate("text", x = 2020.3,
           y = max(banco_nacional_pad$rmm, na.rm = TRUE) * 0.88,
           label = "COVID-19", color = "#3498DB",
           hjust = 0, size = 3.2) +
  scale_color_manual(
    values = c("RMM bruta" = "#2C3E50",
               "RMM padronizada" = "#E67E22")
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, 2)) +
  labs(
    title = paste("Figura 1. Tendência da RMM no Brasil,",
                  min(banco_nacional_pad$ano), "-",
                  max(banco_nacional_pad$ano)),
    subtitle = "RMM bruta e padronizada por idade (por 100.000 NV)",
    x = "Ano", y = "RMM (por 100.000 NV)", color = ""
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig1_tendencia_real.png"),
       fig1, width = 11, height = 6, dpi = 300)
ggsave(file.path(dir_out, "figuras", "fig1_tendencia_real.pdf"),
       fig1, width = 11, height = 6)
cat("   Figura 1 salva.\n")

# --- Fig 2: Por região ---
cores_regiao <- c(
  "Norte" = "#E74C3C", "Nordeste" = "#F39C12",
  "Sudeste" = "#27AE60", "Sul" = "#3498DB",
  "Centro-Oeste" = "#9B59B6"
)

fig2 <- ggplot(banco_regiao, aes(x = ano, y = rmm, color = regiao)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 2011, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.4) +
  scale_color_manual(values = cores_regiao) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title = "Figura 2. RMM por região, Brasil",
    x = "Ano", y = "RMM (por 100.000 NV)", color = "Região"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig2_regiao_real.png"),
       fig2, width = 10, height = 6, dpi = 300)
cat("   Figura 2 salva.\n")

# --- Fig 3: Por raça/cor ---
cores_raca <- c(
  "Branca" = "#3498DB", "Preta" = "#E74C3C",
  "Parda" = "#F39C12", "Indigena" = "#27AE60",
  "Amarela" = "#9B59B6"
)

fig3 <- ggplot(rmm_raca, aes(x = ano, y = rmm, color = raca_cor)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.4) +
  scale_color_manual(values = cores_raca) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title = "Figura 3. RMM por raça/cor, Brasil",
    x = "Ano", y = "RMM (por 100.000 NV)", color = "Raça/Cor"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig3_raca_real.png"),
       fig3, width = 10, height = 6, dpi = 300)
cat("   Figura 3 salva.\n")

# --- Fig 4: Por faixa etária ---
fig4 <- ggplot(rmm_idade, aes(x = ano, y = rmm,
                               color = faixa_idade)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title = "Figura 4. RMM por faixa etária, Brasil",
    x = "Ano", y = "RMM (por 100.000 NV)",
    color = "Faixa Etária"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig4_idade_real.png"),
       fig4, width = 10, height = 6, dpi = 300)
cat("   Figura 4 salva.\n")

# --- Fig 5: Causas ---
ob_causa_ano <- obitos_causa %>%
  group_by(ano) %>%
  mutate(prop = obitos / sum(obitos) * 100) %>%
  ungroup()

fig5 <- ggplot(ob_causa_ano,
               aes(x = ano, y = prop, fill = causa_grupo)) +
  geom_area(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title = "Figura 5. Causas de morte materna, Brasil",
    x = "Ano", y = "Proporção (%)", fill = "Causa"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig5_causas_real.png"),
       fig5, width = 11, height = 6, dpi = 300)
cat("   Figura 5 salva.\n")

# --- Fig 6: Heatmap UF ---
fig6 <- ggplot(banco_uf,
               aes(x = ano, y = uf_sigla, fill = rmm)) +
  geom_tile() +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "RMM") +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  labs(
    title = "Figura 6. Heatmap da RMM por UF, Brasil",
    x = "Ano", y = "UF"
  ) +
  tema_artigo +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(dir_out, "figuras", "fig6_heatmap_real.png"),
       fig6, width = 12, height = 8, dpi = 300)
cat("   Figura 6 salva.\n")

# --- Fig 7: Validação com literatura ---
fig7 <- ggplot(banco_nacional_pad, aes(x = ano, y = rmm)) +
  geom_line(linewidth = 1, color = "#2C3E50") +
  geom_point(size = 2, color = "#2C3E50") +
  geom_point(data = referencia_ms, aes(y = rmm_ms),
             color = "#E74C3C", size = 3, shape = 4, stroke = 1.5) +
  geom_point(data = referencia_who, aes(y = rmm_who),
             color = "#3498DB", size = 3, shape = 2, stroke = 1.5) +
  scale_x_continuous(breaks = seq(2000, 2024, 2)) +
  labs(
    title = "Figura 7. Validação: dados observados vs literatura",
    subtitle = paste(
      "Preto = dados observados |",
      "X vermelho = MS/SVS oficial |",
      "Triângulo azul = WHO"
    ),
    x = "Ano", y = "RMM (por 100.000 NV)"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig7_validacao_real.png"),
       fig7, width = 11, height = 6, dpi = 300)
cat("   Figura 7 salva.\n")

# ============================================================================
# 6. MODELOS COM DADOS REAIS
# ============================================================================

cat("\n6. Modelos de tendência temporal (Prais-Winsten)...\n")

# Preparar dados
rmm_nac <- banco_nacional_pad %>%
  mutate(
    log_rmm = log(rmm),
    log_rmm_pad = log(rmm_padronizada),
    tempo = ano - min(ano) + 1
  )

# 6.1 Prais-Winsten nacional
pw_nac <- pw_fit(log_rmm ~ tempo, data = rmm_nac)
res_nac <- pw_apc(pw_nac)
cat(sprintf("   APC bruta: %.2f%% (IC95%%: %.2f; %.2f) p=%.4f\n",
            res_nac$APC, res_nac$CI_low, res_nac$CI_high, res_nac$p))

pw_nac_pad <- pw_fit(log_rmm_pad ~ tempo, data = rmm_nac)
res_nac_pad <- pw_apc(pw_nac_pad)
cat(sprintf("   APC padronizada: %.2f%% (IC95%%: %.2f; %.2f) p=%.4f\n",
            res_nac_pad$APC, res_nac_pad$CI_low,
            res_nac_pad$CI_high, res_nac_pad$p))

# 6.2 Por período
periodos_pw <- list(
  "Pré-Rede Cegonha (2000-2010)" = 2000:2010,
  "Pós-Rede Cegonha (2011-2019)" = 2011:2019,
  "Pré-COVID (2000-2019)" = 2000:2019
)

resultados_pw <- map_dfr(names(periodos_pw), function(nome) {
  anos_p <- periodos_pw[[nome]]
  df <- rmm_nac %>%
    filter(ano %in% anos_p) %>%
    mutate(tempo = row_number())
  pw <- pw_fit(log_rmm ~ tempo, data = df)
  res <- pw_apc(pw)
  tibble(
    periodo = nome,
    APC = round(res$APC, 2),
    IC95_inf = round(res$CI_low, 2),
    IC95_sup = round(res$CI_high, 2),
    p_valor = round(res$p, 4),
    tendencia = case_when(
      APC > 0 & res$p < 0.05 ~ "Crescente",
      APC < 0 & res$p < 0.05 ~ "Decrescente",
      TRUE ~ "Estacionária"
    )
  )
})

cat("\n   Resultados por período:\n")
print(as.data.frame(resultados_pw))

# 6.3 Por região
resultados_pw_reg <- map_dfr(
  unique(banco_regiao$regiao),
  function(reg) {
    df <- banco_regiao %>%
      filter(regiao == reg) %>%
      arrange(ano) %>%
      mutate(log_rmm = log(rmm), tempo = row_number())
    pw <- pw_fit(log_rmm ~ tempo, data = df)
    res <- pw_apc(pw)
    tibble(
      regiao = reg,
      APC = round(res$APC, 2),
      IC95_inf = round(res$CI_low, 2),
      IC95_sup = round(res$CI_high, 2),
      p_valor = round(res$p, 4),
      tendencia = case_when(
        APC > 0 & res$p < 0.05 ~ "Crescente",
        APC < 0 & res$p < 0.05 ~ "Decrescente",
        TRUE ~ "Estacionária"
      )
    )
  }
)

cat("\n   Resultados por região:\n")
print(as.data.frame(resultados_pw_reg))

# Tabela 5
tab5_data <- bind_rows(
  resultados_pw %>%
    mutate(estrato = periodo) %>%
    select(estrato, APC, IC95_inf, IC95_sup, p_valor, tendencia),
  resultados_pw_reg %>%
    mutate(estrato = regiao) %>%
    select(estrato, APC, IC95_inf, IC95_sup, p_valor, tendencia)
)

tab5_gt <- tab5_data %>%
  mutate(APC_IC = paste0(APC, " (", IC95_inf, "; ", IC95_sup, ")")) %>%
  select(estrato, APC_IC, p_valor, tendencia) %>%
  gt() %>%
  tab_header(
    title = "Tabela 5. APC da RMM — Prais-Winsten",
    subtitle = "Brasil e regiões"
  ) %>%
  cols_label(
    estrato = "Estrato",
    APC_IC = "APC % (IC 95%)",
    p_valor = "p-valor",
    tendencia = "Tendência"
  ) %>%
  tab_source_note(paste("Fonte:", fonte_dados))

gtsave(tab5_gt, file.path(dir_out, "tabelas",
                           "tabela5_pw_real.html"))
cat("   Tabela 5 salva.\n")

# ============================================================================
# 7. SÉRIES TEMPORAIS INTERROMPIDAS (ITS)
# ============================================================================

cat("\n7. Séries temporais interrompidas (ITS)...\n")

its_data <- rmm_nac %>%
  mutate(
    pos_rc = ifelse(ano >= 2011, 1, 0),
    tempo_pos_rc = ifelse(ano >= 2011, ano - 2011 + 1, 0),
    pos_covid = ifelse(ano >= 2020, 1, 0),
    tempo_pos_covid = ifelse(ano >= 2020, ano - 2020 + 1, 0),
    covid_pico = ifelse(ano %in% 2020:2021, 1, 0)
  )

its_mod <- lm(
  log_rmm ~ tempo + pos_rc + tempo_pos_rc +
    covid_pico + pos_covid + tempo_pos_covid,
  data = its_data
)

nw_vcov <- NeweyWest(its_mod, lag = 2)
its_test <- coeftest(its_mod, vcov = nw_vcov)

cat("   Modelo ITS:\n")
print(its_test)

# Tabela 6
its_res <- tidy(its_test) %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercepto",
      term == "tempo" ~ "Tendência pré-intervenção",
      term == "pos_rc" ~ "Mudança de nível (Rede Cegonha)",
      term == "tempo_pos_rc" ~ "Mudança de tendência (Rede Cegonha)",
      term == "covid_pico" ~ "Efeito COVID-19 (2020-2021)",
      term == "pos_covid" ~ "Mudança de nível pós-COVID",
      term == "tempo_pos_covid" ~ "Mudança de tendência pós-COVID"
    ),
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4)
  )

tab6_gt <- its_res %>%
  gt() %>%
  tab_header(
    title = "Tabela 6. Modelo ITS — Rede Cegonha e COVID-19",
    subtitle = "Variável dependente: log(RMM)"
  ) %>%
  cols_label(
    term = "Parâmetro",
    estimate = "β",
    std.error = "EP (Newey-West)",
    statistic = "t",
    p.value = "p-valor"
  ) %>%
  tab_source_note(paste("Fonte:", fonte_dados))

gtsave(tab6_gt, file.path(dir_out, "tabelas",
                           "tabela6_ITS_real.html"))

# Figura 8 - ITS
its_data$predicted <- exp(predict(its_mod))
its_data$cf <- exp(
  coef(its_mod)["(Intercept)"] + coef(its_mod)["tempo"] * its_data$tempo
)

fig8 <- ggplot(its_data, aes(x = ano)) +
  geom_point(aes(y = rmm), size = 2.5, color = "#2C3E50") +
  geom_line(aes(y = predicted, color = "Modelo ITS"), linewidth = 1) +
  geom_line(aes(y = cf, color = "Contrafactual"),
            linewidth = 0.9, linetype = "dashed") +
  geom_vline(xintercept = 2011, linetype = "dotted", alpha = 0.6) +
  geom_vline(xintercept = 2020, linetype = "dotted", alpha = 0.6) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "red") +
  scale_color_manual(
    values = c("Modelo ITS" = "#E74C3C",
               "Contrafactual" = "#3498DB")
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, 2)) +
  labs(
    title = "Figura 8. ITS — Modelo ajustado e contrafactual",
    subtitle = "Rede Cegonha (2011) e COVID-19 (2020-2021)",
    x = "Ano", y = "RMM (por 100.000 NV)", color = ""
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig8_ITS_real.png"),
       fig8, width = 11, height = 6, dpi = 300)
cat("   Figura 8 e Tabela 6 salvos.\n")

# ============================================================================
# 8. MODELOS BRUTOS E AJUSTADOS
# ============================================================================

cat("\n8. Modelos brutos e ajustados...\n")

# Adicionar covariáveis (Rede Cegonha e COVID)
mod_data <- rmm_nac %>%
  mutate(
    rede_cegonha = ifelse(ano >= 2011, 1, 0),
    covid = case_when(
      ano == 2020 ~ 1, ano == 2021 ~ 2,
      ano == 2022 ~ 0.5, TRUE ~ 0
    )
  )

# Modelos brutos
vars_mod <- c("tempo", "rede_cegonha", "covid")
nomes_mod <- c("Tendência temporal", "Rede Cegonha", "COVID-19")

mod_brutos <- map2_dfr(vars_mod, nomes_mod, function(v, n) {
  mod <- lm(as.formula(paste("log_rmm ~", v)), data = mod_data)
  ci <- confint(mod)
  beta <- coef(mod)[v]
  se <- summary(mod)$coefficients[v, "Std. Error"]
  p <- summary(mod)$coefficients[v, "Pr(>|t|)"]
  tibble(
    variavel = n,
    beta_bruto = round(beta, 4),
    IC95_inf = round(ci[v, 1], 4),
    IC95_sup = round(ci[v, 2], 4),
    p_valor = round(p, 4),
    R2 = round(summary(mod)$r.squared, 3)
  )
})

# Modelo ajustado
mod_aj <- lm(log_rmm ~ tempo + rede_cegonha + covid, data = mod_data)
ci_aj <- confint(mod_aj)

mod_ajustados <- map2_dfr(vars_mod, nomes_mod, function(v, n) {
  beta <- coef(mod_aj)[v]
  se <- summary(mod_aj)$coefficients[v, "Std. Error"]
  p <- summary(mod_aj)$coefficients[v, "Pr(>|t|)"]
  tibble(
    variavel = n,
    beta_ajust = round(beta, 4),
    IC95_inf_aj = round(ci_aj[v, 1], 4),
    IC95_sup_aj = round(ci_aj[v, 2], 4),
    p_ajust = round(p, 4)
  )
})

tab7_data <- mod_brutos %>%
  left_join(mod_ajustados, by = "variavel") %>%
  mutate(
    bruto = paste0(beta_bruto, " (", IC95_inf, "; ", IC95_sup, ")"),
    ajustado = paste0(beta_ajust, " (", IC95_inf_aj, "; ",
                      IC95_sup_aj, ")")
  ) %>%
  select(variavel, bruto, p_valor, ajustado, p_ajust)

tab7_gt <- tab7_data %>%
  gt() %>%
  tab_header(
    title = "Tabela 7. Modelos brutos e ajustado — log(RMM)",
    subtitle = "Regressão linear"
  ) %>%
  cols_label(
    variavel = "Variável",
    bruto = "β bruto (IC 95%)",
    p_valor = "p (bruto)",
    ajustado = "β ajustado (IC 95%)",
    p_ajust = "p (ajustado)"
  ) %>%
  tab_spanner(label = "Análise Bruta",
              columns = c(bruto, p_valor)) %>%
  tab_spanner(label = "Análise Ajustada",
              columns = c(ajustado, p_ajust)) %>%
  tab_source_note(paste(
    "R² ajustado:", round(summary(mod_aj)$adj.r.squared, 3),
    "| Fonte:", fonte_dados
  ))

gtsave(tab7_gt, file.path(dir_out, "tabelas",
                           "tabela7_bruto_ajustado_real.html"))
cat("   Tabela 7 salva.\n")

# ============================================================================
# 9. PREVISÃO EXPLORATÓRIA
# ============================================================================

cat("\n9. Previsão exploratória (ARIMA)...\n")

ts_rmm <- ts(rmm_nac$rmm, start = min(rmm_nac$ano), frequency = 1)
fit_arima <- auto.arima(ts_rmm)
fc <- forecast(fit_arima, h = 3)

cat(sprintf("   ARIMA(%s)\n",
            paste(arimaorder(fit_arima), collapse = ",")))
print(fc)

fc_df <- data.frame(
  ano = max(rmm_nac$ano) + 1:3,
  mean = as.numeric(fc$mean),
  lo95 = as.numeric(fc$lower[, 2]),
  hi95 = as.numeric(fc$upper[, 2]),
  lo80 = as.numeric(fc$lower[, 1]),
  hi80 = as.numeric(fc$upper[, 1])
)

fig9 <- ggplot() +
  geom_line(data = rmm_nac, aes(x = ano, y = rmm),
            linewidth = 1, color = "#2C3E50") +
  geom_point(data = rmm_nac, aes(x = ano, y = rmm),
             size = 2, color = "#2C3E50") +
  geom_ribbon(data = fc_df, aes(x = ano, ymin = lo95, ymax = hi95),
              alpha = 0.15, fill = "#E74C3C") +
  geom_ribbon(data = fc_df, aes(x = ano, ymin = lo80, ymax = hi80),
              alpha = 0.25, fill = "#E74C3C") +
  geom_line(data = fc_df, aes(x = ano, y = mean),
            linewidth = 1, color = "#E74C3C", linetype = "dashed") +
  geom_point(data = fc_df, aes(x = ano, y = mean),
             size = 2.5, color = "#E74C3C", shape = 17) +
  scale_x_continuous(breaks = seq(2000, 2028, 2)) +
  labs(
    title = "Figura 9. Previsão exploratória ARIMA",
    subtitle = paste0("ARIMA(",
                      paste(arimaorder(fit_arima), collapse = ","),
                      ") | IC 80% e 95%"),
    x = "Ano", y = "RMM (por 100.000 NV)",
    caption = "Previsão inercial — não incorpora mudanças de política."
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig9_previsao_real.png"),
       fig9, width = 10, height = 6, dpi = 300)
cat("   Figura 9 salva.\n")

# ============================================================================
# 10. MATERIAL SUPLEMENTAR
# ============================================================================

cat("\n10. Material suplementar...\n")

# Diagnósticos ITS
png(file.path(dir_out, "suplementar",
              "fig_S1_diagnostico_real.png"),
    width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(its_mod, which = 1:4)
dev.off()

# Pequenos múltiplos UF
fig_s2 <- banco_uf %>%
  ggplot(aes(x = ano, y = rmm)) +
  geom_line(color = "#2C3E50", linewidth = 0.5) +
  geom_vline(xintercept = 2011, linetype = "dashed",
             color = "#E74C3C", alpha = 0.3, linewidth = 0.3) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = "#3498DB", alpha = 0.3, linewidth = 0.3) +
  facet_wrap(~uf_sigla, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = c(2000, 2012, 2024)) +
  labs(title = "Figura S2. RMM por UF", x = "Ano", y = "RMM") +
  theme_minimal(base_size = 8)

ggsave(file.path(dir_out, "suplementar",
                 "fig_S2_uf_real.png"),
       fig_s2, width = 14, height = 12, dpi = 300)

# Excel consolidado
write_xlsx(list(
  "Tab1_Descritiva" = tab1,
  "Tab2_Regiao" = tab2,
  "Tab3_Raca" = tab3,
  "Tab4_Idade" = tab4,
  "Tab5_PW" = tab5_data,
  "Tab6_ITS" = its_res,
  "Tab7_Modelos" = tab7_data,
  "Validacao" = validacao
), file.path(dir_out, "tabelas", "tabelas_dados_reais.xlsx"))

cat("   Material suplementar gerado.\n")

# ============================================================================
# 11. SUMÁRIO
# ============================================================================

cat("\n============================================================\n")
cat("  ANÁLISE COM DADOS REAIS CONCLUÍDA\n")
cat("============================================================\n\n")

cat("RESULTADOS PRINCIPAIS:\n")
cat(sprintf("  APC (bruta): %.2f%% (IC95%%: %.2f; %.2f)\n",
            res_nac$APC, res_nac$CI_low, res_nac$CI_high))
cat(sprintf("  APC (padronizada): %.2f%% (IC95%%: %.2f; %.2f)\n",
            res_nac_pad$APC, res_nac_pad$CI_low, res_nac_pad$CI_high))
cat(sprintf("  ITS Rede Cegonha: β=%.4f p=%.4f\n",
            its_test["pos_rc", 1], its_test["pos_rc", 4]))
cat(sprintf("  ITS COVID: β=%.4f p=%.4f\n",
            its_test["covid_pico", 1], its_test["covid_pico", 4]))
cat("\n============================================================\n")
