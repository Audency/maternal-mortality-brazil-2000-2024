###############################################################################
#  TENDÊNCIAS E DETERMINANTES DA MORTALIDADE MATERNA NO BRASIL (2000-2023)
#  Análise espaço-temporal, séries interrompidas e exercícios exploratórios
#
#  Script reprodutível — dados simulados com base na literatura
#  Autores: Equipa de investigação
###############################################################################

# ============================================================================
# 0. CONFIGURAÇÃO E PACOTES
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lmtest)
  library(sandwich)
  library(nlme)
  library(forecast)
  library(broom)
  library(gt)
  library(gtsummary)
  library(patchwork)
  library(scales)
  library(viridis)
  library(RColorBrewer)
  library(writexl)
})

# --- Função Prais-Winsten via GLS (AR1) ---
pw_fit <- function(formula, data) {
  # Ajuste GLS com correlação AR(1)
  mod <- gls(formula, data = data, correlation = corAR1(), method = "ML")
  mod
}

pw_apc <- function(mod, var = "tempo") {
  # Extrair APC e IC95% do coeficiente de tempo
  beta <- coef(mod)[var]
  se <- sqrt(vcov(mod)[var, var])
  ci_low <- beta - 1.96 * se
  ci_high <- beta + 1.96 * se
  apc <- (exp(beta) - 1) * 100
  apc_low <- (exp(ci_low) - 1) * 100
  apc_high <- (exp(ci_high) - 1) * 100
  p_val <- 2 * (1 - pnorm(abs(beta / se)))
  rho <- as.numeric(coef(mod$modelStruct$corStruct, unconstrained = FALSE))
  list(APC = apc, CI_low = apc_low, CI_high = apc_high, p = p_val, rho = rho,
       beta = beta, se = se)
}

# Diretório de output
dir_out <- file.path(getwd(), "resultados")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_out, "figuras"), showWarnings = FALSE)
dir.create(file.path(dir_out, "tabelas"), showWarnings = FALSE)
dir.create(file.path(dir_out, "suplementar"), showWarnings = FALSE)

set.seed(2025)

cat("========================================\n")
cat("  ANÁLISE DE MORTALIDADE MATERNA BRASIL\n")
cat("  Período: 2000-2023\n")
cat("========================================\n\n")

# ============================================================================
# 1. SIMULAÇÃO DE DADOS REALISTAS (baseados na literatura SIM/SINASC)
# ============================================================================

cat("1. Gerando dados simulados baseados na literatura...\n")

# --- 1.1 Parâmetros baseados na literatura ---
# RMM Brasil: ~73/100mil NV em 2000, ~57 em 2015, ~55 em 2019,
# ~107 em 2021 (COVID), ~56 em 2023

anos <- 2000:2023
n_anos <- length(anos)

regioes <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
ufs <- data.frame(
  uf = c("AC","AM","AP","PA","RO","RR","TO",
         "AL","BA","CE","MA","PB","PE","PI","RN","SE",
         "ES","MG","RJ","SP",
         "PR","RS","SC",
         "DF","GO","MS","MT"),
  regiao = c(rep("Norte",7), rep("Nordeste",9), rep("Sudeste",4),
             rep("Sul",3), rep("Centro-Oeste",4)),
  stringsAsFactors = FALSE
)

racas <- c("Branca", "Preta", "Parda", "Indigena", "Amarela")
faixas_idade <- c("<20", "20-34", ">=35")
escolaridades <- c("Nenhuma/Fund.Inc.", "Fund.Comp./Medio Inc.",
                   "Medio Comp./Sup.Inc.", "Superior Comp.")

causas <- c("Hipertensivas", "Hemorragia", "Infeccao_puerperal",
            "Abortamento", "Causas_indiretas", "Outras_diretas")

# --- 1.2 RMM Nacional por ano (tendência realista) ---
gerar_rmm_nacional <- function(anos) {
  n <- length(anos)
  # Tendência base: declínio 2000-2015, estabilização 2016-2019, pico COVID, recuperação
  rmm_base <- numeric(n)
  for (i in seq_along(anos)) {
    a <- anos[i]
    if (a <= 2010) {
      rmm_base[i] <- 73 - (a - 2000) * 1.8  # declínio pré-Rede Cegonha
    } else if (a <= 2015) {
      rmm_base[i] <- 55 - (a - 2010) * 0.4  # declínio pós-Rede Cegonha
    } else if (a <= 2019) {
      rmm_base[i] <- 53 + (a - 2015) * 0.8  # leve estagnação/aumento
    } else if (a == 2020) {
      rmm_base[i] <- 74  # 1ª onda COVID
    } else if (a == 2021) {
      rmm_base[i] <- 107  # pico COVID (2ª onda)
    } else if (a == 2022) {
      rmm_base[i] <- 62  # recuperação
    } else {
      rmm_base[i] <- 56  # pós-pandemia
    }
  }
  # Adicionar ruído
  rmm_base + rnorm(n, 0, 1.5)
}

rmm_nacional <- data.frame(
  ano = anos,
  rmm = gerar_rmm_nacional(anos)
)

# --- 1.3 RMM por região ---
fatores_regiao <- c(Norte = 1.35, Nordeste = 1.20, Sudeste = 0.85,
                    Sul = 0.75, `Centro-Oeste` = 0.90)

dados_regiao <- expand_grid(ano = anos, regiao = regioes) %>%
  left_join(rmm_nacional, by = "ano") %>%
  mutate(
    fator = fatores_regiao[regiao],
    rmm_regiao = rmm * fator + rnorm(n(), 0, 3),
    nascidos_vivos = case_when(
      regiao == "Norte" ~ round(runif(n(), 280000, 350000)),
      regiao == "Nordeste" ~ round(runif(n(), 800000, 950000)),
      regiao == "Sudeste" ~ round(runif(n(), 1050000, 1250000)),
      regiao == "Sul" ~ round(runif(n(), 380000, 420000)),
      regiao == "Centro-Oeste" ~ round(runif(n(), 220000, 270000))
    ),
    obitos_maternos = round(rmm_regiao * nascidos_vivos / 100000)
  ) %>%
  select(-rmm, -fator)

# --- 1.4 RMM por UF (27 UFs) ---
dados_uf <- ufs %>%
  cross_join(data.frame(ano = anos)) %>%
  left_join(dados_regiao %>% select(ano, regiao, rmm_regiao),
            by = c("ano", "regiao")) %>%
  mutate(
    # Variação por UF
    rmm_uf = rmm_regiao * runif(n(), 0.7, 1.4) + rnorm(n(), 0, 5),
    rmm_uf = pmax(rmm_uf, 10),  # floor
    nascidos_vivos_uf = case_when(
      uf == "SP" ~ round(runif(n(), 580000, 650000)),
      uf == "MG" ~ round(runif(n(), 240000, 290000)),
      uf == "RJ" ~ round(runif(n(), 200000, 240000)),
      uf == "BA" ~ round(runif(n(), 190000, 230000)),
      uf %in% c("PR","RS") ~ round(runif(n(), 130000, 160000)),
      uf %in% c("PA","CE","PE","MA") ~ round(runif(n(), 100000, 160000)),
      TRUE ~ round(runif(n(), 25000, 80000))
    ),
    obitos_maternos_uf = round(rmm_uf * nascidos_vivos_uf / 100000)
  )

# --- 1.5 Dados por raça/cor ---
fatores_raca <- c(Branca = 0.70, Preta = 1.85, Parda = 1.10,
                  Indigena = 2.50, Amarela = 0.65)
prop_raca <- c(Branca = 0.35, Preta = 0.12, Parda = 0.45,
               Indigena = 0.01, Amarela = 0.02)

dados_raca <- expand_grid(ano = anos, raca = racas) %>%
  left_join(rmm_nacional, by = "ano") %>%
  mutate(
    fator = fatores_raca[raca],
    prop = prop_raca[raca],
    rmm_raca = rmm * fator + rnorm(n(), 0, 4),
    rmm_raca = pmax(rmm_raca, 5),
    nascidos_vivos = round(3000000 * prop * runif(n(), 0.9, 1.1)),
    obitos = round(rmm_raca * nascidos_vivos / 100000)
  ) %>%
  select(-rmm, -fator, -prop)

# --- 1.6 Dados por faixa etária ---
fatores_idade <- c(`<20` = 1.15, `20-34` = 0.80, `>=35` = 1.90)
prop_idade <- c(`<20` = 0.18, `20-34` = 0.65, `>=35` = 0.17)

dados_idade <- expand_grid(ano = anos, faixa_idade = faixas_idade) %>%
  left_join(rmm_nacional, by = "ano") %>%
  mutate(
    fator = fatores_idade[faixa_idade],
    prop = prop_idade[faixa_idade],
    rmm_idade = rmm * fator + rnorm(n(), 0, 3),
    rmm_idade = pmax(rmm_idade, 5),
    nascidos_vivos = round(3000000 * prop * runif(n(), 0.9, 1.1)),
    obitos = round(rmm_idade * nascidos_vivos / 100000)
  ) %>%
  select(-rmm, -fator, -prop)

# --- 1.7 Dados por escolaridade ---
fatores_esc <- c(`Nenhuma/Fund.Inc.` = 1.80, `Fund.Comp./Medio Inc.` = 1.20,
                 `Medio Comp./Sup.Inc.` = 0.75, `Superior Comp.` = 0.45)
prop_esc <- c(`Nenhuma/Fund.Inc.` = 0.30, `Fund.Comp./Medio Inc.` = 0.28,
              `Medio Comp./Sup.Inc.` = 0.28, `Superior Comp.` = 0.14)

dados_escolaridade <- expand_grid(ano = anos, escolaridade = escolaridades) %>%
  left_join(rmm_nacional, by = "ano") %>%
  mutate(
    fator = fatores_esc[escolaridade],
    prop = prop_esc[escolaridade],
    rmm_esc = rmm * fator + rnorm(n(), 0, 4),
    rmm_esc = pmax(rmm_esc, 3),
    nascidos_vivos = round(3000000 * prop * runif(n(), 0.9, 1.1)),
    obitos = round(rmm_esc * nascidos_vivos / 100000)
  ) %>%
  select(-rmm, -fator, -prop)

# --- 1.8 Dados por causa específica ---
# Proporções aproximadas das causas ao longo do tempo
prop_causas_base <- c(Hipertensivas = 0.23, Hemorragia = 0.10,
                      Infeccao_puerperal = 0.07, Abortamento = 0.05,
                      Causas_indiretas = 0.30, Outras_diretas = 0.25)

dados_causa <- expand_grid(ano = anos, causa = causas) %>%
  left_join(rmm_nacional, by = "ano") %>%
  mutate(
    prop_base = prop_causas_base[causa],
    # COVID aumentou causas indiretas
    prop_ajust = case_when(
      causa == "Causas_indiretas" & ano %in% 2020:2021 ~ prop_base + 0.20,
      causa != "Causas_indiretas" & ano %in% 2020:2021 ~ prop_base * 0.75,
      TRUE ~ prop_base
    ),
    rmm_causa = rmm * prop_ajust + rnorm(n(), 0, 1),
    rmm_causa = pmax(rmm_causa, 0.5)
  ) %>%
  select(-rmm, -prop_base, -prop_ajust)

# --- 1.9 Covariáveis do sistema de saúde ---
dados_covars <- data.frame(
  ano = anos,
  cobertura_prenatal_7plus = pmin(85, 45 + (anos - 2000) * 1.8 + rnorm(n_anos, 0, 1)),
  taxa_cesareas = pmin(58, 38 + (anos - 2000) * 0.85 + rnorm(n_anos, 0, 0.8)),
  cobertura_esf = pmin(78, 30 + (anos - 2000) * 2.2 + rnorm(n_anos, 0, 1.5)),
  parto_hospitalar = pmin(99, 95 + (anos - 2000) * 0.18 + rnorm(n_anos, 0, 0.3)),
  rede_cegonha = ifelse(anos >= 2011, 1, 0),
  covid = case_when(
    anos == 2020 ~ 1,
    anos == 2021 ~ 2,  # intensidade maior
    anos == 2022 ~ 0.5,
    TRUE ~ 0
  ),
  idhm_medio = 0.612 + (anos - 2000) * 0.004 + rnorm(n_anos, 0, 0.002)
)

cat("   Dados simulados gerados com sucesso.\n\n")

# ============================================================================
# 2. TABELAS DESCRITIVAS
# ============================================================================

cat("2. Construindo tabelas descritivas...\n")

# --- Tabela 1: Características gerais por período ---
periodos <- cut(rmm_nacional$ano,
                breaks = c(1999, 2010, 2019, 2021, 2024),
                labels = c("2000-2010\n(Pré-Rede Cegonha)",
                           "2011-2019\n(Pós-Rede Cegonha)",
                           "2020-2021\n(COVID-19)",
                           "2022-2023\n(Pós-pandemia)"))

tabela1_data <- rmm_nacional %>%
  mutate(periodo = cut(ano,
                       breaks = c(1999, 2010, 2019, 2021, 2024),
                       labels = c("Pré-Rede Cegonha (2000-2010)",
                                  "Pós-Rede Cegonha (2011-2019)",
                                  "COVID-19 (2020-2021)",
                                  "Pós-pandemia (2022-2023)"))) %>%
  left_join(dados_covars, by = "ano")

tab1_resumo <- tabela1_data %>%
  group_by(periodo) %>%
  summarise(
    n_anos = n(),
    rmm_media = round(mean(rmm), 1),
    rmm_dp = round(sd(rmm), 1),
    rmm_min = round(min(rmm), 1),
    rmm_max = round(max(rmm), 1),
    prenatal_media = round(mean(cobertura_prenatal_7plus), 1),
    cesareas_media = round(mean(taxa_cesareas), 1),
    esf_media = round(mean(cobertura_esf), 1),
    idhm_media = round(mean(idhm_medio), 3),
    .groups = "drop"
  )

# Formatar Tabela 1
tab1_gt <- tab1_resumo %>%
  gt() %>%
  tab_header(
    title = "Tabela 1. Características descritivas por período de análise",
    subtitle = "Mortalidade materna no Brasil, 2000-2023"
  ) %>%
  cols_label(
    periodo = "Período",
    n_anos = "N (anos)",
    rmm_media = "RMM Média",
    rmm_dp = "DP",
    rmm_min = "Min",
    rmm_max = "Máx",
    prenatal_media = "Pré-natal ≥7 (%)",
    cesareas_media = "Cesáreas (%)",
    esf_media = "Cobertura ESF (%)",
    idhm_media = "IDHM Médio"
  ) %>%
  tab_spanner(label = "RMM (por 100.000 NV)", columns = c(rmm_media, rmm_dp, rmm_min, rmm_max)) %>%
  tab_spanner(label = "Indicadores do Sistema", columns = c(prenatal_media, cesareas_media, esf_media, idhm_media)) %>%
  tab_source_note("RMM = Razão de Mortalidade Materna; NV = Nascidos Vivos; DP = Desvio Padrão; ESF = Estratégia Saúde da Família; IDHM = Índice de Desenvolvimento Humano Municipal.")

gtsave(tab1_gt, file.path(dir_out, "tabelas", "tabela1_descritiva.html"))
cat("   Tabela 1 salva.\n")

# --- Tabela 2: RMM por região e período ---
tab2_data <- dados_regiao %>%
  mutate(periodo = cut(ano,
                       breaks = c(1999, 2010, 2019, 2021, 2024),
                       labels = c("2000-2010", "2011-2019", "2020-2021", "2022-2023"))) %>%
  group_by(regiao, periodo) %>%
  summarise(
    rmm_media = round(mean(rmm_regiao), 1),
    rmm_dp = round(sd(rmm_regiao), 1),
    total_obitos = sum(obitos_maternos),
    total_nv = sum(nascidos_vivos),
    .groups = "drop"
  ) %>%
  mutate(rmm_calc = round(total_obitos / total_nv * 100000, 1))

tab2_wide <- tab2_data %>%
  select(regiao, periodo, rmm_media) %>%
  pivot_wider(names_from = periodo, values_from = rmm_media)

tab2_gt <- tab2_wide %>%
  gt() %>%
  tab_header(
    title = "Tabela 2. Razão de Mortalidade Materna média por região e período",
    subtitle = "Brasil, 2000-2023 (por 100.000 nascidos vivos)"
  ) %>%
  cols_label(regiao = "Região")

gtsave(tab2_gt, file.path(dir_out, "tabelas", "tabela2_regiao_periodo.html"))
cat("   Tabela 2 salva.\n")

# --- Tabela 3: RMM por raça/cor ---
tab3_data <- dados_raca %>%
  mutate(periodo = cut(ano,
                       breaks = c(1999, 2010, 2019, 2021, 2024),
                       labels = c("2000-2010", "2011-2019", "2020-2021", "2022-2023"))) %>%
  group_by(raca, periodo) %>%
  summarise(rmm_media = round(mean(rmm_raca), 1), .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = rmm_media)

tab3_gt <- tab3_data %>%
  gt() %>%
  tab_header(
    title = "Tabela 3. Razão de Mortalidade Materna por raça/cor e período",
    subtitle = "Brasil, 2000-2023 (por 100.000 nascidos vivos)"
  ) %>%
  cols_label(raca = "Raça/Cor")

gtsave(tab3_gt, file.path(dir_out, "tabelas", "tabela3_raca_periodo.html"))
cat("   Tabela 3 salva.\n")

# --- Tabela 4: RMM por faixa etária ---
tab4_data <- dados_idade %>%
  mutate(periodo = cut(ano,
                       breaks = c(1999, 2010, 2019, 2021, 2024),
                       labels = c("2000-2010", "2011-2019", "2020-2021", "2022-2023"))) %>%
  group_by(faixa_idade, periodo) %>%
  summarise(rmm_media = round(mean(rmm_idade), 1), .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = rmm_media)

tab4_gt <- tab4_data %>%
  gt() %>%
  tab_header(
    title = "Tabela 4. Razão de Mortalidade Materna por faixa etária e período",
    subtitle = "Brasil, 2000-2023 (por 100.000 nascidos vivos)"
  ) %>%
  cols_label(faixa_idade = "Faixa Etária")

gtsave(tab4_gt, file.path(dir_out, "tabelas", "tabela4_idade_periodo.html"))
cat("   Tabela 4 salva.\n")

# ============================================================================
# 3. FIGURAS
# ============================================================================

cat("\n3. Gerando figuras...\n")

tema_artigo <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# --- Figura 1: Tendência da RMM nacional ---
fig1 <- ggplot(rmm_nacional, aes(x = ano, y = rmm)) +
  geom_line(linewidth = 1, color = "#2C3E50") +
  geom_point(size = 2, color = "#2C3E50") +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "#E74C3C", alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "#3498DB", alpha = 0.7) +
  annotate("text", x = 2011.5, y = max(rmm_nacional$rmm) * 0.95,
           label = "Rede Cegonha", color = "#E74C3C", hjust = 0, size = 3.2) +
  annotate("text", x = 2020.3, y = max(rmm_nacional$rmm) * 0.90,
           label = "COVID-19", color = "#3498DB", hjust = 0, size = 3.2) +
  annotate("rect", xmin = 2020, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "#3498DB") +
  labs(
    title = "Figura 1. Tendência da Razão de Mortalidade Materna no Brasil, 2000-2023",
    subtitle = "RMM por 100.000 nascidos vivos",
    x = "Ano", y = "RMM (por 100.000 NV)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 2)) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig1_tendencia_nacional.png"), fig1,
       width = 10, height = 6, dpi = 300)
ggsave(file.path(dir_out, "figuras", "fig1_tendencia_nacional.pdf"), fig1,
       width = 10, height = 6)
cat("   Figura 1 salva.\n")

# --- Figura 2: RMM por região ---
cores_regiao <- c("Norte" = "#E74C3C", "Nordeste" = "#F39C12",
                  "Sudeste" = "#27AE60", "Sul" = "#3498DB",
                  "Centro-Oeste" = "#9B59B6")

fig2 <- ggplot(dados_regiao, aes(x = ano, y = rmm_regiao, color = regiao)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 2011, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.4) +
  scale_color_manual(values = cores_regiao) +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(
    title = "Figura 2. Razão de Mortalidade Materna por região, Brasil, 2000-2023",
    subtitle = "RMM por 100.000 nascidos vivos",
    x = "Ano", y = "RMM (por 100.000 NV)", color = "Região"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig2_rmm_regiao.png"), fig2,
       width = 10, height = 6, dpi = 300)
cat("   Figura 2 salva.\n")

# --- Figura 3: RMM por raça/cor ---
cores_raca <- c("Branca" = "#3498DB", "Preta" = "#E74C3C",
                "Parda" = "#F39C12", "Indigena" = "#27AE60",
                "Amarela" = "#9B59B6")

fig3 <- ggplot(dados_raca, aes(x = ano, y = rmm_raca, color = raca)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.4) +
  scale_color_manual(values = cores_raca) +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(
    title = "Figura 3. Razão de Mortalidade Materna por raça/cor, Brasil, 2000-2023",
    x = "Ano", y = "RMM (por 100.000 NV)", color = "Raça/Cor"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig3_rmm_raca.png"), fig3,
       width = 10, height = 6, dpi = 300)
cat("   Figura 3 salva.\n")

# --- Figura 4: RMM por faixa etária ---
fig4 <- ggplot(dados_idade, aes(x = ano, y = rmm_idade, color = faixa_idade)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(
    title = "Figura 4. Razão de Mortalidade Materna por faixa etária, Brasil, 2000-2023",
    x = "Ano", y = "RMM (por 100.000 NV)", color = "Faixa Etária"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig4_rmm_idade.png"), fig4,
       width = 10, height = 6, dpi = 300)
cat("   Figura 4 salva.\n")

# --- Figura 5: Causas específicas (áreas empilhadas) ---
dados_causa_prop <- dados_causa %>%
  group_by(ano) %>%
  mutate(prop = rmm_causa / sum(rmm_causa) * 100) %>%
  ungroup()

fig5 <- ggplot(dados_causa_prop, aes(x = ano, y = prop, fill = causa)) +
  geom_area(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2",
                    labels = c("Abortamento", "Causas Indiretas", "Hemorragia",
                               "Hipertensivas", "Infecção Puerperal", "Outras Diretas")) +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(
    title = "Figura 5. Distribuição proporcional das causas de morte materna, Brasil, 2000-2023",
    x = "Ano", y = "Proporção (%)", fill = "Causa"
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig5_causas_proporcao.png"), fig5,
       width = 11, height = 6, dpi = 300)
cat("   Figura 5 salva.\n")

# --- Figura 6: Heatmap UF x Ano ---
heatmap_data <- dados_uf %>%
  select(uf, regiao, ano, rmm_uf) %>%
  mutate(uf = factor(uf, levels = ufs$uf[order(ufs$regiao, ufs$uf)]))

fig6 <- ggplot(heatmap_data, aes(x = ano, y = uf, fill = rmm_uf)) +
  geom_tile() +
  scale_fill_viridis(option = "magma", direction = -1, name = "RMM") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(
    title = "Figura 6. Mapa de calor da RMM por Unidade da Federação, 2000-2023",
    x = "Ano", y = "UF"
  ) +
  tema_artigo +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(dir_out, "figuras", "fig6_heatmap_uf.png"), fig6,
       width = 12, height = 8, dpi = 300)
cat("   Figura 6 salva.\n")

# --- Figura 7: Escolaridade ---
fig7 <- ggplot(dados_escolaridade, aes(x = ano, y = rmm_esc, color = escolaridade)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(2000, 2023, 4)) +
  labs(
    title = "Figura 7. Razão de Mortalidade Materna por escolaridade, Brasil, 2000-2023",
    x = "Ano", y = "RMM (por 100.000 NV)", color = "Escolaridade"
  ) +
  tema_artigo +
  theme(legend.text = element_text(size = 8))

ggsave(file.path(dir_out, "figuras", "fig7_rmm_escolaridade.png"), fig7,
       width = 10, height = 6, dpi = 300)
cat("   Figura 7 salva.\n")

# ============================================================================
# 4. MODELOS - PRAIS-WINSTEN (TENDÊNCIA TEMPORAL)
# ============================================================================

cat("\n4. Modelos de tendência temporal (Prais-Winsten)...\n")

# --- 4.1 Modelo nacional (log-RMM) ---
rmm_nacional <- rmm_nacional %>%
  mutate(log_rmm = log(rmm), tempo = ano - min(ano) + 1)

# Prais-Winsten (GLS com AR1) para tendência geral
pw_nacional <- pw_fit(log_rmm ~ tempo, data = rmm_nacional)
res_nac <- pw_apc(pw_nacional)

apc_nacional <- res_nac$APC
apc_ci_low <- res_nac$CI_low
apc_ci_high <- res_nac$CI_high

cat(sprintf("   APC Nacional (2000-2023): %.2f%% (IC95%%: %.2f; %.2f)\n",
            apc_nacional, apc_ci_low, apc_ci_high))

# --- 4.2 Prais-Winsten por período ---
periodos_analise <- list(
  "Pre-Rede Cegonha (2000-2010)" = 2000:2010,
  "Pos-Rede Cegonha (2011-2019)" = 2011:2019,
  "Pre-COVID (2000-2019)" = 2000:2019
)

resultados_pw <- map_dfr(names(periodos_analise), function(nome) {
  anos_p <- periodos_analise[[nome]]
  df <- rmm_nacional %>% filter(ano %in% anos_p) %>%
    mutate(tempo = row_number())

  pw <- pw_fit(log_rmm ~ tempo, data = df)
  res <- pw_apc(pw)

  tibble(
    periodo = nome,
    APC = round(res$APC, 2),
    IC95_inf = round(res$CI_low, 2),
    IC95_sup = round(res$CI_high, 2),
    rho = round(res$rho, 3),
    p_valor = round(res$p, 4),
    tendencia = case_when(
      APC > 0 & p_valor < 0.05 ~ "Crescente",
      APC < 0 & p_valor < 0.05 ~ "Decrescente",
      TRUE ~ "Estacionária"
    )
  )
})

cat("   Resultados por período:\n")
print(as.data.frame(resultados_pw))

# --- 4.3 Prais-Winsten por região ---
resultados_pw_regiao <- map_dfr(unique(dados_regiao$regiao), function(reg) {
  df <- dados_regiao %>%
    filter(regiao == reg) %>%
    arrange(ano) %>%
    mutate(log_rmm = log(rmm_regiao), tempo = row_number())

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
})

cat("\n   Resultados por região:\n")
print(as.data.frame(resultados_pw_regiao))

# --- Tabela 5: Resultados Prais-Winsten ---
tab5_data <- bind_rows(
  resultados_pw %>% mutate(estrato = periodo) %>% select(estrato, APC, IC95_inf, IC95_sup, p_valor, tendencia),
  resultados_pw_regiao %>% mutate(estrato = regiao) %>% select(estrato, APC, IC95_inf, IC95_sup, p_valor, tendencia)
)

tab5_gt <- tab5_data %>%
  mutate(APC_IC = paste0(APC, " (", IC95_inf, "; ", IC95_sup, ")")) %>%
  select(estrato, APC_IC, p_valor, tendencia) %>%
  gt() %>%
  tab_header(
    title = "Tabela 5. Variação Percentual Anual (APC) da RMM — Regressão de Prais-Winsten",
    subtitle = "Brasil e regiões, 2000-2023"
  ) %>%
  cols_label(
    estrato = "Estrato",
    APC_IC = "APC % (IC 95%)",
    p_valor = "p-valor",
    tendencia = "Tendência"
  )

gtsave(tab5_gt, file.path(dir_out, "tabelas", "tabela5_prais_winsten.html"))
cat("   Tabela 5 (Prais-Winsten) salva.\n")

# ============================================================================
# 5. SÉRIES TEMPORAIS INTERROMPIDAS (ITS)
# ============================================================================

cat("\n5. Séries temporais interrompidas (ITS)...\n")

# --- 5.1 ITS - Rede Cegonha (2011) ---
its_data <- rmm_nacional %>%
  mutate(
    # Rede Cegonha
    pos_rc = ifelse(ano >= 2011, 1, 0),
    tempo_pos_rc = ifelse(ano >= 2011, ano - 2011 + 1, 0),
    # COVID
    pos_covid = ifelse(ano >= 2020, 1, 0),
    tempo_pos_covid = ifelse(ano >= 2020, ano - 2020 + 1, 0),
    # COVID dummy puro (2020-2021)
    covid_pico = ifelse(ano %in% 2020:2021, 1, 0)
  )

# Modelo ITS completo
its_modelo <- lm(log_rmm ~ tempo + pos_rc + tempo_pos_rc +
                   covid_pico + pos_covid + tempo_pos_covid,
                 data = its_data)

# Erros robustos (Newey-West)
nw_vcov <- NeweyWest(its_modelo, lag = 2)
its_coeftest <- coeftest(its_modelo, vcov = nw_vcov)

cat("   Modelo ITS (Rede Cegonha + COVID-19):\n")
print(its_coeftest)

# --- Tabela 6: Resultados ITS ---
its_results <- tidy(its_coeftest) %>%
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

tab6_gt <- its_results %>%
  gt() %>%
  tab_header(
    title = "Tabela 6. Modelo de Séries Temporais Interrompidas (ITS)",
    subtitle = "Efeito da Rede Cegonha (2011) e COVID-19 (2020-2021) na log(RMM)"
  ) %>%
  cols_label(
    term = "Parâmetro",
    estimate = "Coeficiente (β)",
    std.error = "EP (Newey-West)",
    statistic = "t",
    p.value = "p-valor"
  ) %>%
  tab_source_note("EP = Erro Padrão com correção Newey-West (lag=2). Variável dependente: log(RMM).")

gtsave(tab6_gt, file.path(dir_out, "tabelas", "tabela6_ITS.html"))
cat("   Tabela 6 (ITS) salva.\n")

# --- Figura 8: ITS com contrafactual ---
its_data$predicted <- exp(predict(its_modelo))

# Contrafactual (sem Rede Cegonha e sem COVID)
its_data_cf <- its_data %>%
  mutate(
    cf = exp(coef(its_modelo)["(Intercept)"] + coef(its_modelo)["tempo"] * tempo)
  )

fig8 <- ggplot(its_data_cf, aes(x = ano)) +
  geom_point(aes(y = rmm), size = 2.5, color = "#2C3E50") +
  geom_line(aes(y = predicted, color = "Modelo ITS"), linewidth = 1) +
  geom_line(aes(y = cf, color = "Contrafactual (sem intervenções)"),
            linewidth = 0.9, linetype = "dashed") +
  geom_vline(xintercept = 2011, linetype = "dotted", alpha = 0.6) +
  geom_vline(xintercept = 2020, linetype = "dotted", alpha = 0.6) +
  annotate("rect", xmin = 2020, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "red") +
  scale_color_manual(values = c("Modelo ITS" = "#E74C3C",
                                "Contrafactual (sem intervenções)" = "#3498DB")) +
  scale_x_continuous(breaks = seq(2000, 2023, 2)) +
  labs(
    title = "Figura 8. Séries Temporais Interrompidas — Modelo ajustado e contrafactual",
    subtitle = "Intervenções: Rede Cegonha (2011) e COVID-19 (2020-2021)",
    x = "Ano", y = "RMM (por 100.000 NV)", color = ""
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig8_ITS_contrafactual.png"), fig8,
       width = 11, height = 6, dpi = 300)
cat("   Figura 8 salva.\n")

# ============================================================================
# 6. MODELOS BRUTOS E AJUSTADOS (REGRESSÃO)
# ============================================================================

cat("\n6. Modelos brutos e ajustados...\n")

# Dados para modelos
modelo_data <- rmm_nacional %>%
  left_join(dados_covars, by = "ano") %>%
  mutate(
    log_rmm = log(rmm),
    prenatal_z = scale(cobertura_prenatal_7plus),
    cesareas_z = scale(taxa_cesareas),
    esf_z = scale(cobertura_esf),
    idhm_z = scale(idhm_medio)
  )

# --- Modelos brutos (univariados) ---
vars_explicativas <- c("cobertura_prenatal_7plus", "taxa_cesareas",
                       "cobertura_esf", "parto_hospitalar",
                       "rede_cegonha", "covid", "idhm_medio")

nomes_vars <- c("Cobertura pré-natal ≥7 consultas (%)",
                "Taxa de cesáreas (%)",
                "Cobertura ESF (%)",
                "Parto hospitalar (%)",
                "Rede Cegonha (sim/não)",
                "COVID-19 (intensidade)",
                "IDHM médio")

modelos_brutos <- map2_dfr(vars_explicativas, nomes_vars, function(var, nome) {
  formula <- as.formula(paste("log_rmm ~", var))
  mod <- lm(formula, data = modelo_data)
  ci <- confint(mod)

  beta <- coef(mod)[var]
  se <- summary(mod)$coefficients[var, "Std. Error"]
  p <- summary(mod)$coefficients[var, "Pr(>|t|)"]
  r2 <- summary(mod)$r.squared

  tibble(
    variavel = nome,
    beta_bruto = round(beta, 4),
    EP = round(se, 4),
    IC95_inf = round(ci[var, 1], 4),
    IC95_sup = round(ci[var, 2], 4),
    p_valor = round(p, 4),
    R2 = round(r2, 3)
  )
})

cat("   Modelos brutos:\n")
print(as.data.frame(modelos_brutos))

# --- Modelo ajustado (multivariado) ---
mod_ajustado <- lm(log_rmm ~ cobertura_prenatal_7plus + taxa_cesareas +
                     cobertura_esf + rede_cegonha + covid + idhm_medio,
                   data = modelo_data)

mod_ajust_sum <- summary(mod_ajustado)
ci_ajust <- confint(mod_ajustado)

resultados_ajustados <- tidy(mod_ajustado) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    variavel = case_when(
      term == "cobertura_prenatal_7plus" ~ "Cobertura pré-natal ≥7 consultas (%)",
      term == "taxa_cesareas" ~ "Taxa de cesáreas (%)",
      term == "cobertura_esf" ~ "Cobertura ESF (%)",
      term == "rede_cegonha" ~ "Rede Cegonha (sim/não)",
      term == "covid" ~ "COVID-19 (intensidade)",
      term == "idhm_medio" ~ "IDHM médio"
    ),
    beta_ajustado = round(estimate, 4),
    EP_ajust = round(std.error, 4),
    IC95_inf_ajust = round(ci_ajust[term, 1], 4),
    IC95_sup_ajust = round(ci_ajust[term, 2], 4),
    p_valor_ajust = round(p.value, 4)
  ) %>%
  select(variavel, beta_ajustado, EP_ajust, IC95_inf_ajust, IC95_sup_ajust, p_valor_ajust)

cat("\n   Modelo ajustado (R² = ", round(mod_ajust_sum$r.squared, 3),
    ", R² adj = ", round(mod_ajust_sum$adj.r.squared, 3), "):\n")
print(as.data.frame(resultados_ajustados))

# --- Tabela 7: Modelos brutos e ajustados ---
tab7_data <- modelos_brutos %>%
  left_join(resultados_ajustados, by = "variavel") %>%
  mutate(
    bruto = paste0(beta_bruto, " (", IC95_inf, "; ", IC95_sup, ")"),
    ajustado = ifelse(is.na(beta_ajustado), "—",
                      paste0(beta_ajustado, " (", IC95_inf_ajust, "; ", IC95_sup_ajust, ")"))
  ) %>%
  select(variavel, bruto, p_valor, ajustado, p_valor_ajust)

tab7_gt <- tab7_data %>%
  gt() %>%
  tab_header(
    title = "Tabela 7. Associação entre covariáveis e log(RMM) — Modelos brutos e ajustado",
    subtitle = "Regressão linear, Brasil, 2000-2023"
  ) %>%
  cols_label(
    variavel = "Variável",
    bruto = "β Bruto (IC 95%)",
    p_valor = "p (bruto)",
    ajustado = "β Ajustado (IC 95%)",
    p_valor_ajust = "p (ajustado)"
  ) %>%
  tab_spanner(label = "Análise Bruta", columns = c(bruto, p_valor)) %>%
  tab_spanner(label = "Análise Ajustada", columns = c(ajustado, p_valor_ajust)) %>%
  tab_source_note(paste0("Modelo ajustado: R² = ", round(mod_ajust_sum$r.squared, 3),
                         "; R² ajustado = ", round(mod_ajust_sum$adj.r.squared, 3),
                         ". Variável dependente: log(RMM)."))

gtsave(tab7_gt, file.path(dir_out, "tabelas", "tabela7_bruto_ajustado.html"))
cat("   Tabela 7 (bruto vs ajustado) salva.\n")

# ============================================================================
# 7. PREVISÃO EXPLORATÓRIA (ARIMA/ETS)
# ============================================================================

cat("\n7. Previsão exploratória (ARIMA)...\n")

ts_rmm <- ts(rmm_nacional$rmm, start = 2000, frequency = 1)

# Auto ARIMA
fit_arima <- auto.arima(ts_rmm)
fc_arima <- forecast(fit_arima, h = 3)

cat("   Modelo ARIMA selecionado:", paste(arimaorder(fit_arima), collapse = ","), "\n")
cat("   Previsão 2024-2026:\n")
print(fc_arima)

# ETS
fit_ets <- ets(ts_rmm)
fc_ets <- forecast(fit_ets, h = 3)

# --- Figura 9: Previsão ---
fc_df <- data.frame(
  ano = 2024:2026,
  arima_mean = as.numeric(fc_arima$mean),
  arima_lo80 = as.numeric(fc_arima$lower[,1]),
  arima_hi80 = as.numeric(fc_arima$upper[,1]),
  arima_lo95 = as.numeric(fc_arima$lower[,2]),
  arima_hi95 = as.numeric(fc_arima$upper[,2])
)

fig9 <- ggplot() +
  geom_line(data = rmm_nacional, aes(x = ano, y = rmm), linewidth = 1, color = "#2C3E50") +
  geom_point(data = rmm_nacional, aes(x = ano, y = rmm), size = 2, color = "#2C3E50") +
  geom_ribbon(data = fc_df, aes(x = ano, ymin = arima_lo95, ymax = arima_hi95),
              alpha = 0.15, fill = "#E74C3C") +
  geom_ribbon(data = fc_df, aes(x = ano, ymin = arima_lo80, ymax = arima_hi80),
              alpha = 0.25, fill = "#E74C3C") +
  geom_line(data = fc_df, aes(x = ano, y = arima_mean),
            linewidth = 1, color = "#E74C3C", linetype = "dashed") +
  geom_point(data = fc_df, aes(x = ano, y = arima_mean),
             size = 2.5, color = "#E74C3C", shape = 17) +
  scale_x_continuous(breaks = seq(2000, 2026, 2)) +
  labs(
    title = "Figura 9. Previsão exploratória da RMM — ARIMA",
    subtitle = paste0("Modelo: ARIMA(", paste(arimaorder(fit_arima), collapse=","),
                      ") | Previsão 2024-2026 com IC 80% e 95%"),
    x = "Ano", y = "RMM (por 100.000 NV)",
    caption = "Nota: Previsão exploratória/inercial. Não incorpora mudanças futuras de política."
  ) +
  tema_artigo

ggsave(file.path(dir_out, "figuras", "fig9_previsao_arima.png"), fig9,
       width = 10, height = 6, dpi = 300)
cat("   Figura 9 salva.\n")

# ============================================================================
# 8. MATERIAL SUPLEMENTAR
# ============================================================================

cat("\n8. Gerando material suplementar...\n")

# --- Tabela S1: RMM por UF e período ---
tab_s1 <- dados_uf %>%
  mutate(periodo = cut(ano,
                       breaks = c(1999, 2010, 2019, 2021, 2024),
                       labels = c("2000-2010", "2011-2019", "2020-2021", "2022-2023"))) %>%
  group_by(uf, regiao, periodo) %>%
  summarise(rmm_media = round(mean(rmm_uf), 1), .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = rmm_media) %>%
  arrange(regiao, uf)

tab_s1_gt <- tab_s1 %>%
  gt(groupname_col = "regiao") %>%
  tab_header(
    title = "Tabela Suplementar S1. RMM média por UF e período",
    subtitle = "Brasil, 2000-2023 (por 100.000 nascidos vivos)"
  ) %>%
  cols_label(uf = "UF")

gtsave(tab_s1_gt, file.path(dir_out, "suplementar", "tabela_S1_uf_periodo.html"))

# --- Tabela S2: Prais-Winsten por UF ---
resultados_pw_uf <- map_dfr(seq_len(nrow(ufs)), function(i) {
  u <- ufs$uf[i]
  r <- ufs$regiao[i]
  df <- dados_uf %>%
    filter(uf == u) %>%
    arrange(ano) %>%
    mutate(log_rmm = log(rmm_uf), tempo = row_number())

  pw <- tryCatch(pw_fit(log_rmm ~ tempo, data = df),
                 error = function(e) NULL)

  if (is.null(pw)) {
    return(tibble(uf = u, regiao = r, APC = NA,
                  IC95_inf = NA, IC95_sup = NA,
                  p_valor = NA, tendencia = NA))
  }

  res <- pw_apc(pw)
  tibble(
    uf = u, regiao = r,
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

tab_s2_gt <- resultados_pw_uf %>%
  mutate(APC_IC = paste0(APC, " (", IC95_inf, "; ", IC95_sup, ")")) %>%
  select(uf, regiao, APC_IC, p_valor, tendencia) %>%
  gt(groupname_col = "regiao") %>%
  tab_header(
    title = "Tabela Suplementar S2. APC da RMM por UF — Prais-Winsten",
    subtitle = "Brasil, 2000-2023"
  ) %>%
  cols_label(
    uf = "UF",
    APC_IC = "APC % (IC 95%)",
    p_valor = "p-valor",
    tendencia = "Tendência"
  )

gtsave(tab_s2_gt, file.path(dir_out, "suplementar", "tabela_S2_pw_uf.html"))

# --- Tabela S3: Diagnósticos do modelo ITS ---
diag_its <- data.frame(
  Teste = c("Durbin-Watson", "R²", "R² ajustado", "AIC", "BIC", "Shapiro-Wilk (resíduos)"),
  Valor = c(
    round(dwtest(its_modelo)$statistic, 3),
    round(summary(its_modelo)$r.squared, 3),
    round(summary(its_modelo)$adj.r.squared, 3),
    round(AIC(its_modelo), 2),
    round(BIC(its_modelo), 2),
    round(shapiro.test(residuals(its_modelo))$p.value, 4)
  )
)

tab_s3_gt <- diag_its %>%
  gt() %>%
  tab_header(
    title = "Tabela Suplementar S3. Diagnósticos do modelo ITS"
  )

gtsave(tab_s3_gt, file.path(dir_out, "suplementar", "tabela_S3_diagnosticos_ITS.html"))

# --- Figura S1: Diagnóstico de resíduos ---
png(file.path(dir_out, "suplementar", "fig_S1_diagnostico_residuos.png"),
    width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(its_modelo, which = 1:4)
dev.off()

# --- Figura S2: Pequenos múltiplos por UF ---
fig_s2 <- dados_uf %>%
  ggplot(aes(x = ano, y = rmm_uf)) +
  geom_line(color = "#2C3E50", linewidth = 0.5) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "#E74C3C", alpha = 0.4, linewidth = 0.3) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "#3498DB", alpha = 0.4, linewidth = 0.3) +
  facet_wrap(~ uf, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  labs(
    title = "Figura S2. Tendência da RMM por Unidade da Federação, 2000-2023",
    x = "Ano", y = "RMM"
  ) +
  theme_minimal(base_size = 8) +
  theme(strip.text = element_text(face = "bold", size = 7))

ggsave(file.path(dir_out, "suplementar", "fig_S2_pequenos_multiplos_uf.png"), fig_s2,
       width = 14, height = 12, dpi = 300)

# --- Tabela S4: Dados brutos completos ---
write_xlsx(list(
  "RMM_Nacional" = rmm_nacional %>% select(ano, rmm),
  "RMM_Regiao" = dados_regiao,
  "RMM_UF" = dados_uf %>% select(uf, regiao, ano, rmm_uf, nascidos_vivos_uf, obitos_maternos_uf),
  "RMM_Raca" = dados_raca,
  "RMM_Idade" = dados_idade,
  "RMM_Escolaridade" = dados_escolaridade,
  "RMM_Causa" = dados_causa,
  "Covariaveis" = dados_covars
), file.path(dir_out, "suplementar", "dados_completos.xlsx"))

cat("   Material suplementar gerado.\n")

# ============================================================================
# 9. EXPORTAR TODAS AS TABELAS EM EXCEL
# ============================================================================

cat("\n9. Exportando tabelas consolidadas...\n")

write_xlsx(list(
  "Tab1_Descritiva" = tab1_resumo,
  "Tab2_Regiao" = tab2_data,
  "Tab3_Raca" = tab3_data,
  "Tab4_Idade" = tab4_data,
  "Tab5_PraisWinsten" = resultados_pw %>% bind_rows(resultados_pw_regiao %>% mutate(periodo = regiao)),
  "Tab6_ITS" = its_results,
  "Tab7_BrutoAjustado" = tab7_data,
  "TabS1_UF" = tab_s1,
  "TabS2_PW_UF" = resultados_pw_uf
), file.path(dir_out, "tabelas", "todas_tabelas.xlsx"))

cat("   Tabelas exportadas em Excel.\n")

# ============================================================================
# 10. SUMÁRIO FINAL
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("  ANÁLISE CONCLUÍDA COM SUCESSO\n")
cat("============================================================\n\n")

cat("RESULTADOS PRINCIPAIS:\n\n")
cat("1. TENDÊNCIA NACIONAL (Prais-Winsten):\n")
cat(sprintf("   APC 2000-2023: %.2f%% (IC95%%: %.2f; %.2f)\n",
            apc_nacional, apc_ci_low, apc_ci_high))
cat("\n")

cat("2. SÉRIES INTERROMPIDAS (ITS):\n")
rc_coef <- its_coeftest["pos_rc",]
covid_coef <- its_coeftest["covid_pico",]
cat(sprintf("   Rede Cegonha (nível): β=%.4f, p=%.4f\n", rc_coef[1], rc_coef[4]))
cat(sprintf("   COVID-19 (nível): β=%.4f, p=%.4f\n", covid_coef[1], covid_coef[4]))
cat("\n")

cat("3. PREVISÃO ARIMA (exploratória):\n")
cat(sprintf("   2024: %.1f (IC95%%: %.1f - %.1f)\n",
            fc_arima$mean[1], fc_arima$lower[1,2], fc_arima$upper[1,2]))
cat(sprintf("   2025: %.1f (IC95%%: %.1f - %.1f)\n",
            fc_arima$mean[2], fc_arima$lower[2,2], fc_arima$upper[2,2]))
cat(sprintf("   2026: %.1f (IC95%%: %.1f - %.1f)\n",
            fc_arima$mean[3], fc_arima$lower[3,2], fc_arima$upper[3,2]))
cat("\n")

cat("FICHEIROS GERADOS:\n")
cat("  Figuras:       ", dir_out, "/figuras/\n")
cat("  Tabelas:       ", dir_out, "/tabelas/\n")
cat("  Suplementar:   ", dir_out, "/suplementar/\n")
cat("\n")

cat("  9 Figuras (PNG + PDF)\n")
cat("  7 Tabelas principais (HTML + Excel)\n")
cat("  4 Itens suplementares (tabelas + figuras + dados)\n")
cat("\n")
cat("============================================================\n")
