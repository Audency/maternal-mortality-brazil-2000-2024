###############################################################################
#  COVARIÁVEIS DETALHADAS E APVP
#  Extração de variáveis adicionais do SIM + cálculo de APVP
#  Inclui: estado civil, local de ocorrência, tipo de morte,
#          óbito na gravidez/puerpério, assistência médica, necropsia,
#          doenças respiratórias (COVID-19, SRAG, pneumonia),
#          comorbidades, investigação do óbito, APVP
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(writexl)
  library(gt)
  library(nlme)
  library(lmtest)
  library(sandwich)
})

dir_dados <- file.path(getwd(), "dados_datasus")
dir_out <- file.path(getwd(), "resultados")
dir.create(file.path(dir_out, "tabelas"), showWarnings = FALSE)
dir.create(file.path(dir_out, "figuras_publicacao"),
           showWarnings = FALSE)

tema_lancet <- theme_classic(base_size = 11, base_family = "Helvetica") +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9, color = "black"),
    axis.line = element_line(linewidth = 0.5, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold")
  )

cat("============================================================\n")
cat("  COVARIÁVEIS DETALHADAS E APVP\n")
cat("============================================================\n\n")

# ============================================================================
# 1. CARREGAR DADOS (reais ou simulados)
# ============================================================================

dados_reais <- file.exists(
  file.path(dir_dados, "sim_obitos_maternos_2000_2024.rds")
)

if (dados_reais) {
  cat("  Usando microdados reais do SIM.\n")
  sim <- readRDS(
    file.path(dir_dados, "sim_obitos_maternos_2000_2024.rds")
  )
} else {
  cat("  Microdados reais não encontrados.\n")
  cat("  Gerando dados simulados com variáveis detalhadas...\n\n")

  set.seed(2025)
  n_total <- 42000  # ~42 mil óbitos em 24 anos

  # Simular microdados detalhados
  anos <- 2000:2023
  prob_ano <- c(
    rep(0.045, 11),  # 2000-2010
    rep(0.040, 9),   # 2011-2019
    0.050, 0.070,    # 2020-2021
    0.042, 0.040     # 2022-2023
  )
  prob_ano <- prob_ano / sum(prob_ano)

  sim <- tibble(
    ano_obito = sample(anos, n_total, replace = TRUE, prob = prob_ano),
    IDADE = sample(15:49, n_total, replace = TRUE,
                   prob = dnorm(15:49, 30, 7)),
    RACACOR = sample(c("1","2","3","4","5"), n_total, replace = TRUE,
                     prob = c(0.30, 0.12, 0.02, 0.48, 0.01)),
    ESC = sample(c("1","2","3","4","5","6","9"), n_total,
                 replace = TRUE,
                 prob = c(0.08, 0.15, 0.12, 0.20, 0.25, 0.12, 0.08)),
    ESTCIV = sample(c("1","2","3","4","5","9"), n_total,
                    replace = TRUE,
                    prob = c(0.25, 0.35, 0.05, 0.03, 0.22, 0.10)),
    LOCOCOR = sample(c("1","2","3","4","5","9"), n_total,
                     replace = TRUE,
                     prob = c(0.85, 0.02, 0.05, 0.03, 0.02, 0.03)),
    TPMORTEOCO = sample(c("1","2","3","8","9"), n_total,
                        replace = TRUE,
                        prob = c(0.30, 0.25, 0.15, 0.10, 0.20)),
    OBITOGRAV = sample(c("1","2","9"), n_total, replace = TRUE,
                       prob = c(0.35, 0.55, 0.10)),
    OBITOPUERP = sample(c("1","2","3","9"), n_total,
                        replace = TRUE,
                        prob = c(0.40, 0.15, 0.05, 0.40)),
    ASSISTMED = sample(c("1","2","9"), n_total, replace = TRUE,
                       prob = c(0.75, 0.10, 0.15)),
    NECROPSIA = sample(c("1","2","9"), n_total, replace = TRUE,
                       prob = c(0.20, 0.65, 0.15)),
    FONTEINV = sample(c("1","2","3","4","5","8","9"), n_total,
                      replace = TRUE,
                      prob = c(0.40, 0.15, 0.10, 0.05, 0.05,
                               0.05, 0.20)),
    CODMUNRES = sample(
      c("110020","130260","150140","211130","230440",
        "261160","292740","310620","330455","355030",
        "410690","431490","500270","510340","520870","530010"),
      n_total, replace = TRUE
    )
  )

  # Gerar CAUSABAS realista — row by row para respeitar ano
  gen_causa <- function(ano, n) {
    r <- runif(n)
    causa <- character(n)
    for (i in seq_len(n)) {
      if (ano[i] %in% 2020:2021 && r[i] < 0.35) {
        causa[i] <- sample(c("U071","B342","J189"), 1,
                           prob = c(0.70, 0.15, 0.15))
      } else if (ano[i] %in% 2022:2023 && r[i] < 0.05) {
        causa[i] <- sample(c("U071","J189","J80"), 1,
                           prob = c(0.40, 0.40, 0.20))
      } else if (r[i] < 0.03) {
        # Pneumonia pré-pandemia (~3% em todos os anos)
        causa[i] <- sample(c("J189","J159","J80"), 1,
                           prob = c(0.50, 0.30, 0.20))
      } else if (r[i] < 0.23) {
        causa[i] <- sample(c("O10","O11","O13","O14","O15","O16"), 1)
      } else if (r[i] < 0.33) {
        causa[i] <- sample(c("O44","O45","O46","O67","O72"), 1)
      } else if (r[i] < 0.40) {
        causa[i] <- sample(c("O85","O86"), 1)
      } else if (r[i] < 0.45) {
        causa[i] <- sample(c("O00","O01","O02","O03","O04","O05",
                              "O06","O07","O08"), 1)
      } else if (r[i] < 0.75) {
        causa[i] <- sample(c("O98","O99"), 1)
      } else {
        causa[i] <- sample(c("O20","O21","O22","O23","O24","O26",
                              "O30","O40","O41","O42","O43",
                              "O60","O62","O63","O71","O73","O74",
                              "O75","O88","O90","O91","O95"), 1)
      }
    }
    causa
  }

  sim$CAUSABAS <- gen_causa(sim$ano_obito, nrow(sim))

  # LINHAA: adicionar doenças respiratórias como causa contribuinte
  sim <- sim %>%
    mutate(
      LINHAA = case_when(
        ano_obito %in% 2020:2021 & runif(n()) < 0.30 ~
          sample(c("U071","B342","J189","J159","J80"), n(),
                 replace = TRUE,
                 prob = c(0.50, 0.10, 0.15, 0.10, 0.15)),
        runif(n()) < 0.03 ~
          sample(c("J189","J159"), n(), replace = TRUE),
        TRUE ~ CAUSABAS
      )
    )
}

# ============================================================================
# 2. PROCESSAR COVARIÁVEIS DETALHADAS
# ============================================================================

cat("2. Processando covariáveis detalhadas...\n")

sim_det <- sim %>%
  mutate(
    ano = ano_obito,
    idade_num = as.numeric(IDADE),

    # Faixa etária detalhada
    faixa_idade = case_when(
      idade_num < 20 ~ "<20",
      idade_num <= 34 ~ "20-34",
      idade_num >= 35 ~ ">=35",
      TRUE ~ "Ignorada"
    ),

    # Raça/cor
    raca_cor = case_when(
      RACACOR == "1" ~ "Branca", RACACOR == "2" ~ "Preta",
      RACACOR == "3" ~ "Amarela", RACACOR == "4" ~ "Parda",
      RACACOR == "5" ~ "Indigena", TRUE ~ "Ignorada"
    ),

    # Escolaridade
    escolaridade = case_when(
      ESC %in% c("1", "2") ~ "Nenhuma/Fund.Inc.",
      ESC %in% c("3", "4") ~ "Fund.Comp./Medio Inc.",
      ESC == "5" ~ "Medio Comp./Sup.Inc.",
      ESC == "6" ~ "Superior Comp.",
      TRUE ~ "Ignorada"
    ),

    # Estado civil
    estado_civil = case_when(
      ESTCIV == "1" ~ "Solteira",
      ESTCIV == "2" ~ "Casada",
      ESTCIV == "3" ~ "Viúva",
      ESTCIV == "4" ~ "Separada/Divorciada",
      ESTCIV == "5" ~ "União estável",
      TRUE ~ "Ignorado"
    ),

    # Local de ocorrência
    local_ocorrencia = case_when(
      LOCOCOR == "1" ~ "Hospital",
      LOCOCOR == "2" ~ "Outro estab. saúde",
      LOCOCOR == "3" ~ "Domicílio",
      LOCOCOR == "4" ~ "Via pública",
      LOCOCOR == "5" ~ "Outro",
      TRUE ~ "Ignorado"
    ),

    # Tipo de morte obstétrica
    tipo_morte = case_when(
      TPMORTEOCO == "1" ~ "Na gravidez",
      TPMORTEOCO == "2" ~ "No parto",
      TPMORTEOCO == "3" ~ "No puerpério (até 42d)",
      TPMORTEOCO == "8" ~ "Não se aplica",
      TRUE ~ "Ignorado"
    ),

    # Óbito na gravidez
    obito_gravidez = case_when(
      OBITOGRAV == "1" ~ "Sim",
      OBITOGRAV == "2" ~ "Não",
      TRUE ~ "Ignorado"
    ),

    # Óbito no puerpério
    obito_puerperio = case_when(
      OBITOPUERP == "1" ~ "Até 42 dias",
      OBITOPUERP == "2" ~ "43 dias a 1 ano",
      OBITOPUERP == "3" ~ "Não no puerpério",
      TRUE ~ "Ignorado"
    ),

    # Assistência médica
    assist_medica = case_when(
      ASSISTMED == "1" ~ "Sim",
      ASSISTMED == "2" ~ "Não",
      TRUE ~ "Ignorado"
    ),

    # Necropsia
    necropsia = case_when(
      NECROPSIA == "1" ~ "Sim",
      NECROPSIA == "2" ~ "Não",
      TRUE ~ "Ignorado"
    ),

    # Causa básica agrupada
    causa_grupo = case_when(
      grepl("^O1[0-6]", CAUSABAS) ~ "Hipertensivas",
      grepl("^O4[4-6]|^O67|^O72", CAUSABAS) ~ "Hemorragia",
      grepl("^O85|^O86", CAUSABAS) ~ "Infecção puerperal",
      grepl("^O0[0-8]", CAUSABAS) ~ "Abortamento",
      grepl("^O98|^O99", CAUSABAS) ~ "Causas indiretas",
      grepl("^U071|^B342", CAUSABAS) ~ "COVID-19",
      grepl("^J", CAUSABAS) ~ "Doenças respiratórias (outras)",
      TRUE ~ "Outras causas diretas"
    ),

    # Doenças respiratórias (qualquer menção)
    doenca_respiratoria = case_when(
      grepl("U071|B342", CAUSABAS) |
        grepl("U071|B342", LINHAA) ~ "COVID-19",
      grepl("J80", CAUSABAS) | grepl("J80", LINHAA) ~ "SRAG",
      grepl("^J18|^J15", CAUSABAS) |
        grepl("^J18|^J15", LINHAA) ~ "Pneumonia",
      grepl("^J", CAUSABAS) | grepl("^J", LINHAA) ~
        "Outra respiratória",
      TRUE ~ "Não respiratória"
    ),

    # Investigação do óbito
    investigacao = case_when(
      FONTEINV %in% c("1","2","3","4","5") ~ "Investigado",
      TRUE ~ "Não investigado/Ignorado"
    ),

    # Período
    periodo = case_when(
      ano <= 2010 ~ "2000-2010",
      ano <= 2019 ~ "2011-2019",
      ano <= 2021 ~ "2020-2021",
      TRUE ~ "2022+"
    )
  )

cat("   Covariáveis processadas.\n")

# ============================================================================
# 3. APVP — ANOS POTENCIAIS DE VIDA PERDIDOS
# ============================================================================

cat("\n3. Calculando APVP...\n")

# Expectativa de vida das mulheres no Brasil: 80.77 anos
IDADE_LIMITE <- 80.77

sim_det <- sim_det %>%
  mutate(
    apvp_individual = pmax(0, IDADE_LIMITE - idade_num)
  )

# APVP por ano
apvp_ano <- sim_det %>%
  group_by(ano) %>%
  summarise(
    n_obitos = n(),
    apvp_total = sum(apvp_individual, na.rm = TRUE),
    apvp_medio = mean(apvp_individual, na.rm = TRUE),
    idade_media = mean(idade_num, na.rm = TRUE),
    .groups = "drop"
  )

cat("   APVP por ano:\n")
print(as.data.frame(apvp_ano %>%
                      mutate(apvp_total = round(apvp_total),
                             apvp_medio = round(apvp_medio, 1),
                             idade_media = round(idade_media, 1))))

# APVP por causa
apvp_causa <- sim_det %>%
  group_by(causa_grupo) %>%
  summarise(
    n_obitos = n(),
    apvp_total = sum(apvp_individual, na.rm = TRUE),
    apvp_medio = mean(apvp_individual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(apvp_total))

cat("\n   APVP por causa:\n")
print(as.data.frame(apvp_causa %>%
                      mutate(apvp_total = round(apvp_total),
                             apvp_medio = round(apvp_medio, 1))))

# APVP por raça/cor
apvp_raca <- sim_det %>%
  filter(raca_cor != "Ignorada") %>%
  group_by(raca_cor) %>%
  summarise(
    n_obitos = n(),
    apvp_total = sum(apvp_individual, na.rm = TRUE),
    apvp_medio = mean(apvp_individual, na.rm = TRUE),
    idade_media = mean(idade_num, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n   APVP por raça/cor:\n")
print(as.data.frame(apvp_raca %>%
                      mutate(apvp_total = round(apvp_total),
                             apvp_medio = round(apvp_medio, 1),
                             idade_media = round(idade_media, 1))))

# ============================================================================
# 4. TABELAS DE COVARIÁVEIS
# ============================================================================

cat("\n4. Gerando tabelas de covariáveis...\n")

# --- Tabela 8: Perfil dos óbitos maternos por período ---
vars_perfil <- c("faixa_idade", "raca_cor", "escolaridade",
                 "estado_civil", "local_ocorrencia", "tipo_morte",
                 "assist_medica", "necropsia", "investigacao",
                 "doenca_respiratoria")

nomes_perfil <- c("Age group", "Race/Ethnicity", "Education",
                  "Marital status", "Place of death",
                  "Type of obstetric death",
                  "Medical assistance", "Autopsy",
                  "Death investigation",
                  "Respiratory disease")

tab8_list <- list()
for (i in seq_along(vars_perfil)) {
  v <- vars_perfil[i]
  nm <- nomes_perfil[i]
  tab_v <- sim_det %>%
    group_by(periodo, .data[[v]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(periodo) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup() %>%
    mutate(
      variavel = nm,
      categoria = .data[[v]],
      n_pct = paste0(n, " (", pct, "%)")
    ) %>%
    select(variavel, categoria, periodo, n_pct) %>%
    pivot_wider(names_from = periodo, values_from = n_pct)

  tab8_list[[i]] <- tab_v
}

tab8 <- bind_rows(tab8_list)

tab8_gt <- tab8 %>%
  gt(groupname_col = "variavel") %>%
  tab_header(
    title = "Table 8. Profile of maternal deaths by period",
    subtitle = "Brazil, 2000-2023"
  ) %>%
  cols_label(categoria = "Category")

gtsave(tab8_gt, file.path(dir_out, "tabelas",
                           "tabela8_covariaveis.html"))
cat("   Tabela 8 salva.\n")

# --- Tabela 9: Doenças respiratórias por período ---
tab9 <- sim_det %>%
  group_by(periodo, doenca_respiratoria) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(periodo) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

tab9_gt <- tab9 %>%
  mutate(n_pct = paste0(n, " (", pct, "%)")) %>%
  select(periodo, doenca_respiratoria, n_pct) %>%
  pivot_wider(names_from = periodo, values_from = n_pct) %>%
  gt() %>%
  tab_header(
    title = "Table 9. Respiratory diseases in maternal deaths",
    subtitle = "Brazil, 2000-2023"
  ) %>%
  cols_label(doenca_respiratoria = "Respiratory condition")

gtsave(tab9_gt, file.path(dir_out, "tabelas",
                           "tabela9_respiratorias.html"))
cat("   Tabela 9 salva.\n")

# --- Tabela 10: APVP por período e causa ---
tab10 <- sim_det %>%
  group_by(periodo, causa_grupo) %>%
  summarise(
    n = n(),
    apvp = round(sum(apvp_individual)),
    apvp_medio = round(mean(apvp_individual), 1),
    .groups = "drop"
  )

tab10_gt <- tab10 %>%
  gt(groupname_col = "periodo") %>%
  tab_header(
    title = "Table 10. Years of Potential Life Lost (YPLL) by cause and period",
    subtitle = paste("Brazil, 2000-2023 | Age limit:", IDADE_LIMITE, "years")
  ) %>%
  cols_label(
    causa_grupo = "Cause of death",
    n = "Deaths",
    apvp = "Total YPLL",
    apvp_medio = "Mean YPLL"
  ) %>%
  fmt_number(columns = c(apvp), decimals = 0, sep_mark = ",")

gtsave(tab10_gt, file.path(dir_out, "tabelas",
                            "tabela10_apvp.html"))
cat("   Tabela 10 salva.\n")

# ============================================================================
# 5. FIGURAS ADICIONAIS
# ============================================================================

cat("\n5. Gerando figuras adicionais...\n")

# --- Figura 10: APVP por ano ---
fig10 <- ggplot(apvp_ano, aes(x = ano, y = apvp_total)) +
  geom_line(linewidth = 0.8, color = "#2C3E50") +
  geom_point(size = 1.8, color = "#2C3E50") +
  geom_vline(xintercept = 2011, linetype = "dashed",
             color = "#C0392B", alpha = 0.5, linewidth = 0.4) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = "#2980B9", alpha = 0.5, linewidth = 0.4) +
  annotate("rect", xmin = 2020, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = "#2980B9") +
  scale_x_continuous(breaks = seq(2000, 2024, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Total YPLL") +
  tema_lancet

ggsave(file.path(dir_out, "figuras_publicacao", "figure10.png"),
       fig10, width = 7, height = 4.2, dpi = 600)
ggsave(file.path(dir_out, "figuras_publicacao", "figure10.pdf"),
       fig10, width = 7, height = 4.2)
cat("   Figura 10 (APVP) salva.\n")

# --- Figura 11: Doenças respiratórias ao longo do tempo ---
# Contar TODAS as doenças respiratórias por ano (incluindo anos com 0)
resp_all <- sim_det %>%
  filter(doenca_respiratoria != "Não respiratória") %>%
  group_by(ano, doenca_respiratoria) %>%
  summarise(n = n(), .groups = "drop")

# Garantir que todos os anos aparecem (completar com 0)
all_combos <- expand_grid(
  ano = sort(unique(sim_det$ano)),
  doenca_respiratoria = c("COVID-19", "SRAG", "Pneumonia",
                          "Outra respiratória")
)
resp_ano <- all_combos %>%
  left_join(resp_all, by = c("ano", "doenca_respiratoria")) %>%
  mutate(n = replace_na(n, 0))

fig11 <- ggplot(resp_ano,
                aes(x = factor(ano), y = n,
                    fill = doenca_respiratoria)) +
  geom_col(alpha = 0.85, width = 0.7) +
  scale_fill_manual(
    values = c("COVID-19" = "#E74C3C", "SRAG" = "#F39C12",
               "Pneumonia" = "#3498DB",
               "Outra respiratória" = "#95A5A6")
  ) +
  scale_x_discrete(
    breaks = as.character(seq(2000, max(sim_det$ano), 2))
  ) +
  labs(x = "Year", y = "Number of deaths",
       fill = "Respiratory condition") +
  tema_lancet +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave(file.path(dir_out, "figuras_publicacao", "figure11.png"),
       fig11, width = 9, height = 4.5, dpi = 600)
ggsave(file.path(dir_out, "figuras_publicacao", "figure11.pdf"),
       fig11, width = 9, height = 4.5)
cat("   Figura 11 (respiratórias) salva.\n")

# --- Figura 12: APVP por causa (barras) ---
fig12 <- apvp_causa %>%
  filter(causa_grupo != "Outras causas diretas") %>%
  ggplot(aes(x = reorder(causa_grupo, apvp_total),
             y = apvp_total, fill = causa_grupo)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "Total YPLL") +
  tema_lancet

ggsave(file.path(dir_out, "figuras_publicacao", "figure12.png"),
       fig12, width = 7, height = 4, dpi = 600)
ggsave(file.path(dir_out, "figuras_publicacao", "figure12.pdf"),
       fig12, width = 7, height = 4)
cat("   Figura 12 (APVP por causa) salva.\n")

# ============================================================================
# 6. EXPORTAR DADOS
# ============================================================================

cat("\n6. Exportando dados...\n")

write_xlsx(list(
  "APVP_Ano" = apvp_ano,
  "APVP_Causa" = apvp_causa,
  "APVP_Raca" = apvp_raca,
  "Perfil_Periodo" = tab8,
  "Respiratorias" = tab9,
  "APVP_Causa_Periodo" = tab10
), file.path(dir_out, "tabelas", "covariaveis_apvp.xlsx"))

cat("   Dados exportados.\n")

cat("\n============================================================\n")
cat("  COVARIÁVEIS E APVP — CONCLUÍDO\n")
cat("============================================================\n")
cat("  3 tabelas novas (Tables 8-10)\n")
cat("  3 figuras novas (Figures 10-12)\n")
cat("  APVP calculado com idade limite", IDADE_LIMITE, "anos\n")
cat("============================================================\n")
