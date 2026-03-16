###############################################################################
#  DOWNLOAD DE DADOS REAIS DO DATASUS
#  SIM (óbitos maternos) + SINASC (nascidos vivos)
#  Período: 2000-2024
###############################################################################

suppressPackageStartupMessages({
  library(microdatasus)
  library(tidyverse)
  library(writexl)
})

dir_dados <- file.path(getwd(), "dados_datasus")
dir.create(dir_dados, showWarnings = FALSE, recursive = TRUE)

cat("============================================================\n")
cat("  DOWNLOAD DE DADOS DO DATASUS\n")
cat("  SIM (Mortalidade) + SINASC (Nascidos Vivos)\n")
cat("  Período: 2000-2024\n")
cat("============================================================\n\n")

# ============================================================================
# 1. DOWNLOAD DOS ÓBITOS MATERNOS (SIM)
# ============================================================================

cat("1. Baixando dados de óbitos (SIM/DATASUS)...\n")
cat("   Isto pode demorar vários minutos por ano.\n\n")

ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
         "RO","RR","RS","SC","SE","SP","TO")

# Baixar SIM ano a ano
anos_download <- 2000:2024

sim_all <- list()

for (ano in anos_download) {
  cat(sprintf("   SIM %d ... ", ano))
  tryCatch({
    # fetch_datasus baixa microdados do SIM
    sim_ano <- fetch_datasus(
      year_start = ano,
      year_end = ano,
      information_system = "SIM-DO"
    )

    # Processar com process_sim
    sim_proc <- process_sim(sim_ano)

    # Filtrar óbitos maternos (CID-10: O00-O99, A34, incluindo tardios)
    # Capítulo XV da CID-10: O00-O99
    sim_materno <- sim_proc %>%
      filter(grepl("^O", CAUSABAS) | CAUSABAS == "A34") %>%
      mutate(ano_obito = ano)

    sim_all[[as.character(ano)]] <- sim_materno
    cat(sprintf("OK (%d óbitos maternos)\n", nrow(sim_materno)))
  }, error = function(e) {
    cat(sprintf("ERRO: %s\n", e$message))
  })
}

# Combinar todos os anos
if (length(sim_all) > 0) {
  sim_completo <- bind_rows(sim_all)

  # Salvar dados brutos
  saveRDS(sim_completo, file.path(dir_dados, "sim_obitos_maternos_2000_2024.rds"))
  cat(sprintf("\n   Total de óbitos maternos baixados: %d\n", nrow(sim_completo)))
  cat("   Arquivo salvo: sim_obitos_maternos_2000_2024.rds\n\n")
} else {
  cat("\n   AVISO: Nenhum dado do SIM foi baixado.\n\n")
}

# ============================================================================
# 2. DOWNLOAD DOS NASCIDOS VIVOS (SINASC)
# ============================================================================

cat("2. Baixando dados de nascidos vivos (SINASC/DATASUS)...\n")
cat("   Isto pode demorar vários minutos por ano.\n\n")

sinasc_all <- list()

for (ano in anos_download) {
  cat(sprintf("   SINASC %d ... ", ano))
  tryCatch({
    sinasc_ano <- fetch_datasus(
      year_start = ano,
      year_end = ano,
      information_system = "SINASC"
    )

    sinasc_proc <- process_sinasc(sinasc_ano)
    sinasc_proc$ano_nasc <- ano

    sinasc_all[[as.character(ano)]] <- sinasc_proc
    cat(sprintf("OK (%d nascidos vivos)\n", nrow(sinasc_proc)))
  }, error = function(e) {
    cat(sprintf("ERRO: %s\n", e$message))
  })
}

if (length(sinasc_all) > 0) {
  sinasc_completo <- bind_rows(sinasc_all)
  saveRDS(sinasc_completo, file.path(dir_dados, "sinasc_nascidos_vivos_2000_2024.rds"))
  cat(sprintf("\n   Total de nascidos vivos baixados: %d\n", nrow(sinasc_completo)))
  cat("   Arquivo salvo: sinasc_nascidos_vivos_2000_2024.rds\n\n")
} else {
  cat("\n   AVISO: Nenhum dado do SINASC foi baixado.\n\n")
}

# ============================================================================
# 3. CONSTRUIR BANCO ANALÍTICO AGREGADO
# ============================================================================

cat("3. Construindo banco analítico agregado...\n")

if (exists("sim_completo") && exists("sinasc_completo")) {

  # --- Mapeamento UF -> Região ---
  uf_regiao <- data.frame(
    uf_sigla = c("AC","AM","AP","PA","RO","RR","TO",
                 "AL","BA","CE","MA","PB","PE","PI","RN","SE",
                 "ES","MG","RJ","SP",
                 "PR","RS","SC",
                 "DF","GO","MS","MT"),
    regiao = c(rep("Norte",7), rep("Nordeste",9), rep("Sudeste",4),
               rep("Sul",3), rep("Centro-Oeste",4)),
    stringsAsFactors = FALSE
  )

  # --- 3.1 Óbitos maternos por ano, UF, causa ---
  # Classificar causas
  sim_analise <- sim_completo %>%
    mutate(
      uf_res = substr(CODMUNRES, 1, 2),
      causa_grupo = case_when(
        grepl("^O1[0-6]", CAUSABAS) ~ "Hipertensivas",
        grepl("^O4[4-6]|^O67|^O72", CAUSABAS) ~ "Hemorragia",
        grepl("^O85|^O86", CAUSABAS) ~ "Infeccao_puerperal",
        grepl("^O0[0-8]", CAUSABAS) ~ "Abortamento",
        grepl("^O98|^O99", CAUSABAS) ~ "Causas_indiretas",
        TRUE ~ "Outras_diretas"
      ),
      # Raça/cor
      raca_cor = case_when(
        RACACOR == "1" ~ "Branca",
        RACACOR == "2" ~ "Preta",
        RACACOR == "3" ~ "Amarela",
        RACACOR == "4" ~ "Parda",
        RACACOR == "5" ~ "Indigena",
        TRUE ~ "Ignorada"
      ),
      # Faixa etária
      idade_num = as.numeric(IDADE),
      faixa_idade = case_when(
        idade_num < 20 ~ "<20",
        idade_num >= 20 & idade_num <= 34 ~ "20-34",
        idade_num >= 35 ~ ">=35",
        TRUE ~ "Ignorada"
      ),
      # Escolaridade
      escolaridade_grupo = case_when(
        ESC %in% c("1", "2") ~ "Nenhuma/Fund.Inc.",
        ESC %in% c("3", "4") ~ "Fund.Comp./Medio Inc.",
        ESC == "5" ~ "Medio Comp./Sup.Inc.",
        ESC == "6" ~ "Superior Comp.",
        TRUE ~ "Ignorada"
      ),
      # Excluir tardios (O96-O97) do indicador principal
      morte_tardia = grepl("^O9[67]", CAUSABAS),
      # CID para indicador oficial: O00-O95, O98-O99
      obito_materno_oficial = grepl("^O", CAUSABAS) &
        !grepl("^O9[67]", CAUSABAS)
    )

  # --- 3.2 Tabela: óbitos por ano e UF ---
  obitos_uf_ano <- sim_analise %>%
    filter(obito_materno_oficial) %>%
    group_by(ano_obito, uf_res) %>%
    summarise(obitos_maternos = n(), .groups = "drop")

  # --- 3.3 Nascidos vivos por ano e UF ---
  sinasc_analise <- sinasc_completo %>%
    mutate(uf_res = substr(CODMUNRES, 1, 2))

  nv_uf_ano <- sinasc_analise %>%
    group_by(ano_nasc, uf_res) %>%
    summarise(nascidos_vivos = n(), .groups = "drop")

  # --- 3.4 Código UF -> Sigla ---
  cod_uf <- data.frame(
    cod = c("11","12","13","14","15","16","17",
            "21","22","23","24","25","26","27","28","29",
            "31","32","33","35",
            "41","42","43",
            "50","51","52","53"),
    uf_sigla = c("RO","AC","AM","RR","PA","AP","TO",
                 "MA","PI","CE","RN","PB","PE","AL","SE","BA",
                 "MG","ES","RJ","SP",
                 "PR","SC","RS",
                 "MS","MT","GO","DF"),
    stringsAsFactors = FALSE
  )

  # --- 3.5 Montar banco analítico ---
  banco_analitico <- obitos_uf_ano %>%
    rename(cod = uf_res, ano = ano_obito) %>%
    full_join(nv_uf_ano %>% rename(cod = uf_res, ano = ano_nasc),
              by = c("ano", "cod")) %>%
    left_join(cod_uf, by = "cod") %>%
    left_join(uf_regiao, by = "uf_sigla") %>%
    mutate(
      obitos_maternos = replace_na(obitos_maternos, 0),
      nascidos_vivos = replace_na(nascidos_vivos, 0),
      rmm = ifelse(nascidos_vivos > 0,
                    obitos_maternos / nascidos_vivos * 100000, NA)
    ) %>%
    filter(!is.na(uf_sigla)) %>%
    arrange(ano, uf_sigla)

  # --- 3.6 Banco nacional ---
  banco_nacional <- banco_analitico %>%
    group_by(ano) %>%
    summarise(
      obitos_maternos = sum(obitos_maternos),
      nascidos_vivos = sum(nascidos_vivos),
      .groups = "drop"
    ) %>%
    mutate(rmm = obitos_maternos / nascidos_vivos * 100000)

  # --- 3.7 Banco por região ---
  banco_regiao <- banco_analitico %>%
    group_by(ano, regiao) %>%
    summarise(
      obitos_maternos = sum(obitos_maternos),
      nascidos_vivos = sum(nascidos_vivos),
      .groups = "drop"
    ) %>%
    mutate(rmm = obitos_maternos / nascidos_vivos * 100000)

  # --- 3.8 Óbitos por raça/cor ---
  obitos_raca <- sim_analise %>%
    filter(obito_materno_oficial) %>%
    group_by(ano_obito, raca_cor) %>%
    summarise(obitos = n(), .groups = "drop") %>%
    rename(ano = ano_obito)

  # --- 3.9 Óbitos por faixa etária ---
  obitos_idade <- sim_analise %>%
    filter(obito_materno_oficial) %>%
    group_by(ano_obito, faixa_idade) %>%
    summarise(obitos = n(), .groups = "drop") %>%
    rename(ano = ano_obito)

  # --- 3.10 Óbitos por escolaridade ---
  obitos_escolaridade <- sim_analise %>%
    filter(obito_materno_oficial) %>%
    group_by(ano_obito, escolaridade_grupo) %>%
    summarise(obitos = n(), .groups = "drop") %>%
    rename(ano = ano_obito)

  # --- 3.11 Óbitos por causa ---
  obitos_causa <- sim_analise %>%
    filter(obito_materno_oficial) %>%
    group_by(ano_obito, causa_grupo) %>%
    summarise(obitos = n(), .groups = "drop") %>%
    rename(ano = ano_obito)

  # --- Salvar tudo ---
  saveRDS(banco_analitico, file.path(dir_dados, "banco_analitico_uf.rds"))
  saveRDS(banco_nacional, file.path(dir_dados, "banco_analitico_nacional.rds"))
  saveRDS(banco_regiao, file.path(dir_dados, "banco_analitico_regiao.rds"))
  saveRDS(obitos_raca, file.path(dir_dados, "obitos_raca.rds"))
  saveRDS(obitos_idade, file.path(dir_dados, "obitos_idade.rds"))
  saveRDS(obitos_escolaridade, file.path(dir_dados, "obitos_escolaridade.rds"))
  saveRDS(obitos_causa, file.path(dir_dados, "obitos_causa.rds"))

  # Excel consolidado
  write_xlsx(list(
    "Nacional" = banco_nacional,
    "Regiao" = banco_regiao,
    "UF" = banco_analitico,
    "Raca_Cor" = obitos_raca,
    "Faixa_Etaria" = obitos_idade,
    "Escolaridade" = obitos_escolaridade,
    "Causa" = obitos_causa
  ), file.path(dir_dados, "banco_analitico_completo.xlsx"))

  cat("\n   Banco analítico construído com sucesso!\n")
  cat("   Ficheiros salvos em:", dir_dados, "\n\n")

  # --- Resumo ---
  cat("RESUMO DOS DADOS:\n")
  cat(sprintf("   Período: %d - %d\n", min(banco_nacional$ano),
              max(banco_nacional$ano)))
  cat(sprintf("   Total óbitos maternos: %d\n",
              sum(banco_nacional$obitos_maternos)))
  cat(sprintf("   Total nascidos vivos: %s\n",
              format(sum(banco_nacional$nascidos_vivos), big.mark = ".")))
  cat(sprintf("   RMM média nacional: %.1f por 100.000 NV\n",
              mean(banco_nacional$rmm, na.rm = TRUE)))
  cat(sprintf("   RMM mínima: %.1f (%d)\n",
              min(banco_nacional$rmm, na.rm = TRUE),
              banco_nacional$ano[which.min(banco_nacional$rmm)]))
  cat(sprintf("   RMM máxima: %.1f (%d)\n",
              max(banco_nacional$rmm, na.rm = TRUE),
              banco_nacional$ano[which.max(banco_nacional$rmm)]))

} else {
  cat("   AVISO: Dados não disponíveis para construir banco analítico.\n")
  cat("   Verifique se os downloads do SIM e SINASC foram bem-sucedidos.\n")
}

cat("\n============================================================\n")
cat("  DOWNLOAD CONCLUÍDO\n")
cat("============================================================\n")
