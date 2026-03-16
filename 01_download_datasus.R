###############################################################################
#  DOWNLOAD DE DADOS REAIS DO DATASUS (versão otimizada para memória)
#  SIM (óbitos maternos) + SINASC (nascidos vivos)
#  Período: 2000-2024
#
#  Estratégia: processar e agregar ano a ano, libertando memória
###############################################################################

suppressPackageStartupMessages({
  library(microdatasus)
  library(tidyverse)
  library(writexl)
})

dir_dados <- file.path(getwd(), "dados_datasus")
dir.create(dir_dados, showWarnings = FALSE, recursive = TRUE)

anos_download <- 2000:2024

cod_uf <- data.frame(
  cod = c("11", "12", "13", "14", "15", "16", "17",
          "21", "22", "23", "24", "25", "26", "27", "28", "29",
          "31", "32", "33", "35",
          "41", "42", "43",
          "50", "51", "52", "53"),
  uf_sigla = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
               "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
               "MG", "ES", "RJ", "SP",
               "PR", "SC", "RS",
               "MS", "MT", "GO", "DF"),
  regiao = c(rep("Norte", 7), rep("Nordeste", 9),
             rep("Sudeste", 4), rep("Sul", 3),
             rep("Centro-Oeste", 4)),
  stringsAsFactors = FALSE
)

cat("============================================================\n")
cat("  DOWNLOAD DATASUS — Versão otimizada\n")
cat("  Período: 2000-2024\n")
cat("============================================================\n\n")

# ============================================================================
# 1. SIM — Agregar ano a ano
# ============================================================================

cat("1. Baixando e agregando SIM (óbitos maternos)...\n\n")

# Listas para guardar dados agregados por ano
obitos_nac <- list()
obitos_uf <- list()
obitos_raca <- list()
obitos_idade <- list()
obitos_esc <- list()
obitos_causa <- list()
sim_micro <- list()  # microdados filtrados (só maternos, ~2000 por ano)

for (ano in anos_download) {
  cat(sprintf("   SIM %d ... ", ano))
  tryCatch({
    raw <- fetch_datasus(
      year_start = ano, year_end = ano,
      information_system = "SIM-DO"
    )
    proc <- process_sim(raw)
    rm(raw); gc(verbose = FALSE)

    # Filtrar óbitos maternos
    mat <- proc %>%
      filter(grepl("^O", CAUSABAS) | CAUSABAS == "A34") %>%
      mutate(
        ano_obito = ano,
        uf_cod = substr(CODMUNRES, 1, 2),
        obito_oficial = grepl("^O", CAUSABAS) &
          !grepl("^O9[67]", CAUSABAS),
        causa_grupo = case_when(
          grepl("^O1[0-6]", CAUSABAS) ~ "Hipertensivas",
          grepl("^O4[4-6]|^O67|^O72", CAUSABAS) ~ "Hemorragia",
          grepl("^O85|^O86", CAUSABAS) ~ "Infeccao_puerperal",
          grepl("^O0[0-8]", CAUSABAS) ~ "Abortamento",
          grepl("^O98|^O99", CAUSABAS) ~ "Causas_indiretas",
          TRUE ~ "Outras_diretas"
        ),
        raca_cor = case_when(
          RACACOR == "1" ~ "Branca",
          RACACOR == "2" ~ "Preta",
          RACACOR == "3" ~ "Amarela",
          RACACOR == "4" ~ "Parda",
          RACACOR == "5" ~ "Indigena",
          TRUE ~ "Ignorada"
        ),
        idade_num = as.numeric(IDADE),
        faixa_idade = case_when(
          idade_num < 20 ~ "<20",
          idade_num <= 34 ~ "20-34",
          idade_num >= 35 ~ ">=35",
          TRUE ~ "Ignorada"
        ),
        escolaridade_grupo = case_when(
          ESC %in% c("1", "2") ~ "Nenhuma/Fund.Inc.",
          ESC %in% c("3", "4") ~ "Fund.Comp./Medio Inc.",
          ESC == "5" ~ "Medio Comp./Sup.Inc.",
          ESC == "6" ~ "Superior Comp.",
          TRUE ~ "Ignorada"
        )
      )

    rm(proc); gc(verbose = FALSE)

    of <- mat %>% filter(obito_oficial)

    # Agregar
    obitos_nac[[as.character(ano)]] <- tibble(
      ano = ano, obitos_maternos = nrow(of)
    )

    obitos_uf[[as.character(ano)]] <- of %>%
      count(uf_cod) %>%
      mutate(ano = ano) %>%
      rename(obitos = n)

    obitos_raca[[as.character(ano)]] <- of %>%
      count(raca_cor) %>%
      mutate(ano = ano) %>%
      rename(obitos = n)

    obitos_idade[[as.character(ano)]] <- of %>%
      count(faixa_idade) %>%
      mutate(ano = ano) %>%
      rename(obitos = n)

    obitos_esc[[as.character(ano)]] <- of %>%
      count(escolaridade_grupo) %>%
      mutate(ano = ano) %>%
      rename(obitos = n)

    obitos_causa[[as.character(ano)]] <- of %>%
      count(causa_grupo) %>%
      mutate(ano = ano) %>%
      rename(obitos = n)

    cat(sprintf("OK (%d óbitos maternos)\n", nrow(of)))
    rm(mat, of); gc(verbose = FALSE)

  }, error = function(e) {
    cat(sprintf("ERRO: %s\n", e$message))
  })
}

# Consolidar SIM
df_obitos_nac <- bind_rows(obitos_nac)
df_obitos_uf <- bind_rows(obitos_uf)
df_obitos_raca <- bind_rows(obitos_raca)
df_obitos_idade <- bind_rows(obitos_idade)
df_obitos_esc <- bind_rows(obitos_esc)
df_obitos_causa <- bind_rows(obitos_causa)

cat(sprintf("\n   Total SIM: %d óbitos em %d anos\n\n",
            sum(df_obitos_nac$obitos_maternos), nrow(df_obitos_nac)))

# ============================================================================
# 2. SINASC — Agregar ano a ano (só contagens)
# ============================================================================

cat("2. Baixando e agregando SINASC (nascidos vivos)...\n\n")

nv_nac <- list()
nv_uf <- list()
nv_raca <- list()
nv_idade <- list()

for (ano in anos_download) {
  cat(sprintf("   SINASC %d ... ", ano))
  tryCatch({
    raw <- fetch_datasus(
      year_start = ano, year_end = ano,
      information_system = "SINASC"
    )
    proc <- process_sinasc(raw)
    rm(raw); gc(verbose = FALSE)

    proc <- proc %>%
      mutate(
        uf_cod = substr(CODMUNRES, 1, 2),
        raca_cor = case_when(
          RACACOR == "1" ~ "Branca",
          RACACOR == "2" ~ "Preta",
          RACACOR == "3" ~ "Amarela",
          RACACOR == "4" ~ "Parda",
          RACACOR == "5" ~ "Indigena",
          TRUE ~ "Ignorada"
        ),
        idade_mae = as.numeric(IDADEMAE),
        faixa_idade = case_when(
          idade_mae < 20 ~ "<20",
          idade_mae <= 34 ~ "20-34",
          idade_mae >= 35 ~ ">=35",
          TRUE ~ "Ignorada"
        )
      )

    nv_nac[[as.character(ano)]] <- tibble(
      ano = ano, nascidos_vivos = nrow(proc)
    )

    nv_uf[[as.character(ano)]] <- proc %>%
      count(uf_cod) %>%
      mutate(ano = ano) %>%
      rename(nv = n)

    nv_raca[[as.character(ano)]] <- proc %>%
      count(raca_cor) %>%
      mutate(ano = ano) %>%
      rename(nv = n)

    nv_idade[[as.character(ano)]] <- proc %>%
      count(faixa_idade) %>%
      mutate(ano = ano) %>%
      rename(nv = n)

    cat(sprintf("OK (%s nascidos vivos)\n",
                format(nrow(proc), big.mark = ".")))
    rm(proc); gc(verbose = FALSE)

  }, error = function(e) {
    cat(sprintf("ERRO: %s\n", e$message))
  })
}

# Consolidar SINASC
df_nv_nac <- bind_rows(nv_nac)
df_nv_uf <- bind_rows(nv_uf)
df_nv_raca <- bind_rows(nv_raca)
df_nv_idade <- bind_rows(nv_idade)

cat(sprintf("\n   Total SINASC: %s NV em %d anos\n\n",
            format(sum(df_nv_nac$nascidos_vivos), big.mark = "."),
            nrow(df_nv_nac)))

# ============================================================================
# 3. CONSTRUIR BANCOS ANALÍTICOS
# ============================================================================

cat("3. Construindo bancos analíticos...\n")

# Nacional
banco_nacional <- df_obitos_nac %>%
  left_join(df_nv_nac, by = "ano") %>%
  mutate(rmm = obitos_maternos / nascidos_vivos * 100000)

# Por UF
banco_uf <- df_obitos_uf %>%
  left_join(df_nv_uf, by = c("ano", "uf_cod")) %>%
  left_join(cod_uf, by = c("uf_cod" = "cod")) %>%
  mutate(rmm = obitos / nv * 100000) %>%
  filter(!is.na(uf_sigla))

# Por região
banco_regiao <- banco_uf %>%
  group_by(ano, regiao) %>%
  summarise(
    obitos_maternos = sum(obitos, na.rm = TRUE),
    nascidos_vivos = sum(nv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rmm = obitos_maternos / nascidos_vivos * 100000)

# Salvar tudo
saveRDS(banco_nacional,
        file.path(dir_dados, "banco_analitico_nacional.rds"))
saveRDS(banco_regiao,
        file.path(dir_dados, "banco_analitico_regiao.rds"))
saveRDS(banco_uf,
        file.path(dir_dados, "banco_analitico_uf.rds"))
saveRDS(df_obitos_raca,
        file.path(dir_dados, "obitos_raca.rds"))
saveRDS(df_obitos_idade,
        file.path(dir_dados, "obitos_idade.rds"))
saveRDS(df_obitos_esc,
        file.path(dir_dados, "obitos_escolaridade.rds"))
saveRDS(df_obitos_causa,
        file.path(dir_dados, "obitos_causa.rds"))
saveRDS(df_nv_raca,
        file.path(dir_dados, "nv_raca.rds"))
saveRDS(df_nv_idade,
        file.path(dir_dados, "nv_idade.rds"))

# Excel
write_xlsx(list(
  Nacional = banco_nacional,
  Regiao = banco_regiao,
  UF = banco_uf %>% select(ano, uf_sigla, regiao, obitos, nv, rmm),
  Obitos_Raca = df_obitos_raca,
  Obitos_Idade = df_obitos_idade,
  Obitos_Escolaridade = df_obitos_esc,
  Obitos_Causa = df_obitos_causa
), file.path(dir_dados, "banco_analitico_completo.xlsx"))

cat("   Bancos salvos em:", dir_dados, "\n\n")

# Resumo
cat("============================================================\n")
cat("  RESUMO\n")
cat("============================================================\n")
cat(sprintf("  Período: %d - %d\n",
            min(banco_nacional$ano), max(banco_nacional$ano)))
cat(sprintf("  Total óbitos maternos: %s\n",
            format(sum(banco_nacional$obitos_maternos), big.mark = ".")))
cat(sprintf("  Total nascidos vivos: %s\n",
            format(sum(banco_nacional$nascidos_vivos), big.mark = ".")))
cat(sprintf("  RMM média: %.1f por 100.000 NV\n",
            mean(banco_nacional$rmm, na.rm = TRUE)))
cat(sprintf("  RMM mínima: %.1f (%d)\n",
            min(banco_nacional$rmm, na.rm = TRUE),
            banco_nacional$ano[which.min(banco_nacional$rmm)]))
cat(sprintf("  RMM máxima: %.1f (%d)\n",
            max(banco_nacional$rmm, na.rm = TRUE),
            banco_nacional$ano[which.max(banco_nacional$rmm)]))
cat("============================================================\n")
