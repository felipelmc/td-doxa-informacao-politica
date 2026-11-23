# 01: limpeza do banco bruto do survey DOXA/Quaest (RJ, jul. 2023).
#
# Entrada: data/raw/quaest_rj_2023_td.rds (colunas do survey usadas no TD; ver data/raw/README.md)
# Saída:   data/processed/survey_limpo.rds
#          output/diagnosticos/fluxo_amostra.csv

suppressPackageStartupMessages({
  library(dplyr)
})
source(here::here("R/recodificacoes.R"))
source(here::here("R/censo.R"))

bruto <- readRDS(here::here("data/raw/quaest_rj_2023_td.rds"))
attr(bruto, "variable.labels") <- NULL  # enunciados ficam no codebook e nos dicionários (scripts/03_banco_analise.R)

### ----- seleção de colunas -----
df <- bruto %>%
  select(
    mun, cod_setor, zona, cod_ibge,
    sexo, idadef, esc, rendaf,
    crivo_votou_1t, cen2_votou_1t, cen_t2, cen2_votou_1t_govrj, cen_t2_18,
    adpi1_lula, adpi1_gov,
    freq_inf_pol, inf1, redes_pol, redes_pol_out,
    ideologia, part_pref, cor
  ) %>%
  mutate(id = row_number())  # posição da linha no banco bruto

n_bruto <- nrow(df)

### ----- recodificação de escolaridade -----
df <- df %>%
  mutate(esc_faixa = recodificar_escolaridade(esc))

### ----- remove respondentes com menos de 18 anos e sem informação de cor -----
n_menores <- sum(df$idadef == "16 - 17 ANOS")
df <- df %>% filter(idadef != "16 - 17 ANOS")
sem_cor <- c("NÃO SABE RESPONDER (NÃO LER)", "NÃO QUIS RESPONDER (NÃO LER)")
n_sem_cor <- sum(df$cor %in% sem_cor)
df <- df %>%
  filter(!cor %in% sem_cor) %>%
  droplevels()

### ----- cor em quatro categorias e área (capital / RM / demais) -----
df <- df %>%
  mutate(
    cor = recodificar_cor(cor),
    cod_ibge = as.character(cod_ibge),
    area3 = classificar_area(cod_ibge, ler_rm())
  )

stopifnot(!anyNA(df$esc_faixa), !anyNA(df$cor), !anyNA(df$area3))

### ----- gravação -----
saveRDS(marcar_utf8(df), here::here("data/processed/survey_limpo.rds"))

dir.create(here::here("output/diagnosticos"), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(
  tibble(
    etapa = c("Banco bruto", "Menos de 18 anos (excluídos)", "Sem informação de cor ou raça (excluídos)", "Amostra analisada"),
    n = c(n_bruto, n_menores, n_sem_cor, nrow(df))
  ),
  here::here("output/diagnosticos/fluxo_amostra.csv")
)
cat("01_limpeza: ", nrow(df), " respondentes (", n_bruto, " no bruto; ", n_menores, " menores de 18; ",
    n_sem_cor, " sem cor)\n", sep = "")
