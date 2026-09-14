# 02 — Pesos amostrais: calibração por raking às margens do Censo 2022.
#
# Margens: área × sexo, área × faixa etária, área × escolaridade e cor ou raça (4 categorias).
# Área: capital, RM exceto capital (LC estadual 184/2018) e demais municípios.
#
# Entrada: data/processed/survey_limpo.rds, data/external/ (Censo 2022 e composição da RM)
# Saída:   data/processed/survey_ponderado.rds
#          output/diagnosticos/pesos_resumo.csv, pesos_marginais.csv, pesos_margens_calibracao.csv,
#          erros_padrao_desenho.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
})
source(here::here("R/censo.R"))
source(here::here("R/desenho.R"))

df <- readRDS(here::here("data/processed/survey_limpo.rds"))
censo <- ler_censo()

desenho <- desenho_calibrado(df, censo)
df$peso <- unname(weights(desenho))
saveRDS(df, here::here("data/processed/survey_ponderado.rds"))

# diagnósticos ---------
dir_diag <- here::here("output/diagnosticos")
dir.create(dir_diag, showWarnings = FALSE, recursive = TRUE)

comparar_margem <- function(vars) {
  amostra <- df %>%
    mutate(across(all_of(vars), as.character)) %>%
    group_by(across(all_of(vars))) %>%
    summarise(n = n(), peso = sum(peso), .groups = "drop")
  pop <- margem_censo(censo, vars)
  full_join(amostra, pop, by = vars) %>%
    mutate(margem = paste(vars, collapse = " × "),
           pct_amostra = 100 * n / sum(n, na.rm = TRUE),
           pct_ponderada = 100 * peso / sum(peso, na.rm = TRUE),
           pct_censo = 100 * Freq / sum(Freq, na.rm = TRUE)) %>%
    tidyr::unite("categoria", all_of(vars), sep = " | ") %>%
    select(margem, categoria, n, pct_amostra, pct_ponderada, pct_censo, peso, populacao = Freq)
}

margens <- purrr::map_dfr(list(c("area3", "sexo"), c("area3", "idadef"), c("area3", "esc_faixa"), "cor"),
                          comparar_margem)
readr::write_csv(margens, here::here(dir_diag, "pesos_margens_calibracao.csv"))
desvio_max <- max(abs(margens$peso - margens$populacao))
stopifnot(!anyNA(margens$n), desvio_max < 1)  # totais calibrados coincidem com o Censo (tolerância: 1 pessoa)

marginais <- purrr::map_dfr(c("sexo", "idadef", "esc_faixa", "area3", "cor"), comparar_margem)
readr::write_csv(marginais, here::here(dir_diag, "pesos_marginais.csv"))

w <- df$peso
resumo <- tibble(
  metodo = "raking: área×sexo, área×idade, área×escolaridade, cor",
  n = length(w), soma_pesos = sum(w), populacao_censo = sum(censo$valor),
  desvio_max_margens = desvio_max,
  peso_min = min(w), peso_mediana = median(w), peso_max = max(w), razao_max_min = max(w) / min(w),
  deff_kish = length(w) * sum(w^2) / sum(w)^2, n_efetivo = sum(w)^2 / sum(w^2),
  setores = n_distinct(df$cod_setor)
)
readr::write_csv(resumo, here::here(dir_diag, "pesos_resumo.csv"))

# erros-padrão sob diferentes especificações do desenho (para documentação) ---------
indicadores <- df %>%
  transmute(
    diario = as.numeric(freq_inf_pol == "TODOS OS DIAS"),
    nunca = as.numeric(freq_inf_pol == "NUNCA"),
    tv = as.numeric(inf1 == "NOTICIÁRIO DA TV"),
    redes = as.numeric(inf1 == "REDES SOCIAIS COMO FACEBOOK, INSTAGRAM OU TWITTER")
  )
especificacoes <- list(
  "sem conglomerado, pesos calibrados" = svydesign(ids = ~1, weights = ~peso, data = bind_cols(df, indicadores)),
  "setor como conglomerado, pesos calibrados" = svydesign(ids = ~cod_setor, weights = ~peso, data = bind_cols(df, indicadores)),
  "setor como conglomerado + calibração (usado no TD)" = desenho_calibrado(bind_cols(df, indicadores), censo)
)
erros <- purrr::imap_dfr(especificacoes, function(d, nome) {
  est <- svymean(~diario + nunca + tv + redes, d)
  tibble(especificacao = nome, indicador = names(coef(est)),
         estimativa_pct = 100 * unname(coef(est)), erro_padrao_pp = 100 * unname(SE(est)))
})
readr::write_csv(erros, here::here(dir_diag, "erros_padrao_desenho.csv"))

cat("02_pesos: deff de Kish =", round(resumo$deff_kish, 2), "| n efetivo =", round(resumo$n_efetivo),
    "| razão máx/mín =", round(resumo$razao_max_min, 1), "| desvio máx. nas margens =", signif(desvio_max, 2), "\n")
