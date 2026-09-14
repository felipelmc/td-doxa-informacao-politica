# 03 — Banco de análise do TD e dicionários de dados.
#
# Entrada: data/processed/survey_ponderado.rds, data/raw/quaest_rj_2023_td.rds (enunciados)
# Saída:   data/analysis/td_informacao.csv (UTF-8) e td_informacao.rds (fatores ordenados)
#          data/analysis/dicionario_variaveis.csv e dicionario_categorias.csv

suppressPackageStartupMessages({
  library(dplyr)
})
source(here::here("R/rotulos.R"))
source(here::here("R/recodificacoes.R"))

df <- readRDS(here::here("data/processed/survey_ponderado.rds")) %>%
  derivar_variaveis_analise()

enunciados <- attr(readRDS(here::here("data/raw/quaest_rj_2023_td.rds")), "variable.labels")

# colunas do banco de análise: identificação e desenho, perfil, informação, política ---------
colunas <- c(
  "id", "peso", "cod_setor", "cod_ibge", "mun", "zona",
  "sexo", "idadef", "cor", "esc_faixa", "esc2", "area3", "rendaf", "renda_grp",
  "freq_inf_pol", "freq4", "inf1", "fonte4", "fonte5", "redes_pol", "redes_pol_out", "redes_rec", "redes_grp",
  "crivo_votou_1t", "cen2_votou_1t", "cen_t2", "voto2t_3", "cen2_votou_1t_govrj", "gov_cat", "gov_2", "cen_t2_18",
  "adpi1_lula", "aval_lula3", "adpi1_gov", "aval_gov3",
  "ideologia", "ideologia_grp", "part_pref", "part_pref_grp"
)
stopifnot(all(colunas %in% names(df)), all(colunas %in% names(ROTULOS_VARIAVEIS)))

banco <- df %>%
  select(all_of(colunas)) %>%
  mutate(
    sexo = factor(sexo, levels = names(ROTULOS_CATEGORIAS$sexo)),
    esc_faixa = factor(esc_faixa, levels = names(ROTULOS_CATEGORIAS$esc_faixa)),
    cor = factor(cor, levels = names(ROTULOS_CATEGORIAS$cor)),
    area3 = factor(area3, levels = names(ROTULOS_CATEGORIAS$area3)),
    redes_pol_out = na_if(stringr::str_trim(as.character(redes_pol_out)), "")
  ) %>%
  marcar_utf8()
stopifnot(!anyNA(banco[c("sexo", "esc_faixa", "cor", "area3")]))

dir_analise <- here::here("data/analysis")
dir.create(dir_analise, showWarnings = FALSE, recursive = TRUE)
saveRDS(banco, file.path(dir_analise, "td_informacao.rds"))
readr::write_csv(banco, file.path(dir_analise, "td_informacao.csv"), na = "")

# dicionário de variáveis ---------
derivadas <- tribble(
  ~variavel, ~variavel_origem, ~regra,
  "renda_grp", "rendaf", "Até 1 SM e 1-2 SM -> Até 2 salários mínimos; 2-3 e 3-5 SM -> Mais de 2 a 5; acima de 5 SM -> Mais de 5",
  "freq4", "freq_inf_pol", "Todos os dias -> Todo dia; algumas vezes/uma vez por semana -> Toda semana; algumas vezes por mês e raramente -> Mensal ou raramente; Nunca -> Nunca; Não sabe -> NA",
  "fonte4", "inf1", "Noticiário da TV -> TV; redes sociais, WhatsApp/Telegram, sites/blogs/portais e busca no Google -> Internet; rádio, jornais/revistas e conversas -> Outras fontes; nenhum destes, não me informo e não sabe -> Não se informa",
  "fonte5", "inf1", "Como fonte4, mas Internet dividida em Redes sociais e mensageiros (redes sociais, WhatsApp/Telegram) e Sites, portais e busca (sites/blogs/portais, busca no Google)",
  "redes_rec", "redes_pol, redes_pol_out", "Respostas \"outra\" reclassificadas pelo texto aberto: nenhuma/não usa -> Não usa redes sociais; Telegram -> WhatsApp ou Telegram; Kwai -> TikTok ou Kwai; Google, sites ou TV -> Não é rede social; demais -> Outra rede",
  "redes_grp", "redes_rec", "Não usa redes sociais, Facebook, Instagram, WhatsApp ou Telegram, YouTube e Twitter (X) mantidos; TikTok ou Kwai, outra rede, não é rede social e não sabe -> Outras respostas",
  "esc2", "esc_faixa", "Sem instrução/fundamental incompleto e fundamental completo/médio incompleto -> Até médio incompleto; médio completo/superior incompleto e superior completo -> Médio completo ou mais",
  "voto2t_3", "cen_t2", "Lula; Bolsonaro; branco/nulo e não foi votar -> Branco, nulo ou não votou; não sabe e não quis responder -> NA",
  "gov_cat", "cen2_votou_1t_govrj, crivo_votou_1t", "Castro; Freixo; demais candidatos (inclui Witzel, com candidatura indeferida) -> Outros candidatos; branco/nulo; não votou no 1º turno (crivo_votou_1t = NÃO); não quis responder ou não sabe se votou",
  "gov_2", "gov_cat", "Castro ou Freixo; demais -> NA",
  "aval_lula3", "adpi1_lula", "Ótimo e bom -> Ótimo ou bom; regular (+ ou -) -> Regular; ruim e péssimo -> Ruim ou péssimo; não sabe e não quis responder -> Não sabe ou não respondeu",
  "aval_gov3", "adpi1_gov", "Como aval_lula3",
  "ideologia_grp", "ideologia", "Esquerda e centro-esquerda -> Esquerda; centro; direita e centro-direita -> Direita; não se identifica -> Nenhuma; não sabe e não quis responder",
  "part_pref_grp", "part_pref", "PT; PL; nenhum -> Nenhum; demais partidos -> Outros partidos; não sabe e não quis responder",
  "esc_faixa", "esc", "Faixas de nível de instrução do Censo 2022: até ginásio incompleto (inclui primário completo) -> Sem instrução e fundamental incompleto; ginásio completo e colegial incompleto -> Fundamental completo e médio incompleto; colegial completo e universitário incompleto/técnico -> Médio completo e superior incompleto; universitário completo e pós -> Superior completo",
  "cor", "cor", "Amarela e indígena -> OUTRA; respondentes sem informação de cor foram excluídos da amostra",
  "area3", "cod_ibge", "Rio de Janeiro (capital); demais municípios da Região Metropolitana segundo a LC estadual 184/2018 (22 municípios, com Petrópolis); demais municípios do estado",
  "peso", "sexo, idadef, esc_faixa, area3, cor", "Raking às margens do Censo 2022 (18 anos ou mais): área × sexo, área × faixa etária, área × escolaridade e cor; ver scripts/02_pesos.R"
)

tipo_coluna <- function(x) {
  if (is.factor(x)) "categórica" else if (is.numeric(x)) "numérica" else "texto"
}
significado_na <- c(
  freq4 = "Não sabe responder",
  voto2t_3 = "Não sabe ou não quis responder",
  gov_2 = "Não votou em Castro nem em Freixo",
  cen2_votou_1t_govrj = "Não votou no 1º turno ou não sabe se votou (pergunta filtrada por crivo_votou_1t)",
  cen2_votou_1t = "Não votou no 1º turno ou não sabe se votou (pergunta filtrada por crivo_votou_1t)",
  redes_pol_out = "Resposta diferente de \"outra\""
)

dicionario <- tibble(variavel = colunas) %>%
  mutate(
    rotulo = unname(ROTULOS_VARIAVEIS[variavel]),
    enunciado_questionario = coalesce(unname(enunciados[variavel]),
                                      c(cod_setor = "CÓDIGO SETOR (codebook; não perguntado)")[variavel]),
    tipo = purrr::map_chr(variavel, \(v) tipo_coluna(banco[[v]])),
    n_validos = purrr::map_int(variavel, \(v) sum(!is.na(banco[[v]]))),
    n_na = purrr::map_int(variavel, \(v) sum(is.na(banco[[v]]))),
    significado_na = unname(significado_na[variavel])
  ) %>%
  left_join(derivadas, by = "variavel") %>%
  mutate(
    origem = case_when(
      variavel %in% c("id", "peso") ~ "construída no pipeline",
      variavel %in% derivadas$variavel & !variavel %in% c("cor") ~ "derivada",
      TRUE ~ "questionário (bruta)"
    ),
    enunciado_questionario = if_else(origem == "derivada" & variavel != "esc_faixa", NA_character_, enunciado_questionario)
  ) %>%
  select(variavel, rotulo, origem, variavel_origem, regra, enunciado_questionario, tipo, n_validos, n_na, significado_na)
readr::write_csv(dicionario, file.path(dir_analise, "dicionario_variaveis.csv"), na = "")

# dicionário de categorias ---------
categorias <- purrr::map_dfr(colunas, function(v) {
  x <- banco[[v]]
  if (!is.factor(x)) return(NULL)
  n <- table(x)
  pct <- tapply(banco$peso, x, sum)
  mapa <- ROTULOS_CATEGORIAS[[v]]
  tibble(
    variavel = v,
    ordem = seq_along(levels(x)),
    categoria = levels(x),
    rotulo_publicacao = if (is.null(mapa)) levels(x) else coalesce(unname(mapa[levels(x)]), levels(x)),
    n = as.integer(n[levels(x)]),
    pct_ponderado = round(100 * as.numeric(pct[levels(x)]) / sum(banco$peso[!is.na(x)]), 2)
  )
})
readr::write_csv(categorias, file.path(dir_analise, "dicionario_categorias.csv"), na = "")

cat("03_banco_analise:", nrow(banco), "respondentes,", ncol(banco), "colunas; dicionários com",
    nrow(dicionario), "variáveis e", nrow(categorias), "categorias\n")
