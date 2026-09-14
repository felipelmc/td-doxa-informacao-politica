# Desenho amostral e calibração dos pesos.
#
# A amostra da Quaest foi coletada por cotas em setores censitários. Os pesos são calibrados por raking
# às distribuições do Censo 2022 (população de 18 anos ou mais do RJ), com margens cruzadas por área:
# área × sexo, área × faixa etária, área × escolaridade e cor ou raça (4 categorias).
# O desenho usa o setor censitário como unidade primária de amostragem (conglomerado).

formulas_calibracao <- list(~area3 + sexo, ~area3 + idadef, ~area3 + esc_faixa, ~cor)

margens_calibracao <- function(censo) {
  list(
    margem_censo(censo, c("area3", "sexo")),
    margem_censo(censo, c("area3", "idadef")),
    margem_censo(censo, c("area3", "esc_faixa")),
    margem_censo(censo, "cor")
  )
}

# Desenho calibrado: conglomerado por setor censitário e raking às margens do Censo.
# Os erros-padrão calculados a partir deste objeto levam em conta a conglomeração e a calibração.
desenho_calibrado <- function(dados, censo = ler_censo()) {
  dados <- as.data.frame(dados)
  inicial <- survey::svydesign(ids = ~cod_setor, weights = rep(1, nrow(dados)), data = dados)
  withCallingHandlers(
    survey::rake(inicial, formulas_calibracao, margens_calibracao(censo),
                 control = list(maxit = 500, epsilon = 1e-6)),
    warning = function(w) stop("Raking não convergiu: ", conditionMessage(w))
  )
}

# Desenho usado nas análises do TD. Recalibra a partir do banco (rápido) e confere que os pesos
# coincidem com a coluna `peso` salva pelo pipeline.
desenho_td <- function(dados) {
  d <- desenho_calibrado(dados)
  if (!is.null(dados$peso)) stopifnot(isTRUE(all.equal(unname(weights(d)), dados$peso, tolerance = 1e-8)))
  d
}

# Análise de sensibilidade (Anexo B): mantém as margens demográficas e acrescenta a razão entre os votos
# válidos de Lula e Bolsonaro no 2º turno de 2022 no RJ (TSE), preservando o total ponderado de quem não
# declarou voto em nenhum dos dois. Requer a variável voto2t_3 (R/recodificacoes.R).
desenho_calibrado_voto <- function(dados, censo = ler_censo()) {
  tse <- readr::read_csv(here::here("data/external/tse_rj_candidatos.csv"), show_col_types = FALSE)
  tse <- tse[tse$pleito == 2022 & tse$turno == 2 & tse$cargo == "Presidente", ]
  p_lula <- tse$votos[tse$candidato == "LULA"] / sum(tse$votos)

  dados <- as.data.frame(dados)
  dados$voto_calibracao <- dplyr::case_when(
    dados$voto2t_3 %in% "Lula" ~ "Lula",
    dados$voto2t_3 %in% "Bolsonaro" ~ "Bolsonaro",
    TRUE ~ "Outros"
  )
  base <- stats::weights(desenho_calibrado(dados, censo))
  total_lb <- sum(base[dados$voto_calibracao != "Outros"])
  margem_voto <- data.frame(
    voto_calibracao = c("Lula", "Bolsonaro", "Outros"),
    Freq = c(total_lb * p_lula, total_lb * (1 - p_lula), sum(base) - total_lb)
  )
  inicial <- survey::svydesign(ids = ~cod_setor, weights = rep(1, nrow(dados)), data = dados)
  withCallingHandlers(
    survey::rake(inicial, c(formulas_calibracao, list(~voto_calibracao)), c(margens_calibracao(censo), list(margem_voto)),
                 control = list(maxit = 500, epsilon = 1e-6)),
    warning = function(w) stop("Raking não convergiu: ", conditionMessage(w))
  )
}
