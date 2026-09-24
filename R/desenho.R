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
