# Desenho amostral e calibração dos pesos.
#
# A Quaest coletou a amostra por cotas em setores censitários. Os pesos são calibrados por raking às
# distribuições do Censo 2022 (população de 18 anos ou mais do RJ), com margens cruzadas por área:
# área × sexo, área × faixa etária, área × escolaridade e cor ou raça (4 categorias).
# O setor censitário é a unidade primária de amostragem (conglomerado).

formulas_calibracao <- list(~area3 + sexo, ~area3 + idadef, ~area3 + esc_faixa, ~cor)

# Margens do Censo na mesma ordem de formulas_calibracao.
montar_margens_calibracao <- function(censo) {
  list(
    calcular_margem_censo(censo, c("area3", "sexo")),
    calcular_margem_censo(censo, c("area3", "idadef")),
    calcular_margem_censo(censo, c("area3", "esc_faixa")),
    calcular_margem_censo(censo, "cor")
  )
}

# Desenho com conglomerado por setor censitário e raking às margens do Censo.
# Os erros-padrão calculados com ele levam em conta a conglomeração e a calibração.
montar_desenho_calibrado <- function(dados, censo = ler_censo()) {
  dados <- as.data.frame(dados)
  desenho_inicial <- survey::svydesign(ids = ~cod_setor, weights = rep(1, nrow(dados)), data = dados)
  withCallingHandlers(
    survey::rake(desenho_inicial, formulas_calibracao, montar_margens_calibracao(censo),
                 control = list(maxit = 500, epsilon = 1e-6)),
    warning = function(w) stop("Raking não convergiu: ", conditionMessage(w))
  )
}

# Desenho usado nas análises do TD. Recalibra a partir do banco de análise e confere que os pesos
# coincidem com a coluna `peso` gravada em scripts/02_pesos.R.
montar_desenho_td <- function(dados) {
  desenho <- montar_desenho_calibrado(dados)
  stopifnot(isTRUE(all.equal(unname(weights(desenho)), dados$peso, tolerance = 1e-8)))
  desenho
}
