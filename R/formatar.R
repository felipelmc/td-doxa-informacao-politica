# Formatação de números no padrão brasileiro e consulta às estimativas salvas (usadas no texto do TD).

fmt_num <- function(x, digitos = 1) {
  out <- formatC(abs(x), format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
  ifelse(round(x, digitos) < 0, paste0("−", out), out)
}
fmt_int <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x, digitos = 0) paste0(fmt_num(x, digitos), "%")

# Linha única da tabela de proporções (output/estimativas/proporcoes.rds).
extrair_proporcao <- function(prop, variavel, categoria, por = NA, grupo = NA, recorte = "total") {
  selecionada <- prop$variavel == variavel & prop$categoria == categoria & prop$recorte == recorte &
    (if (is.na(por)) is.na(prop$por) else prop$por %in% por) &
    (if (is.na(grupo)) is.na(prop$grupo) else prop$grupo %in% grupo)
  linha <- prop[selecionada, ]
  if (nrow(linha) != 1) stop("Estimativa não encontrada ou ambígua: ", paste(variavel, categoria, por, grupo, recorte))
  linha
}
