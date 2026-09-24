# Formatação de números no padrão brasileiro e consulta às estimativas salvas (usadas no texto do TD).

fmt_num <- function(x, digitos = 1) {
  out <- formatC(abs(x), format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
  ifelse(round(x, digitos) < 0, paste0("−", out), out)
}
fmt_int <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x, digitos = 0) paste0(fmt_num(x, digitos), "%")

# Linha única da tabela de proporções (output/estimativas/proporcoes.rds).
buscar_prop <- function(tab, variavel, categoria, por = NA, grupo = NA, recorte = "total") {
  sel <- tab$variavel == variavel & tab$categoria == categoria & tab$recorte == recorte &
    (if (is.na(por)) is.na(tab$por) else tab$por %in% por) &
    (if (is.na(grupo)) is.na(tab$grupo) else tab$grupo %in% grupo)
  linha <- tab[sel, ]
  if (nrow(linha) != 1) stop("Estimativa não encontrada ou ambígua: ", paste(variavel, categoria, por, grupo, recorte))
  linha
}
