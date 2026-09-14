# Formatação de números no padrão brasileiro e consulta às estimativas salvas (usadas no texto do TD).

fmt_num <- function(x, digitos = 1) {
  out <- formatC(abs(x), format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
  ifelse(round(x, digitos) < 0, paste0("−", out), out)
}
fmt_int <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x, digitos = 0) paste0(fmt_num(x, digitos), "%")
fmt_pp <- function(x, digitos = 1) paste0(fmt_num(x, digitos), " p.p.")
fmt_ic <- function(inf, sup, digitos = 1) paste0("IC 95%: ", fmt_num(inf, digitos), " a ", fmt_num(sup, digitos))
fmt_p <- function(p) ifelse(p < 0.001, "< 0,001", fmt_num(p, 3))

# Linha única da tabela de proporções (output/estimativas/proporcoes.rds).
buscar_prop <- function(tab, variavel, categoria, por = NA, grupo = NA, recorte = "total") {
  sel <- tab$variavel == variavel & tab$categoria == categoria & tab$recorte == recorte &
    (if (is.na(por)) is.na(tab$por) else tab$por %in% por) &
    (if (is.na(grupo)) is.na(tab$grupo) else tab$grupo %in% grupo)
  linha <- tab[sel, ]
  if (nrow(linha) != 1) stop("Estimativa não encontrada ou ambígua: ", paste(variavel, categoria, por, grupo, recorte))
  linha
}

# Texto de uma proporção: "21%" ou "21% (IC 95%: 18,6 a 23,3)".
txt_prop <- function(tab, ..., digitos = 0, ic = FALSE) {
  l <- buscar_prop(tab, ...)
  if (isTRUE(l$instavel)) warning("Estimativa instável citada no texto: ", paste(list(...), collapse = " / "))
  if (ic) paste0(fmt_pct(l$pct, digitos), " (", fmt_ic(l$ic_inf, l$ic_sup), ")") else fmt_pct(l$pct, digitos)
}

# Texto de uma diferença salva em output/estimativas/diferencas.rds: "9,1 p.p. (IC 95%: 2,8 a 15,5)".
txt_dif <- function(tab, id, ic = TRUE, digitos = 1) {
  l <- tab[tab$id == id, ]
  if (nrow(l) != 1) stop("Diferença não encontrada: ", id)
  if (ic) paste0(fmt_pp(l$dif_pp, digitos), " (", fmt_ic(l$ic_inf, l$ic_sup, digitos), ")") else fmt_pp(l$dif_pp, digitos)
}
