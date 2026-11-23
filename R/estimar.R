# Funções de estimação com o desenho calibrado (ver R/desenho.R).
#
# Convenções:
# - proporções em %, com intervalo de confiança de 95% em escala logit (svyciprop), sempre dentro de [0, 100];
# - n_grupo: número de entrevistas no grupo (denominador); n_categoria: entrevistas na categoria;
# - n_efetivo: n efetivo de Kish dos pesos no grupo;
# - instavel = TRUE quando n_grupo < 30 ou coeficiente de variação > 30%: a estimativa não é interpretada no texto.
#
# Em desenhos calibrados, subset() mantém todas as linhas e zera o peso das que ficam fora do domínio, para
# estimar corretamente a variância. Por isso as contagens usam apenas as linhas com peso maior que zero.

LIMITE_N <- 30       # mínimo de entrevistas no grupo
LIMITE_CV <- 0.30    # máximo do coeficiente de variação

calcular_n_efetivo <- function(w) sum(w)^2 / sum(w^2)

tem_peso <- function(desenho) stats::weights(desenho) > 0

# Categorias presentes em x, na ordem dos níveis quando x é fator.
listar_categorias <- function(x) if (is.factor(x)) levels(droplevels(x)) else sort(unique(x))

# Proporção de cada categoria de `var`, na população ou dentro de cada grupo de `por`.
# Quem não respondeu a `var` (ou a `por`) sai do denominador.
calcular_proporcoes <- function(desenho, var, por = NULL) {
  validos <- !is.na(desenho$variables[[var]])
  if (!is.null(por)) validos <- validos & !is.na(desenho$variables[[por]])
  desenho <- subset(desenho, validos)

  categorias <- listar_categorias(desenho$variables[[var]][tem_peso(desenho)])
  grupos <- if (is.null(por)) NA_character_ else listar_categorias(desenho$variables[[por]][tem_peso(desenho)])

  linhas <- list()
  for (grupo in grupos) {
    desenho_grupo <- if (is.na(grupo)) desenho else subset(desenho, desenho$variables[[por]] %in% grupo)
    no_grupo <- tem_peso(desenho_grupo)
    x <- desenho_grupo$variables[[var]]
    n_grupo <- sum(no_grupo)

    for (categoria in categorias) {
      n_categoria <- sum(no_grupo & x %in% categoria)
      if (n_categoria == 0 || n_categoria == n_grupo) {
        # categoria ausente ou única no grupo: 0% ou 100%, sem intervalo
        pct <- ifelse(n_categoria == 0, 0, 100)
        ic <- c(NA_real_, NA_real_)
        ep <- NA_real_
        cv <- NA_real_
      } else {
        desenho_categoria <- stats::update(desenho_grupo, .ind = as.numeric(x %in% categoria))
        est <- survey::svyciprop(~.ind, desenho_categoria, method = "logit", level = 0.95)
        p <- as.numeric(est)
        se <- as.numeric(survey::SE(est))
        pct <- 100 * p
        ic <- 100 * as.numeric(stats::confint(est))
        ep <- 100 * se
        cv <- se / p
      }
      linhas[[length(linhas) + 1]] <- tibble::tibble(
        variavel = var, por = if (is.null(por)) NA_character_ else por, grupo = grupo, categoria = categoria,
        pct = pct, ic_inf = ic[1], ic_sup = ic[2], ep = ep, cv = cv,
        n_categoria = n_categoria, n_grupo = n_grupo, n_efetivo = calcular_n_efetivo(stats::weights(desenho_grupo)[no_grupo])
      )
    }
  }

  dplyr::bind_rows(linhas) %>%
    dplyr::mutate(instavel = n_grupo < LIMITE_N | is.na(cv) | cv > LIMITE_CV)
}

# Diferença entre dois grupos (a − b) na proporção da(s) categoria(s) `categoria` de `var`, em pontos percentuais.
# É o coeficiente de um modelo linear ponderado no domínio dos dois grupos, com IC pelos graus de liberdade do desenho.
# `dominio` (opcional) restringe a comparação, por exemplo quote(esc2 == "Médio completo ou mais").
calcular_diferenca_grupos <- function(desenho, var, categoria, por, a, b, dominio = NULL) {
  no_dominio <- !is.na(desenho$variables[[var]]) & desenho$variables[[por]] %in% c(a, b)
  if (!is.null(dominio)) no_dominio <- no_dominio & (eval(dominio, desenho$variables) %in% TRUE)
  desenho_dominio <- subset(desenho, no_dominio)
  desenho_dominio <- stats::update(desenho_dominio,
                                   .y = as.numeric(desenho_dominio$variables[[var]] %in% categoria),
                                   .g = as.numeric(desenho_dominio$variables[[por]] %in% a))

  modelo <- survey::svyglm(.y ~ .g, design = desenho_dominio)
  graus_liberdade <- survey::degf(desenho_dominio)
  dif <- stats::coef(modelo)[[".g"]]
  ep <- sqrt(diag(stats::vcov(modelo)))[[".g"]]
  no_grupo <- tem_peso(desenho_dominio)

  tibble::tibble(
    variavel = var, categoria = paste(categoria, collapse = " + "), por = por,
    grupo_a = paste(a, collapse = " + "), grupo_b = paste(b, collapse = " + "),
    dominio = if (is.null(dominio)) NA_character_ else deparse(dominio),
    dif_pp = 100 * dif,
    ic_inf = 100 * (dif - stats::qt(0.975, graus_liberdade) * ep),
    ic_sup = 100 * (dif + stats::qt(0.975, graus_liberdade) * ep),
    p_valor = 2 * stats::pt(abs(dif / ep), graus_liberdade, lower.tail = FALSE),
    n_a = sum(no_grupo & desenho_dominio$variables[[por]] %in% a),
    n_b = sum(no_grupo & desenho_dominio$variables[[por]] %in% b)
  )
}

# Diferença entre duas categorias (ou conjuntos de categorias) da mesma variável na mesma população, em p.p.
calcular_diferenca_categorias <- function(desenho, var, a, b, dominio = NULL) {
  validos <- !is.na(desenho$variables[[var]])
  if (!is.null(dominio)) validos <- validos & (eval(dominio, desenho$variables) %in% TRUE)
  desenho_dominio <- subset(desenho, validos)
  desenho_dominio <- stats::update(desenho_dominio,
                                   .d = as.numeric(desenho_dominio$variables[[var]] %in% a) -
                                     as.numeric(desenho_dominio$variables[[var]] %in% b))

  est <- survey::svymean(~.d, desenho_dominio)
  graus_liberdade <- survey::degf(desenho_dominio)
  dif <- as.numeric(stats::coef(est))
  ep <- as.numeric(survey::SE(est))

  tibble::tibble(
    variavel = var, categoria_a = paste(a, collapse = " + "), categoria_b = paste(b, collapse = " + "),
    dominio = if (is.null(dominio)) NA_character_ else deparse(dominio),
    dif_pp = 100 * dif,
    ic_inf = 100 * (dif - stats::qt(0.975, graus_liberdade) * ep),
    ic_sup = 100 * (dif + stats::qt(0.975, graus_liberdade) * ep),
    p_valor = 2 * stats::pt(abs(dif / ep), graus_liberdade, lower.tail = FALSE),
    n = sum(tem_peso(desenho_dominio))
  )
}
