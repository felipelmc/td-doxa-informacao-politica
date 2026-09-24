# Funções de estimação com o desenho calibrado (ver R/desenho.R).
#
# Convenções:
# - proporções em %, com intervalo de confiança de 95% em escala logit (svyciprop), sempre dentro de [0, 100];
# - n_grupo: número de entrevistas no grupo (denominador); n_categoria: entrevistas na categoria;
# - n_efetivo: n efetivo de Kish dos pesos no grupo;
# - instavel = TRUE quando n_grupo < 30 ou coeficiente de variação > 30%: a estimativa não é interpretada no texto.
#
# Observação: em desenhos calibrados, subset() mantém todas as linhas e zera o peso das que ficam fora do
# domínio (para estimar corretamente a variância). Por isso as contagens usam apenas as linhas com peso > 0.

LIMITE_N <- 30
LIMITE_CV <- 0.30

n_efetivo <- function(w) sum(w)^2 / sum(w^2)

ativos <- function(design) stats::weights(design) > 0

# Proporções de cada categoria de `var`, no total ou dentro dos grupos de `por`.
# NAs em `var` ou em `por` saem do denominador.
proporcoes <- function(design, var, por = NULL) {
  dados <- design$variables
  validos <- !is.na(dados[[var]])
  if (!is.null(por)) validos <- validos & !is.na(dados[[por]])
  sub <- subset(design, validos)
  x_ativo <- sub$variables[[var]][ativos(sub)]
  categorias <- if (is.factor(x_ativo)) levels(droplevels(x_ativo)) else sort(unique(x_ativo))
  grupos <- if (is.null(por)) list(NULL) else {
    g <- sub$variables[[por]][ativos(sub)]
    as.list(if (is.factor(g)) levels(droplevels(g)) else sort(unique(g)))
  }

  purrr::map_dfr(grupos, function(g) {
    dg <- if (is.null(g)) sub else subset(sub, sub$variables[[por]] %in% g)
    ok <- ativos(dg)
    x <- dg$variables[[var]]
    purrr::map_dfr(categorias, function(k) {
      n_k <- sum(ok & x %in% k)
      base <- tibble::tibble(
        variavel = var, por = if (is.null(por)) NA_character_ else por, grupo = if (is.null(g)) NA_character_ else g,
        categoria = k, n_categoria = n_k, n_grupo = sum(ok), n_efetivo = n_efetivo(stats::weights(dg)[ok])
      )
      if (n_k == 0 || n_k == sum(ok)) {
        return(dplyr::mutate(base, pct = ifelse(n_k == 0, 0, 100), ic_inf = NA_real_, ic_sup = NA_real_,
                             ep = NA_real_, cv = NA_real_))
      }
      dk <- stats::update(dg, .ind = as.numeric(x %in% k))
      est <- survey::svyciprop(~.ind, dk, method = "logit", level = 0.95)
      p <- as.numeric(est)
      se <- as.numeric(survey::SE(est))
      ic <- as.numeric(stats::confint(est))
      dplyr::mutate(base, pct = 100 * p, ic_inf = 100 * ic[1], ic_sup = 100 * ic[2], ep = 100 * se, cv = se / p)
    })
  }) %>%
    dplyr::mutate(instavel = n_grupo < LIMITE_N | is.na(cv) | cv > LIMITE_CV) %>%
    dplyr::relocate(pct, ic_inf, ic_sup, ep, cv, .after = categoria)
}

# Diferença entre dois grupos (a - b) na proporção da(s) categoria(s) `categoria` de `var`, em pontos percentuais.
# Estimada como coeficiente de um modelo linear ponderado no domínio dos dois grupos (IC com gl do desenho).
# `dominio` (opcional): expressão que restringe a comparação, por exemplo quote(esc2 == "Médio completo ou mais").
diferenca_grupos <- function(design, var, categoria, por, a, b, dominio = NULL) {
  dados <- design$variables
  no_dominio <- !is.na(dados[[var]]) & dados[[por]] %in% c(a, b)
  if (!is.null(dominio)) no_dominio <- no_dominio & (eval(dominio, dados) %in% TRUE)
  sub <- subset(design, no_dominio)
  sub <- stats::update(sub, .y = as.numeric(sub$variables[[var]] %in% categoria),
                       .g = as.numeric(sub$variables[[por]] %in% a))
  m <- survey::svyglm(.y ~ .g, design = sub)
  gl <- survey::degf(sub)
  coef <- stats::coef(m)[[".g"]]
  ep <- sqrt(diag(stats::vcov(m)))[[".g"]]
  ok <- ativos(sub)
  tibble::tibble(
    variavel = var, categoria = paste(categoria, collapse = " + "), por = por,
    grupo_a = paste(a, collapse = " + "), grupo_b = paste(b, collapse = " + "),
    dominio = if (is.null(dominio)) NA_character_ else deparse(dominio),
    dif_pp = 100 * coef, ic_inf = 100 * (coef - stats::qt(0.975, gl) * ep), ic_sup = 100 * (coef + stats::qt(0.975, gl) * ep),
    p_valor = 2 * stats::pt(abs(coef / ep), gl, lower.tail = FALSE),
    n_a = sum(ok & sub$variables[[por]] %in% a), n_b = sum(ok & sub$variables[[por]] %in% b)
  )
}

# Diferença entre duas categorias (ou conjuntos de categorias) da mesma variável na mesma população, em p.p.
diferenca_categorias <- function(design, var, a, b, dominio = NULL) {
  dados <- design$variables
  validos <- !is.na(dados[[var]])
  if (!is.null(dominio)) validos <- validos & (eval(dominio, dados) %in% TRUE)
  sub <- subset(design, validos)
  sub <- stats::update(sub, .d = as.numeric(sub$variables[[var]] %in% a) - as.numeric(sub$variables[[var]] %in% b))
  est <- survey::svymean(~.d, sub)
  gl <- survey::degf(sub)
  coef <- as.numeric(stats::coef(est))
  ep <- as.numeric(survey::SE(est))
  tibble::tibble(
    variavel = var, categoria_a = paste(a, collapse = " + "), categoria_b = paste(b, collapse = " + "),
    dominio = if (is.null(dominio)) NA_character_ else deparse(dominio),
    dif_pp = 100 * coef, ic_inf = 100 * (coef - stats::qt(0.975, gl) * ep), ic_sup = 100 * (coef + stats::qt(0.975, gl) * ep),
    p_valor = 2 * stats::pt(abs(coef / ep), gl, lower.tail = FALSE), n = sum(ativos(sub))
  )
}
