# Gráficos do TD (padrão dos Textos para Discussão do DOXA)
#
# As funções abaixo recebem tabelas já estimadas (output/estimativas/proporcoes.rds) e só desenham.
# O título fica fora da imagem (legenda do Quarto: "Gráfico N — título"); a nota de fonte fica dentro,
# para que cada figura exportada seja autossuficiente. Paletas seguras para daltonismo, uma por dimensão.

FONTE_GRAFICOS <- "Arial"
LARGURA_FIGURA <- 6.3  # polegadas: largura útil da página A4 com margens de 3 cm e 2 cm

PALETAS <- list(
  # frequência com que se informa (ordinal): roxos sequenciais
  freq4 = c("Todo dia" = "#54278F", "Toda semana" = "#807DBA", "Mensal ou raramente" = "#BCBDDC", "Nunca" = "#E5E4F0"),
  # grupos de fonte principal: Okabe-Ito
  fonte4 = c("TV" = "#E69F00", "Internet" = "#0072B2", "Outras fontes" = "#009E73", "Não se informa" = "#A6A6A6"),
  # avaliação de governo (divergente): verde-azulado a marrom
  avaliacao = c("Ótimo ou bom" = "#01665E", "Regular" = "#B8B8B8", "Ruim ou péssimo" = "#8C510A",
                "Não sabe ou não respondeu" = "#E8E8E8"),
  cinza_escuro = "#4D4D4D",
  cinza_claro = "#BDBDBD"
)

# texto branco sobre as cores escuras de cada paleta
TEXTO_CLARO <- c("Todo dia", "Toda semana", "Ótimo ou bom", "Ruim ou péssimo")

tema_td <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size, base_family = FONTE_GRAFICOS) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey88", linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = "grey15", size = base_size),
      axis.title = ggplot2::element_text(colour = "grey15", size = base_size),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = base_size),
      legend.position = "top",
      legend.justification = "left",
      legend.location = "plot",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey25", size = base_size - 1, lineheight = 1.1),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(4, 6, 4, 2)
    )
}

nota_fonte <- function(complemento = NULL, largura = 118) {
  partes <- c("Fonte: Elaboração própria, com dados do survey DOXA/Quaest (jul. 2023).", complemento)
  paste(unlist(lapply(partes, strwrap, width = largura)), collapse = "\n")
}

# Salva a figura em PDF vetorial (Arial embutida, via svglite + rsvg) e em PNG de 300 dpi.
salvar_figura <- function(grafico, nome, altura, largura = LARGURA_FIGURA, pasta = here::here("output/figuras")) {
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  svg <- tempfile(fileext = ".svg")
  svglite::svglite(svg, width = largura, height = altura, system_fonts = list(sans = FONTE_GRAFICOS))
  print(grafico)
  grDevices::dev.off()
  rsvg::rsvg_pdf(svg, file.path(pasta, paste0(nome, ".pdf")))
  ragg::agg_png(file.path(pasta, paste0(nome, ".png")), width = largura, height = altura, units = "in", res = 300)
  print(grafico)
  grDevices::dev.off()
  invisible(file.path(pasta, paste0(nome, ".pdf")))
}

pct_eixo <- function(limite = NULL, folga = 0.04) {
  ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), limits = if (is.null(limite)) NULL else c(0, limite),
                              expand = ggplot2::expansion(mult = c(0, folga)))
}

rotulo_pct <- function(x) ifelse(x < 0.5, "<1%", paste0(formatC(x, format = "f", digits = 0), "%"))

rotulo_n <- function(rotulo, n) paste0(rotulo, " (n = ", formatC(n, big.mark = ".", decimal.mark = ",", format = "d"), ")")

# Barras horizontais com IC para a distribuição de uma variável na população.
# `tabela`: linhas de proporcoes() com colunas categoria, pct, ic_inf, ic_sup; `cor`: vetor de cores por linha.
grafico_barras <- function(tabela, rotulos, cores, limite = NULL, legenda = NULL, nota = nota_fonte()) {
  tabela <- tabela %>%
    dplyr::mutate(rotulo = factor(rotulos, levels = rev(unique(rotulos))), cor = cores, texto = rotulo_pct(pct))
  g <- ggplot2::ggplot(tabela, ggplot2::aes(x = pct, y = rotulo)) +
    ggplot2::geom_col(ggplot2::aes(fill = cor), width = 0.72) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ic_inf, xmax = ic_sup), orientation = "y", width = 0.25, linewidth = 0.35,
                           colour = "grey10", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = pmax(ic_sup, pct, na.rm = TRUE), label = texto), hjust = -0.25, size = 8 / ggplot2::.pt,
                       family = FONTE_GRAFICOS, colour = "grey10") +
    pct_eixo(limite, folga = 0.1) +
    ggplot2::labs(x = NULL, y = NULL, caption = nota) +
    tema_td()
  if (is.null(legenda)) {
    g + ggplot2::scale_fill_identity()
  } else {
    g + ggplot2::scale_fill_identity(guide = "legend", breaks = unname(legenda), labels = names(legenda))
  }
}

# Barras 100% empilhadas: uma barra por grupo, segmentos na ordem de `niveis`.
# `tabela`: linhas de proporcoes() com colunas painel, grupo_rotulo, categoria, pct.
grafico_empilhado <- function(tabela, niveis, cores, nota = nota_fonte(), minimo_rotulo = 5) {
  tabela <- tabela %>%
    dplyr::mutate(categoria = factor(categoria, levels = niveis),
                  texto = ifelse(pct >= minimo_rotulo, paste0(formatC(pct, format = "f", digits = 0), "%"), ""),
                  cor_texto = ifelse(categoria %in% TEXTO_CLARO, "white", "grey10"))
  ggplot2::ggplot(tabela, ggplot2::aes(x = pct, y = grupo_rotulo, fill = categoria)) +
    ggplot2::geom_col(width = 0.72, colour = "white", linewidth = 0.3, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = texto, colour = cor_texto, group = categoria),
                       position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
                       size = 8 / ggplot2::.pt, family = FONTE_GRAFICOS) +
    ggplot2::scale_fill_manual(values = cores, breaks = niveis, drop = FALSE) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::facet_grid(painel ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL, caption = nota) +
    tema_td() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", hjust = 0, vjust = 1),
                   axis.text.x = ggplot2::element_blank())
}

# Pontos com IC: categorias no eixo vertical, grupos diferenciados por forma e tom de cinza.
# Estimativas instáveis (n < 30 ou CV > 30%) aparecem com marcador vazado.
# `tabela`: linhas de proporcoes() com colunas categoria_rotulo, grupo_rotulo, pct, ic_inf, ic_sup, instavel.
grafico_pontos <- function(tabela, titulo_painel = NULL, limite = NULL) {
  grupos <- levels(tabela$grupo_rotulo)
  formas <- c(21, 24, 22)[seq_along(grupos)]
  tons <- c("grey10", "grey45", "grey75")[seq_along(grupos)]
  tabela <- tabela %>% dplyr::mutate(preenchimento = ifelse(instavel, "white", tons[as.integer(grupo_rotulo)]))
  pd <- ggplot2::position_dodge(width = 0.6, reverse = TRUE)
  ggplot2::ggplot(tabela, ggplot2::aes(x = pct, y = categoria_rotulo, group = grupo_rotulo)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = ic_inf, xmax = ic_sup, colour = grupo_rotulo), linewidth = 0.45,
                            position = pd, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape = grupo_rotulo, fill = preenchimento, colour = grupo_rotulo), size = 1.9,
                        stroke = 0.5, position = pd) +
    ggplot2::scale_shape_manual(values = stats::setNames(formas, grupos)) +
    ggplot2::scale_colour_manual(values = stats::setNames(tons, grupos)) +
    ggplot2::scale_fill_identity() +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(fill = tons))) +
    pct_eixo(limite) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = titulo_painel) +
    tema_td() +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(face = "bold", size = 9, margin = ggplot2::margin(b = 2)),
                   plot.title.position = "plot",
                   panel.grid.major.y = ggplot2::element_line(colour = "grey94", linewidth = 0.3))
}
