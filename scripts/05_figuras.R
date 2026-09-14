# 05 — Figuras do TD.
#
# Entrada: output/estimativas/proporcoes.rds
# Saída:   output/figuras/grafico1_frequencia ... grafico7_avaliacao_fonte (PDF vetorial e PNG 300 dpi)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})
source(here::here("R/rotulos.R"))
source(here::here("R/graficos.R"))

prop <- readRDS(here::here("output/estimativas/proporcoes.rds"))
total <- function(variavel) filter(prop, recorte == "total", is.na(por), variavel == !!variavel)
por_grupo <- function(variavel, por, recorte = "total") filter(prop, recorte == !!recorte, por == !!por, variavel == !!variavel)
n_total <- max(total("sexo")$n_grupo)
nota_ic <- "Estimativas ponderadas; as linhas indicam intervalos de confiança de 95%."
fmt_pct_fig <- function(x) paste0(formatC(x, format = "f", digits = 0), "%")

# Gráfico 1: frequência com que se informa sobre política ----------------------------------------
g1_dados <- total("freq_inf_pol") %>% arrange(match(categoria, names(ROTULOS_CATEGORIAS$freq_inf_pol)))
grupo_freq <- c("TODOS OS DIAS" = "Todo dia", "ALGUMAS VEZES POR SEMANA" = "Toda semana", "UMA VEZ POR SEMANA" = "Toda semana",
                "ALGUMAS VEZES POR MÊS" = "Mensal ou raramente", "RARAMENTE" = "Mensal ou raramente", "NUNCA" = "Nunca")
g1 <- grafico_barras(
  g1_dados,
  rotulos = unname(ROTULOS_CATEGORIAS$freq_inf_pol[g1_dados$categoria]),
  cores = coalesce(unname(PALETAS$freq4[grupo_freq[g1_dados$categoria]]), PALETAS$cinza_claro),
  legenda = PALETAS$freq4,
  nota = nota_fonte(paste0(nota_ic, " As cores indicam os agrupamentos usados nos cruzamentos. n = ",
                           formatC(n_total, big.mark = ".", decimal.mark = ",", format = "d"), "."))
) + geom_col(aes(x = pct, y = rotulo), fill = NA, colour = "grey55", linewidth = 0.2, width = 0.72)
salvar_figura(g1, "grafico1_frequencia", altura = 2.7)

# Gráfico 2: fonte principal de informação -------------------------------------------------------
g2_dados <- total("inf1") %>% arrange(match(categoria, names(ROTULOS_CATEGORIAS$inf1)))
grupo_fonte <- c("NOTICIÁRIO DA TV" = "TV",
                 "REDES SOCIAIS COMO FACEBOOK, INSTAGRAM OU TWITTER" = "Internet", "WHATSAPP OU TELEGRAM" = "Internet",
                 "SITES, BLOGS E PORTAIS DE NOTÍCIAS" = "Internet", "BUSCA NO GOOGLE" = "Internet",
                 "RÁDIO" = "Outras fontes", "JORNAIS IMPRESSOS E REVISTAS" = "Outras fontes",
                 "CONVERSAS COM AMIGOS, FAMILIARES" = "Outras fontes", "CONVERSAS COM COLEGAS DE TRABALHOS" = "Outras fontes",
                 "NENHUM DESTES" = "Não se informa", "NÃO ME INFORMO SOBRE POLÍTICA (NÃO LER)" = "Não se informa",
                 "NÃO SABE RESPONDER (NÃO LER)" = "Não se informa")
stopifnot(all(g2_dados$categoria %in% names(grupo_fonte)))
g2 <- grafico_barras(
  g2_dados,
  rotulos = unname(ROTULOS_CATEGORIAS$inf1[g2_dados$categoria]),
  cores = unname(PALETAS$fonte4[grupo_fonte[g2_dados$categoria]]),
  legenda = PALETAS$fonte4,
  nota = nota_fonte(paste0(nota_ic, " Resposta única. As cores indicam os grupos de fonte usados nos cruzamentos. n = ",
                           formatC(n_total, big.mark = ".", decimal.mark = ",", format = "d"), "."))
)
salvar_figura(g2, "grafico2_fontes", altura = 3.5)

# Gráfico 3: rede social mais usada para se informar sobre política -----------------------------------
g3_dados <- total("redes_rec") %>% arrange(match(categoria, names(ROTULOS_CATEGORIAS$redes_rec)))
nao_rede <- c("Não usa redes sociais", "Não é rede social (Google, sites ou TV)", "Não sabe ou não respondeu", "Outra rede")
g3 <- grafico_barras(
  g3_dados,
  rotulos = g3_dados$categoria,
  cores = ifelse(g3_dados$categoria %in% nao_rede, PALETAS$cinza_claro, PALETAS$cinza_escuro),
  nota = nota_fonte(paste0(nota_ic, " Respostas abertas da opção \"outra\" reclassificadas (ver dicionário de dados). n = ",
                           formatC(n_total, big.mark = ".", decimal.mark = ",", format = "d"), "."))
)
salvar_figura(g3, "grafico3_redes", altura = 2.9)

# Gráfico 4: frequência por voto declarado ----------------------------------------------------------
com_n <- function(tab, rotulo_painel, rotulos = NULL) {
  tab %>%
    mutate(painel = rotulo_painel,
           grupo_nome = if (is.null(rotulos)) grupo else unname(rotulos[grupo]),
           grupo_rotulo = rotulo_n(grupo_nome, n_grupo))
}
g4_dados <- bind_rows(
  total("freq4") %>% mutate(grupo = "Total") %>% com_n(" "),
  por_grupo("freq4", "voto2t_3") %>% com_n("Presidente,\n2º turno de 2022"),
  por_grupo("freq4", "gov_2") %>% com_n("Governador,\n1º turno de 2022")
) %>%
  mutate(painel = factor(painel, levels = unique(painel)),
         grupo_rotulo = factor(grupo_rotulo, levels = rev(unique(grupo_rotulo))))
g4 <- grafico_empilhado(
  g4_dados, niveis = names(PALETAS$freq4), cores = PALETAS$freq4,
  nota = nota_fonte(paste0(
    "Estimativas ponderadas. Voto declarado; \"não sabe\" sobre a frequência (",
    fmt_pct_fig(total("freq_inf_pol")$pct[total("freq_inf_pol")$categoria == "NÃO SABE RESPONDER (NÃO LER)"]),
    ") e sobre o voto excluídos. O recorte do governador inclui apenas os eleitores declarados de Cláudio Castro e Marcelo Freixo (",
    fmt_pct_fig(100 * sum(distinct(por_grupo("freq4", "gov_2"), grupo, n_grupo)$n_grupo) / n_total), " da amostra)."
  ))
)
salvar_figura(g4, "grafico4_frequencia_voto", altura = 3.2)

# Gráfico 5: grupo de fonte principal por voto declarado ---------------------------------------------
rotulos_fonte5 <- ROTULOS_CATEGORIAS$fonte5
pontos <- function(tab) {
  tab %>%
    mutate(categoria_rotulo = factor(categoria, levels = rev(names(rotulos_fonte5))),
           grupo_rotulo = rotulo_n(grupo, n_grupo),
           grupo_rotulo = factor(grupo_rotulo, levels = unique(grupo_rotulo)))
}
g5a <- grafico_pontos(pontos(por_grupo("fonte5", "voto2t_3")), "Voto declarado para presidente, 2º turno de 2022", limite = 55)
g5b <- grafico_pontos(pontos(por_grupo("fonte5", "gov_2")), "Voto declarado para governador, 1º turno de 2022", limite = 55)
g5 <- (g5a / g5b) +
  plot_annotation(caption = nota_fonte(paste(nota_ic, "Marcadores vazados: estimativas instáveis (n < 30 ou coeficiente de variação > 30%).")),
                  theme = tema_td())
salvar_figura(g5, "grafico5_fontes_voto", altura = 5.2)

# Gráfico 7: avaliação dos governos por grupo de fonte principal ---------------------------------------
g6_dados <- bind_rows(
  por_grupo("aval_lula3", "fonte5") %>% com_n("Governo Lula"),
  por_grupo("aval_gov3", "fonte5") %>% com_n("Governo\nCláudio Castro")
) %>%
  mutate(painel = factor(painel, levels = unique(painel)),
         grupo_rotulo = factor(grupo_rotulo, levels = rev(unique(grupo_rotulo))))
g6 <- grafico_empilhado(
  g6_dados, niveis = names(PALETAS$avaliacao), cores = PALETAS$avaliacao,
  nota = nota_fonte("Estimativas ponderadas. Os grupos de fonte seguem a pergunta sobre a fonte pela qual mais se informa sobre política.")
)
salvar_figura(g6, "grafico7_avaliacao_fonte", altura = 4.1)

# Gráfico 6: frequência com que se informa, por avaliação dos governos ----------------------------------
avaliacoes <- c("Ótimo ou bom", "Regular", "Ruim ou péssimo")
g7_dados <- bind_rows(
  por_grupo("freq4", "aval_lula3") %>% filter(grupo %in% avaliacoes) %>% com_n("Governo Lula"),
  por_grupo("freq4", "aval_gov3") %>% filter(grupo %in% avaliacoes) %>% com_n("Governo\nCláudio Castro")
) %>%
  mutate(painel = factor(painel, levels = unique(painel)),
         grupo_rotulo = factor(grupo_rotulo, levels = rev(unique(grupo_rotulo))))
g7 <- grafico_empilhado(
  g7_dados, niveis = names(PALETAS$freq4), cores = PALETAS$freq4,
  nota = nota_fonte("Estimativas ponderadas. Exclui quem não sabe ou não quis avaliar o governo e quem não sabe com que frequência se informa.")
)
salvar_figura(g7, "grafico6_frequencia_avaliacao", altura = 3.0)

cat("05_figuras: 7 gráficos salvos em output/figuras\n")
