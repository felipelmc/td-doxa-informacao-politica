# 05: figuras do TD.
#
# Entrada: output/estimativas/proporcoes.rds
# Saída:   output/figuras/grafico1_frequencia ... grafico9_avaliacao_regiao (PDF vetorial e PNG 300 dpi)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})
source(here::here("R/rotulos.R"))
source(here::here("R/formatar.R"))
source(here::here("R/graficos.R"))

prop <- readRDS(here::here("output/estimativas/proporcoes.rds"))

# distribuição de `var` na população e dentro de cada grupo de `var_grupo`
extrair_total <- function(var) filter(prop, recorte == "total", is.na(por), variavel == var)
extrair_por_grupo <- function(var, var_grupo) filter(prop, recorte == "total", por == var_grupo, variavel == var)

n_total <- fmt_int(max(extrair_total("sexo")$n_grupo))
nota_ic <- "Estimativas ponderadas; as linhas indicam intervalos de confiança de 95%."

### ----- Gráfico 1: frequência com que se informa sobre política -----
g1_dados <- extrair_total("freq_inf_pol") %>% arrange(match(categoria, names(ROTULOS_CATEGORIAS$freq_inf_pol)))
grupo_freq <- c("TODOS OS DIAS" = "Todo dia", "ALGUMAS VEZES POR SEMANA" = "Toda semana", "UMA VEZ POR SEMANA" = "Toda semana",
                "ALGUMAS VEZES POR MÊS" = "Mensal ou raramente", "RARAMENTE" = "Mensal ou raramente", "NUNCA" = "Nunca")
g1 <- plotar_barras(
  g1_dados,
  rotulos = unname(ROTULOS_CATEGORIAS$freq_inf_pol[g1_dados$categoria]),
  cores = coalesce(unname(PALETAS$freq4[grupo_freq[g1_dados$categoria]]), PALETAS$cinza_claro),
  legenda = PALETAS$freq4,
  nota = nota_fonte(paste0(nota_ic, " As cores indicam os agrupamentos usados nos cruzamentos. n = ", n_total, "."))
) + geom_col(aes(x = pct, y = rotulo), fill = NA, colour = "grey55", linewidth = 0.2, width = 0.72)
salvar_figura(g1, "grafico1_frequencia", altura = 2.7)

### ----- Gráfico 2: fonte principal de informação -----
g2_dados <- extrair_total("inf1") %>% arrange(match(categoria, names(ROTULOS_CATEGORIAS$inf1)))
grupo_fonte <- c("NOTICIÁRIO DA TV" = "TV",
                 "REDES SOCIAIS COMO FACEBOOK, INSTAGRAM OU TWITTER" = "Redes sociais e mensageiros",
                 "WHATSAPP OU TELEGRAM" = "Redes sociais e mensageiros",
                 "SITES, BLOGS E PORTAIS DE NOTÍCIAS" = "Sites, portais e busca", "BUSCA NO GOOGLE" = "Sites, portais e busca",
                 "RÁDIO" = "Outras fontes", "JORNAIS IMPRESSOS E REVISTAS" = "Outras fontes",
                 "CONVERSAS COM AMIGOS, FAMILIARES" = "Outras fontes", "CONVERSAS COM COLEGAS DE TRABALHOS" = "Outras fontes",
                 "NENHUM DESTES" = "Não se informa", "NÃO ME INFORMO SOBRE POLÍTICA (NÃO LER)" = "Não se informa",
                 "NÃO SABE RESPONDER (NÃO LER)" = "Não se informa")
stopifnot(all(g2_dados$categoria %in% names(grupo_fonte)))
g2 <- plotar_barras(
  g2_dados,
  rotulos = unname(ROTULOS_CATEGORIAS$inf1[g2_dados$categoria]),
  cores = unname(PALETAS$fonte5[grupo_fonte[g2_dados$categoria]]),
  legenda = PALETAS$fonte5,
  nota = nota_fonte(paste0(nota_ic, " Resposta única. As cores indicam os grupos de fonte usados nos cruzamentos. n = ", n_total, "."))
) + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
salvar_figura(g2, "grafico2_fontes", altura = 3.5)

### ----- Gráfico 3: rede social mais usada para se informar sobre política -----
g3_dados <- extrair_total("redes_rec") %>% arrange(match(categoria, names(ROTULOS_CATEGORIAS$redes_rec)))
nao_rede <- c("Não usa redes sociais", "Não é rede social (Google, sites ou TV)", "Não sabe ou não respondeu", "Outra rede")
g3 <- plotar_barras(
  g3_dados,
  rotulos = g3_dados$categoria,
  cores = ifelse(g3_dados$categoria %in% nao_rede, PALETAS$cinza_claro, PALETAS$cinza_escuro),
  nota = nota_fonte(paste0(nota_ic, " Respostas abertas da opção \"outra\" reclassificadas (ver dicionário de dados). n = ", n_total, "."))
)
salvar_figura(g3, "grafico3_redes", altura = 2.9)

# painel e rótulo de cada grupo (com o n) nos gráficos empilhados; `rotulos` troca o nome do grupo
rotular_grupos <- function(tab, rotulo_painel, rotulos = NULL) {
  tab %>%
    mutate(painel = rotulo_painel,
           grupo_nome = if (is.null(rotulos)) grupo else unname(rotulos[grupo]),
           grupo_rotulo = rotulo_n(grupo_nome, n_grupo))
}
# painéis e grupos na ordem em que aparecem na tabela, de cima para baixo
ordenar_grupos <- function(tab) {
  tab %>%
    mutate(painel = factor(painel, levels = unique(painel)),
           grupo_rotulo = factor(grupo_rotulo, levels = rev(unique(grupo_rotulo))))
}
regioes <- c("RIO CAPITAL" = "Capital", "RM (EXCETO CAPITAL)" = "RM (exceto capital)", "DEMAIS MUNICÍPIOS" = "Demais municípios")
eleitores <- c("Lula", "Bolsonaro", "Não foi votar")
painel_presidente <- "Presidente,\n2º turno de 2022"
painel_governador <- "Governador,\n1º turno de 2022"
nota_voto <- paste0(
  "Estimativas ponderadas. Voto declarado; não inclui quem votou branco ou nulo no 2º turno, quem não soube ou não quis ",
  "informar o voto nem quem não votou em Cláudio Castro ou Marcelo Freixo para governador."
)

### ----- Gráfico 4: frequência e fonte principal por região -----
# dois blocos, cada um com título e legenda próprios, separados por um espaço
titulo_bloco <- function(titulo) {
  list(labs(title = titulo),
       theme(plot.title = element_text(face = "bold", size = 9, hjust = 0), plot.title.position = "plot",
             strip.text.y.left = element_blank()))
}
g4a <- plotar_empilhado(
  extrair_por_grupo("freq4", "area3") %>% rotular_grupos(" ", regioes) %>% ordenar_grupos(),
  niveis = names(PALETAS$freq4), cores = PALETAS$freq4, nota = NULL
) + titulo_bloco("Frequência com que se informa sobre política")
g4b <- plotar_empilhado(
  extrair_por_grupo("fonte5", "area3") %>% rotular_grupos(" ", regioes) %>% ordenar_grupos(),
  niveis = names(PALETAS$fonte5), cores = PALETAS$fonte5, nota = NULL
) + titulo_bloco("Fonte principal de informação sobre política") + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
g4 <- (g4a / plot_spacer() / g4b) + plot_layout(heights = c(1, 0.16, 1)) +
  plot_annotation(caption = nota_fonte(paste(
    "Estimativas ponderadas. A Região Metropolitana segue a LC estadual 184/2018 e inclui Petrópolis.",
    "\"Não sabe\" sobre a frequência excluído."
  )), theme = tema_td())
salvar_figura(g4, "grafico4_regiao", altura = 4.3)

### ----- Gráfico 5: frequência por voto declarado -----
g5_dados <- bind_rows(
  extrair_por_grupo("freq4", "voto2t_4") %>% filter(grupo %in% eleitores) %>% rotular_grupos(painel_presidente),
  extrair_por_grupo("freq4", "gov_2") %>% rotular_grupos(painel_governador)
) %>% ordenar_grupos()
g5 <- plotar_empilhado(g5_dados, niveis = names(PALETAS$freq4), cores = PALETAS$freq4,
                        nota = nota_fonte(paste(nota_voto, "\"Não sabe\" sobre a frequência excluído.")))
salvar_figura(g5, "grafico5_frequencia_voto", altura = 3.1)

### ----- Gráfico 6: fonte principal por voto declarado -----
g6_dados <- bind_rows(
  extrair_por_grupo("fonte5", "voto2t_4") %>% filter(grupo %in% eleitores) %>% rotular_grupos(painel_presidente),
  extrair_por_grupo("fonte5", "gov_2") %>% rotular_grupos(painel_governador)
) %>% ordenar_grupos()
g6 <- plotar_empilhado(g6_dados, niveis = names(PALETAS$fonte5), cores = PALETAS$fonte5,
                        nota = nota_fonte(paste(nota_voto, "Os grupos de fonte seguem a pergunta sobre a fonte pela qual mais se informa sobre política."))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
salvar_figura(g6, "grafico6_fontes_voto", altura = 3.4)

### ----- Gráfico 7: frequência com que se informa, por avaliação dos governos -----
avaliacoes <- c("Ótimo ou bom", "Regular", "Ruim ou péssimo")
g7_dados <- bind_rows(
  extrair_por_grupo("freq4", "aval_lula3") %>% filter(grupo %in% avaliacoes) %>% rotular_grupos("Governo Lula"),
  extrair_por_grupo("freq4", "aval_gov3") %>% filter(grupo %in% avaliacoes) %>% rotular_grupos("Governo\nCláudio Castro")
) %>% ordenar_grupos()
g7 <- plotar_empilhado(
  g7_dados, niveis = names(PALETAS$freq4), cores = PALETAS$freq4,
  nota = nota_fonte("Estimativas ponderadas. Exclui quem não sabe ou não quis avaliar o governo e quem não sabe com que frequência se informa.")
)
salvar_figura(g7, "grafico7_frequencia_avaliacao", altura = 3.0)

### ----- Gráfico 8: avaliação do governo Lula por grupo de fonte principal -----
g8_dados <- extrair_por_grupo("aval_lula3", "fonte5") %>% rotular_grupos("Governo Lula") %>% ordenar_grupos()
g8 <- plotar_empilhado(
  g8_dados, niveis = names(PALETAS$avaliacao), cores = PALETAS$avaliacao,
  nota = nota_fonte("Estimativas ponderadas. Os grupos de fonte seguem a pergunta sobre a fonte pela qual mais se informa sobre política.")
)
salvar_figura(g8, "grafico8_avaliacao_fonte", altura = 2.5)

### ----- Gráfico 9: avaliação dos governos por região -----
g9_dados <- bind_rows(
  extrair_por_grupo("aval_lula3", "area3") %>% rotular_grupos("Governo Lula", regioes),
  extrair_por_grupo("aval_gov3", "area3") %>% rotular_grupos("Governo\nCláudio Castro", regioes)
) %>% ordenar_grupos()
g9 <- plotar_empilhado(
  g9_dados, niveis = names(PALETAS$avaliacao), cores = PALETAS$avaliacao,
  nota = nota_fonte("Estimativas ponderadas. A Região Metropolitana segue a LC estadual 184/2018 e inclui Petrópolis.")
)
salvar_figura(g9, "grafico9_avaliacao_regiao", altura = 2.6)

cat("05_figuras: 9 gráficos salvos em output/figuras\n")
