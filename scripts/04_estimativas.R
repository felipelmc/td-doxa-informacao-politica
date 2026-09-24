# 04 — Estimativas do TD: proporções, diferenças e tabelas.
#
# Todas as estimativas usam o desenho calibrado (setor censitário como conglomerado + raking às margens do
# Censo 2022; ver R/desenho.R), com IC de 95% em escala logit.
#
# Entrada: data/analysis/td_informacao.rds, data/external/ (Censo 2022 e TSE)
# Saída (output/estimativas/, em .rds e .csv):
#   proporcoes: todos os percentuais citados no texto e desenhados nos gráficos
#   diferencas: diferenças entre grupos (p.p., IC 95%) que sustentam as comparações do texto e as ressalvas de margem de erro
#   tabela1_perfil_amostra, tabela2_perfil_tv_internet, tabela3_informacao_opiniao
#   voto_declarado_tse: voto declarado no survey e resultado oficial no RJ

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(survey)
})
for (arquivo in c("censo", "desenho", "estimar", "rotulos")) source(here::here("R", paste0(arquivo, ".R")))

# avisos esperados em domínios de desenhos calibrados (linhas com peso zero) não são exibidos
silenciar <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("zero weight", conditionMessage(w))) invokeRestart("muffleWarning")
  })
}

banco <- readRDS(here::here("data/analysis/td_informacao.rds"))
censo <- ler_censo()

preparar <- function(design) {
  update(design, usa_rede_politica = factor(case_when(
    redes_rec %in% "Não sabe ou não respondeu" ~ NA_character_,
    redes_rec %in% c("Não usa redes sociais", "Não é rede social (Google, sites ou TV)") ~ "Não",
    !is.na(redes_rec) ~ "Sim"
  ), levels = c("Sim", "Não")))
}
d <- preparar(desenho_td(banco))

freq_informa <- c("Todo dia", "Toda semana", "Mensal ou raramente")
recortes <- list(
  total = d,
  sem_nunca = subset(d, freq4 %in% freq_informa)
)

# 1. Proporções ------------------------------------------------------------------------------------
vars_perfil <- c("sexo", "idadef", "esc_faixa", "cor", "renda_grp", "area3", "freq4", "redes_grp",
                 "usa_rede_politica", "ideologia_grp", "part_pref_grp", "voto2t_3")
especificacoes <- bind_rows(
  tibble(recorte = "total", por = NA_character_,
         variavel = c("freq_inf_pol", "freq4", "inf1", "fonte4", "fonte5", "redes_rec", "redes_grp", "usa_rede_politica",
                      "aval_lula3", "aval_gov3", "voto2t_3", "voto2t_4", "gov_cat", "ideologia_grp", "part_pref_grp",
                      "sexo", "idadef", "esc_faixa", "esc2", "cor", "renda_grp", "area3")),
  # regiões (gráficos 4 e 9)
  tibble(recorte = "total", por = "area3", variavel = c("freq4", "fonte5", "aval_lula3", "aval_gov3")),
  # públicos da TV e da internet (tabela 2)
  tibble(recorte = "total", por = "fonte4", variavel = vars_perfil),
  tibble(recorte = "total", por = "idadef", variavel = "fonte4"),
  tibble(recorte = "total", por = "esc_faixa", variavel = "fonte4"),
  tibble(recorte = "sem_nunca", por = NA_character_, variavel = "fonte4"),
  # preferências eleitorais (gráficos 5 e 6)
  tibble(recorte = "total", por = "voto2t_4", variavel = c("freq4", "fonte5")),
  tibble(recorte = "total", por = "gov_2", variavel = c("freq4", "fonte5")),
  # avaliação de governo (gráficos 7 e 8)
  tibble(recorte = "total", por = "aval_lula3", variavel = "freq4"),
  tibble(recorte = "total", por = "aval_gov3", variavel = "freq4"),
  tibble(recorte = "total", por = "fonte5", variavel = "aval_lula3"),
  # formação de opinião (tabela 3)
  tibble(recorte = "total", por = "freq4", variavel = c("aval_lula3", "aval_gov3", "ideologia_grp", "part_pref_grp", "voto2t_3"))
)

calcular_proporcoes <- function(desenhos, specs) {
  pmap_dfr(specs, function(recorte, por, variavel) {
    silenciar(proporcoes(desenhos[[recorte]], variavel, if (is.na(por)) NULL else por)) %>%
      mutate(recorte = recorte, .before = 1)
  })
}
prop <- calcular_proporcoes(recortes, especificacoes)

# 2. Diferenças (IC 95% e p-valor) ----------------------------------------------------------------
# Não aparecem no texto: documentam as comparações feitas nele e indicam quando uma diferença fica dentro da
# margem de erro (ic_exclui_zero = FALSE).
dg <- function(id, descricao, ...) silenciar(diferenca_grupos(d, ...)) %>% mutate(id = id, descricao = descricao, .before = 1)
dc <- function(id, descricao, ...) silenciar(diferenca_categorias(d, ...)) %>% mutate(id = id, descricao = descricao, .before = 1)
redes <- "Redes sociais e mensageiros"
sites <- "Sites, portais e busca"
ns <- "Não sabe ou não respondeu"
bnv <- "Branco, nulo ou não votou"
nv <- "Não foi votar"
baixa <- c("Mensal ou raramente", "Nunca")
capital <- "RIO CAPITAL"
rm <- "RM (EXCETO CAPITAL)"
demais <- "DEMAIS MUNICÍPIOS"
ob <- "Ótimo ou bom"

dif <- bind_rows(
  # frequência, fontes e regiões
  dc("diario_menos_rara_nunca", "Informa-se todo dia − raramente ou nunca (população)", "freq_inf_pol", "TODOS OS DIAS", c("RARAMENTE", "NUNCA")),
  dg("nunca_rm_capital", "Nunca se informa: RM − capital", "freq4", "Nunca", "area3", rm, capital),
  dg("nunca_demais_capital", "Nunca se informa: demais municípios − capital", "freq4", "Nunca", "area3", demais, capital),
  dg("diario_capital_rm", "Informa-se todo dia: capital − RM", "freq4", "Todo dia", "area3", capital, rm),
  dg("diario_capital_demais", "Informa-se todo dia: capital − demais municípios", "freq4", "Todo dia", "area3", capital, demais),
  dg("tv_capital_resto", "TV como fonte: capital − RM e demais municípios", "fonte5", "TV", "area3", capital, c(rm, demais)),
  # públicos da TV e da internet
  dc("tv_menos_internet", "TV − internet como fonte principal (população)", "fonte4", "TV", "Internet"),
  dc("tv_menos_internet_sem_nunca", "TV − internet como fonte principal, entre quem se informa ao menos raramente",
     "fonte4", "TV", "Internet", dominio = quote(freq4 %in% c("Todo dia", "Toda semana", "Mensal ou raramente"))),
  dg("internet_jovens_idosos", "Internet como fonte: 18 a 24 anos − 60 anos ou mais", "fonte4", "Internet", "idadef", "18 - 24 ANOS", "60 ANOS OU MAIS"),
  dg("tv_idosos_jovens", "TV como fonte: 60 anos ou mais − 18 a 24 anos", "fonte4", "TV", "idadef", "60 ANOS OU MAIS", "18 - 24 ANOS"),
  dg("internet_superior_sem_instrucao", "Internet como fonte: superior completo − sem instrução/fundamental incompleto",
     "fonte4", "Internet", "esc_faixa", "Superior completo", "Sem instrução e fundamental incompleto"),
  # preferências eleitorais
  dg("diario_lula_naovotou", "Informa-se todo dia: Lula − não foi votar", "freq4", "Todo dia", "voto2t_4", "Lula", nv),
  dg("diario_bolsonaro_naovotou", "Informa-se todo dia: Bolsonaro − não foi votar", "freq4", "Todo dia", "voto2t_4", "Bolsonaro", nv),
  dg("diario_lula_bolsonaro", "Informa-se todo dia: Lula − Bolsonaro", "freq4", "Todo dia", "voto2t_4", "Lula", "Bolsonaro"),
  dg("baixa_naovotou_lula", "Mensal, raramente ou nunca: não foi votar − Lula", "freq4", baixa, "voto2t_4", nv, "Lula"),
  dg("baixa_naovotou_bolsonaro", "Mensal, raramente ou nunca: não foi votar − Bolsonaro", "freq4", baixa, "voto2t_4", nv, "Bolsonaro"),
  dg("tv_lula_bolsonaro", "TV como fonte: Lula − Bolsonaro", "fonte5", "TV", "voto2t_4", "Lula", "Bolsonaro"),
  dg("redes_bolsonaro_lula", "Redes e mensageiros como fonte: Bolsonaro − Lula", "fonte5", redes, "voto2t_4", "Bolsonaro", "Lula"),
  dg("nunca_castro_freixo", "Nunca se informa: Castro − Freixo", "freq4", "Nunca", "gov_2", "Castro", "Freixo"),
  dg("sites_freixo_castro", "Sites, portais e busca como fonte: Freixo − Castro", "fonte5", sites, "gov_2", "Freixo", "Castro"),
  # avaliação de governo
  dc("lula_ob_diario_nunca", "Avalia Lula como ótimo ou bom: todo dia − nunca", "freq4", "Todo dia", "Nunca", dominio = quote(aval_lula3 %in% "Ótimo ou bom")),
  dg("diario_ob_regular_lula", "Informa-se todo dia: avalia Lula ótimo/bom − regular", "freq4", "Todo dia", "aval_lula3", ob, "Regular"),
  dg("baixa_regular_ob_lula", "Mensal, raramente ou nunca: avalia Lula regular − ótimo/bom", "freq4", baixa, "aval_lula3", "Regular", ob),
  dc("castro_ob_nunca_diario", "Avalia Castro como ótimo ou bom: nunca − todo dia", "freq4", "Nunca", "Todo dia", dominio = quote(aval_gov3 %in% "Ótimo ou bom")),
  dc("castro_ob_nunca_mensal", "Avalia Castro como ótimo ou bom: nunca − mensal ou raramente", "freq4", "Nunca", "Mensal ou raramente", dominio = quote(aval_gov3 %in% "Ótimo ou bom")),
  dg("lula_ob_tv_redes", "Avalia Lula como ótimo ou bom: TV − redes e mensageiros", "aval_lula3", ob, "fonte5", "TV", redes),
  dg("lula_rp_redes_tv", "Avalia Lula como ruim ou péssimo: redes e mensageiros − TV", "aval_lula3", "Ruim ou péssimo", "fonte5", redes, "TV"),
  dg("lula_ob_tv_redes_eleitores_lula", "Avalia Lula como ótimo ou bom: TV − redes e mensageiros (eleitores de Lula)", "aval_lula3", ob, "fonte5", "TV", redes, dominio = quote(voto2t_4 %in% "Lula")),
  dg("lula_ob_tv_redes_eleitores_bolsonaro", "Avalia Lula como ótimo ou bom: TV − redes e mensageiros (eleitores de Bolsonaro)", "aval_lula3", ob, "fonte5", "TV", redes, dominio = quote(voto2t_4 %in% "Bolsonaro")),
  # informação e formação de opinião (tabela 3)
  dg("opiniao_ns_castro", "Não sabe avaliar o governo Castro: nunca se informa − todo dia", "aval_gov3", ns, "freq4", "Nunca", "Todo dia"),
  dg("opiniao_ns_castro_mensal", "Não sabe avaliar o governo Castro: nunca se informa − mensal ou raramente", "aval_gov3", ns, "freq4", "Nunca", "Mensal ou raramente"),
  dg("opiniao_ns_lula", "Não sabe avaliar o governo Lula: nunca se informa − todo dia", "aval_lula3", ns, "freq4", "Nunca", "Todo dia"),
  dg("opiniao_sem_ideologia", "Sem ideologia ou não sabe: nunca se informa − todo dia", "ideologia_grp", c("Nenhuma", ns), "freq4", "Nunca", "Todo dia"),
  dg("opiniao_sem_partido", "Sem partido preferido ou não sabe: nunca se informa − todo dia", "part_pref_grp", c("Nenhum", ns), "freq4", "Nunca", "Todo dia"),
  dg("opiniao_bnv", "Branco, nulo ou não votou no 2º turno: nunca se informa − todo dia", "voto2t_3", bnv, "freq4", "Nunca", "Todo dia"),
  # avaliação de governo por região
  dg("lula_ob_capital_rm", "Avalia Lula como ótimo ou bom: capital − RM", "aval_lula3", ob, "area3", capital, rm),
  dg("lula_ob_demais_rm", "Avalia Lula como ótimo ou bom: demais municípios − RM", "aval_lula3", ob, "area3", demais, rm),
  dg("lula_ob_capital_demais", "Avalia Lula como ótimo ou bom: capital − demais municípios", "aval_lula3", ob, "area3", capital, demais),
  dg("castro_ob_rm_capital", "Avalia Castro como ótimo ou bom: RM − capital", "aval_gov3", ob, "area3", rm, capital),
  dg("castro_ob_demais_capital", "Avalia Castro como ótimo ou bom: demais municípios − capital", "aval_gov3", ob, "area3", demais, capital),
  dg("castro_ob_rm_demais", "Avalia Castro como ótimo ou bom: RM − demais municípios", "aval_gov3", ob, "area3", rm, demais)
)

# perfil TV × internet (tabela 2): diferença internet − TV em cada categoria
perfil_linhas <- prop %>%
  filter(recorte == "total", por == "fonte4", variavel %in% vars_perfil, grupo %in% c("TV", "Internet")) %>%
  distinct(variavel, categoria)
dif_perfil <- pmap_dfr(perfil_linhas, function(variavel, categoria) {
  dg(paste("perfil", variavel, categoria, sep = "|"), paste("Perfil (internet − TV):", variavel, "=", categoria),
     variavel, categoria, "fonte4", "Internet", "TV")
})
dif_sem_nunca <- bind_rows(
  dg("perfil_sem_nunca|idadef|60 ANOS OU MAIS", "Perfil (internet − TV), sem quem nunca se informa: 60 anos ou mais",
     "idadef", "60 ANOS OU MAIS", "fonte4", "Internet", "TV", dominio = quote(freq4 %in% c("Todo dia", "Toda semana", "Mensal ou raramente"))),
  dg("perfil_sem_nunca|esc_faixa|Superior completo", "Perfil (internet − TV), sem quem nunca se informa: superior completo",
     "esc_faixa", "Superior completo", "fonte4", "Internet", "TV", dominio = quote(freq4 %in% c("Todo dia", "Toda semana", "Mensal ou raramente"))),
  dg("perfil_sem_nunca|voto2t_3|Bolsonaro", "Perfil (internet − TV), sem quem nunca se informa: voto declarado em Bolsonaro",
     "voto2t_3", "Bolsonaro", "fonte4", "Internet", "TV", dominio = quote(freq4 %in% c("Todo dia", "Toda semana", "Mensal ou raramente")))
)
dif <- bind_rows(dif, dif_perfil, dif_sem_nunca) %>%
  mutate(ic_exclui_zero = ic_inf > 0 | ic_sup < 0)

# 3. Tabelas do corpo do texto ---------------------------------------------------------------------
# Tabela 1: perfil da amostra sem peso, Censo 2022 e ponderada
tabela1 <- map_dfr(c("sexo", "idadef", "esc_faixa", "cor", "area3", "renda_grp"), function(v) {
  amostra <- banco %>%
    mutate(categoria = as.character(.data[[v]])) %>%
    group_by(categoria) %>%
    summarise(n = n(), peso = sum(peso), .groups = "drop") %>%
    mutate(pct_amostra = 100 * n / sum(n), pct_ponderada = 100 * peso / sum(peso))
  if (v %in% names(censo)) {
    pop <- censo %>% group_by(categoria = .data[[v]]) %>% summarise(pop = sum(valor), .groups = "drop") %>%
      mutate(pct_censo = 100 * pop / sum(pop))
    amostra <- left_join(amostra, pop %>% select(categoria, pct_censo), by = "categoria")
  } else {
    amostra$pct_censo <- NA_real_
  }
  amostra %>%
    mutate(variavel = v, rotulo_variavel = ROTULOS_VARIAVEIS[[v]], rotulo = rotular(categoria, v),
           ordem = match(categoria, names(ROTULOS_CATEGORIAS[[v]]))) %>%
    arrange(ordem) %>%
    select(variavel, rotulo_variavel, categoria, rotulo, n, pct_amostra, pct_censo, pct_ponderada)
})

# Tabela 2: perfil de quem se informa principalmente pela TV e pela internet
tabela2 <- prop %>%
  filter(recorte == "total", por == "fonte4", variavel %in% vars_perfil, grupo %in% c("TV", "Internet")) %>%
  select(variavel, categoria, grupo, pct, n_grupo, instavel) %>%
  pivot_wider(names_from = grupo, values_from = c(pct, n_grupo, instavel)) %>%
  left_join(prop %>% filter(recorte == "total", is.na(por), variavel %in% vars_perfil) %>% select(variavel, categoria, pct_total = pct),
            by = c("variavel", "categoria")) %>%
  mutate(rotulo_variavel = unname(ROTULOS_VARIAVEIS[variavel]),
         rotulo = map2_chr(categoria, variavel, \(k, v) if (v == "usa_rede_politica") k else as.character(rotular(k, v))),
         ordem_variavel = match(variavel, vars_perfil),
         ordem = map2_int(categoria, variavel, \(k, v) {
           if (v == "usa_rede_politica") match(k, c("Sim", "Não")) else match(k, names(ROTULOS_CATEGORIAS[[v]]))
         })) %>%
  arrange(ordem_variavel, ordem)

# Tabela 3: falta de opinião política por frequência com que se informa
indicadores_opiniao <- tribble(
  ~indicador, ~variavel, ~categorias,
  "Não sabe avaliar o governo Cláudio Castro", "aval_gov3", list(ns),
  "Não sabe avaliar o governo Lula", "aval_lula3", list(ns),
  "Não se identifica com nenhuma ideologia ou não sabe", "ideologia_grp", list(c("Nenhuma", ns)),
  "Não tem partido preferido ou não sabe", "part_pref_grp", list(c("Nenhum", ns)),
  "Votou branco, nulo ou não foi votar no 2º turno de 2022", "voto2t_3", list(bnv)
)
tabela3 <- pmap_dfr(indicadores_opiniao, function(indicador, variavel, categorias) {
  categorias <- unlist(categorias)
  prop %>%
    filter(recorte == "total", por == "freq4", variavel == !!variavel, categoria %in% categorias) %>%
    group_by(grupo) %>%
    # somas de categorias têm variação relativa menor; o alerta vale para indicadores de uma só categoria
    summarise(pct = sum(pct), instavel = length(categorias) == 1 & any(instavel), n_grupo = first(n_grupo),
              .groups = "drop") %>%
    mutate(indicador = indicador)
}) %>%
  mutate(indicador = factor(indicador, levels = indicadores_opiniao$indicador),
         grupo = factor(grupo, levels = names(ROTULOS_CATEGORIAS$freq4))) %>%
  arrange(indicador, grupo) %>%
  select(indicador, grupo, pct, instavel) %>%
  pivot_wider(names_from = grupo, values_from = c(pct, instavel)) %>%
  mutate(indicador = as.character(indicador))

# 4. Voto declarado e resultado oficial (TSE) ---------------------------------------------------------
tse <- readr::read_csv(here::here("data/external/tse_rj_candidatos.csv"), show_col_types = FALSE)
pct_tse <- function(pleito_, turno_, cargo_, candidato_) {
  tse$pct_validos[tse$pleito == pleito_ & tse$turno == turno_ & tse$cargo == cargo_ & tse$candidato == candidato_]
}
validos_gov <- c("CLÁUDIO CASTRO", "MARCELO FREIXO", "RODRIGO NEVES", "PAULO GANIME", "CYRO GARCIA",
                 "EDUARDO SERRA", "JULIETE PANTOJA")  # Witzel e Luiz Eugênio tiveram votos anulados
nao_validos_1t <- c("VOTOU BRANCO/NULO", "NÃO QUIS RESPONDER (NÃO LER)")

# percentual ponderado dos votos válidos declarados
prop_ponderada <- function(x, dominio) 100 * sum(banco$peso[dominio & x]) / sum(banco$peso[dominio])
lb <- banco$voto2t_4 %in% c("Lula", "Bolsonaro")
t1 <- !is.na(banco$cen2_votou_1t) & !banco$cen2_votou_1t %in% nao_validos_1t
gov <- banco$cen2_votou_1t_govrj %in% validos_gov
t18 <- banco$cen_t2_18 %in% c("JAIR BOLSONARO", "FERNANDO HADDAD")
voto_declarado_tse <- tribble(
  ~resultado, ~tse, ~survey,
  "Presidente, 2º turno de 2022: Lula", pct_tse(2022, 2, "Presidente", "LULA"), prop_ponderada(banco$voto2t_4 %in% "Lula", lb),
  "Presidente, 2º turno de 2022: Bolsonaro", pct_tse(2022, 2, "Presidente", "JAIR BOLSONARO"), prop_ponderada(banco$voto2t_4 %in% "Bolsonaro", lb),
  "Presidente, 1º turno de 2022: Lula", pct_tse(2022, 1, "Presidente", "LULA"), prop_ponderada(banco$cen2_votou_1t %in% "LULA", t1),
  "Presidente, 1º turno de 2022: Bolsonaro", pct_tse(2022, 1, "Presidente", "JAIR BOLSONARO"), prop_ponderada(banco$cen2_votou_1t %in% "JAIR BOLSONARO", t1),
  "Governador, 1º turno de 2022: Cláudio Castro", pct_tse(2022, 1, "Governador", "CLÁUDIO CASTRO"), prop_ponderada(banco$cen2_votou_1t_govrj %in% "CLÁUDIO CASTRO", gov),
  "Governador, 1º turno de 2022: Marcelo Freixo", pct_tse(2022, 1, "Governador", "MARCELO FREIXO"), prop_ponderada(banco$cen2_votou_1t_govrj %in% "MARCELO FREIXO", gov),
  "Presidente, 2º turno de 2018: Bolsonaro", pct_tse(2018, 2, "Presidente", "JAIR BOLSONARO"), prop_ponderada(banco$cen_t2_18 %in% "JAIR BOLSONARO", t18)
)

# 5. Gravação ----------------------------------------------------------------------------------------
dir_est <- here::here("output/estimativas")
dir.create(dir_est, showWarnings = FALSE, recursive = TRUE)
saidas <- list(
  proporcoes = prop, diferencas = dif,
  tabela1_perfil_amostra = tabela1, tabela2_perfil_tv_internet = tabela2, tabela3_informacao_opiniao = tabela3,
  voto_declarado_tse = voto_declarado_tse
)
iwalk(saidas, function(tabela, nome) {
  saveRDS(tabela, file.path(dir_est, paste0(nome, ".rds")))
  readr::write_csv(tabela, file.path(dir_est, paste0(nome, ".csv")), na = "")
})
cat("04_estimativas:", nrow(prop), "proporções,", nrow(dif), "diferenças\n")
