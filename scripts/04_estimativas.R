# 04 — Estimativas do TD: proporções, diferenças, testes, tabelas e anexos.
#
# Todas as estimativas usam o desenho calibrado (setor censitário como conglomerado + raking às margens do
# Censo 2022; ver R/desenho.R), com IC de 95% em escala logit.
#
# Entrada: data/analysis/td_informacao.rds, data/external/ (Censo 2022 e TSE)
# Saída (output/estimativas/, em .rds e .csv):
#   proporcoes, diferencas, testes
#   tabela1_perfil_amostra, tabela2_nunca_regiao_sexo, tabela3_perfil_tv_internet, tabela4_informacao_opiniao
#   anexo_b_validacao_tse, anexo_b_sensibilidade_voto

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
  sem_nunca = subset(d, freq4 %in% freq_informa),
  esc_ate_medio_incompleto = subset(d, esc2 %in% "Até médio incompleto"),
  esc_medio_ou_mais = subset(d, esc2 %in% "Médio completo ou mais"),
  eleitores_lula = subset(d, voto2t_3 %in% "Lula"),
  eleitores_bolsonaro = subset(d, voto2t_3 %in% "Bolsonaro")
)

# 1. Proporções ------------------------------------------------------------------------------------
vars_perfil <- c("sexo", "idadef", "esc_faixa", "cor", "renda_grp", "area3", "freq4", "redes_grp",
                 "usa_rede_politica", "ideologia_grp", "part_pref_grp", "voto2t_3")
especificacoes <- bind_rows(
  tibble(recorte = "total", por = NA_character_,
         variavel = c("freq_inf_pol", "freq4", "inf1", "fonte4", "fonte5", "redes_rec", "redes_grp", "usa_rede_politica",
                      "aval_lula3", "aval_gov3", "voto2t_3", "gov_cat", "ideologia_grp", "part_pref_grp",
                      "sexo", "idadef", "esc_faixa", "esc2", "cor", "renda_grp", "area3")),
  tibble(recorte = "total", por = "fonte4", variavel = vars_perfil),
  tibble(recorte = "total", por = "voto2t_3", variavel = c("freq4", "fonte4", "fonte5")),
  tibble(recorte = "total", por = "gov_2", variavel = c("freq4", "fonte4", "fonte5", "esc_faixa", "esc2")),
  tibble(recorte = "total", por = "fonte5", variavel = c("aval_lula3", "aval_gov3", "voto2t_3", "ideologia_grp")),
  tibble(recorte = "total", por = "aval_lula3", variavel = "freq4"),
  tibble(recorte = "total", por = "aval_gov3", variavel = "freq4"),
  tibble(recorte = "total", por = "freq4",
         variavel = c("aval_lula3", "aval_gov3", "ideologia_grp", "part_pref_grp", "voto2t_3", "fonte4")),
  tibble(recorte = "total", por = "area3", variavel = c("freq4", "fonte4", "aval_gov3", "esc2", "esc_faixa")),
  tibble(recorte = "total", por = "sexo", variavel = c("freq4", "fonte4")),
  tibble(recorte = "total", por = "esc2", variavel = "freq4"),
  tibble(recorte = "total", por = "esc_faixa", variavel = c("freq4", "fonte4")),
  tibble(recorte = "total", por = "idadef", variavel = "fonte4"),
  tibble(recorte = "total", por = "ideologia_grp", variavel = "fonte5"),
  tibble(recorte = "total", por = "part_pref_grp", variavel = "fonte5"),
  tibble(recorte = c("esc_ate_medio_incompleto", "esc_medio_ou_mais"), por = "area3", variavel = "freq4"),
  tibble(recorte = c("esc_ate_medio_incompleto", "esc_medio_ou_mais"), por = "sexo", variavel = "freq4"),
  tibble(recorte = c("eleitores_lula", "eleitores_bolsonaro"), por = "fonte5", variavel = "aval_lula3"),
  tibble(recorte = "sem_nunca", por = NA_character_, variavel = "fonte4"),
  tibble(recorte = "sem_nunca", por = "fonte4", variavel = c("idadef", "esc_faixa", "voto2t_3", "renda_grp"))
)

calcular_proporcoes <- function(desenhos, specs) {
  pmap_dfr(specs, function(recorte, por, variavel) {
    silenciar(proporcoes(desenhos[[recorte]], variavel, if (is.na(por)) NULL else por)) %>%
      mutate(recorte = recorte, .before = 1)
  })
}
prop <- calcular_proporcoes(recortes, especificacoes)

# 2. Diferenças (IC 95% e p-valor) ----------------------------------------------------------------
dg <- function(id, descricao, ...) silenciar(diferenca_grupos(d, ...)) %>% mutate(id = id, descricao = descricao, .before = 1)
dc <- function(id, descricao, ...) silenciar(diferenca_categorias(d, ...)) %>% mutate(id = id, descricao = descricao, .before = 1)
esc_baixa <- quote(esc2 %in% "Até médio incompleto")
esc_alta <- quote(esc2 %in% "Médio completo ou mais")
redes <- "Redes sociais e mensageiros"
sites <- "Sites, portais e busca"
ns <- "Não sabe ou não respondeu"
bnv <- "Branco, nulo ou não votou"

dif <- bind_rows(
  # seção 3: frequência e fontes
  dc("tv_menos_internet", "TV − internet como fonte principal (população)", "fonte4", "TV", "Internet"),
  dc("tv_menos_internet_sem_nunca", "TV − internet como fonte principal, entre quem se informa ao menos raramente",
     "fonte4", "TV", "Internet", dominio = quote(freq4 %in% c("Todo dia", "Toda semana", "Mensal ou raramente"))),
  dg("internet_jovens_idosos", "Internet como fonte: 18 a 24 anos − 60 anos ou mais", "fonte4", "Internet", "idadef", "18 - 24 ANOS", "60 ANOS OU MAIS"),
  dg("tv_idosos_jovens", "TV como fonte: 60 anos ou mais − 18 a 24 anos", "fonte4", "TV", "idadef", "60 ANOS OU MAIS", "18 - 24 ANOS"),
  dg("internet_superior_sem_instrucao", "Internet como fonte: superior completo − sem instrução/fundamental incompleto",
     "fonte4", "Internet", "esc_faixa", "Superior completo", "Sem instrução e fundamental incompleto"),
  dg("nunca_rm_capital", "Nunca se informa: RM − capital", "freq4", "Nunca", "area3", "RM (EXCETO CAPITAL)", "RIO CAPITAL"),
  dg("nunca_demais_capital", "Nunca se informa: demais municípios − capital", "freq4", "Nunca", "area3", "DEMAIS MUNICÍPIOS", "RIO CAPITAL"),
  dg("nunca_rm_capital_esc_baixa", "Nunca se informa: RM − capital (até médio incompleto)", "freq4", "Nunca", "area3", "RM (EXCETO CAPITAL)", "RIO CAPITAL", dominio = esc_baixa),
  dg("nunca_demais_capital_esc_baixa", "Nunca se informa: demais − capital (até médio incompleto)", "freq4", "Nunca", "area3", "DEMAIS MUNICÍPIOS", "RIO CAPITAL", dominio = esc_baixa),
  dg("nunca_rm_capital_esc_alta", "Nunca se informa: RM − capital (médio completo ou mais)", "freq4", "Nunca", "area3", "RM (EXCETO CAPITAL)", "RIO CAPITAL", dominio = esc_alta),
  dg("nunca_demais_capital_esc_alta", "Nunca se informa: demais − capital (médio completo ou mais)", "freq4", "Nunca", "area3", "DEMAIS MUNICÍPIOS", "RIO CAPITAL", dominio = esc_alta),
  dg("diario_capital_demais", "Informa-se todo dia: capital − demais municípios", "freq4", "Todo dia", "area3", "RIO CAPITAL", "DEMAIS MUNICÍPIOS"),
  dg("internet_capital_demais", "Internet como fonte: capital − demais municípios", "fonte4", "Internet", "area3", "RIO CAPITAL", "DEMAIS MUNICÍPIOS"),
  dg("nunca_mulheres_homens", "Nunca se informa: mulheres − homens", "freq4", "Nunca", "sexo", "FEMININO", "MASCULINO"),
  dg("nunca_mulheres_homens_esc_baixa", "Nunca se informa: mulheres − homens (até médio incompleto)", "freq4", "Nunca", "sexo", "FEMININO", "MASCULINO", dominio = esc_baixa),
  dg("nunca_mulheres_homens_esc_alta", "Nunca se informa: mulheres − homens (médio completo ou mais)", "freq4", "Nunca", "sexo", "FEMININO", "MASCULINO", dominio = esc_alta),
  dg("internet_mulheres_homens", "Internet como fonte: mulheres − homens", "fonte4", "Internet", "sexo", "FEMININO", "MASCULINO"),
  # seção 5: informação e política
  dg("diario_lula_bnv", "Informa-se todo dia: Lula − branco/nulo/não votou", "freq4", "Todo dia", "voto2t_3", "Lula", bnv),
  dg("diario_bolsonaro_bnv", "Informa-se todo dia: Bolsonaro − branco/nulo/não votou", "freq4", "Todo dia", "voto2t_3", "Bolsonaro", bnv),
  dg("diario_lula_bolsonaro", "Informa-se todo dia: Lula − Bolsonaro", "freq4", "Todo dia", "voto2t_3", "Lula", "Bolsonaro"),
  dg("nunca_bnv_lula", "Nunca se informa: branco/nulo/não votou − Lula", "freq4", "Nunca", "voto2t_3", bnv, "Lula"),
  dg("nunca_bnv_bolsonaro", "Nunca se informa: branco/nulo/não votou − Bolsonaro", "freq4", "Nunca", "voto2t_3", bnv, "Bolsonaro"),
  dg("nunca_castro_freixo", "Nunca se informa: Castro − Freixo", "freq4", "Nunca", "gov_2", "Castro", "Freixo"),
  dg("diario_freixo_castro", "Informa-se todo dia: Freixo − Castro", "freq4", "Todo dia", "gov_2", "Freixo", "Castro"),
  dg("tv_lula_bolsonaro", "TV como fonte: Lula − Bolsonaro", "fonte5", "TV", "voto2t_3", "Lula", "Bolsonaro"),
  dg("redes_bolsonaro_lula", "Redes e mensageiros como fonte: Bolsonaro − Lula", "fonte5", redes, "voto2t_3", "Bolsonaro", "Lula"),
  dg("sites_lula_bolsonaro", "Sites, portais e busca como fonte: Lula − Bolsonaro", "fonte5", sites, "voto2t_3", "Lula", "Bolsonaro"),
  dg("internet_bolsonaro_lula", "Internet (agregada) como fonte: Bolsonaro − Lula", "fonte4", "Internet", "voto2t_3", "Bolsonaro", "Lula"),
  dg("tv_castro_freixo", "TV como fonte: Castro − Freixo", "fonte5", "TV", "gov_2", "Castro", "Freixo"),
  dg("redes_castro_freixo", "Redes e mensageiros como fonte: Castro − Freixo", "fonte5", redes, "gov_2", "Castro", "Freixo"),
  dg("sites_freixo_castro", "Sites, portais e busca como fonte: Freixo − Castro", "fonte5", sites, "gov_2", "Freixo", "Castro"),
  dg("superior_freixo_castro", "Ensino superior completo: eleitores de Freixo − de Castro", "esc_faixa", "Superior completo", "gov_2", "Freixo", "Castro"),
  dg("redes_pl_pt", "Redes e mensageiros como fonte: simpatizantes do PL − do PT", "fonte5", redes, "part_pref_grp", "PL", "PT"),
  dg("tv_pt_pl", "TV como fonte: simpatizantes do PT − do PL", "fonte5", "TV", "part_pref_grp", "PT", "PL"),
  dg("redes_direita_esquerda", "Redes e mensageiros como fonte: direita − esquerda", "fonte5", redes, "ideologia_grp", "Direita", "Esquerda"),
  dg("sites_esquerda_direita", "Sites, portais e busca como fonte: esquerda − direita", "fonte5", sites, "ideologia_grp", "Esquerda", "Direita"),
  dg("lula_ob_tv_redes", "Avalia Lula como ótimo ou bom: TV − redes e mensageiros", "aval_lula3", "Ótimo ou bom", "fonte5", "TV", redes),
  dg("lula_ob_sites_redes", "Avalia Lula como ótimo ou bom: sites e busca − redes e mensageiros", "aval_lula3", "Ótimo ou bom", "fonte5", sites, redes),
  dg("lula_rp_redes_tv", "Avalia Lula como ruim ou péssimo: redes e mensageiros − TV", "aval_lula3", "Ruim ou péssimo", "fonte5", redes, "TV"),
  dg("lula_ob_tv_redes_eleitores_lula", "Avalia Lula como ótimo ou bom: TV − redes e mensageiros (eleitores de Lula)", "aval_lula3", "Ótimo ou bom", "fonte5", "TV", redes, dominio = quote(voto2t_3 %in% "Lula")),
  dg("lula_ob_tv_redes_eleitores_bolsonaro", "Avalia Lula como ótimo ou bom: TV − redes e mensageiros (eleitores de Bolsonaro)", "aval_lula3", "Ótimo ou bom", "fonte5", "TV", redes, dominio = quote(voto2t_3 %in% "Bolsonaro")),
  dg("castro_rp_sites_tv", "Avalia Castro como ruim ou péssimo: sites e busca − TV", "aval_gov3", "Ruim ou péssimo", "fonte5", sites, "TV"),
  dg("castro_ns_naoinforma_tv", "Não sabe avaliar o governo Castro: não se informa − TV", "aval_gov3", ns, "fonte5", "Não se informa", "TV"),
  dg("diario_ob_regular_lula", "Informa-se todo dia: avalia Lula ótimo/bom − regular", "freq4", "Todo dia", "aval_lula3", "Ótimo ou bom", "Regular"),
  dg("diario_rp_regular_lula", "Informa-se todo dia: avalia Lula ruim/péssimo − regular", "freq4", "Todo dia", "aval_lula3", "Ruim ou péssimo", "Regular"),
  dg("diario_ob_regular_castro", "Informa-se todo dia: avalia Castro ótimo/bom − regular", "freq4", "Todo dia", "aval_gov3", "Ótimo ou bom", "Regular"),
  dg("diario_rp_regular_castro", "Informa-se todo dia: avalia Castro ruim/péssimo − regular", "freq4", "Todo dia", "aval_gov3", "Ruim ou péssimo", "Regular"),
  dg("nunca_ob_regular_castro", "Nunca se informa: avalia Castro ótimo/bom − regular", "freq4", "Nunca", "aval_gov3", "Ótimo ou bom", "Regular"),
  # informação e opinião (tabela 4)
  dg("opiniao_ns_castro", "Não sabe avaliar o governo Castro: nunca se informa − todo dia", "aval_gov3", ns, "freq4", "Nunca", "Todo dia"),
  dg("opiniao_ns_lula", "Não sabe avaliar o governo Lula: nunca se informa − todo dia", "aval_lula3", ns, "freq4", "Nunca", "Todo dia"),
  dg("opiniao_sem_ideologia", "Sem ideologia ou não sabe: nunca se informa − todo dia", "ideologia_grp", c("Nenhuma", ns), "freq4", "Nunca", "Todo dia"),
  dg("opiniao_sem_partido", "Sem partido preferido ou não sabe: nunca se informa − todo dia", "part_pref_grp", c("Nenhum", ns), "freq4", "Nunca", "Todo dia"),
  dg("opiniao_bnv", "Branco, nulo ou não votou no 2º turno: nunca se informa − todo dia", "voto2t_3", bnv, "freq4", "Nunca", "Todo dia"),
  dg("ns_castro_rm_capital", "Não sabe avaliar o governo Castro: RM − capital", "aval_gov3", ns, "area3", "RM (EXCETO CAPITAL)", "RIO CAPITAL"),
  dg("ns_castro_demais_capital", "Não sabe avaliar o governo Castro: demais municípios − capital", "aval_gov3", ns, "area3", "DEMAIS MUNICÍPIOS", "RIO CAPITAL")
)

# perfil TV × internet (tabela 3): diferença internet − TV em cada categoria
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

# 3. Testes de Rao-Scott -----------------------------------------------------------------------------
tr <- function(id, v1, v2, dominio = NULL) silenciar(teste_rao_scott(d, v1, v2, dominio)) %>% mutate(id = id, .before = 1)
tv_internet <- quote(fonte4 %in% c("TV", "Internet"))
testes <- bind_rows(
  tr("freq4_area3", "freq4", "area3"),
  tr("freq4_area3_esc_baixa", "freq4", "area3", esc_baixa),
  tr("freq4_area3_esc_alta", "freq4", "area3", esc_alta),
  tr("freq4_sexo", "freq4", "sexo"),
  tr("freq4_esc2", "freq4", "esc2"),
  tr("fonte4_area3", "fonte4", "area3"),
  tr("fonte4_sexo", "fonte4", "sexo"),
  tr("fonte4_idadef", "fonte4", "idadef"),
  tr("fonte4_esc_faixa", "fonte4", "esc_faixa"),
  tr("freq4_voto2t_3", "freq4", "voto2t_3"),
  tr("freq4_gov_2", "freq4", "gov_2"),
  tr("fonte5_voto2t_3", "fonte5", "voto2t_3"),
  tr("fonte5_gov_2", "fonte5", "gov_2"),
  tr("fonte5_partido_pt_pl", "fonte5", "part_pref_grp", quote(part_pref_grp %in% c("PT", "PL"))),
  tr("fonte5_ideologia_esq_dir", "fonte5", "ideologia_grp", quote(ideologia_grp %in% c("Esquerda", "Direita"))),
  tr("aval_lula3_fonte5", "aval_lula3", "fonte5"),
  tr("aval_gov3_fonte5", "aval_gov3", "fonte5"),
  tr("freq4_aval_lula3", "freq4", "aval_lula3"),
  tr("freq4_aval_gov3", "freq4", "aval_gov3"),
  tr("aval_gov3_area3", "aval_gov3", "area3"),
  tr("aval_gov3_freq4", "aval_gov3", "freq4"),
  tr("ideologia_grp_freq4", "ideologia_grp", "freq4"),
  tr("part_pref_grp_freq4", "part_pref_grp", "freq4"),
  tr("voto2t_3_freq4", "voto2t_3", "freq4"),
  map_dfr(vars_perfil, \(v) tr(paste0("perfil_", v), v, "fonte4", tv_internet))
)

# 4. Tabelas do corpo do texto ---------------------------------------------------------------------
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

# Tabela 2: nunca se informa, por região e por sexo, no total e por escolaridade
tabela2 <- prop %>%
  filter(variavel == "freq4", categoria == "Nunca", por %in% c("area3", "sexo"),
         recorte %in% c("total", "esc_ate_medio_incompleto", "esc_medio_ou_mais")) %>%
  mutate(rotulo = map2_chr(grupo, por, \(g, p) as.character(rotular(g, p))),
         ordem = map2_int(grupo, por, \(g, p) match(g, names(ROTULOS_CATEGORIAS[[p]])))) %>%
  select(por, grupo, rotulo, ordem, recorte, pct, ic_inf, ic_sup, n_grupo, instavel) %>%
  arrange(desc(por == "area3"), ordem)

# Tabela 3: perfil de quem se informa principalmente pela TV e pela internet
tabela3 <- prop %>%
  filter(recorte == "total", por == "fonte4", variavel %in% vars_perfil, grupo %in% c("TV", "Internet")) %>%
  select(variavel, categoria, grupo, pct, n_grupo, instavel) %>%
  pivot_wider(names_from = grupo, values_from = c(pct, n_grupo, instavel)) %>%
  left_join(prop %>% filter(recorte == "total", is.na(por), variavel %in% vars_perfil) %>% select(variavel, categoria, pct_total = pct),
            by = c("variavel", "categoria")) %>%
  left_join(dif_perfil %>% transmute(variavel, categoria, dif_pp, dif_ic_inf = ic_inf, dif_ic_sup = ic_sup, p_valor), by = c("variavel", "categoria")) %>%
  mutate(rotulo_variavel = unname(ROTULOS_VARIAVEIS[variavel]),
         rotulo = map2_chr(categoria, variavel, \(k, v) if (v == "usa_rede_politica") k else as.character(rotular(k, v))),
         ordem_variavel = match(variavel, vars_perfil),
         ordem = map2_int(categoria, variavel, \(k, v) {
           if (v == "usa_rede_politica") match(k, c("Sim", "Não")) else match(k, names(ROTULOS_CATEGORIAS[[v]]))
         })) %>%
  arrange(ordem_variavel, ordem)

# Tabela 4: falta de opinião política por frequência com que se informa
indicadores_opiniao <- tribble(
  ~indicador, ~variavel, ~categorias, ~id_dif,
  "Não sabe avaliar o governo Cláudio Castro", "aval_gov3", list(ns), "opiniao_ns_castro",
  "Não sabe avaliar o governo Lula", "aval_lula3", list(ns), "opiniao_ns_lula",
  "Não se identifica com nenhuma ideologia ou não sabe", "ideologia_grp", list(c("Nenhuma", ns)), "opiniao_sem_ideologia",
  "Não tem partido preferido ou não sabe", "part_pref_grp", list(c("Nenhum", ns)), "opiniao_sem_partido",
  "Votou branco, nulo ou não foi votar no 2º turno de 2022", "voto2t_3", list(bnv), "opiniao_bnv"
)
tabela4 <- pmap_dfr(indicadores_opiniao, function(indicador, variavel, categorias, id_dif) {
  categorias <- unlist(categorias)
  prop %>%
    filter(recorte == "total", por == "freq4", variavel == !!variavel, categoria %in% categorias) %>%
    group_by(grupo) %>%
    # somas de categorias têm variação relativa menor; o alerta vale para indicadores de uma só categoria
    summarise(pct = sum(pct), instavel = length(categorias) == 1 & any(instavel), n_grupo = first(n_grupo),
              .groups = "drop") %>%
    mutate(indicador = indicador, id_dif = id_dif)
}) %>%
  mutate(grupo = factor(grupo, levels = names(ROTULOS_CATEGORIAS$freq4))) %>%
  arrange(grupo) %>%
  select(indicador, grupo, pct, instavel) %>%
  pivot_wider(names_from = grupo, values_from = c(pct, instavel)) %>%
  left_join(indicadores_opiniao %>% select(indicador, id_dif), by = "indicador") %>%
  left_join(dif %>% select(id, dif_pp, ic_inf, ic_sup), by = c("id_dif" = "id"))

# 5. Anexo B: validação com o TSE e sensibilidade à calibração por voto -------------------------------
dv <- preparar(desenho_calibrado_voto(banco, censo))
tse <- readr::read_csv(here::here("data/external/tse_rj_candidatos.csv"), show_col_types = FALSE)
tse_totais <- readr::read_csv(here::here("data/external/tse_rj_totais.csv"), show_col_types = FALSE)
pct_tse <- function(pleito_, turno_, cargo_, candidato_) {
  tse$pct_validos[tse$pleito == pleito_ & tse$turno == turno_ & tse$cargo == cargo_ & tse$candidato == candidato_]
}
validos_gov <- c("CLÁUDIO CASTRO", "MARCELO FREIXO", "RODRIGO NEVES", "PAULO GANIME", "CYRO GARCIA",
                 "EDUARDO SERRA", "JULIETE PANTOJA")  # Witzel e Luiz Eugênio tiveram votos anulados
nao_validos_1t <- c("VOTOU BRANCO/NULO", "NÃO QUIS RESPONDER (NÃO LER)")

prop_ponderada <- function(w, x, dominio) 100 * sum(w[dominio & x]) / sum(w[dominio])
validacao <- function(w) {
  v <- banco
  lb <- v$voto2t_3 %in% c("Lula", "Bolsonaro")
  t1 <- !is.na(v$cen2_votou_1t) & !v$cen2_votou_1t %in% nao_validos_1t
  gov <- v$cen2_votou_1t_govrj %in% validos_gov
  t18 <- v$cen_t2_18 %in% c("JAIR BOLSONARO", "FERNANDO HADDAD")
  c(
    "Presidente, 2º turno de 2022: Lula" = prop_ponderada(w, v$voto2t_3 %in% "Lula", lb),
    "Presidente, 2º turno de 2022: Bolsonaro" = prop_ponderada(w, v$voto2t_3 %in% "Bolsonaro", lb),
    "Presidente, 1º turno de 2022: Lula" = prop_ponderada(w, v$cen2_votou_1t %in% "LULA", t1),
    "Presidente, 1º turno de 2022: Bolsonaro" = prop_ponderada(w, v$cen2_votou_1t %in% "JAIR BOLSONARO", t1),
    "Governador, 1º turno de 2022: Cláudio Castro" = prop_ponderada(w, v$cen2_votou_1t_govrj %in% "CLÁUDIO CASTRO", gov),
    "Governador, 1º turno de 2022: Marcelo Freixo" = prop_ponderada(w, v$cen2_votou_1t_govrj %in% "MARCELO FREIXO", gov),
    "Governador, 1º turno de 2022: Rodrigo Neves" = prop_ponderada(w, v$cen2_votou_1t_govrj %in% "RODRIGO NEVES", gov),
    "Governador, 1º turno de 2022: Paulo Ganime" = prop_ponderada(w, v$cen2_votou_1t_govrj %in% "PAULO GANIME", gov),
    "Presidente, 2º turno de 2018: Bolsonaro" = prop_ponderada(w, v$cen_t2_18 %in% "JAIR BOLSONARO", t18)
  )
}
anexo_b_validacao <- tibble(
  resultado = names(validacao(weights(d))),
  tse = c(pct_tse(2022, 2, "Presidente", "LULA"), pct_tse(2022, 2, "Presidente", "JAIR BOLSONARO"),
          pct_tse(2022, 1, "Presidente", "LULA"), pct_tse(2022, 1, "Presidente", "JAIR BOLSONARO"),
          pct_tse(2022, 1, "Governador", "CLÁUDIO CASTRO"), pct_tse(2022, 1, "Governador", "MARCELO FREIXO"),
          pct_tse(2022, 1, "Governador", "RODRIGO NEVES"), pct_tse(2022, 1, "Governador", "PAULO GANIME"),
          pct_tse(2018, 2, "Presidente", "JAIR BOLSONARO")),
  pesos_principais = unname(validacao(weights(d))),
  calibrado_por_voto = unname(validacao(weights(dv)))
)
lb_castro <- banco$gov_2 %in% "Castro" & banco$voto2t_3 %in% c("Lula", "Bolsonaro")
anexo_b_validacao <- bind_rows(
  anexo_b_validacao,
  tibble(resultado = "Eleitores de Castro que declaram voto em Lula no 2º turno (entre Lula e Bolsonaro)", tse = NA_real_,
         pesos_principais = prop_ponderada(weights(d), banco$voto2t_3 %in% "Lula", lb_castro),
         calibrado_por_voto = prop_ponderada(weights(dv), banco$voto2t_3 %in% "Lula", lb_castro))
)

sens_specs <- tribble(
  ~rotulo, ~variavel, ~categoria, ~por, ~grupo,
  "Informa-se todo dia", "freq4", "Todo dia", NA, NA,
  "Nunca se informa", "freq4", "Nunca", NA, NA,
  "Fonte principal: TV", "fonte4", "TV", NA, NA,
  "Fonte principal: internet", "fonte4", "Internet", NA, NA,
  "Fonte principal: redes sociais e mensageiros", "fonte5", redes, NA, NA,
  "Fonte principal: sites, portais e busca", "fonte5", sites, NA, NA,
  "TV como fonte: eleitores de Lula", "fonte5", "TV", "voto2t_3", "Lula",
  "TV como fonte: eleitores de Bolsonaro", "fonte5", "TV", "voto2t_3", "Bolsonaro",
  "Redes e mensageiros: eleitores de Bolsonaro", "fonte5", redes, "voto2t_3", "Bolsonaro",
  "Redes e mensageiros: eleitores de Lula", "fonte5", redes, "voto2t_3", "Lula",
  "Sites, portais e busca: eleitores de Freixo", "fonte5", sites, "gov_2", "Freixo",
  "Nunca se informa: eleitores de Castro", "freq4", "Nunca", "gov_2", "Castro",
  "Nunca se informa: eleitores de Freixo", "freq4", "Nunca", "gov_2", "Freixo",
  "Avalia o governo Lula como ótimo ou bom", "aval_lula3", "Ótimo ou bom", NA, NA,
  "Avalia o governo Castro como ótimo ou bom", "aval_gov3", "Ótimo ou bom", NA, NA,
  "Voto declarado em Bolsonaro: quem se informa pela TV", "voto2t_3", "Bolsonaro", "fonte4", "TV",
  "Voto declarado em Bolsonaro: quem se informa pela internet", "voto2t_3", "Bolsonaro", "fonte4", "Internet"
)
prop_voto <- calcular_proporcoes(
  list(total = dv),
  distinct(sens_specs, variavel, por) %>% mutate(recorte = "total", .before = 1)
)
buscar <- function(tab, variavel, categoria, por, grupo) {
  tab$pct[tab$recorte == "total" & tab$variavel == variavel & tab$categoria == categoria &
            (if (is.na(por)) is.na(tab$por) else tab$por %in% por) &
            (if (is.na(grupo)) is.na(tab$grupo) else tab$grupo %in% grupo)]
}
anexo_b_sensibilidade <- sens_specs %>%
  mutate(
    pesos_principais = pmap_dbl(list(variavel, categoria, por, grupo), \(v, k, p, g) buscar(prop, v, k, p, g)),
    calibrado_por_voto = pmap_dbl(list(variavel, categoria, por, grupo), \(v, k, p, g) buscar(prop_voto, v, k, p, g)),
    diferenca_pp = calibrado_por_voto - pesos_principais
  ) %>%
  select(rotulo, pesos_principais, calibrado_por_voto, diferenca_pp)

dif_voto <- function(...) silenciar(diferenca_grupos(dv, ...))
anexo_b_sensibilidade <- bind_rows(
  anexo_b_sensibilidade,
  tibble(
    rotulo = c("Diferença TV: Lula − Bolsonaro (p.p.)", "Diferença redes e mensageiros: Bolsonaro − Lula (p.p.)"),
    pesos_principais = c(dif$dif_pp[dif$id == "tv_lula_bolsonaro"], dif$dif_pp[dif$id == "redes_bolsonaro_lula"]),
    calibrado_por_voto = c(dif_voto("fonte5", "TV", "voto2t_3", "Lula", "Bolsonaro")$dif_pp,
                           dif_voto("fonte5", redes, "voto2t_3", "Bolsonaro", "Lula")$dif_pp)
  ) %>% mutate(diferenca_pp = calibrado_por_voto - pesos_principais)
)
w_v <- weights(dv)
anexo_b_resumo_pesos <- tibble(
  esquema = c("Pesos principais", "Calibrado também por voto"),
  n_efetivo = c(n_efetivo(weights(d)), n_efetivo(w_v)),
  razao_max_min = c(max(weights(d)) / min(weights(d)), max(w_v) / min(w_v))
)

# 6. Gravação ----------------------------------------------------------------------------------------
dir_est <- here::here("output/estimativas")
dir.create(dir_est, showWarnings = FALSE, recursive = TRUE)
saidas <- list(
  proporcoes = prop, diferencas = dif, testes = testes,
  tabela1_perfil_amostra = tabela1, tabela2_nunca_regiao_sexo = tabela2,
  tabela3_perfil_tv_internet = tabela3, tabela4_informacao_opiniao = tabela4,
  anexo_b_validacao_tse = anexo_b_validacao, anexo_b_sensibilidade_voto = anexo_b_sensibilidade,
  anexo_b_resumo_pesos = anexo_b_resumo_pesos
)
iwalk(saidas, function(tabela, nome) {
  saveRDS(tabela, file.path(dir_est, paste0(nome, ".rds")))
  readr::write_csv(tabela, file.path(dir_est, paste0(nome, ".csv")), na = "")
})
cat("04_estimativas:", nrow(prop), "proporções,", nrow(dif), "diferenças,", nrow(testes), "testes\n")
