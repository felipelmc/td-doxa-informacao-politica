# Dicionário central de rótulos e ordem das categorias.
#
# Para cada variável, os NOMES do vetor são os valores gravados no banco e os VALORES são os rótulos usados
# no texto, nas tabelas e nos gráficos. A ordem dos elementos define a ordem de exibição e os níveis dos
# fatores das variáveis derivadas. Usado por R/recodificacoes.R e pelos scripts 03 a 05.

identidade <- function(...) {
  x <- c(...)
  stats::setNames(x, x)
}

ROTULOS_CATEGORIAS <- list(
  sexo = c("FEMININO" = "Mulheres", "MASCULINO" = "Homens"),
  idadef = c("18 - 24 ANOS" = "18 a 24 anos", "25 - 34 ANOS" = "25 a 34 anos", "35 - 44 ANOS" = "35 a 44 anos",
             "45 - 59 ANOS" = "45 a 59 anos", "60 ANOS OU MAIS" = "60 anos ou mais"),
  esc_faixa = c("Sem instrução e fundamental incompleto" = "Sem instrução ou fundamental incompleto",
                "Fundamental completo e médio incompleto" = "Fundamental completo ou médio incompleto",
                "Médio completo e superior incompleto" = "Médio completo ou superior incompleto",
                "Superior completo" = "Superior completo"),
  cor = c("BRANCA" = "Branca", "PRETA" = "Preta", "PARDA" = "Parda", "OUTRA" = "Amarela ou indígena"),
  area3 = c("RIO CAPITAL" = "Capital", "RM (EXCETO CAPITAL)" = "Região Metropolitana (exceto capital)",
            "DEMAIS MUNICÍPIOS" = "Demais municípios"),
  esc2 = identidade("Até médio incompleto", "Médio completo ou mais"),
  renda_grp = identidade("Até 2 salários mínimos", "Mais de 2 a 5 salários mínimos", "Mais de 5 salários mínimos"),
  freq_inf_pol = c("TODOS OS DIAS" = "Todos os dias", "ALGUMAS VEZES POR SEMANA" = "Algumas vezes por semana",
                   "UMA VEZ POR SEMANA" = "Uma vez por semana", "ALGUMAS VEZES POR MÊS" = "Algumas vezes por mês",
                   "RARAMENTE" = "Raramente", "NUNCA" = "Nunca", "NÃO SABE RESPONDER (NÃO LER)" = "Não sabe"),
  freq4 = identidade("Todo dia", "Toda semana", "Mensal ou raramente", "Nunca"),
  inf1 = c("NOTICIÁRIO DA TV" = "Noticiário da TV",
           "REDES SOCIAIS COMO FACEBOOK, INSTAGRAM OU TWITTER" = "Redes sociais (Facebook, Instagram ou Twitter)",
           "WHATSAPP OU TELEGRAM" = "WhatsApp ou Telegram",
           "SITES, BLOGS E PORTAIS DE NOTÍCIAS" = "Sites, blogs e portais de notícias",
           "BUSCA NO GOOGLE" = "Busca no Google",
           "RÁDIO" = "Rádio",
           "JORNAIS IMPRESSOS E REVISTAS" = "Jornais impressos e revistas",
           "CONVERSAS COM AMIGOS, FAMILIARES" = "Conversas com amigos e familiares",
           "CONVERSAS COM COLEGAS DE TRABALHOS" = "Conversas com colegas de trabalho",
           "NENHUM DESTES" = "Nenhum destes",
           "NÃO ME INFORMO SOBRE POLÍTICA (NÃO LER)" = "Não me informo sobre política",
           "NÃO SABE RESPONDER (NÃO LER)" = "Não sabe"),
  fonte4 = identidade("TV", "Internet", "Outras fontes", "Não se informa"),
  fonte5 = identidade("TV", "Redes sociais e mensageiros", "Sites, portais e busca", "Outras fontes", "Não se informa"),
  redes_rec = identidade("Não usa redes sociais", "Facebook", "Instagram", "WhatsApp ou Telegram", "YouTube",
                         "Twitter (X)", "TikTok ou Kwai", "Outra rede", "Não é rede social (Google, sites ou TV)",
                         "Não sabe ou não respondeu"),
  redes_grp = identidade("Não usa redes sociais", "Facebook", "Instagram", "WhatsApp ou Telegram", "YouTube",
                         "Twitter (X)", "Outras respostas"),
  voto2t_3 = identidade("Lula", "Bolsonaro", "Branco, nulo ou não votou"),
  voto2t_4 = identidade("Lula", "Bolsonaro", "Branco ou nulo", "Não foi votar"),
  gov_cat = identidade("Castro", "Freixo", "Outros candidatos", "Branco ou nulo", "Não votou no 1º turno",
                       "Não sabe ou não respondeu"),
  gov_2 = identidade("Castro", "Freixo"),
  aval_lula3 = identidade("Ótimo ou bom", "Regular", "Ruim ou péssimo", "Não sabe ou não respondeu"),
  aval_gov3 = identidade("Ótimo ou bom", "Regular", "Ruim ou péssimo", "Não sabe ou não respondeu"),
  ideologia_grp = identidade("Esquerda", "Centro", "Direita", "Nenhuma", "Não sabe ou não respondeu"),
  part_pref_grp = identidade("PT", "PL", "Outros partidos", "Nenhum", "Não sabe ou não respondeu")
)

ROTULOS_VARIAVEIS <- c(
  id = "Identificador do respondente (posição da linha no banco bruto)",
  peso = "Peso amostral calibrado (raking às margens do Censo 2022)",
  cod_setor = "Código do setor censitário (unidade primária de amostragem)",
  cod_ibge = "Código IBGE do município",
  mun = "Município",
  zona = "Tipo de setor (urbano ou rural)",
  sexo = "Sexo",
  idadef = "Faixa etária",
  cor = "Cor ou raça",
  esc_faixa = "Escolaridade",
  esc2 = "Escolaridade (2 níveis)",
  area3 = "Região",
  rendaf = "Renda familiar mensal",
  renda_grp = "Renda familiar mensal (faixas agregadas)",
  freq_inf_pol = "Frequência com que se informa sobre política",
  freq4 = "Frequência com que se informa sobre política (4 categorias)",
  inf1 = "Fonte principal de informação sobre política",
  fonte4 = "Fonte principal de informação sobre política (4 grupos)",
  fonte5 = "Fonte principal de informação sobre política (5 grupos)",
  redes_pol = "Rede social mais usada para se informar sobre política",
  redes_pol_out = "Rede social mais usada: texto da resposta \"outra\"",
  redes_rec = "Rede social mais usada para se informar sobre política (respostas abertas recodificadas)",
  redes_grp = "Rede social mais usada para se informar sobre política (agregada)",
  crivo_votou_1t = "Votou no 1º turno de 2022",
  cen2_votou_1t = "Voto declarado para presidente no 1º turno de 2022",
  cen_t2 = "Voto declarado para presidente no 2º turno de 2022",
  voto2t_3 = "Voto declarado para presidente no 2º turno de 2022 (3 grupos)",
  voto2t_4 = "Voto declarado para presidente no 2º turno de 2022 (4 grupos)",
  cen2_votou_1t_govrj = "Voto declarado para governador no 1º turno de 2022",
  gov_cat = "Voto declarado para governador no 1º turno de 2022 (6 grupos)",
  gov_2 = "Voto declarado para governador no 1º turno de 2022 (Castro ou Freixo)",
  cen_t2_18 = "Voto declarado para presidente no 2º turno de 2018",
  adpi1_lula = "Avaliação do governo Lula",
  aval_lula3 = "Avaliação do governo Lula (3 categorias e não sabe)",
  adpi1_gov = "Avaliação do governo Cláudio Castro",
  aval_gov3 = "Avaliação do governo Cláudio Castro (3 categorias e não sabe)",
  ideologia = "Posição ideológica",
  ideologia_grp = "Posição ideológica (agregada)",
  part_pref = "Partido preferido",
  part_pref_grp = "Partido preferido (agregado)"
)

# Rótulo de publicação de valores de uma variável (mantém o valor original quando não há rótulo).
rotular <- function(x, variavel) {
  mapa <- ROTULOS_CATEGORIAS[[variavel]]
  x <- as.character(x)
  if (is.null(mapa)) return(x)
  extras <- setdiff(unique(x[!is.na(x)]), names(mapa))
  factor(dplyr::coalesce(unname(mapa[x]), x), levels = unique(c(unname(mapa), extras)))
}
