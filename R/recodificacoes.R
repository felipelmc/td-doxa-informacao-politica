# Recodificações das variáveis do survey usadas na ponderação e nas análises.

# Escolaridade harmonizada com as faixas de nível de instrução do Censo 2022.
recodificar_escolaridade <- function(esc) {
  esc <- as.character(esc)
  dplyr::case_when(
    esc %in% c(
      "ANALFABETO/NÃO SABE LER E ESCREVER",
      "NUNCA FREQUENTOU A ESCOLA, MAS SABE LER E ESCREVER/PRIMÁRIO INCOMPLETO (ATÉ 3ª SÉRIE OU 4º ANO DO ENS. FUND.)",
      "PRIMÁRIO COMPLETO (4ª SÉRIE OU 5ª ANO DO ENS. FUND.)",  # no IBGE, fundamental completo exige a 8ª série/9º ano
      "GINÁSIO INCOMPLETO (ATÉ 7ª SÉRIE OU 8ª ANO DO ENS. FUND.)"
    ) ~ "Sem instrução e fundamental incompleto",
    esc %in% c(
      "GINÁSIO COMPLETO (8ª SÉRIE OU 9º ANO DO ENS. FUND.)",
      "COLEGIAL INCOMPLETO (ATÉ 2ª SÉRIE DO ENS. MÉDIO)"
    ) ~ "Fundamental completo e médio incompleto",
    esc %in% c(
      "COLEGIAL COMPLETO (3ª SÉRIE DO ENS. MÉDIO)",
      "ENSINO UNIVERSITÁRIO INCOMPLETO OU ESPECIALIZAÇÃO (TÉCNICO APÓS ENSINO MÉDIO)"
    ) ~ "Médio completo e superior incompleto",
    esc %in% c(
      "ENSINO UNIVERSITÁRIO COMPLETO",
      "PÓS-GRADUAÇÃO OU MAIS"
    ) ~ "Superior completo",
    TRUE ~ NA_character_  # categoria não mapeada: falha explícita em 01_limpeza.R
  )
}

# Cor ou raça em quatro categorias, como no Censo (amarela e indígena agrupadas).
recodificar_cor <- function(cor) {
  cor <- as.character(cor)
  dplyr::case_when(
    cor == "BRANCA" ~ "BRANCA",
    cor == "PRETA" ~ "PRETA",
    cor == "PARDA" ~ "PARDA",
    cor %in% c("AMARELA", "INDÍGENA") ~ "OUTRA"
  )
}

# Marca como UTF-8 os textos e os níveis de fator de um data frame. Alguns pacotes exigem a marcação explícita
# e dão erro de codificação sem ela, a depender do locale da máquina.
marcar_utf8 <- function(dados) {
  utf8_fator <- function(x) {
    levels(x) <- enc2utf8(levels(x))
    x
  }
  dplyr::mutate(dados, dplyr::across(dplyr::where(is.character), enc2utf8),
                dplyr::across(dplyr::where(is.factor), utf8_fator))
}

# Variáveis derivadas usadas no TD. Requer R/rotulos.R (ROTULOS_CATEGORIAS define os níveis dos fatores).
derivar_variaveis_analise <- function(df) {
  ns_nr <- c("NÃO SABE RESPONDER (NÃO LER)", "NÃO QUIS RESPONDER (NÃO LER)")
  fontes_redes <- c("REDES SOCIAIS COMO FACEBOOK, INSTAGRAM OU TWITTER", "WHATSAPP OU TELEGRAM")
  fontes_sites <- c("SITES, BLOGS E PORTAIS DE NOTÍCIAS", "BUSCA NO GOOGLE")
  fontes_outras <- c("RÁDIO", "JORNAIS IMPRESSOS E REVISTAS", "CONVERSAS COM AMIGOS, FAMILIARES",
                     "CONVERSAS COM COLEGAS DE TRABALHOS")
  fontes_nenhuma <- c("NENHUM DESTES", "NÃO ME INFORMO SOBRE POLÍTICA (NÃO LER)", "NÃO SABE RESPONDER (NÃO LER)")
  fator <- function(x, variavel) factor(x, levels = names(ROTULOS_CATEGORIAS[[variavel]]))
  avaliacao3 <- function(x) {
    x <- as.character(x)
    dplyr::case_when(
      x %in% c("ÓTIMO", "BOM") ~ "Ótimo ou bom",
      x %in% c("REGULAR MAIS PARA POSITIVO", "REGULAR MAIS PARA NEGATIVO") ~ "Regular",
      x %in% c("RUIM", "PÉSSIMO") ~ "Ruim ou péssimo",
      x %in% ns_nr ~ "Não sabe ou não respondeu"
    )
  }
  resposta_aberta <- stringr::str_to_lower(stringr::str_trim(as.character(df$redes_pol_out)))

  df <- df %>%
    dplyr::mutate(
      freq4 = fator(dplyr::case_when(
        freq_inf_pol == "TODOS OS DIAS" ~ "Todo dia",
        freq_inf_pol %in% c("ALGUMAS VEZES POR SEMANA", "UMA VEZ POR SEMANA") ~ "Toda semana",
        freq_inf_pol %in% c("ALGUMAS VEZES POR MÊS", "RARAMENTE") ~ "Mensal ou raramente",
        freq_inf_pol == "NUNCA" ~ "Nunca"
      ), "freq4"),
      fonte4 = fator(dplyr::case_when(
        inf1 == "NOTICIÁRIO DA TV" ~ "TV",
        inf1 %in% c(fontes_redes, fontes_sites) ~ "Internet",
        inf1 %in% fontes_outras ~ "Outras fontes",
        inf1 %in% fontes_nenhuma ~ "Não se informa"
      ), "fonte4"),
      fonte5 = fator(dplyr::case_when(
        inf1 == "NOTICIÁRIO DA TV" ~ "TV",
        inf1 %in% fontes_redes ~ "Redes sociais e mensageiros",
        inf1 %in% fontes_sites ~ "Sites, portais e busca",
        inf1 %in% fontes_outras ~ "Outras fontes",
        inf1 %in% fontes_nenhuma ~ "Não se informa"
      ), "fonte5"),
      # rede social mais usada, com as respostas abertas ("outra") reclassificadas
      redes_rec = fator(dplyr::case_when(
        redes_pol == "NÃO USO REDES SOCIAIS (NÃO LER)" ~ "Não usa redes sociais",
        redes_pol == "FACEBOOK" ~ "Facebook",
        redes_pol == "INSTAGRAM" ~ "Instagram",
        redes_pol == "WHATSAPP" ~ "WhatsApp ou Telegram",
        redes_pol == "YOUTUBE" ~ "YouTube",
        redes_pol == "TWITTER" ~ "Twitter (X)",
        redes_pol == "TIKTOK" ~ "TikTok ou Kwai",
        redes_pol %in% ns_nr ~ "Não sabe ou não respondeu",
        redes_pol == "OUTRA. QUAL? ABERTA" & stringr::str_detect(resposta_aberta, "^nenhu|^nao usa") ~ "Não usa redes sociais",
        redes_pol == "OUTRA. QUAL? ABERTA" & stringr::str_detect(resposta_aberta, "telegr") ~ "WhatsApp ou Telegram",
        redes_pol == "OUTRA. QUAL? ABERTA" & stringr::str_detect(resposta_aberta, "kwai|kuai|ckai") ~ "TikTok ou Kwai",
        redes_pol == "OUTRA. QUAL? ABERTA" & stringr::str_detect(resposta_aberta, "google|site|sitrs|globo|^tv$|canal tv") ~
          "Não é rede social (Google, sites ou TV)",
        redes_pol == "OUTRA. QUAL? ABERTA" ~ "Outra rede"
      ), "redes_rec"),
      redes_grp = fator(dplyr::case_when(
        redes_rec %in% c("Não usa redes sociais", "Facebook", "Instagram", "WhatsApp ou Telegram", "YouTube",
                         "Twitter (X)") ~ as.character(redes_rec),
        !is.na(redes_rec) ~ "Outras respostas"
      ), "redes_grp"),
      esc2 = fator(dplyr::case_when(
        esc_faixa %in% c("Sem instrução e fundamental incompleto", "Fundamental completo e médio incompleto") ~
          "Até médio incompleto",
        esc_faixa %in% c("Médio completo e superior incompleto", "Superior completo") ~ "Médio completo ou mais"
      ), "esc2"),
      voto2t_3 = fator(dplyr::case_when(
        cen_t2 == "LULA" ~ "Lula",
        cen_t2 == "JAIR BOLSONARO" ~ "Bolsonaro",
        cen_t2 %in% c("BRANCO/NULO", "NÃO FOI VOTAR") ~ "Branco, nulo ou não votou"
      ), "voto2t_3"),
      voto2t_4 = fator(dplyr::case_when(
        cen_t2 == "LULA" ~ "Lula",
        cen_t2 == "JAIR BOLSONARO" ~ "Bolsonaro",
        cen_t2 == "BRANCO/NULO" ~ "Branco ou nulo",
        cen_t2 == "NÃO FOI VOTAR" ~ "Não foi votar"
      ), "voto2t_4"),
      gov_cat = fator(dplyr::case_when(
        cen2_votou_1t_govrj == "CLÁUDIO CASTRO" ~ "Castro",
        cen2_votou_1t_govrj == "MARCELO FREIXO" ~ "Freixo",
        cen2_votou_1t_govrj == "VOTOU BRANCO/NULO" ~ "Branco ou nulo",
        cen2_votou_1t_govrj == "NÃO QUIS RESPONDER (NÃO LER)" ~ "Não sabe ou não respondeu",
        !is.na(cen2_votou_1t_govrj) ~ "Outros candidatos",
        crivo_votou_1t == "NÃO" ~ "Não votou no 1º turno",
        TRUE ~ "Não sabe ou não respondeu"
      ), "gov_cat"),
      gov_2 = fator(dplyr::if_else(gov_cat %in% c("Castro", "Freixo"), as.character(gov_cat), NA_character_), "gov_2"),
      aval_lula3 = fator(avaliacao3(adpi1_lula), "aval_lula3"),
      aval_gov3 = fator(avaliacao3(adpi1_gov), "aval_gov3"),
      renda_grp = fator(dplyr::case_when(
        rendaf %in% levels(rendaf)[1:2] ~ "Até 2 salários mínimos",
        rendaf %in% levels(rendaf)[3:4] ~ "Mais de 2 a 5 salários mínimos",
        rendaf %in% levels(rendaf)[5:8] ~ "Mais de 5 salários mínimos"
      ), "renda_grp"),
      ideologia_grp = fator(dplyr::case_when(
        ideologia %in% c("ESQUERDA", "CENTRO-ESQUERDA") ~ "Esquerda",
        ideologia == "CENTRO" ~ "Centro",
        ideologia %in% c("DIREITA", "CENTRO-DIREITA") ~ "Direita",
        ideologia == "NÃO ME IDENTIFICO COM NENHUMA IDEOLOGIA (NÃO LER)" ~ "Nenhuma",
        ideologia %in% ns_nr ~ "Não sabe ou não respondeu"
      ), "ideologia_grp"),
      part_pref_grp = fator(dplyr::case_when(
        part_pref == "PT" ~ "PT",
        part_pref == "PL" ~ "PL",
        part_pref == "NENHUM/NÃO TENHO PARTIDO POLÍTICO PREFERIDO" ~ "Nenhum",
        part_pref %in% ns_nr ~ "Não sabe ou não respondeu",
        !is.na(part_pref) ~ "Outros partidos"
      ), "part_pref_grp")
    )

  # nenhuma categoria de origem pode ficar sem classificação
  stopifnot(
    identical(is.na(df$fonte4), is.na(df$inf1)),
    identical(is.na(df$fonte5), is.na(df$inf1)),
    identical(is.na(df$redes_rec), is.na(df$redes_pol)),
    identical(is.na(df$redes_grp), is.na(df$redes_pol)),
    identical(is.na(df$esc2), is.na(df$esc_faixa)),
    identical(is.na(df$voto2t_4), is.na(df$voto2t_3)),
    identical(is.na(df$aval_lula3), is.na(df$adpi1_lula)),
    identical(is.na(df$aval_gov3), is.na(df$adpi1_gov)),
    identical(is.na(df$renda_grp), is.na(df$rendaf)),
    identical(is.na(df$ideologia_grp), is.na(df$ideologia)),
    identical(is.na(df$part_pref_grp), is.na(df$part_pref)),
    all(is.na(df$freq4) == (is.na(df$freq_inf_pol) | df$freq_inf_pol == "NÃO SABE RESPONDER (NÃO LER)")),
    !anyNA(df$gov_cat)
  )
  df
}
