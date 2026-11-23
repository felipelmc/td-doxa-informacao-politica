# Funções para ler o extrato do Censo 2022 (SIDRA, tabela 10061) e harmonizá-lo com as categorias do survey.

# Códigos IBGE dos municípios da Região Metropolitana (Lei Complementar estadual nº 184/2018, 22 municípios).
ler_rm <- function() {
  rm_rj <- readr::read_csv(here::here("data/external/rm_rj.csv"), col_types = readr::cols(.default = "c"))
  rm_rj$cod_ibge[rm_rj$rm == "1"]
}

# Classifica os municípios nas três áreas usadas no TD.
classificar_area <- function(cod_ibge, municipios_rm) {
  cod_ibge <- as.character(cod_ibge)
  dplyr::case_when(
    cod_ibge == "3304557" ~ "RIO CAPITAL",
    cod_ibge %in% municipios_rm ~ "RM (EXCETO CAPITAL)",
    TRUE ~ "DEMAIS MUNICÍPIOS"
  )
}

# Lê o extrato do Censo e recodifica para as categorias do survey.
ler_censo <- function() {
  municipios_rm <- ler_rm()
  readr::read_csv(here::here("data/external/censo2022_t10061_rj_municipios.csv.gz"),
                  col_types = readr::cols(.default = "c")) %>%
    dplyr::transmute(
      cod_ibge,
      area3 = classificar_area(cod_ibge, municipios_rm),
      esc_faixa = nivel_instrucao,
      idadef = dplyr::case_when(
        grupo_idade == "18 a 24 anos" ~ "18 - 24 ANOS",
        grupo_idade %in% c("25 a 29 anos", "30 a 34 anos") ~ "25 - 34 ANOS",
        grupo_idade %in% c("35 a 39 anos", "40 a 44 anos") ~ "35 - 44 ANOS",
        grupo_idade %in% c("45 a 49 anos", "50 a 54 anos", "55 a 59 anos") ~ "45 - 59 ANOS",
        grupo_idade %in% c("60 a 64 anos", "65 a 69 anos", "70 a 74 anos", "75 a 79 anos", "80 anos ou mais") ~ "60 ANOS OU MAIS"
      ),
      sexo = dplyr::case_when(sexo == "Homens" ~ "MASCULINO", sexo == "Mulheres" ~ "FEMININO"),
      cor = dplyr::case_when(
        cor_raca == "Branca" ~ "BRANCA",
        cor_raca == "Preta" ~ "PRETA",
        cor_raca == "Parda" ~ "PARDA",
        cor_raca %in% c("Amarela", "Indígena") ~ "OUTRA"
      ),
      # "-" na SIDRA indica zero
      valor = dplyr::coalesce(suppressWarnings(as.numeric(valor)), 0)
    )
}

# População em cada combinação das variáveis indicadas, no formato que survey::rake() pede (coluna Freq).
calcular_margem_censo <- function(censo, vars) {
  stopifnot(!anyNA(censo[vars]))
  censo %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars))) %>%
    dplyr::summarise(Freq = sum(valor), .groups = "drop") %>%
    as.data.frame()
}
