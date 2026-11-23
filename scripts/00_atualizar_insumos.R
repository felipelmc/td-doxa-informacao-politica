# 00: atualiza os insumos externos. Requer internet e é opcional.
#
# O pipeline principal (run_all.R) NÃO executa este script: ele lê as cópias salvas em data/external/.
# Rode apenas se quiser baixar de novo os dados de origem:
#   Rscript scripts/00_atualizar_insumos.R
#
# Gera:
#   data/external/censo2022_t10061_rj_municipios.csv.gz  (IBGE/SIDRA, tabela 10061, 92 municípios do RJ)
#   data/external/rm_rj.csv                              (composição da Região Metropolitana)
#   data/external/tse_rj_candidatos.csv e tse_rj_totais.csv  (resultados eleitorais no RJ)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

options(timeout = 900)
dir_ext <- here::here("data/external")
dir.create(dir_ext, showWarnings = FALSE, recursive = TRUE)

### ----- Censo 2022 (SIDRA, tabela 10061) -----
# Pessoas de 18 anos ou mais por nível de instrução, grupo de idade, sexo e cor ou raça.
municipios_rj <- c(
  3300605,3302205,3302304,3303104,3304102,3300902,3303005,3304706,3301009,3301405,3302403,3304805,3305000,
  3301108,3301207,3301603,3302106,3305703,3303708,3305406,3306008,3300506,3301504,3304607,3305307,3305901,
  3300308,3300407,3304201,3304508,3306107,3306305,3303401,3303906,3305802,3301801,3302809,3302908,3304003,
  3304409,3306206,3300803,3301306,3304300,3305604,3301702,3301900,3302007,3302502,3302601,3302700,3303203,
  3303302,3303500,3303609,3304904,3305109,3300209,3300704,3305208,3305505,3300100,3303807,3304557,3302056,
  3304151,3302254,3305158,3303856,3300258,3300233,3300936,3301876,3302452,3303955,3304110,3304755,3305133,
  3305554,3305752,3300159,3300225,3300456,3300951,3301157,3301850,3302270,3304128,3304144,3304524,3306156,
  3302858
)
stopifnot(length(unique(municipios_rj)) == 92)

api_sidra <- paste0(
  "https://apisidra.ibge.gov.br/values/t/10061/n6/", paste(municipios_rj, collapse = ","),
  "/v/allxp/p/all/c1568/allxt/c58/1145,1146,1147,1148,1149,1150,1151,1152,1153,1154,1155,2503,100052",
  "/c2/allxt/c86/allxt"
)
bruto <- jsonlite::fromJSON(api_sidra)
cabecalho <- as.character(unlist(bruto[1, ]))
bruto <- bruto[-1, ]
names(bruto) <- cabecalho

censo <- tibble(
  cod_ibge = bruto[["Município (Código)"]],
  municipio = bruto[["Município"]],
  variavel = bruto[["Variável"]],
  ano = bruto[["Ano"]],
  cod_instrucao = bruto[["Nível de instrução (Código)"]],
  nivel_instrucao = bruto[["Nível de instrução"]],
  cod_idade = bruto[["Grupo de idade (Código)"]],
  grupo_idade = bruto[["Grupo de idade"]],
  cod_sexo = bruto[["Sexo (Código)"]],
  sexo = bruto[["Sexo"]],
  cod_cor = bruto[["Cor ou raça (Código)"]],
  cor_raca = bruto[["Cor ou raça"]],
  valor = bruto[["Valor"]]  # texto original da API ("-" indica zero)
)
stopifnot(n_distinct(censo$cod_ibge) == 92)
arquivo_censo <- file.path(dir_ext, "censo2022_t10061_rj_municipios.csv.gz")
write_csv(censo, arquivo_censo)

### ----- Região Metropolitana -----
# Composição legal (Lei Complementar estadual nº 184, de 27/12/2018): 22 municípios, incluindo a capital.
municipios_rm <- c(
  3300456,3300803,3301702,3301850,3301900,3302007,3302270,3302502,3302700,3302858,3303203,3303302,3303500,
  3303609,3303906,3304144,3304300,3304557,3304904,3305109,3305554,3305752
)
stopifnot(length(unique(municipios_rm)) == 22, all(municipios_rm %in% municipios_rj))

rm_rj <- censo %>%
  distinct(cod_ibge, municipio) %>%
  mutate(rm = as.integer(cod_ibge %in% as.character(municipios_rm))) %>%
  arrange(cod_ibge)
stopifnot(sum(rm_rj$rm) == 22)
write_csv(rm_rj, file.path(dir_ext, "rm_rj.csv"))

### ----- TSE: resultados no RJ -----
# 2022: API oficial de resultados do TSE. 2018 (2º turno): valores publicados pelo TSE,
# transcritos da Wikipédia (sem endpoint equivalente para 2018).
num_br <- function(x) as.numeric(gsub(",", ".", x))
pleitos <- tribble(
  ~pleito, ~turno, ~cargo,        ~url,
  2022,    1,      "Presidente",  "https://resultados.tse.jus.br/oficial/ele2022/544/dados-simplificados/rj/rj-c0001-e000544-r.json",
  2022,    2,      "Presidente",  "https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/rj/rj-c0001-e000545-r.json",
  2022,    1,      "Governador",  "https://resultados.tse.jus.br/oficial/ele2022/546/dados-simplificados/rj/rj-c0003-e000546-r.json"
)
resultados <- pmap(pleitos, function(pleito, turno, cargo, url) {
  x <- jsonlite::fromJSON(url)
  list(
    totais = tibble(pleito, turno, cargo, eleitorado = num_br(x$e), comparecimento = num_br(x$c),
                    abstencao = num_br(x$a), validos = num_br(x$vv), brancos = num_br(x$vb),
                    nulos = num_br(x$tvn), fonte = url),
    candidatos = tibble(pleito, turno, cargo, candidato = x$cand$nm, votos = num_br(x$cand$vap),
                        pct_validos = num_br(x$cand$pvap), fonte = url)
  )
})
fonte_2018 <- "TSE, via https://pt.wikipedia.org/wiki/Resultados_da_elei%C3%A7%C3%A3o_presidencial_no_Brasil_em_2018"
candidatos <- bind_rows(
  map(resultados, "candidatos"),
  tibble(pleito = 2018, turno = 2, cargo = "Presidente", candidato = c("JAIR BOLSONARO", "FERNANDO HADDAD"),
         votos = c(5669059, 2673386), pct_validos = c(67.95, 32.05), fonte = fonte_2018)
) %>%
  mutate(candidato = gsub("&apos;", "'", candidato, fixed = TRUE))
write_csv(candidatos, file.path(dir_ext, "tse_rj_candidatos.csv"))
write_csv(bind_rows(map(resultados, "totais")), file.path(dir_ext, "tse_rj_totais.csv"))

### ----- Proveniência -----
arquivos <- setdiff(list.files(dir_ext, pattern = "\\.(csv|gz)$", full.names = TRUE), file.path(dir_ext, "hashes.csv"))
hashes <- tibble(arquivo = basename(arquivos),
                 sha256 = map_chr(arquivos, \(f) digest::digest(file = f, algo = "sha256")))
write_csv(hashes, file.path(dir_ext, "hashes.csv"))
cat("Insumos atualizados em", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
print(hashes)
