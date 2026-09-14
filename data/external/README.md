# Insumos externos

Cópias locais dos dados de terceiros usados no pipeline. Com elas, o projeto roda sem acesso à internet.
Para baixar tudo de novo: `Rscript scripts/00_atualizar_insumos.R` (a última atualização foi em 14/09/2026). As somas SHA-256
de cada arquivo estão em `hashes.csv`.

| Arquivo | Conteúdo | Origem |
|---|---|---|
| `censo2022_t10061_rj_municipios.csv.gz` | Censo Demográfico 2022, tabela 10061: pessoas de 18 anos ou mais por nível de instrução, grupo de idade, sexo e cor ou raça, nos 92 municípios do RJ. A coluna `valor` guarda o texto original da API; "-" indica zero. | IBGE, API SIDRA (`apisidra.ibge.gov.br/values/t/10061/...`) |
| `rm_rj.csv` | Municípios do RJ; `rm` indica os 22 municípios da Região Metropolitana, incluindo a capital e Petrópolis | Lei Complementar estadual nº 184, de 27/12/2018 |
| `tse_rj_candidatos.csv` | Votos e percentual de votos válidos por candidato no RJ: presidente (1º e 2º turnos de 2022, 2º turno de 2018) e governador (1º turno de 2022) | TSE, API de resultados (`resultados.tse.jus.br`, 2022); para 2018, valores do TSE transcritos da Wikipédia |
| `tse_rj_totais.csv` | Eleitorado, comparecimento, abstenção, votos válidos, brancos e nulos no RJ (2022) | TSE, API de resultados |

O script `tests/checar_insumos.R`, chamado pelo `run_all.R`, confere se cada arquivo tem a soma SHA-256 registrada em
`hashes.csv`.
