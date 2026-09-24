# Executa todo o pipeline, do banco bruto ao PDF do TD, sem acesso à internet.
# Uso (na raiz do projeto): Rscript run_all.R
#
# Cada etapa roda em um processo R novo, para não depender de objetos deixados por etapas anteriores.
# Os insumos externos já estão em data/external/ (para atualizá-los: scripts/00_atualizar_insumos.R).

etapas <- c(
  "scripts/01_limpeza.R",
  "scripts/02_pesos.R",
  "scripts/03_banco_analise.R",
  "scripts/04_estimativas.R",
  "scripts/05_figuras.R"
)
relatorios <- "reports/report1-informacao.qmd"

rscript <- file.path(R.home("bin"), "Rscript")

message(">> tests/checar_insumos.R")
if (system2(rscript, shQuote(here::here("tests/checar_insumos.R"))) != 0) stop("Os insumos em data/external não conferem")

# estimativas e figuras são sempre recriadas do zero
unlink(here::here(c("output/estimativas", "output/figuras")), recursive = TRUE)

for (etapa in etapas) {
  message("\n>> ", etapa)
  if (system2(rscript, shQuote(here::here(etapa))) != 0) stop("Falha na etapa ", etapa)
}

for (relatorio in relatorios) {
  message("\n>> quarto render ", relatorio)
  if (system2("quarto", c("render", shQuote(here::here(relatorio)))) != 0) stop("Falha ao renderizar ", relatorio)
}

dir.create(here::here("output"), showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), here::here("output/sessionInfo.txt"))
message("\nPipeline concluído.")
