# O Quarto executa o R dentro de reports/. Este arquivo ativa o ambiente renv da raiz do projeto,
# para que os relatórios usem as mesmas versões de pacotes registradas em renv.lock.
local({
  raiz <- normalizePath("..", mustWork = TRUE)
  Sys.setenv(RENV_PROJECT = raiz)
  source(file.path(raiz, "renv", "activate.R"))
})
