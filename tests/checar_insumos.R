# Checa se os insumos externos em data/external são os registrados em data/external/hashes.csv (SHA-256).
# Uso: Rscript tests/checar_insumos.R

dir_ext <- here::here("data/external")
hashes <- readr::read_csv(file.path(dir_ext, "hashes.csv"), show_col_types = FALSE)

arquivos <- file.path(dir_ext, hashes$arquivo)
stopifnot(all(file.exists(arquivos)))
atual <- vapply(arquivos, \(f) digest::digest(file = f, algo = "sha256"), character(1))

divergentes <- hashes$arquivo[atual != hashes$sha256]
if (length(divergentes) > 0) stop("SHA-256 diferente do registrado em hashes.csv: ", paste(divergentes, collapse = ", "))
cat("OK:", nrow(hashes), "insumos conferem com hashes.csv.\n")
