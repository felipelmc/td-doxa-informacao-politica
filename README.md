# Como a população do estado do Rio de Janeiro se informa sobre política?

Pacote de replicação do Texto para Discussão do DOXA (Laboratório de Estudos Eleitorais, de Comunicação Política e
Opinião Pública do IESP-UERJ) sobre acesso à informação política no estado do Rio de Janeiro, com dados do _survey_
DOXA/Quaest de julho de 2023.

## Estrutura

| Pasta | Conteúdo |
|---|---|
| `data/raw/` | Banco bruto com as 22 colunas do _survey_ usadas no TD (`quaest_rj_2023_td.rds`) e codebook dessas colunas (`codebook_td.md` e `.csv`) ([README](data/raw/README.md)) |
| `data/external/` | Extratos do Censo 2022 (SIDRA, tabela 10061), composição da Região Metropolitana e resultados do TSE, com proveniência ([README](data/external/README.md)) |
| `data/processed/` | Banco limpo (`survey_limpo.rds`) e ponderado (`survey_ponderado.rds`) |
| `data/analysis/` | Banco de análise do TD (`td_informacao.csv` e `.rds`) e dicionários de variáveis e de categorias |
| `R/` | Funções: Censo, recodificações, rótulos, desenho amostral, estimação, gráficos e formatação |
| `scripts/` | Etapas do pipeline, de `01_limpeza.R` a `05_figuras.R`, e `00_atualizar_insumos.R`, que baixa os insumos externos |
| `reports/` | Texto do TD (`report1-informacao.qmd`) e o PDF renderizado |
| `output/` | Estimativas e tabelas (`estimativas/`), figuras em PDF e PNG (`figuras/`), diagnósticos dos pesos (`diagnosticos/`) e `sessionInfo.txt` |
| `tests/` | Checagem dos insumos externos |

## Como reproduzir

Requisitos: R 4.5.2, [Quarto](https://quarto.org) 1.9 ou superior com TinyTeX (`quarto install tinytex`) e a fonte Arial
instalada no sistema. Sem a Arial, troque `mainfont` e `sansfont` no cabeçalho do TD e `FONTE_GRAFICOS` em
`R/graficos.R` (por exemplo, por TeX Gyre Heros).

```r
# no R, na raiz do projeto: instala as versões de pacotes registradas em renv.lock
renv::restore()
```

```sh
# no terminal, na raiz do projeto: roda todo o pipeline, sem acesso à internet
Rscript run_all.R
```

O `run_all.R` confere os insumos externos (SHA-256), executa as etapas 01 a 05 em processos separados, renderiza o TD e
grava `output/sessionInfo.txt`. Para baixar de novo os dados do Censo, da Região Metropolitana e do TSE, rode
`Rscript scripts/00_atualizar_insumos.R` (requer internet). O banco bruto traz apenas as colunas do _survey_ usadas no
TD; o banco completo da Quaest não é distribuído.

| Etapa | O que faz |
|---|---|
| `01_limpeza.R` | Lê o banco bruto, seleciona variáveis, recodifica escolaridade, cor e região, remove menores de 18 anos e quem não informou cor |
| `02_pesos.R` | Calibra os pesos por _raking_ às margens do Censo 2022 e grava os diagnósticos |
| `03_banco_analise.R` | Cria as variáveis derivadas e grava o banco de análise e os dicionários |
| `04_estimativas.R` | Calcula proporções, diferenças entre grupos e tabelas, e compara o voto declarado com o resultado do TSE |
| `05_figuras.R` | Desenha os gráficos do TD |

## Amostra, pesos e incerteza

- **Amostra.** O banco bruto tem 1.266 entrevistas. Foram excluídos 15 respondentes com menos de 18 anos e 22 que não
  informaram cor ou raça, e restaram 1.229.
- **Pesos.** O _raking_ usa quatro margens do Censo 2022 (população de 18 anos ou mais): área × sexo, área × faixa etária,
  área × escolaridade e cor ou raça (4 categorias). A área separa a capital, os demais municípios da Região Metropolitana
  (Lei Complementar estadual nº 184/2018, com Petrópolis) e os demais municípios. O efeito de desenho decorrente da
  ponderação é de 1,18.
- **Incerteza.** O desenho usa o setor censitário como unidade primária e incorpora a calibração (`R/desenho.R`). Os
  intervalos de confiança de proporções são calculados em escala logit. Como a amostra é por cotas, os intervalos são
  aproximações.
- **Regra de interpretação.** Estimativas com menos de 30 entrevistas ou coeficiente de variação acima de 30% são
  sinalizadas (`instavel` em `output/estimativas/proporcoes.csv`) e não são interpretadas no texto.

## Onde está cada resultado do TD

| No TD | Arquivo |
|---|---|
| Gráficos 1 a 9 | `output/figuras/grafico1_frequencia` a `grafico9_avaliacao_regiao` (`.pdf` e `.png`) |
| Tabela 1 | `output/estimativas/tabela1_perfil_amostra.csv` |
| Tabela 2 | `output/estimativas/tabela2_perfil_tv_internet.csv` |
| Tabela 3 | `output/estimativas/tabela3_informacao_opiniao.csv` |
| Voto declarado e resultado do TSE (seção 2) | `output/estimativas/voto_declarado_tse.csv` |
| Números citados no texto | `output/estimativas/proporcoes.csv`, lido pelo `.qmd` |
| Comparações entre grupos citadas no texto | `output/estimativas/diferencas.csv` (diferença em pontos percentuais, IC 95% e p-valor; `ic_exclui_zero = FALSE` indica diferença dentro da margem de erro) |

## Dados e licença

O código está sob a licença MIT ([LICENSE](LICENSE)). O _survey_ foi contratado pelo DOXA (IESP-UERJ) e realizado pela
Quaest. Os dados do Censo 2022 são do IBGE, e os resultados eleitorais, do TSE.
