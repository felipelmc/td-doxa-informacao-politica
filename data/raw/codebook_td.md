# Codebook do banco usado no TD

Colunas do survey DOXA/Quaest (RJ, julho de 2023) usadas no Texto para Discussão sobre informação política. Enunciados e categorias vêm dos metadados do banco original da Quaest; as contagens (n) são nas 1.266 entrevistas do banco bruto, antes das exclusões e sem pesos. As variáveis derivadas estão em `data/analysis/dicionario_variaveis.csv`.

Siglas dos enunciados: RU = resposta única; NÃO LER = opção não lida ao entrevistado.

## `mun`

**(NÃO PERGUNTAR) MUNICÍPIO: (ANOTAR DE ACORDO COM A FOLHA DE COTA)**

Tipo: categórica. Uso no TD: Nome do município. Metadado do banco de análise, não entra nas estimativas.

35 valores distintos; 1.266 entrevistas com a informação preenchida.

## `cod_setor`

**Código setor**

Tipo: texto. Uso no TD: Setor censitário: unidade primária de amostragem no desenho (scripts/04_estimativas.R).

199 valores distintos; 1.266 entrevistas com a informação preenchida.

## `zona`

**(NÃO PERGUNTAR) TIPO DE SETOR**

Tipo: categórica. Uso no TD: Tipo de setor. Metadado do banco de análise, não entra nas estimativas.

| Código | Categoria | n |
|---:|---|---:|
| 1 | URBANO | 1.167 |
| 2 | RURAL | 99 |

## `cod_ibge`

**(NÃO PERGUNTAR) CODIGO IBGE**

Tipo: numérica. Uso no TD: Código do município → area3 (capital, RM pela LC 184/2018, demais municípios): calibração e cruzamentos.

35 valores distintos; 1.266 entrevistas com a informação preenchida.

## `sexo`

**Questão 1.** (NÃO PERGUNTAR, APENAS REGISTRE) SEXO:

Tipo: categórica. Uso no TD: Calibração dos pesos (área × sexo) e cruzamentos.

| Código | Categoria | n |
|---:|---|---:|
| 1 | MASCULINO | 582 |
| 2 | FEMININO | 684 |

## `idadef`

**Questão 2.** (CODIFICAÇÃO AUTOMÁTICA) QUANTOS ANOS VOCÊ TEM? FAIXA

Tipo: categórica. Uso no TD: Calibração dos pesos (área × faixa etária), exclusão dos menores de 18 anos e perfil de TV × internet.

| Código | Categoria | n |
|---:|---|---:|
| 1 | 16 - 17 ANOS | 15 |
| 2 | 18 - 24 ANOS | 147 |
| 3 | 25 - 34 ANOS | 238 |
| 4 | 35 - 44 ANOS | 250 |
| 5 | 45 - 59 ANOS | 309 |
| 6 | 60 ANOS OU MAIS | 307 |

## `esc`

**Questão 3.** ATÉ QUAL ANO/SÉRIE VOCÊ ESTUDOU E CONCLUIU NA ESCOLA? (RU)

Tipo: categórica. Uso no TD: Origem de esc_faixa: calibração (área × escolaridade) e perfil de TV × internet.

| Código | Categoria | n |
|---:|---|---:|
| 1 | ANALFABETO/NÃO SABE LER E ESCREVER | 10 |
| 2 | NUNCA FREQUENTOU A ESCOLA, MAS SABE LER E ESCREVER/PRIMÁRIO INCOMPLETO (ATÉ 3ª SÉRIE OU 4º ANO DO ENS. FUND.) | 47 |
| 3 | PRIMÁRIO COMPLETO (4ª SÉRIE OU 5ª ANO DO ENS. FUND.) | 128 |
| 4 | GINÁSIO INCOMPLETO (ATÉ 7ª SÉRIE OU 8ª ANO DO ENS. FUND.) | 99 |
| 5 | GINÁSIO COMPLETO (8ª SÉRIE OU 9º ANO DO ENS. FUND.) | 96 |
| 6 | COLEGIAL INCOMPLETO (ATÉ 2ª SÉRIE DO ENS. MÉDIO) | 116 |
| 7 | COLEGIAL COMPLETO (3ª SÉRIE DO ENS. MÉDIO) | 407 |
| 8 | ENSINO UNIVERSITÁRIO INCOMPLETO OU ESPECIALIZAÇÃO (TÉCNICO APÓS ENSINO MÉDIO) | 145 |
| 9 | ENSINO UNIVERSITÁRIO COMPLETO | 176 |
| 10 | PÓS-GRADUAÇÃO OU MAIS | 42 |
| 99 | NÃO QUIS RESPONDER | 0 |

## `rendaf`

**Questão 6.** (APRESENTE ANEXO “RENDA FAMILIAR”) APENAS PARA FAZERMOS UMA CLASSIFICAÇÃO SOCIOECONÔMICA, VOCÊ PODERIA ME DIZER EM QUAL DESSAS FAIXAS ESTÁ A SUA RENDA FAMILIAR MENSAL HOJE? PODE FALAR O NÚMERO DA CARTELA, SE PREFERIR.  (A RENDA FAMILIAR MENSAL

Tipo: categórica. Uso no TD: Origem de renda_grp: perfil de TV × internet (não entra na calibração).

| Código | Categoria | n |
|---:|---|---:|
| 1 | ATÉ R$ 1.320,00 (ATÉ 1 SALÁRIO MÍNIMO) | 295 |
| 2 | DE R$ 1.320,01 A R$ 2.640,00 (+ DE 1 ATÉ 2 SM) | 222 |
| 3 | DE R$ 2.640,01 A R$ 3.960,00 (+ DE 2 ATÉ 3 SM) | 232 |
| 4 | DE R$ 3.960,01 A R$ 6.600,00 (+ DE 3 ATÉ 5 SM) | 205 |
| 5 | DE R$ 6.600,01 A R$ 13.200,00 (+ DE 5 ATÉ 10 SM) | 256 |
| 6 | DE R$ 13.200,01 A R$ 19.800,00 (+ DE 10 ATÉ 15 SM) | 46 |
| 7 | DE R$ 19.800,01 A R$ 26.400,00 (+ DE 15 ATÉ 20 SM) | 8 |
| 8 | MAIS DE R$ 26.400,00 (+ DE 20 SM) | 2 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 0 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 0 |

## `cor`

**Questão 64.** QUAL É A SUA COR OU RAÇA: BRANCA, PRETA, PARDA, AMARELA OU INDÍGENA? (RU)

Tipo: categórica. Uso no TD: Origem de cor em 4 categorias (calibração) e exclusão de quem não informou.

| Código | Categoria | n |
|---:|---|---:|
| 1 | BRANCA | 343 |
| 2 | PRETA | 326 |
| 3 | PARDA | 536 |
| 4 | AMARELA | 30 |
| 5 | INDÍGENA | 9 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 17 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 5 |

## `crivo_votou_1t`

**Questão 9.** VOCÊ FOI VOTAR NO PRIMEIRO TURNO DAS ELEIÇÕES DE 2022?

Tipo: categórica. Uso no TD: Filtro das perguntas de voto no 1º turno → gov_cat.

| Código | Categoria | n |
|---:|---|---:|
| 1 | SIM | 1.031 |
| 2 | NÃO | 230 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 4 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 1 |

## `cen2_votou_1t`

**Questão 10.** (MOSTRAR ANEXO CIRCULAR “CENÁRIO PRESIDENTE PRIMEIRO TURNO”) EM QUEM VOCÊ VOTOU PARA PRESIDENTE NO PRIMEIRO TURNO? (LER TODAS AS OPÇÕES – RU)

Tipo: categórica. Uso no TD: Comparação do voto declarado com o TSE, presidente no 1º turno de 2022 (output/estimativas/voto_declarado_tse.csv).

| Código | Categoria | n |
|---:|---|---:|
| 1 | CIRO GOMES | 23 |
| 2 | CONSTITUINTE EYMAEL | 0 |
| 3 | FELIPE D’AVILA | 2 |
| 4 | JAIR BOLSONARO | 405 |
| 5 | LÉO PÉRICLES | 1 |
| 6 | LULA | 457 |
| 7 | PADRE KELMON | 1 |
| 8 | SIMONE TEBET | 37 |
| 9 | SOFIA MANZANO | 0 |
| 10 | SORAYA THRONICKE | 4 |
| 11 | VERA | 0 |
| 12 | VOTOU BRANCO/NULO | 68 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 33 |
|  | (sem resposta) | 235 |

## `cen_t2`

**Questão 11.** E EM QUEM VOCÊ VOTOU NO SEGUNDO TURNO PARA PRESIDENTE? VOCÊ VOTOU NO JAIR BOLSONARO, NO LULA, VOTOU EM BRANCO/NULO OU NÃO FOI VOTAR? (RU)

Tipo: categórica. Uso no TD: Origem de voto2t_3 (Lula; Bolsonaro; branco, nulo ou não votou) e voto2t_4 (Lula; Bolsonaro; branco ou nulo; não foi votar): cruzamentos e comparação com o TSE.

| Código | Categoria | n |
|---:|---|---:|
| 1 | JAIR BOLSONARO | 404 |
| 2 | LULA | 539 |
| 12 | BRANCO/NULO | 92 |
| 13 | NÃO FOI VOTAR | 200 |
| 88 | NÃO SABE/NÃO LEMBRA (NÃO LER) | 14 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 17 |

## `cen2_votou_1t_govrj`

**Questão 12.** (MOSTRAR ANEXO CIRCULAR “CENÁRIO GOVERNADOR PRIMEIRO TURNO 2022”) E PARA GOVERNADOR? EM QUEM VOCÊ VOTOU PARA GOVERNADOR DO RIO JANEIRO NO PRIMEIRO TURNO? (LER TODAS AS OPÇÕES – RU)

Tipo: categórica. Uso no TD: Origem de gov_cat e gov_2 (Castro × Freixo): cruzamentos e comparação com o TSE.

| Código | Categoria | n |
|---:|---|---:|
| 1 | CLÁUDIO CASTRO | 406 |
| 2 | CYRO GARCIA | 21 |
| 3 | EDUARDO SERRA | 12 |
| 4 | JULIETE PANTOJA | 6 |
| 5 | LUIZ EUGÊNIO | 2 |
| 6 | MARCELO FREIXO | 182 |
| 7 | PAULO GANIME | 11 |
| 8 | RODRIGO NEVES | 39 |
| 9 | WILSON WITZEL | 41 |
| 12 | VOTOU BRANCO/NULO | 192 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 119 |
|  | (sem resposta) | 235 |

## `cen_t2_18`

**Questão 13.** PENSANDO AGORA NAS ELEIÇÕES DE 2018. EM QUEM VOCÊ VOTOU NO SEGUNDO TURNO PARA PRESIDENTE EM 2018: VOCÊ VOTOU NO {0}, NO {1}, VOTOU BRANCO/NULO OU NÃO FOI VOTAR? (RU)

Tipo: categórica. Uso no TD: Comparação do voto declarado com o TSE, presidente no 2º turno de 2018 (output/estimativas/voto_declarado_tse.csv).

| Código | Categoria | n |
|---:|---|---:|
| 1 | JAIR BOLSONARO | 534 |
| 2 | FERNANDO HADDAD | 332 |
| 12 | BRANCO/NULO/NENHUM DESSES (NÃO LER) | 273 |
| 88 | NÃO SABE/NÃO LEMBRA (NÃO LER) | 96 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 31 |

## `adpi1_lula`

**Questão 16.** NA SUA OPINIÃO, O PRESIDENTE LULA ESTÁ FAZENDO UM GOVERNO ÓTIMO, BOM, REGULAR, RUIM OU PÉSSIMO? (RU) (ATENÇÃO ENTREVISTADOR(A): SE A RESPOSTA FOR "REGULAR", PERGUNTAR SE É MAIS PARA POSITIVO OU MAIS PARA NEGATIVO)

Tipo: categórica. Uso no TD: Origem de aval_lula3: avaliação do governo Lula por fonte, por frequência e por região.

| Código | Categoria | n |
|---:|---|---:|
| 1 | ÓTIMO | 155 |
| 2 | BOM | 324 |
| 3 | REGULAR MAIS PARA POSITIVO | 266 |
| 4 | REGULAR MAIS PARA NEGATIVO | 135 |
| 5 | RUIM | 96 |
| 6 | PÉSSIMO | 222 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 54 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 14 |

## `adpi1_gov`

**Questão 17.** COMO VOCÊ AVALIA O GOVERNO DE CLÁUDIO CASTRO? NA SUA OPINIÃO, O GOVERNADOR CLÁUDIO CASTRO ESTÁ FAZENDO UM GOVERNO ÓTIMO, BOM, REGULAR, RUIM OU PÉSSIMO? (RU) (ATENÇÃO ENTREVISTADOR(A): SE A RESPOSTA FOR "REGULAR", PERGUNTAR SE É MAIS PARA POSI

Tipo: categórica. Uso no TD: Origem de aval_gov3: avaliação do governo Castro por frequência e por região, e formação de opinião.

| Código | Categoria | n |
|---:|---|---:|
| 1 | ÓTIMO | 44 |
| 2 | BOM | 251 |
| 3 | REGULAR MAIS PARA POSITIVO | 284 |
| 4 | REGULAR MAIS PARA NEGATIVO | 186 |
| 5 | RUIM | 152 |
| 6 | PÉSSIMO | 210 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 129 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 10 |

## `freq_inf_pol`

**Questão 47.** COM QUE FREQUÊNCIA VOCÊ SE INFORMA SOBRE POLÍTICA? (LER OPÇÕES - RU)

Tipo: categórica. Uso no TD: Gráfico 1 → freq4 nos cruzamentos.

| Código | Categoria | n |
|---:|---|---:|
| 1 | TODOS OS DIAS | 251 |
| 2 | ALGUMAS VEZES POR SEMANA | 163 |
| 3 | UMA VEZ POR SEMANA | 76 |
| 4 | ALGUMAS VEZES POR MÊS | 81 |
| 5 | RARAMENTE | 347 |
| 6 | NUNCA | 338 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 10 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 0 |

## `inf1`

**Questão 48.** DENTRE AS OPÇÕES QUE VOU LER, ME DIGA ATRAVÉS DE QUAL FONTE VOCÊ MAIS SE INFORMA SOBRE POLÍTICA? (LER OPÇÕES - RU)

Tipo: categórica. Uso no TD: Gráfico 2 → fonte4 e fonte5 nos cruzamentos.

| Código | Categoria | n |
|---:|---|---:|
| 1 | NOTICIÁRIO DA TV | 483 |
| 2 | JORNAIS IMPRESSOS E REVISTAS | 31 |
| 3 | RÁDIO | 30 |
| 4 | SITES, BLOGS E PORTAIS DE NOTÍCIAS | 99 |
| 5 | REDES SOCIAIS COMO FACEBOOK, INSTAGRAM OU TWITTER | 255 |
| 6 | WHATSAPP OU TELEGRAM | 38 |
| 7 | CONVERSAS COM AMIGOS, FAMILIARES | 91 |
| 8 | CONVERSAS COM COLEGAS DE TRABALHOS | 37 |
| 9 | BUSCA NO GOOGLE | 45 |
| 66 | NENHUM DESTES | 97 |
| 77 | NÃO ME INFORMO SOBRE POLÍTICA (NÃO LER) | 54 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 6 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 0 |

## `redes_pol`

**Questão 49.** QUAL DESSAS REDES SOCIAIS VOCÊ MAIS UTILIZA PARA SE INFORMAR SOBRE POLÍTICA? (LER OPÇÕES - RU)

Tipo: categórica. Uso no TD: Gráfico 3 → redes_rec e redes_grp.

| Código | Categoria | n |
|---:|---|---:|
| 1 | FACEBOOK | 261 |
| 2 | TWITTER | 83 |
| 3 | YOUTUBE | 117 |
| 4 | WHATSAPP | 134 |
| 5 | INSTAGRAM | 194 |
| 6 | TIKTOK | 35 |
| 77 | NÃO USO REDES SOCIAIS (NÃO LER) | 329 |
| 80 | OUTRA. QUAL? ABERTA | 66 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 39 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 8 |

## `redes_pol_out`

**Questão 49.** QUAL DESSAS REDES SOCIAIS VOCÊ MAIS UTILIZA PARA SE INFORMAR SOBRE POLÍTICA? (LER OPÇÕES - RU) [Other (specify)]

Tipo: texto. Uso no TD: Respostas abertas da opção "outra", reclassificadas em redes_rec (R/recodificacoes.R).

Texto livre; 66 entrevistas com a informação preenchida.

## `ideologia`

**Questão 59.** EM UMA ESCALA DE DIREITA, CENTRO E ESQUERDA, ONDE VOCÊ SE CLASSIFICA? (RU)  (ATENÇÃO ENTREVISTADOR(A): SE RESPONDER DIREITA, PERGUNTAR: DIREITA, OU CENTRO-DIREITA? SE RESPONDER ESQUERDA, PERGUNTAR: ESQUERDA, OU CENTRO-ESQUERDA?)

Tipo: categórica. Uso no TD: Origem de ideologia_grp: perfil de TV × internet e formação de opinião.

| Código | Categoria | n |
|---:|---|---:|
| 1 | ESQUERDA | 256 |
| 2 | CENTRO-ESQUERDA | 36 |
| 3 | CENTRO | 139 |
| 4 | CENTRO-DIREITA | 41 |
| 5 | DIREITA | 344 |
| 6 | NÃO ME IDENTIFICO COM NENHUMA IDEOLOGIA (NÃO LER) | 366 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 79 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 5 |

## `part_pref`

**Questão 60.** QUAL O SEU PARTIDO POLÍTICO DE PREFERÊNCIA? (ESPONTÂNEA - NÃO LER OPÇÕES DE RESPOSTA - RU)

Tipo: categórica. Uso no TD: Origem de part_pref_grp: perfil de TV × internet e formação de opinião.

| Código | Categoria | n |
|---:|---|---:|
| 0 | NENHUM/NÃO TENHO PARTIDO POLÍTICO PREFERIDO | 710 |
| 1 | PT | 265 |
| 2 | PL | 106 |
| 3 | MDB | 6 |
| 4 | PP | 1 |
| 5 | UNIÃO BRASIL | 5 |
| 6 | PSL | 6 |
| 7 | PSD | 4 |
| 8 | PDT | 29 |
| 9 | PSB | 2 |
| 10 | PSOL | 32 |
| 11 | REPUBLICANOS | 0 |
| 80 | OUTRO. QUAL? ABERTA | 29 |
| 88 | NÃO SABE RESPONDER (NÃO LER) | 62 |
| 99 | NÃO QUIS RESPONDER (NÃO LER) | 9 |

