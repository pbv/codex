# Improvements

## 1.

## 2.

## 3. 

## 4. Automatic Translation of Exercise

- Fizemos alterações nos ficheiros Codex/Site.hs e Codex/Utils.hs e criamos o ficheiro Translate.hs.

- No ficheiro Codex/Site.hs:
Alteramos a função *handleGet*, de modo a:
Adicionar suporte a tradução, antes de carregar o ficheiro Markdown com o exercício é verificado se há um parâmetro de idioma na URL (?lang=pt, por exemplo). Se presente, o código tenta traduzir o conteúdo do ficheiro chamando *translateMarkdown* do Translate.hs. Caso contrário, o código somente lê o ficheiro original.

- No ficheiro Codex/Utills.hs:
Adicionamos as funções *addLanguage* e *mapLangCode* que adicionam o código da lingua no ficheiro traduzido a ser salvo.

- No ficheiro Codex/Translate.hs:
Fazemos a chamada ao DeepL, implementamos a função *translateMarkdown* que traduz o conteúdo do exercício desejado, separando com base nos delimitadores definidos as partes do ficheiro que não devem ser traduzidas, trocadas por {{PLACEHOLDERX}} (X um numero incremental) durante a tradução.

- Para testar:
-- stack build (adicionamos uma nova dependencia no stack.yaml, http-conduit)
-- stack repl
-- main
-- http://127.0.0.1:8000/page/hs_passwd.md (exemplo de um exercicio original)
-- http://127.0.0.1:8000/page/hs_passwd.md?lang=pt (acrescentar "?lang=pt" para fazer a traducao do exercicio)
