Для запуска скомпилированного файла выполните:
```
    cabal new-update
    cabal new-run texgen -- examples/nibergall.ttex examples/nibergall.tex
```

Для запуска трансляции из интерпретатора выполните:
```
    cabal new-update
    cabal new-repl
    :l Main
    processFile "examples/nibergall.ttex" "examples/nibergall.tex"
```
