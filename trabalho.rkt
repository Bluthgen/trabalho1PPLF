#lang racket/gui

(require csv-reading)
(require srfi/19)
(require memoize)
(require plot)
(require rackunit)
(require rackunit/text-ui)

(define (csvfile->list filename)
  (call-with-input-file filename csv->list))

;Definição dos containers dos dados
(struct dadosEmpresa (data close) #:transparent)

(define dataP(csvfile->list "acoesP.csv"))
(define dataG(csvfile->list "acoesG.csv"))
(define dataM(csvfile->list "acoesM.csv"))

;Função que vai separar os campos dos arquivos originais em campos das structs
(define (separaDados dados)
  (cond
    [(empty? dados) empty]
    [else
     (cons (string-split (first (first dados)) ";" #:repeat? #t) (separaDados (rest dados)))]))

(define (quebraEmStruct dados)
  (cond
    [(empty? dados) empty]
    [else
     (cons (dadosEmpresa (first (first dados)) (second (first dados))) (quebraEmStruct (rest dados)))]))

;Definição das listas de dados que serão utilizadas pelo resto do programa
(define dadosAcoesPetrobras(quebraEmStruct (separaDados dataP)))
(define dadosAcoesGoogle(quebraEmStruct (separaDados dataG)))
(define dadosAcoesMicrosoft(quebraEmStruct (separaDados dataM)))

;Função para transformar strings em Datas
(define (converteData elem)
  (cond
    [(number? elem) elem]
    [else(string->date (dadosEmpresa-data elem) "~d/~m/~Y")]))

;Função para realizar uma função sobre uma string que representa uma data
(define (operaEmData f elem)
  (cond
    [(number? elem) elem]
    [else (f (string->date (dadosEmpresa-data elem) "~d/~m/~Y"))]))

;Funções para a busca de datas
(define (findProxData data lista)
  (cond
    [(empty? list) "Fora do Escopo"]
    [(or (> (date-year data) 2018) (< (date-year data) 2018)) "Fora do Escopo"]
    [(< (date-month(converteData (first lista))) (date-month data)) (findProxData data (rest lista))]
    [(= (date-month(converteData (first lista))) (date-month data))
     (cond
       [(> (date-day(converteData (first lista))) (date-day data)) (dadosEmpresa-data(first lista))]
       [else (findProxData data (rest lista))])]
    [(> (date-month(converteData (first lista))) (date-month data)) (dadosEmpresa-data(first lista))]))

(define (proximaDataValida data)
  (cond
    [(string? data)
     (findProxData (string->date data "~d/~m/~Y") dadosAcoesPetrobras)] ;Utilizando os dados da Petrobras aqui, qualquer uma funcionaria da mesma maneira
    [else empty]))

(define (findUltimData data lista anterior)
  (cond
    [(empty? list) "Fora do Escopo"]
    [(or (> (date-year data) 2018) (< (date-year data) 2018) (> (date-month data) 5)) "Fora do Escopo"]
    [(< (date-month(converteData (first lista))) (date-month data)) (findUltimData data (rest lista) (dadosEmpresa-data(first lista)))]
    [(= (date-month(converteData (first lista))) (date-month data))
     (cond
       [(< (date-day(converteData (first lista))) (date-day data)) (findUltimData data (rest lista) (dadosEmpresa-data(first lista)))]
       [else anterior])]
    [(> (date-month(converteData (first lista))) (date-month data)) anterior]))

(define (anteriorDataValida data)
  (cond
    [(string? data)
     (findUltimData (string->date data "~d/~m/~Y") dadosAcoesPetrobras empty)]
    [else empty]))

;Funções de ordenação, normal e em ordem reversa
(define (ordenaPorParametro lista param)
  (sort lista
        string<?
        #:key param))

(define (ordenaPorParametroR lista param)
  (sort lista
        string>?
        #:key param))

;Estou utilizando o bubble sort para ordenar as datas, porque é um método estável
(define (bubble-sort <? v f)
  (define len (vector-length v))
  (define ref vector-ref)
  (let loop ([max len] 
             [again? #f])
    (for ([i (in-range 0 (- max 1))]
          [j (in-range 1 max)])
      (define vi (ref v i))
      (define di (operaEmData f vi))
      (when (<? (operaEmData f (ref v j)) di)
        (vector-set! v i (ref v j))
        (vector-set! v j vi)
        (set! again? #t)))
    (when again? (loop (- max 1) #f)))
  v)

;Ordenando por dia, depois por mês e por fim, por ano
(define (ordenaPorData lista)
  (vector->list (bubble-sort < (bubble-sort < (bubble-sort < (list->vector lista) date-day) date-month) date-year)))

(define (ordenaPorValor lista)
  (ordenaPorParametro lista dadosEmpresa-close))

(define (ordenaPorDataR lista)
  (vector->list (bubble-sort > (bubble-sort > (bubble-sort > (list->vector lista) date-day) date-month) date-year)))

(define (ordenaPorValorR lista)
  (ordenaPorParametroR lista dadosEmpresa-close))


;Funções de cálculos

;Soma todos os valores de close de uma empresa
(define (calculaSoma lista)
  (cond
    [(empty? lista) 0]
    [else (+ (string->number(dadosEmpresa-close(first lista))) (calculaSoma (rest lista)))]))

(define (calculaMedia lista)
  (cond
    [(empty? lista) 0]
    [else (/ (calculaSoma lista) (length lista))]))

;Definição dos valores das médias, para serem utilizados em cálculos
(define mediaP(calculaMedia dadosAcoesPetrobras))
(define mediaG(calculaMedia dadosAcoesGoogle))
(define mediaM(calculaMedia dadosAcoesMicrosoft))

;Funções para o cálculo da Correlação entre duas empresas
(define (calculaSomaDaCovar lista1 lista2)
  (cond
    [(or (empty? lista1) (empty? lista2)) 0]
    [else (+ (* (- (string->number(dadosEmpresa-close(first lista1))) (calculaMedia lista1))
             (- (string->number(dadosEmpresa-close(first lista2))) (calculaMedia lista2)))
             (calculaSomaDaCovar (rest lista1) (rest lista2)))]))

(define (calculaCovariancia lista1 lista2)
  (cond
    [(or (empty? lista1) (empty? lista2)) 0]
    [else (/ (calculaSomaDaCovar lista1 lista2) (- (length lista1) 1))]))

(define (calculaSomaDaVariancia lista)
  (cond
    [(empty? lista) 0]
    [else (+ (sqr (- (string->number(dadosEmpresa-close(first lista))) (calculaMedia lista))) (calculaSomaDaVariancia (rest lista)))]))

(define (calculaVariancia lista)
  (cond
    [(empty? lista) 0]
    [else (/ (calculaSomaDaVariancia lista) (- (length lista) 1))]))

(define (calculaDesvioPadrao lista)
  (sqrt (calculaVariancia lista)))

;Cálculo da Correlação
(define (calculaCoeficiente lista1 lista2)
  (cond
    [(equal? lista1 lista2) 1]
    [else (/ (calculaCovariancia lista1 lista2) (* (calculaDesvioPadrao lista1) (calculaDesvioPadrao lista2)))]))

;Cálculos para o cálculo da Média Móvel Simples
(define (somaNElementos lista n)
  (cond
    [(empty? lista) 0]
    [(> n 0) (+ (string->number(dadosEmpresa-close(first lista))) (somaNElementos (rest lista) (- n 1)))]
    [else (string->number(dadosEmpresa-close(first lista)))]))

(define (MM lista n)
  (cond
    [(empty? lista) 0]
    [(< n 1) (list 0)]
    [(> n 104) (list +inf.f)]
    [(= (length lista) n) (list (/ (somaNElementos lista n) n))]
    [else
     (cons (/ (somaNElementos lista n) n) (MM (rest lista) n))]))

;Função para retornar o último elemento de uma lista, para cálculos futuros
(define (last list)
        (cond
            [(empty? list) 0]
            [(number? list) list]
            [(empty? (rest list)) (first list)]
            [else (last (rest list))]))

;Cálculo do coeficiente da Média Móvel Exponencial
(define (alfaMME n)
  (/ 2 (+ n 1)))

;Cálculos do coeficiente da Média Móvel Modificada / Smoothed Moving Median
(define (alfaMMS n)
  (/ 1 n))

;Cálculo do MME para listas de structs
(define/memo (calculaMME lista n listaOriginal)
  (cond
    [(empty? lista) 0]
    [(> n 104) (list +inf.f)]
    [(< n 1) (list 0)]
    [(dadosEmpresa? lista) (list (+(*(-(string->number(dadosEmpresa-close lista))(last (MM listaOriginal n))) (alfaMME n))(last (MM listaOriginal n))))]
    [(empty? (rest lista)) (list (+(*(-(string->number(dadosEmpresa-close(first lista)))(last(MM listaOriginal n))) (alfaMME n))(last(MM listaOriginal n))))]
    [else (cons (+(*(-(string->number(dadosEmpresa-close(first lista))) (first (calculaMME (rest lista) n listaOriginal)))(alfaMME n))(first (calculaMME (rest lista) n listaOriginal)))
                (calculaMME (rest lista) n listaOriginal))]))

;Cálculo do MME para listas de números, para cálculos do MACD-sinal
(define/memo (calculaMME2 lista n listaOriginal)
  (cond
    [(empty? lista) 0]
    [(> n 104) (list +inf.f)]
    [(< n 1) (list 0)]
    [(number? lista) (list (+(*(-(first lista)(last (MM2 listaOriginal n)))(alfaMME n))(last (MM2 listaOriginal n))))]
    [(empty? (rest lista)) (list (+(*(-(first lista)(last (MM2 listaOriginal n)))(alfaMME n))(last (MM2 listaOriginal n))))]
    [else (cons (+(*(-(first lista)(first (calculaMME2 (rest lista) n listaOriginal)))(alfaMME n))(first (calculaMME2 (rest lista) n listaOriginal)))
                (calculaMME2 (rest lista) n listaOriginal))]))

;Cálculo da Média Móvel Modificada / Smoothed Moving Median, para o cálculo do RSI
(define/memo (calculaMMS lista n listaOriginal)
  (cond
    [(empty? lista) 0]
    [(> n 104) (list +inf.f)]
    [(< n 1) (list 0)]
    [(number? lista) (list (+(*(-(first lista)(last(MM2 listaOriginal n)))(alfaMMS n))(last (MM2 listaOriginal n))))]
    [(empty? (rest lista)) (list (+(*(-(first lista)(last (MM2 listaOriginal n)))(alfaMMS n))(last(MM2 listaOriginal n))))]
    [else (cons (+(*(-(first lista)(first (calculaMMS (rest lista) n listaOriginal)))(alfaMMS n))(first (calculaMMS (rest lista) n listaOriginal)))
                (calculaMMS (rest lista) n listaOriginal))]))


(define (MME lista n)
  (reverse (calculaMME (reverse lista) n lista)))

(define (MME2 lista n)
  (reverse(calculaMME2 (reverse lista) n lista)))

(define (MMS lista n)
  (reverse (calculaMMS (reverse lista) n lista)))

;Aplica uma média e retorna o valor na posição dia correspondente ao dia
(define (getM m dia lista n)
  (list-ref (m lista n) (- dia 1)))

;Funções para criar as listas de Ups e Downs do cálculo do RSI
(define (criaLista lista comp f s)
  (cond
    [(or (empty? lista) (empty? (rest lista))) empty]
    [else (cond
            [(comp (string->number(dadosEmpresa-close(first lista))) (string->number(dadosEmpresa-close(second lista)))) (cons 0 (criaLista (rest lista) comp f s))]
            [else (cons (- (string->number(dadosEmpresa-close(f lista))) (string->number(dadosEmpresa-close(s lista)))) (criaLista (rest lista) comp f s))])]))

(define (listaUps lista)
  (criaLista lista > second first))

(define (listaDowns lista)
  (criaLista lista < first second))

;Versão da Média Móvel para listas de números
(define (somaNElementos2 lista n)
  (cond
    [(empty? lista) 0]
    [(> n 0) (+ (first lista) (somaNElementos2 (rest lista) (- n 1)))]
    [else (first lista)]))

(define (MM2 lista n)
  (cond
    [(empty? lista) 0]
    [(> n (length lista))]
    [(= (length lista) n) (list (/ (somaNElementos2 lista n) n))]
    [else
     (cons (/ (somaNElementos2 lista n) n) (MM2 (rest lista) n))]))

;Cálculos do RSI
(define/memo (calculaRS lista n)
  (cond
    [(= 0 (first (MMS (listaDowns lista) n))) +inf.0]
    [else (/ (first (MMS (listaUps lista) n)) (first (MMS (listaDowns lista) n)))]))

(define/memo (calculaRSI lista n)
  (- 100 (/ 100 (+ 1 (calculaRS lista n)))))

(define/memo (listaRSI lista n)
  (cond
    [(< n 2) (calculaRSI lista n)]
    [else (cons  (listaRSI lista (- n 1)) (calculaRSI lista n))]))

(define (RSI lista n)
  (flatten (listaRSI lista n)))

;Subtração de duas listas
(define (listaSubtraida lista1 lista2)
  (cond
    [(or (empty? lista1) (empty? lista2)) empty]
    [(or (number? lista1) (number? lista2)) (list (- lista1 lista2))]
    [else (cons (- (first lista1) (first lista2)) (listaSubtraida (rest lista1) (rest lista2)))]))

;Função que remove os últimos n elementos de uma lista
(define (removeNUltimos n lista)
  (cond
    [(< n 0) lista]
    [(or (> n (length lista)) (= n (length lista))) empty]
    [else (cons (first lista) (removeNUltimos n (rest lista)))]))

;Funções para definir qual operação foi selecionada pelo usuário
(define (calculaOper n)
  (cond
    [(equal? (oper (qualOper escolhaOper)) calculaRSI)((oper (qualOper escolhaOper)) (removeNUltimos n (qualEmpresa escolha1)) escolhaPeriodo)]
    [else (last ((oper (qualOper escolhaOper)) (removeNUltimos n (qualEmpresa escolha1)) escolhaPeriodo))]))

(define (graficoOper n)
  (calculaOper (- 95 n)))

;Cálculos do MACD, neste trabalho só será utilizado o MACD-linha
(define (MACD-linha lista periodo)
  (listaSubtraida (MME lista 12) (MME lista 26)))

(define (MACD-sinal lista)
  (MME2 (MACD-linha lista 0) 9))

(define (MACD-histograma lista)
  (listaSubtraida (MACD-linha lista 0) (MACD-sinal lista)))

;Função para desenhar o gráfico representando os valores das ações de uma empresa
(define (graficoListagem n)
  (cond
    [(< n 1) (string->number(dadosEmpresa-close(first (qualEmpresa escolha1))))]
    [else (string->number(dadosEmpresa-close(list-ref (qualEmpresa escolha1) (floor n))))]))

(define (encontraValorPorData empresa dia)
  (cond
    [(empty? empresa) +inf.0]
    [(equal? (dadosEmpresa-data (first empresa)) dia) (string->number (dadosEmpresa-close (first empresa)))]
    [else (encontraValorPorData (rest empresa) dia)]))

;Funções para determinar as escolhas selecionadas pelo usuário
(define (qualEmpresa nome)
  (cond
    [(equal? nome "Google") dadosAcoesGoogle]
    [(equal? nome "Microsoft") dadosAcoesMicrosoft]
    [(equal? nome "Petrobras") dadosAcoesPetrobras]
    [else 0]))

(define (qualNome num)
  (cond
    [(= num 0) "Google"]
    [(= num 1) "Microsoft"]
    [(= num 2) "Petrobras"]
    [else 0]))

(define (qualOper num)
  (cond
    [(= num 0) "Média Móvel"]
    [(= num 1) "MME"]
    [(= num 3) "MACD"]
    [(= num 2) "RSI"]))

(define (oper nome)
  (cond
    [(equal? nome "Média Móvel") MM]
    [(equal? nome "MME") MME]
    [(equal? nome "MACD") MACD-linha]
    [(equal? nome "RSI") calculaRSI]))

(define (setDataDia dia)
  (cond
    [(equal? escolhaDataMes "02") (cond
                               [(> 28 (string->number dia)) (set! escolhaDataDia dia)(send msgDatas set-label "Data inválida")]
                               [else (set! escolhaDataDia dia) (set! escolhaData (string-append (string->number escolhaDataDia) "/" (string->number escolhaDataMes) "/" (string->number escolhaDataAno)))])]
    [(or (equal? escolhaDataMes "04") (equal? escolhaDataMes "06") (equal? escolhaDataMes "09") (equal? escolhaDataMes "11")) (cond
                               [(> 31 (string->number dia)) (set! escolhaDataDia dia)(send msgDatas set-label "Data inválida")]
                               [else (set! escolhaDataDia dia) (set! escolhaData (string-append (string->number escolhaDataDia) "/" (string->number escolhaDataMes) "/" (string->number escolhaDataAno)))])]
    [else (set! escolhaDataDia dia) (set! escolhaData (string-append escolhaDataDia "/" escolhaDataMes "/" escolhaDataAno))]))

(define escolha1 "Google")
(define escolha2 "Google")
(define escolhaOper 0)
(define escolhaPeriodo 14)
(define escolhaData "02/01/2018")
(define escolhaDataDia "02")
(define escolhaDataMes "01")
(define escolhaDataAno "2018")

;Definições das partes da interface gráfica

;Definições de telas
(define frame (new frame% [label "Trabalho1 - PPLF"]))

(define frameListagem (new frame% [label "Listar ações"][width 800][height 600]))

(define frameGraficos (new frame% [label "Gráfico"][width 800][height 600]))

(define frameDatas (new frame% [label "Calcular data válida"]))

(define frameCalculos (new frame% [label "Cálculos sobre as ações"]))

(define frameCorrel (new frame% [label "Correlação das ações de duas empresas"]
                         [width 500]))

;Definições de telas de gráficos
(define canvasG (new canvas% [parent frameGraficos]
                     [label "Gráfico"][min-width 500][min-height 500]))

(define canvasL (new canvas% [parent frameListagem]
                     [label "Ações"][min-width 500][min-height 500]))

; Definições de menssagens
(define msg (new message% [parent frame]
                          [label "Bem vindo a aplicação"]))

(define msgDatas (new message% [parent frameDatas]
                          [label (string-append "Próxima Data Válida: " (proximaDataValida escolhaData))]))

(define msgCorrel (new message% [parent frameCorrel]
                       [min-width 500]
                       [label (string-append "Correlação entre a " escolha1 " e a " escolha2 ": " (number->string(calculaCoeficiente (qualEmpresa escolha1) (qualEmpresa escolha2))))]))
 
(define msgCalculos (new message% [parent frameCalculos]
                       [min-width 500]
                       [label (string-append "Resultado da operação " (qualOper escolhaOper) " para a " escolha1 ": " (number->string(last ((oper (qualOper escolhaOper)) (qualEmpresa escolha1) escolhaPeriodo))))]))


; Definições de botões
(define listar (new button% [parent frame]
             [label "Ver a lista de ações de uma empresa"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send frameListagem show #t) (plot/dc (function graficoListagem 0 103) (send canvasL get-dc) 0 0 500 500))]) )

(define verDatas (new button% [parent frame]
             [label "Calcular datas válidas"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send frameDatas show #t))]))

(define prox (new button% [parent frameDatas]
             [label "Calcular a próxima data válida"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msgDatas set-label (string-append "Próxima Data Válida: " (proximaDataValida escolhaData))))]))

(define anterior (new button% [parent frameDatas]
             [label "Calcular a última data válida"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msgDatas set-label (string-append "Última Data Válida: " (anteriorDataValida escolhaData))))]))

(define verGraficos (new button% [parent frame]
             [label "Visualizar gráficos sobre as ações de uma empresa"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send frameGraficos show #t)(plot/dc (function graficoOper 4 95) (send canvasG get-dc) 0 0 500 500))]))

(define verCalculos (new button% [parent frame]
             [label "Realizar cálculos sobre as ações de uma empresa"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send frameCalculos show #t))]))

(define correl (new button% [parent frame]
             [label "Ver a correlação das movimentações de ações de duas empresas"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send frameCorrel show #t))]))

;Definições de áreas de input do usuário
(define escolheEmpresaG (new choice% [parent frameGraficos]
             [label "Qual das empresas?"]
             [choices (list "Google" "Microsoft" "Petrobras")]
             [callback (λ (c e) (set! escolha1 (qualNome(send c get-selection))) (plot/dc (function graficoOper 4 95) (send canvasG get-dc) 0 0 500 500))]))

(define escolheEmpresaL (new choice% [parent frameListagem]
             [label "Qual das empresas?"]
             [choices (list "Google" "Microsoft" "Petrobras")]
             [callback (λ (c e) (set! escolha1 (qualNome(send c get-selection))) (plot/dc (function graficoListagem 0 103) (send canvasL get-dc) 0 0 500 500))]))

(define escolheEmpresaC (new choice% [parent frameCalculos]
             [label "Qual das empresas?"]
             [choices (list "Google" "Microsoft" "Petrobras")]
             [callback (λ (c e) (set! escolha1 (qualNome(send c get-selection))) (send msgCalculos set-label (string-append "Resultado da operação " (qualOper escolhaOper) " para a " escolha1 ": " (number->string(last ((oper (qualOper escolhaOper)) (qualEmpresa escolha1) escolhaPeriodo))))))]))

(define escolheEmpresa1 (new choice% [parent frameCorrel]
             [label "Qual é a primeira empresa?"]
             [choices (list "Google" "Microsoft" "Petrobras")]
             [callback (λ (c e) (set! escolha1 (qualNome(send c get-selection))) (send msgCorrel set-label (string-append "Correlação entre a " escolha1 " e a " escolha2 ": " (number->string(calculaCoeficiente (qualEmpresa escolha1) (qualEmpresa escolha2))) ".")))]))

(define escolheEmpresa2 (new choice% [parent frameCorrel]
             [label "Qual é a segunda empresa?"]
             [choices (list "Google" "Microsoft" "Petrobras")]
             [callback (λ (c e) (set! escolha2 (qualNome(send c get-selection))) (send msgCorrel set-label (string-append "Correlação entre a " escolha1 " e a " escolha2 ": " (number->string(calculaCoeficiente (qualEmpresa escolha1) (qualEmpresa escolha2))) ".")))]))

(define escolheOperacaoG (new choice% [parent frameGraficos]
             [label "Qual das operações?"]
             [choices (list "Média Móvel Simples" "Média Móvel Exponencial" "RSI" "MACD")]
             [callback (λ (c e) (set! escolhaOper (send c get-selection)) (plot/dc (function graficoOper 4 95) (send canvasG get-dc) 0 0 500 500))]))

(define escolheOperacaoC (new choice% [parent frameCalculos]
             [label "Qual das operações?"]
             [choices (list "Média Móvel Simples" "Média Móvel Exponencial" "RSI" "MACD")]
             [callback (λ (c e) (set! escolhaOper (send c get-selection)) (send msgCalculos set-label (string-append "Resultado da operação " (qualOper escolhaOper) " para a " escolha1 ": " (number->string(last ((oper (qualOper escolhaOper)) (qualEmpresa escolha1) escolhaPeriodo))))))]))

(define escolhePeriodoG (new text-field% [parent frameGraficos]
                 [label "Para qual período?"]
                 [init-value "14"]
                 [callback (λ (c e) (set! escolhaPeriodo (string->number(send c get-value))) (plot/dc (function graficoOper 4 95) (send canvasG get-dc) 0 0 500 500))]))

(define escolhePeriodoC (new text-field% [parent frameCalculos]
                 [label "Para qual período?"]
                 [init-value "14"]
                 [callback (λ (c e) (set! escolhaPeriodo (string->number(send c get-value))) (send msgCalculos set-label (string-append "Resultado da operação " (qualOper escolhaOper) " para a " escolha1 ": " (number->string(last ((oper (qualOper escolhaOper)) (qualEmpresa escolha1) escolhaPeriodo))))))]))

(define escolheDataDia (new text-field% [parent frameDatas]
                 [label "Para qual dia?"]
                 [init-value "02"]
                 [callback (λ (c e) (cond
                                      [(number? (string->number (send c get-value)))
                                       (cond
                                         [(and (> (string->number (send c get-value)) 0) (< (string->number (send c get-value)) 32))(setDataDia (send c get-value))])]))]))


(define escolheDataMes (new text-field% [parent frameDatas]
                 [label "Para qual mês?"]
                 [init-value "01"]
                 [callback (λ (c e) (cond
                                      [(number? (string->number (send c get-value)))
                                       (cond
                                         [(and (< (string->number (send c get-value)) 13) (> (string->number (send c get-value)) 0))(set! escolhaDataMes (send c get-value)) (set! escolhaData (string-append escolhaDataDia "/" escolhaDataMes "/" escolhaDataAno))])]))]))

;Testes unitários
(define MM-tests
  (test-suite
   "MM tests"
   (check-equal? (MM2 empty 10) 0)
   (check-equal? (MM2 (list 1 2 3 4 5 6 7 8 9 10) 1) (list 3 5 7 9 11 13 15 17 19 10))
   (check-equal? (> (last (MM dadosAcoesPetrobras 10)) 14.04) true)
   (check-equal? (< (last (MM dadosAcoesPetrobras 10)) 14.05) true)
   (check-equal? (> (last (MM dadosAcoesGoogle 14)) 1078.67) true)
   (check-equal? (< (last (MM dadosAcoesGoogle 14)) 1078.68) true)
   (check-equal? (> (last (MM dadosAcoesMicrosoft 20)) 97.25) true)
   (check-equal? (< (last (MM dadosAcoesMicrosoft 20)) 97.26) true)
   (check-equal? (> (last (MME dadosAcoesPetrobras 10)) 13.45) true)
   (check-equal? (< (last (MME dadosAcoesPetrobras 10)) 13.46) true)
   (check-equal? (> (last (RSI dadosAcoesPetrobras 14)) 18.96) true)
   (check-equal? (< (last (RSI dadosAcoesPetrobras 14)) 18.97) true)))

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

(executa-testes MM-tests)

;Abre a interface
(send frame show #t)