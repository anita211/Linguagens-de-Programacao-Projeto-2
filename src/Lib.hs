module Lib where

import ModeloDados

{-
 *** Aluno: Anita Garcia Lagos Oliveira
 *** Matricula: 211068243

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade estoque =
    let medicamentoExiste (m, q) = m == medicamento
        moveUltimoParaInicio [] = []
        moveUltimoParaInicio lista = (last lista):(init lista)
        atualizarEstoque [] = [(medicamento, quantidade)]
        atualizarEstoque (e:es)
            | medicamentoExiste e = (medicamento, quantidade + snd e) : es
            | otherwise = e : atualizarEstoque es
        concerta estoqueOld estoqueNew
            | length estoqueOld == length estoqueNew = estoqueNew
            | otherwise = moveUltimoParaInicio estoqueNew
    in concerta estoque (atualizarEstoque estoque)

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento medicamento estoque =
        let atualiza = comprarMedicamento medicamento (-1) estoque
            concerta estoqueOld estoqueNew
                | length estoqueOld == length estoqueNew = Just estoqueNew
                | otherwise = Nothing
        in concerta estoque atualiza

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento medicamento (e:es)
        | fst e == medicamento = snd e
        | otherwise = consultarMedicamento medicamento es

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

quicksort :: Ord(t) => [t] -> [t]
quicksort [] = []
quicksort (x:xs) =
    let menores = quicksort [y | y <- xs, y <= x]
        maiores = quicksort [y | y <- xs, y > x]
    in menores ++ [x] ++ maiores

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos receita = 
        let atualiza prescricao estoque = comprarMedicamento (fst prescricao) (length (snd prescricao)) estoque
            demanda [] estoque = estoque
            demanda (p:ps) estoque = demanda ps (atualiza p estoque)
        in quicksort (demanda receita [])

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

saoDistintos :: (Eq a) => [a] -> Bool
saoDistintos [] = True
saoDistintos (x:xs)
    | elem x xs = False
    | otherwise = saoDistintos xs

receituarioValido :: Receituario -> Bool
receituarioValido receita = (checaMedicamentos (medicamentos receita)) && (checaHorarios (horaMedicamentos receita))
        where medicamentos [] = []
              medicamentos (p:ps) = (fst p):(medicamentos ps)
              horaMedicamentos receita = [snd p | p <- receita]
              checaMedicamentos medicamentosLista = ( saoDistintos medicamentosLista ) && ( (quicksort medicamentosLista) == medicamentosLista)
              checaHorarios horariosLista = ( and (map (saoDistintos) horariosLista) ) && ( (map (quicksort) horariosLista) == horariosLista )

planoValido :: PlanoMedicamento -> Bool
planoValido plano =
        let horarios [] = []
            horarios (h:hs) = (fst h):(horarios hs)
            medicamentosHora plano = [snd t | t <- plano]
            checaHorarios horariosLista = ( saoDistintos horariosLista ) && ( (quicksort horariosLista) == horariosLista)
            checaMedicamentos medicamentosLista = ( and (map (saoDistintos) medicamentosLista) ) && ( (map (quicksort) medicamentosLista) == medicamentosLista )
        in (checaHorarios (horarios plano)) && (checaMedicamentos (medicamentosHora plano))

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

plantaoValido :: Plantao -> Bool
plantaoValido plantao = 
    let horarios [] = []
        horarios (h:hs) = (fst h):(horarios hs)
        checaHorarios horariosLista = ( saoDistintos horariosLista ) && ( (quicksort horariosLista) == horariosLista)
        cuidadosPorHorario plantao = [snd t | t <- plantao]
        listaMedComprar cuidado = [medicamento | (Comprar medicamento _) <-cuidado]
        listaMedMedicar cuidado = [medicamento | (Medicar medicamento) <- cuidado]
        checaCuidados [] = True
        checaCuidados (c:cs) = saoDistintos ( (listaMedComprar c) ++ (listaMedMedicar c) ) && (quicksort (listaMedMedicar c) == listaMedMedicar c) && checaCuidados cs
    in (checaHorarios (horarios plantao)) && (checaCuidados (cuidadosPorHorario plantao))
{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario lista = gera lista "receita"

-- Função geral
gera :: (Eq g) => (Ord g) => (Eq t) => (Ord t) => [(t,[g])] -> String -> [(g,[t])]
gera lista flag
        | flag == "receita" = eliminarDuplasRepetidas (quicksort (agruparPorPrimeiro (expandeR (firsts lista) (seconds lista))))
        | flag == "plano" = eliminarDuplasRepetidas (quicksort (agruparPorPrimeiro (expandeP (firsts lista) (seconds lista))))
        where juntaEmDupla x yLista = [(y, x) | y <- yLista  ]
              expandeR [] [] = []
              expandeR (x:xs) (y:ys) = (juntaEmDupla x y) ++ (expandeR xs ys)
              expandeP [] [] = []
              expandeP (x:xs) (y:ys) = (juntaEmDupla x y) ++ (expandeP xs ys)

-- Funções que agrupam os primeiros elementos da dupla e depois os segundos
firsts :: [(a,[b])] -> [a]
firsts listaDeDuplas = map (fst) listaDeDuplas
seconds :: [(a,[b])] -> [[b]]
seconds listaDeDuplas = map (snd) listaDeDuplas

-- Função para agrupar duplas por primeiro elemento e criar uma lista de segundos elementos
agruparPorPrimeiro :: (Eq a) => [(a, b)] -> [(a, [b])]
agruparPorPrimeiro [] = []
agruparPorPrimeiro ((x, y):resto) = (x, elementos) : agruparPorPrimeiro naoAgrupados
  where
    (elementos, naoAgrupados) = extrairPorPrimeiro x ((x, y) : resto)

-- Função para extrair elementos com o mesmo primeiro elemento
extrairPorPrimeiro :: (Eq a) => a -> [(a, b)] -> ([b], [(a, b)])
extrairPorPrimeiro _ [] = ([], [])
extrairPorPrimeiro x lista@((a, b):resto) --lista@ mantem a referencia original da lista
    | x == a = (b : elementos, resto')
    | otherwise = (elementos, lista)
  where
    (elementos, resto') = extrairPorPrimeiro x resto

-- Função para eliminar duplas com primeiros elementos repetidos
eliminarDuplasRepetidas :: (Eq a) => [(a, b)] -> [(a, b)]
eliminarDuplasRepetidas [] = []
eliminarDuplasRepetidas (x:xs) = x : eliminarDuplasRepetidas (removerRepetidos (fst x) xs)
  where
    removerRepetidos _ [] = []
    removerRepetidos p ((y, z):ys)
        | p == y = removerRepetidos p ys
        | otherwise = (y, z) : removerRepetidos p ys

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano = gera plano "plano"

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

tomarMedicamento' :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
tomarMedicamento' medicamento estoque = comprarMedicamento medicamento (-1) estoque

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao plantao estoque =
        let cuidados [] = []
            cuidados ( (_,cs) : ps ) = [c | c <- cs] ++ cuidados ps
            executaCuidado [] v = v
            executaCuidado (c:cs) v = case c of
                Comprar m q -> executaCuidado cs (comprarMedicamento m q v)
                Medicar m -> executaCuidado cs (tomarMedicamento' m v)
            concerta estoqueNew
                | and (map (>0) (map (snd) estoqueNew)) = Just estoqueNew
                | otherwise = Nothing
        in concerta (executaCuidado (cuidados plantao) estoque)

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.

Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

eJust :: Maybe a -> Bool
eJust valor = case valor of
    Nothing -> False
    otherwise -> True

ministrados:: Plantao -> EstoqueMedicamentos -> EstoqueMedicamentos
ministrados plantao estoque = 
        let cuidados [] = []
            cuidados ( (_,cs) : ps ) = [c | c <- cs] ++ cuidados ps
            executaCuidado [] v = v
            executaCuidado (c:cs) v = case c of
                Comprar m q -> executaCuidado cs (comprarMedicamento m q v)
                Medicar m -> executaCuidado cs (tomarMedicamento' m v)
        in executaCuidado (cuidados plantao) estoque

analisaEstoque :: EstoqueMedicamentos -> EstoqueMedicamentos -> Bool
analisaEstoque estoqueTotal estoqueNecessario = and (zipWith (>=) (map (snd) estoqueTotal) (map (snd) estoqueNecessario))
    
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz plantao plano estoque = eJust (executaPlantao plantao estoque) && ( analisaEstoque (ministrados plantao estoque) (demandaMedicamentos (geraReceituarioPlano plano)) )
                
{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto plano estoque = 
    let
        medicamentosNoPlano = concatMap snd plano -- Lista de todos os medicamentos no plano
        medicamentosNoEstoque = map fst estoque -- Lista de medicamentos no estoque

        -- Função para criar um cuidado de compra de medicamento
        criarCuidadoCompra :: Medicamento -> Quantidade -> Cuidado
        criarCuidadoCompra m q = Comprar m q

        -- Função para criar um cuidado de medicar medicamento
        criarCuidadoMedicar :: Medicamento -> Cuidado
        criarCuidadoMedicar m = Medicar m

        -- Função para gerar os cuidados necessários para um medicamento
        gerarCuidados :: Medicamento -> [Cuidado]
        gerarCuidados m
            | m `elem` medicamentosNoPlano =
                let quantNoPlano = length $ filter (== m) medicamentosNoPlano
                    quantNoEstoque = case lookup m estoque of
                        Just q -> q
                        Nothing -> 0
                    qtdComprar = max 0 (quantNoPlano - quantNoEstoque)
                in [criarCuidadoCompra m qtdComprar]
            | m `elem` medicamentosNoEstoque = [criarCuidadoMedicar m]

        -- Função para gerar o plantão
        gerarPlantao :: [Medicamento] -> Horario -> Plantao
        gerarPlantao [] _ = []
        gerarPlantao (m:ms) h = (h, gerarCuidados m) : gerarPlantao ms (h+1) 

    in gerarPlantao (medicamentosNoPlano) (1)
