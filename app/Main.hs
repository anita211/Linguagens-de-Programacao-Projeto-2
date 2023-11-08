module Main (main) where

import Lib
import ModeloDados

med1 :: Medicamento
med1 = "Alprazolam"

med2 :: Medicamento
med2 = "Pantoprazol"

estoque :: EstoqueMedicamentos
estoque = [(med1, 30), (med2, 1)]

receituario :: Receituario
receituario = [(med1, [6, 16]), (med2, [8, 18])]

main = do
  
  putStrLn "## TRABALHO 02 -- Linguagens de Programacao 2023.2 UnB ##"
  putStrLn
    ( "Executar o plantao '"
        ++ show plantao
        ++ "' com o estoque '"
        ++ show estoque
        ++ "' ira resultar no estoque final '"
        ++ show (executaPlantao plantao estoque)
        ++ "'"
    )
  where
    plantao = plantaoCorreto (geraPlanoReceituario receituario) estoque

