-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g146 where

import LI11819

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot = undefined


--disparaNaLinha :: Estado -> Estado 
--disparaNaLinha (Estado mapa ((Jogador (x,y) d v cl ch):t) dps) |



existeJogadorLinha :: [Jogador] -> Jogador -> Bool
existeJogadorLinha [] _ = False
existeJogadorLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | x == a && y == b = (existeJogadorLinha t (Jogador (a,b) di vi l h))
                                                                           | x == a = True
                                                                           | otherwise = False 


existeJogadorColuna :: [Jogador] -> Jogador -> Bool
existeJogadorColuna [] _ = False
existeJogadorColuna ((Jogador (x,y) d _ _ _ ):t) (Jogador (a,b) di vi l h) | x == a && y == b = (existeJogadorColuna t (Jogador (a,b) di vi l h))
                                                                           | y == b = True
                                                                           | otherwise = False 

estaVoltadoLinha :: [Jogador] -> Jogador -> Bool
estoVoltadoLinha [] _ = False
estaVoltadoLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | existeJogadorLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && y > b && di == D = True
                                                                         | existeJogadorLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && y < b && di == E = True
                                                                         | otherwise = False

estaVoltadoColuna :: [Jogador] -> Jogador -> Bool
estoVoltadoColuna [] _ = False
estaVoltadoColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | existeJogadorColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && x > a && di == B = True
                                                                          | existeJogadorColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && x < a && di == C = True
                                                                          | otherwise = False

