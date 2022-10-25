module Ficha1 where


data Hora = H Int Int deriving (Show, Eq)

horaValida' :: Hora -> Bool
horaValida' (H h m) = elem h [0..23] && elem m [0..59]

horaDepois :: Hora->Hora->Bool
horaDepois (H h1 h2) (H h3 h4)
                             |h3>h1= True
                             |otherwise= h4>=h2

horapmin :: Hora->Int 
horapmin (H h m)= 60*h + m 

minphora :: Int->Hora 
minphora min = H (div min 60 ) (mod min 60)

diffhoras :: Hora->Hora-> Hora
diffhoras h1 h2 = minphora(horapmin h1 - horapmin h2)

diffhorass :: Hora->Hora-> Hora
diffhorass (H h1 h2) (H h3 h4 ) 
                                | h2-h4 >=0 = H (h1-h3) (h2-h4)
                                | otherwise = H (h1-h3 -1) (60 -(h4-h2))          

addmins :: Hora->Int->Hora         
addmins h m = minphora (horapmin h + m)
                                     

-- hor2min' (H h m) = 60 * h + m

-- min2hor' min = H (div min 60) (mod min 60)

-- hordiff' h1 h2 = min2hor' $ abs $ hor2min' h1 - hor2min' h2

-- addmins' hor min = min2hor' $ mod (hor2min' hor + min) 144

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double 

posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a


posy :: Ponto -> Double 


posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

raio :: Ponto -> Double 

raio (Cartesiano x y) = sqrt (x ^ 2 + y ^ 2)
raio (Polar r a) = r
angulo:: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a


dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((x' - x) ^ 2 + (y' - y) ^ 2)
    where x = posx p1
          y = posy p1
          x' = posx p2
          y' = posy p2

-- --Defina uma função que recebe uma lista e desloca cada elemento da lista, n posições
-- para a direita. Os elementos do final da lista são colocados no início (e.g. [1,2,3,4,5]
-- daria [4,5,1,2,3], para n=2).


deslocarlista :: [a] -> Int-> [a]
deslocarlista m 0 = m 
deslocarlista [] m = []
deslocarlista (m:t) k =  deslocarlista (last t:m:init t) (k-1)

deslocarbro :: [a]->Int-> [a]
deslocarbro [] _ = []
deslocarbro (k) n 
                    |n == 0 = k
                    | n >  length k = let chunk= mod n (length k) in 
                     drop chunk k ++ take chunk k
                    | otherwise = drop n k ++ take n k