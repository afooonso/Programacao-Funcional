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

diffhoras :: Hora->Hora-> Int
diffhoras h1 h2 =horapmin h2 - horapmin h1

diffhorass :: Hora->Hora-> Hora
diffhorass (H h1 h2) (H h3 h4 ) 
                                | h4-h2>=0 = H (h3-h1) (h4-h2)
                                | h3-h2==0 = H (0) (60-abs(h4-h2))       
                                |otherwise = H (h3-h1-1) (60-abs(h4-h2))          

addmins :: Hora->Int->Hora         
addmins h m = minphora (horapmin h + m)
                                     

-- hor2min' (H h m) = 60 * h + m

-- min2hor' min = H (div min 60) (mod min 60)

-- hordiff' h1 h2 = min2hor' $ abs $ hor2min' h1 - hor2min' h2

-- addmins' hor min = min2hor' $ mod (hor2min' hor + min) 144