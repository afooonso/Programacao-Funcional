module Fichas3 where
import Ficha1 as F1

type Etapa= (F1.Hora, F1.Hora)
type Viagem= [Etapa]

--a
estabemconstruido :: Etapa ->Bool
estabemconstruido (h , m) = F1.horaValida' h && F1.horaValida'  m && F1.horaDepois  h m 

--b
viagembemconstruida :: Viagem->Bool
viagembemconstruida []= True
viagembemconstruida (m:[]) = estabemconstruido m
viagembemconstruida ((m1,m2):(t1,t2):k) = estabemconstruido (m1,m2) && F1.horaDepois t1 m2  && viagembemconstruida k

--c
calcularhorapartidachegada :: Viagem -> (F1.Hora,F1.Hora)
calcularhorapartidachegada [] = (H 0 0, H 0 0)
calcularhorapartidachegada m = (fst (head m) , snd (last m) )


--d
tempoTotalViagem :: Viagem-> F1.Hora 
tempoTotalViagem [] = H 0 0
tempoTotalViagem ((h1,h2):t)= somahoras (F1.diffhorass h2 h1) (tempoTotalViagem t)
somahoras :: F1.Hora->F1.Hora->F1.Hora 
somahoras (H h1 m1) (H h2 m2) 
                             |m2+m1>=60 = H (h1+h2+1) (m2+m1 - 60)
                             |otherwise = H (h1+h2) (m2+m1)
                             
                                                         
--e

tempoTotaldeEspera :: Viagem-> F1.Hora 
tempoTotaldeEspera [] = H 0 0 
tempoTotaldeEspera (a:[]) = H 0 0 
tempoTotaldeEspera ((_,h1):(m,t):k ) = somahoras (F1.diffhoras m h1) (tempoTotaldeEspera ((m,t):k) )


--f
tempoEfetivo :: Viagem-> F1.Hora
tempoEfetivo h = somahoras (tempoTotalViagem h) (tempoTotaldeEspera h)

--2

--a)

type Poligonal= [F1.Ponto]

comprimentolinha :: Poligonal->Double
comprimentolinha [] = 0
comprimentolinha (a:[]) = 0
comprimentolinha (a:b:k) = F1.dist b a + comprimentolinha (b:k)

--b)
linhaFechada :: Poligonal -> Bool
linhaFechada p = length p >= 3 && head p == last p





--5 
data Movimento = Credito Float | Debito Float
    deriving Show

data Data = D Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show


--a
extValor :: Extracto ->Float-> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext sld ((d,q,Credito mov):k)) m
                                     | mov > m = Credito mov : extValor (Ext sld k) m
                                     | otherwise = extValor (Ext sld k) m
--b
filtro ::Extracto->[String]->[(Data,Movimento)]
filtro _ [] = []
filtro  (Ext sld ((d,q,mov):k)) l 
                               | q `elem` l = (d,mov): (filtro (Ext sld k) l)
                               | otherwise = filtro (Ext sld k ) l

--c
creDeb :: Extracto -> (Float,Float)
creDeb (Ext sld []) = (0,0)
creDeb (Ext sld ((d,q,  Credito m):k)) =(m+x1,x2)
                                     where (x1,x2)= creDeb (Ext sld k)
creDeb (Ext sld ((d,q,  Debito m):k)) =(x1,m+x2)
                                     where (x1,x2)= creDeb (Ext sld k)



teste :: Extracto -> Bool
teste (Ext sld ((d,q,mov):k)) = case mov of Credito x ->True     
                                            Debito x->False  


--d
saldo :: Extracto ->Float
saldo (Ext sld []) = sld     
saldo ((Ext sld ((d,q,Debito x ):k))) = saldo (Ext (sld + x) k)
saldo ((Ext sld ((d,q,Credito x ):k))) = saldo (Ext (sld - x) k)
                                                   