type Equipamento = String
type Uso = (Equipamento, Int)
type ListaUso = [Uso]

inv :: ListaUso -> Bool
inv [] = True
inv (a : as)
 | (snd a) > 0 = inv as
 | otherwise = False

duracaoDe :: Equipamento -> ListaUso -> Int
duracaoDe x [] = 0
duracaoDe x (a : as)
 | currEquip == x = currTime + duracaoDe x as
 | otherwise = duracaoDe x as
    where
        currEquip = fst a
        currTime = snd a


bemFormada :: ListaUso -> Bool
bemFormada [] = True
bemFormada (a : as)
 | not (inv (a : as)) = False
 | otherwise =  (currTime < 24) || bemFormada as
    where
        currTime = duracaoDe (fst a) (a : as)


removerEqp :: Equipamento -> ListaUso -> ListaUso
removerEqp x [] = []
removerEqp x l = [k | k <- l, fst k /= x]

type Preço = Int
type Tarifa = (Equipamento, Preço)
type Tarifas = [Tarifa]

definidoEm :: ListaUso -> Tarifas -> Bool
definidoEm [] l = True
definidoEm l [] = False
definidoEm (a : as) t
 | notElem equipamento ([fst x | x <- t]) = False
 | otherwise = definidoEm as t
    where equipamento = fst a



