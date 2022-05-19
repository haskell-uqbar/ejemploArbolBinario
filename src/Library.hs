module Library where
import PdePreludat

data Arbol a = Vacio | Arbol {izq:: Arbol a, valor:: a, der::Arbol a} deriving Eq

ordenar:: Ord a => [a]->[a]
ordenar = arbol2lista.lista2arbol

lista2arbol::Ord a => [a]->Arbol a
lista2arbol elementos = foldr insertar Vacio elementos

insertar:: Ord a => a -> Arbol a -> Arbol a
insertar elemento Vacio = Arbol Vacio elemento Vacio
insertar elemento arbol 
  | elemento <= valor arbol = arbol{ izq = insertar elemento (izq arbol)}
  | otherwise = arbol {der = insertar elemento (der arbol)}

arbol2lista:: Ord a => Arbol a -> [a]
arbol2lista Vacio = []
arbol2lista arbol = arbol2lista (izq arbol) ++ [valor arbol] ++ arbol2lista (der arbol)

-- Para inspeccionar el arbol
instance (Show a) => Show (Arbol a) where
    show Vacio = "*"
    show arbol = "( " ++ show (izq arbol) ++ " <- " ++ show (valor arbol) ++ " -> " ++ show (der arbol) ++ " )"

-- Ejemplo
--      2
--     / \
--    1   3 
hoja1 = Arbol Vacio 1 Vacio
hoja3 = Arbol Vacio 2 Vacio
raiz2 = Arbol hoja1 2 hoja3


