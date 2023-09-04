

--p1 [1,3,4,5,1,2] [2,3,4,1,2] = [5,1]
p1 :: Eq a => [a] -> [a] -> [a] 
p1 l [] = l 
p1 l (h:t) = p1 (remo h l) t 

-- remove o primeiro occorencia de x
remo :: Eq a => a -> [a] -> [a]
remo x [] = [] 
remo x (h:t) = if x == h
               then t
               else h: remo x t


-- 2 
type MSet a = [(a,Int)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
--'c'  [('c',3), ('a',9)]
removeMSet x (h:t) = if x == fst h 
                     then if snd h - 1 == 0
                          then t 
                          else (x,snd h - 1):t
                     else h: removeMSet x t

--[('c',3), ('a',9)] -> (['c','a'],12)
calcula :: MSet a -> ([a],Int)

          -- fold   (funçao ) (aco) lista
--             l    (b->a->b) -> b -> t a     =  -> b
--
--
calcula  l = foldr (\(x,y) a ->  (x: fst a , y + snd a  ) ) ([] , 0) l

calcula2 l = foldl (\a (x,y) ->  (x: fst a , y + snd a  ) ) ([] , 0) l

calcula3 l = foldr auxfc ([] , 0) l

auxfc :: (a,Int) -> ([a],Int) -> ([a],Int)

auxfc (x,y) (lx,sy) = (x:lx, y+sy )

-- 3 
-- partes "ola;sou;o;rubik" -> ["ola","sou","o","rubik"]

partes :: String -> Char -> [String]
partes "" x = []
partes l  x = pri : partes (drop len l) x
    where pri = part1 l x
          len = (length pri )+ 1

part1 :: String -> Char -> String
part1  ""   x = ""
part1 (h:t) x = if x == h 
                then "" 
                else h: part1 t x--"ola;sou;o;rubik" ';' -> "ola" 
-- 4
data BTree a = Empty | Node a (BTree a) (BTree a) 

a1 = Node 5 (Node 3 Empty Empty ) 
            (Node 7 Empty  (Node 9 Empty Empty)) 



minA :: BTree a -> a 
minA (Node x Empty _) = x 
minA (Node x e _ ) = minA e 


asm :: BTree a -> BTree a 
asm Empty = Empty
asm (Node x Empty d) = d 
asm (Node x e d) = Node x (asm e) d


remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty 
remove x (Node z e d)
    | x > z = Node z e (remove x d)
    | x < z = Node z (remove x e) d
    | x == z= (Node minw e k)  
    where minw = minA d
          k  = asm  d

instance Show a => Show ( BTree a ) where
    show Empty = "*"
    show (Node x e d) =   "("++ show e ++ "<-" ++ (show x)++" -> " ++ show d ++ ")"


-- 5 

mysortOn :: Ord b => (a->b) ->[a] -> [a]
mysortOn f []    = [] 
mysortOn f (h:t) = insere f h (mysortOn f t) 


insere :: Ord b => (a -> b ) -> a -> [a] -> [a]
insere f x [] = [x]
insere f x (h:t) = if f x > f h
                   then h: insere f x t
                   else x:h:t
-- 6

data FileSystem = File Nome | Dir Nome [FileSystem] deriving Show

type Nome = String

fs1 = Dir "usr" [Dir "xxx" [File "abc.txt",File "readme", Dir "PF" [File "exemplo.hs"]],
                 Dir "yyy" [],  Dir "zzz" [  Dir "tmp" [], File "teste.c" ]]


fich :: FileSystem -> [Nome] 
fich (File n) = [n]
fich (Dir x l) =  concat $ map fich l

fich2 (Dir x l) = concat rf 
    where rf = [  fich f | f <- l ]

  --  [[abc,pf,readem],[teste],[exemplo]] -> [abc,pf,read,tes,exe]
-- b

dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles f [] = Nothing
dirFiles (File n) (h:t) = Nothing

dirFiles (Dir x l) [z] = if x == z 
                         then Just (fi3 l)
                         else Nothing

dirFiles (Dir x l)(h:t) = if x /= h 
                          then Nothing
                          else Just $ concat (aux rf)

    where rf = [dirFiles fi  t | fi <- l]
          --rf = [Nothing,Just [nomeasdasdasm],Nothing,Nothgin]

aux :: [Maybe a] -> [a]
aux [] = [] 
aux (Nothing :t) = aux t
aux ((Just x):t) = x: aux t




fi3 :: [FileSystem] -> [Nome] 
fi3 [] = [] 
fi3  ((Dir x l):t) = fi3 t
fi3 ((File nome):t) = nome: fi3 t
--------------------------------

listaFich :: FileSystem ->  IO ()
listaFich fs = do 
    putStrLn " dá me uma diretoria"
    path <- getLine
    if dirFiles fs (partes path '/') == Nothing 
        then do putStr "n e dire"
    else putStrLn $ unwords $ f $ dirFiles fs (partes path '/')




f (Just x) = x




