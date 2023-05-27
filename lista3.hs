-- Exercício 1)
-- Função que retorna o quadrado de um número
-- Funcão que calcule a soma do quadrado dos cem primeiros inteiros
quadradoCemPrimeiros :: [Int]
quadradoCemPrimeiros = [x*x | x<-[1..100]] -- retorna uma lista com elementos x*x, tal que x é uma lista de 1 a 100

-- Exercício 2)
-- Recebe um valor inteiro e um elemento para replicar. Retorna uma lista de elementos "y" replicado "x" vezes
replica :: Int -> a -> [a]
replica x y = [y | x <- [1..x]]

-- Exercício 3)
-- Funcão auxilar que verifica se três números formam um triângulo pitagórico
ePitagorico :: Int -> Int -> Int -> Bool
ePitagorico a b c = a*a + b*b == c*c

-- Recebe uma valor inteiro como intervalo e retorna uma lista de tuplas de três inteiros. A tupla contém todos os 
-- valores que formam um triângulo pitagórico no intervalo dado.
intervaloPitagorico :: Int -> [(Int, Int, Int)]
intervaloPitagorico a = [(x, y, z) | x<-[1..a], y<-[1..a], z<-[1..a], (ePitagorico x y z)] -- "a" é o intervalo passado como parâmetro. Retorna uma lista de tupla com (x,y,z), tal que x, y, z é todo elemento da lista que vai de 1 a "a" e ePitagorico é verdadeiro

-- Exercício 4)
-- Retorna todos os divisores de "x" numa lista
divisores :: Int -> [Int]
divisores x = [a | a <- [1..(x-1)], (mod x a) == 0]

-- Recebe uma lista de inteiros e retorna o valor da soma desses elementos
somaElementos :: [Int] -> Int
somaElementos [] = 0
somaElementos (a:b) = a + somaElementos b

-- Recebe um valor como limite e retorna uma lista com todos os números nesse intervalo cuja soma de seus divisores (sem o próprio valor) é igual a ele mesmo
ePerfeito :: Int -> [Int]
ePerfeito x = [a | a <- [1..x], somaElementos(divisores a) == a]

-- Exercício 5)
-- qissojss

-- Exercício 6)
-- ??

-- Exercício 7)
-- Recebe duas listas e concatena seus elementos correspondentes em tuplas
juntaLista :: [a] -> [a] -> [(a,a)]
juntaLista (a:as) (b:bs) = [(a,b)] ++ juntaLista as bs
juntaLista _ _ = []

-- Multiplica os elementos de uma dupla
multiplicaElementos :: (Int, Int) -> [Int]
multiplicaElementos (a, b) = [a*b]

-- Retorna uma lista onde cada elemento corresponde à multiplicação dos elementos da dupla
multiplicaElementosCorrespondentes :: [Int] -> [Int] -> [Int]
multiplicaElementosCorrespondentes a b = [x | z <- (juntaLista a b), x <- (multiplicaElementos z)]

-- Calcula o produto escalar
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar a b = somaElementos (multiplicaElementosCorrespondentes a b)

-- Exercício 8) 

-- Exercício 9) 
-- Funções para usar no filtro
dobra :: Int -> Int
dobra x = 2*x

triplica :: Int -> Int
triplica x = 3*x 

-- Filtro 1, exemplo:
filtro :: (Int -> Int) -> Int -> Int
filtro f x = (f x)

-- Função para o filtro
eImpar :: Int -> Bool
eImpar x = mod x 2 /= 0

-- Filtro 2
filtraLista :: (Int -> Bool) -> [Int] -> [Int]
filtraLista _ [] = []
filtraLista f (a:b) 
    | (f a) = a: (filtraLista eImpar b)
    | otherwise = filtraLista eImpar b

-- Função para o filtro
soma :: Int -> Int -> Int
soma x y = x+y

-- Filtro 3
filtraLista2 :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
filtraLista2 _ _ [] = []
filtraLista2 f x (a:b) = (f x a): filtraLista2 f x b 

-- Retorna uma lista dos números ímpares de 1 a 10, com cada elemento acrescido em 7
imparesMaisSete :: [Int]
imparesMaisSete = filtraLista2 soma 7 (filtraLista eImpar [1..10])

teste = [(+7) x | x <- [1..10], odd x]

-- Exercício 10)
eMaior9 :: Int -> Bool
eMaior9 x = x>9

-- Filtro 3
filtraLista3 :: (Int -> Bool) -> [Int] -> [Int]
filtraLista3 _ [] = []
filtraLista3 f (a:b) 
    | f a = a: filtraLista3 f b
    | otherwise = filtraLista3 f b

listaDeUnidadesInvertida :: Int -> [Int]
listaDeUnidadesInvertida x 
    | x<10 = [x]
    | otherwise = (mod x 10) : listaDeUnidadesInvertida (div x 10)

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:[]) = [a]
inverteLista (a:b) = inverteLista b ++ [a]


-- Função que ordena uma lista
eMenor :: Int -> Int -> Int
eMenor x y 
    | x<y = x 
    | otherwise = y

menorDaLista :: [Int] -> Int
menorDaLista (a:[]) = a
menorDaLista (a:b) = eMenor a (menorDaLista b)

removeElemento :: Int -> [Int] -> [Int]
removeElemento x (a:b)
    | x==a = b 
    | otherwise = a : removeElemento x b

ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (a:[]) = [a]
ordenaLista (a:b) = menorDaLista (a:b) : ordenaLista (removeElemento (menorDaLista (a:b)) (a:b))