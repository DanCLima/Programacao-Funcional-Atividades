import Data.Char

-- define o período de recursão
periodo::Int
periodo = 7

-- Exercício 1)
-- a) Funções
f1 :: Float -> Float
f1 x
    | x>=0 = (x+4)/(x+2)
    | otherwise = 2/x

f2 :: (Int, Int) -> Int
f2 (a,b)
    | a>=b = a+b 
    | otherwise = a-b

f3 :: (Int, Int, Int) -> Int
f3 (a,b,c)
    | (a+b)>c = a+b+c 
    | (a+b)<c = a-b-c 
    | (a+b)==c = 0

-- 2) Fatorial
fat :: Int -> Int 
fat 0 = 1
fat x = x * fat (x-1)

-- 3)
-- Função que soma
soma :: Int -> Int -> Int
soma x y = x+y

-- Função que multiplica utilizando a soma
multiplica :: Int -> Int -> Int
multiplica _ 0 = 0
multiplica 0 _ = 0
multiplica x y = x + (multiplica x (y-1))

-- 4)
-- Função que cria uma lista de um elemento
criaListaInt :: a -> [a] 
criaListaInt x = [x]

-- Função que eleva 10 a x potência
dezElevadoA :: Int -> Int
dezElevadoA 0 = 1
dezElevadoA 1 = 10
dezElevadoA x = 10 * dezElevadoA (x-1)

-- Função que combina uma lista de inteiros para gerar um número inteiro
saiLista :: [Int] -> Int
saiLista [] = 0
saiLista (a:[]) = a
saiLista (a:b) = a*(dezElevadoA(length (a:b)-1)) + saiLista b

-- Função que inverte uma lista de inteiros
inverteInt :: Int -> [Int]
inverteInt x
    | x<0 = inverteInt (abs x)
    | x>=0 && x<10 = [x]
    |otherwise = [mod x 10] ++ (inverteInt (div x 10))

-- 5) 
-- Função que calcula o quadrado de um número
quadrado :: Int -> Int 
quadrado x = x*x

-- Função que calcula a quarta potência de um número
quartaPotencia :: Int -> Int 
quartaPotencia x = quadrado (quadrado x)

-- 6)
iesimo :: Int -> Double
iesimo 0 = sqrt 6
iesimo x = sqrt (6 + iesimo (x-1))

-- 7) m!/n!(m-n)!
combinacao :: Int -> Int -> Double
combinacao m n = (fromIntegral (fat m))/((fromIntegral (fat n))*(fromIntegral (fat (m-n))))

-- 8) MDC
mdc :: Int -> Int -> Int 
mdc m n
    | (mod m n) /=0 = mdc n (mod m n)
    | otherwise = n

-- 9) Multiplos num intervalo
quantosMultiplos :: Int -> Int -> Int -> Int
quantosMultiplos 0 _ b = b
quantosMultiplos 1 _ b = b
quantosMultiplos x a b
    | ((mod a x)==0) && (a==b) = 1
    | ((mod a x)/=0) && (a==b) = 0
    | ((mod a x)==0) && (a<b) = 1 + (quantosMultiplos x (a+1) b)
    | otherwise = quantosMultiplos x (a+1) b

-- 10) Último Dígito
ultimoDigito :: Int -> Int
ultimoDigito x = mod x 10

-- 11) 
-- Gera uma lista de inteiros, a partir de um número inteiro, porém invertido. Exemplo: 1475 = [5,7,4,1]
intParaListaContra :: Int -> [Int]
intParaListaContra x
    | x<0 = intParaListaContra (abs x)
    | x>=0 && x<10 = [x]
    | otherwise = (mod x 10) : (intParaListaContra (div x 10))

-- Retorna o menor de dois valores inteiros
eMenor :: Int -> Int -> Int
eMenor x y
    | x<=y = x
    | otherwise = y

-- Retorna o elemento de menor valor dentro de uma lista de inteiros
menorElementoLista :: [Int] -> Int
menorElementoLista [] = 0
menorElementoLista (a:[]) = a
menorElementoLista (a:b) = eMenor a (menorElementoLista b)

-- Remove um elemento, passado como parâmetro, da lista
removeElementoLista :: Int -> [Int] -> [Int]
removeElementoLista _ [] = []
removeElementoLista x (a:b)
    | x==a = b 
    | otherwise = a: (removeElementoLista x b)

-- Ordena uma lista
ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (a:[]) = [a]
ordenaLista (a:b) = (menorElementoLista (a:b)) : ordenaLista (removeElementoLista (menorElementoLista (a:b)) (a:b))

-- Inverte uma lista de inteiros, exemplo: [4,3,8] = [8,3,4]
inverteListaInt :: [Int] -> [Int]
inverteListaInt [] = []
inverteListaInt (a:[]) = [a]
inverteListaInt (a:b) = (inverteListaInt b) ++ [a]

-- Transforma um número inteiro em uma lista
intParaLista :: Int -> [Int]
intParaLista x = (inverteListaInt (intParaListaContra x))

-- Devolve o elemento da posição escolhida de uma lista
elementoEscolhidoLista :: Int -> [Int] -> Int
elementoEscolhidoLista 0 (a:_) = a
elementoEscolhidoLista x (a:b) = elementoEscolhidoLista (x-1) b

-- Devolve o algarismo de um número inteiro dado uma posição
digitoEscolhido :: Int -> Int -> Int
digitoEscolhido x y = elementoEscolhidoLista x (inverteListaInt (intParaListaContra y))

-- 12) Verifica se todos os números inteiros são diferentes
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = x/=y && y/=z && x/=z

-- 13) 
quantosIguais :: Int -> Int -> Int -> Int 
quantosIguais x y z 
    | todosDiferentes x y z = 0
    | x==y && x==z = 3
    | otherwise = 2

-- 14) 
-- tabela de vendas
vendas :: Int -> Int
vendas 1 = 41
vendas 2 = 72
vendas 3 = 41
vendas 4 = 2
vendas 5 = 91
vendas 6 = 55
vendas 7 = 41
vendas _ = 0

-- tabela de vendas para o exercício 14)c
vendas2 :: Int -> Int
vendas2 1 = 0
vendas2 2 = 0
vendas2 3 = 0
vendas2 4 = 0
vendas2 5 = 0
vendas2 6 = 0
vendas2 7 = 50
vendas2 _ = 0

-- a)
-- Percorre a tabela de vendas, dado um período, e retorna quantas vendas são menores do que um valor passado como parâmetro
quantasVendasMenores :: Int -> Int -> Int -> Int
quantasVendasMenores 0 _ _ = 0
quantasVendasMenores _ _ 0 = 0
quantasVendasMenores x a b 
    | (vendas b)<x && a==b = 1
    | (vendas b)>=x && a==b = 0
    | (vendas b)<x = 1 + (quantasVendasMenores x a (b-1))
    | otherwise = quantasVendasMenores x a (b-1)

-- b)
-- Percorre a tabela de vendas e retorna true se não houver dias nos quais as vendas foram nulas
semZerosPeriodo :: Int -> Bool
semZerosPeriodo 0 = True
semZerosPeriodo x = (vendas x)/=0 && (semZerosPeriodo (x-1)) 

-- c)
-- Percorre a tabela de vendas 2 e retorna uma lista contendo os dias de todas as vendas iguais a zero
zerosNoPeriodo :: Int -> [Int]
zerosNoPeriodo 0 = []
zerosNoPeriodo x
    | (vendas2 x)==0 = x: (zerosNoPeriodo (x-1))
    | otherwise = zerosNoPeriodo (x-1)

-- d)
-- Recebe um valor de venda, um período e retorna quais dias venderam menos do que o valor passado como parâmetro
listaVendasMenoresQue :: Int -> Int -> [Int]
listaVendasMenoresQue _ 0 = []
listaVendasMenoresQue x y
    | (vendas y)<x = y : (listaVendasMenoresQue x (y-1))
    | otherwise = listaVendasMenoresQue x (y-1)

-- 16)
funny :: Int -> Int -> Int -> Bool
funny x y z = (x>z) && (x>y)

-- 17)
maiusculo :: Char -> Char
maiusculo x
    | isLower x = chr ((ord x)-32)
    | otherwise = x

-- 18) 
charParaNum :: Char -> Int
charParaNum x
    | isDigit x = ord x - 48
    | otherwise = -1