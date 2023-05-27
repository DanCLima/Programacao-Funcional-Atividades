module Programa where
import Data.Char

-- Exercício 1)
-- a)
realReal :: Float -> Float
realReal x
   | x >= 0 = (x+4)/(x+2)
   | otherwise = 2/x

-- b)
r2Real :: Int -> Int -> Int
r2Real x y
    | x>=y = x+y
    | otherwise = x-y

-- c) 
r3Real :: Int -> Int -> Int -> Int
r3Real x y z
    | (x+y) > z = x+y+z
    | (x+y) < z = x-y-z
    | otherwise = 0

-- Exercício 2)
--fat :: a -> a
fat 0 = 1
fat x = x * fat (x-1)
-- Para calcular o fatorial de um número precisamos ter a base definida para que a recursão chegue ao fim em algum momento (fat 0 = 1). 

-- Exercício 3)
-- Função que multiplique dois parâmetros a partir da função soma
soma :: Int -> Int -> Int
soma x y = x + y

multiplica :: Int -> Int -> Int
multiplica _ 0 = 0
multiplica 0 _ = 0
multiplica x y = soma x (multiplica x (y-1))
    
-- Exercício 4)
-- Função que inverta os dígitos de um número inteiro
-- Obs: inverte os dígitos de -999 a 999
inverteInt :: Int -> Int
inverteInt x
    | (x >= (-999)) && (x < 0) = inverteInt (abs x) * (-1)
    | (x>=0) && (x<10) = x
    | (x>=10) && (x<100) = ((mod x 10) * 10) + (div x 10)
    | (x>=100) && (x<1000) = ((mod (mod x 100) 10) * 100) + ((div (mod x 100) 10) * 10) + (div x 100)

-- Exercício 5)
-- Crie uma função que eleve a quarta potência, utilizando a função quadrado
quadrado :: Int -> Int
quadrado x = x * x

quartaPotencia :: Int -> Int
quartaPotencia x = quadrado (quadrado x)

-- Exercício 6)
iesimo :: Float -> Float
iesimo x
    | (x<=0) = sqrt 6
    | otherwise = sqrt (6 + (iesimo (x-1)))

-- Exercício 7)
-- Escreva, em Haskell, uma função que informa de quantas maneiras é possível escolher n objetos
-- em uma coleção original de m objetos, para m ≥ n.
combinacaoS :: Float -> Float -> Float
combinacaoS m n 
    | m < n = 0
    | otherwise = (fat m)/((fat n)*(fat(m-n)))

-- Exercício 8)
-- Função recursiva que calcule o MDC de dois parâmetros
mdc :: Int -> Int -> Int
mdc m n 
    | mod m n /= 0 = mdc n (mod m n)
    | otherwise = n

-- Exercício 9)
multiplos :: Int -> Int -> Int -> Int
multiplos 0 a b 
    | a<=b = b-a+1
    | otherwise = multiplos 0 b a  
multiplos x a b 
    | a > b = multiplos x b a       
    | (a==b) && (mod a x) == 0 = 1
    | (a==b) && (mod a x) /= 0 = 0
    | (mod a x) == 0 = 1 + (multiplos x (a+1) b)
    | otherwise = multiplos x (a+1) b
    
-- Exercício 10)
ultimoDigito :: Int -> Int
ultimoDigito x
    | x<0 = ultimoDigito (abs x)
    | x>=0 && x<=9 = x
    | x>=10 = ultimoDigito (mod x 10) 

-- Exercício 11)
-- Função que recebe um número inteiro e coloca todos os dígitos desse número numa lista de inteiros invertidos
digitoPorDigito :: Int -> [Int]
digitoPorDigito a 
    | div a 10 < 1 = [mod a 10]
    | div a 10 >= 1 = [mod a 10] ++ digitoPorDigito (div a 10)

-- Função que recebe uma número inteiro 'a', uma lista e retorna o elemento na posicão a determinada.
posicaoEscolhida :: Int -> [Int] -> Int
posicaoEscolhida 0 (a:b) = a
posicaoEscolhida x (a:b) 
    | x>=(length (a:b)) = -1
    | otherwise = posicaoEscolhida (x-1) b

-- Função que retorna o dígito de um número inteiro de acordo com a posição informada
digitoEscolhido :: Int -> Int -> Int
digitoEscolhido a b = posicaoEscolhida a (inverteListaInt (digitoPorDigito b))
    
converte :: Int -> Char
converte x = chr x

-- Exercício 12)
-- a) O programador está comparando apenas se o segundo parâmetro é diferente dos demais, abrindo exceção para que o primeiro valor seja igual ao terceiro.
-- Dessa forma, ao informar 1 3 1 como entrada, o resultado será True para todosDiferentes.

-- b)
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x/=y) && (y/=z) && (x/=z)

-- Exercício 13)
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais x y z
    | x==y && x==z = 3
    | x==y && x/=z = 2
    | x/=y && x==z = 2
    | x/=y && y==z = 2
    | otherwise = 0

-- Exercício 14)

-- define o período de recursão
periodo::Int
periodo = 7

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

-- a) Função que recebe uma quantidade de venda diária, o início de um período, o final do período e retorna a quantidade
-- de vendas inferior ao valor informado dentro do período.
vMenores :: Int -> Int -> Int -> Int
vMenores x y z
    | y>z = (vMenores x z y)
    | x>(vendas y) && y==z = 1
    | x>(vendas y) && y==z = 0
    | x>(vendas y) = 1 + vMenores x (y+1) z
    | otherwise = vMenores x (y+1) z

-- b) Função que retorna True se, dentro do período passado, não há vendas com valor 0.
semZeroPeriodo :: Int -> Bool
semZeroPeriodo x
    | x==1 && (vendas x)==0 = False
    | x==1 && (vendas x)/= 0 = True
    | (vendas x) == 0 = False
    | (vendas x) /= 0 = True && semZeroPeriodo (x-1)

-- c) Função que retorna uma lista com todos os dias que as vendas foram nulas.
zerosPeriodo :: Int -> [Int]
zerosPeriodo x
    | x==1 && (vendas2 x)==0 = [x]
    | x==1 && (vendas2 x)/=0 = []
    | (vendas2 x)==0 = x:(zerosPeriodo (x-1))
    | otherwise = zerosPeriodo (x-1)

-- d) Função que retorna uma lista de dias nos quais as vendas foram abaixo de um valor passado como parâmetro.
diasVMenores :: Int -> Int -> [Int]
diasVMenores x y 
    | x>(vendas y) && y==1 = [y]
    | x<(vendas y) && y==1 = []
    | x>(vendas y) = y:(diasVMenores x (y-1))
    | otherwise = diasVMenores x (y-1)

-- Exercício 15)
-- Função que calcule a posição de x, passado como parâmetro, na sequência de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 0
fibonacci x = fibonacci (x-2) + fibonacci (x-1)
-- antFib :: Int -> Int
-- antFib x
--     |
--     | x/= antFib = (-1)
--     | otherwise = x
-- NÃO TERMINEI

-- Exercício 16)
-- Função equivalente ao do enunciado, com apenas um casamento de padrão.
divertido :: Int -> Int -> Int -> Bool
divertido x y z = (y<x) && (x>z)

-- Exercício 17)
-- Função que converta uma letra minúscula para maiúscula. Caso a entrada não seja uma letra minúscula,
-- retorna a própria entrada.
minMai :: Char -> Char
minMai x 
    | isLower x = chr ((ord x)-32)
    | otherwise = x

-- Exercício 18)
-- Função que converte um dígito numérico do tipo Char para a sua representação Int. 
-- Se a entrada não for um dígito numérico, retornar -1.
charParaNum :: Char -> Int
charParaNum x
    | isDigit x = (ord x)
    | otherwise = (-1)

-- Exercício 19)
-- Função que recebe uma string s e um inteiro n. Retorna a string s concatenada n vezes.
duplicada :: String -> Int -> String
duplicada _ 0 = ""
duplicada s n = s ++ (duplicada s (n-1))

-- Exercício 20)
-- Função que receba uma string s e um valor inteiro n como entrada. Caso o valor n seja 
-- maior do que a string, retornar a string com o tamanho n, concatenando ">" no seu início.
empurreParaDireita :: String -> Int -> String
empurreParaDireita s n
    | n<=(length s) = s
    | otherwise = ">" ++ empurreParaDireita s (n-1)

-- Exercício 21)
-- &- :: Int -> Int -> Int
-- &- x y = x - 2*y
-- NÃO FIZ


-- Exercício 22)
-- Função que inverte os elementos de uma lista de inteiros
inverteListaInt :: [Int] -> [Int]
inverteListaInt [] = []
inverteListaInt [a] = [a]
inverteListaInt (a:b) = inverteListaInt b ++ [a]

-- Exercício 23)
-- Funções auxiliares eImpar e ePar que identificam os elementos ímpares/pares de uma lista e constrói uma lista só de ímpares/pares
eImpar :: [Int] -> [Int]
eImpar [] = []
eImpar (a:b) 
    | mod a 2 /= 0 = a:(eImpar b)
    | otherwise = eImpar b

ePar :: [Int] -> [Int]
ePar [] = []
ePar (a:b)
    | mod a 2 == 0 = a:(ePar b)
    | otherwise = ePar b

-- Função que recebe uma lista de inteiros e retorna uma tupla de duas listas, a primeira com os valores ímpares e a segunda com valores pares.
imparPar :: [Int] -> ([Int], [Int])
imparPar [] = ([],[])
imparPar (a:b) = (eImpar (a:b), ePar (a:b))

-- Exercício 24)
-- Função que recebe uma lista de inteiros e retorna uma string contendo letras do alfebeto cuja posição é dada pelos elementos da lista.
alfabeto :: [Int] -> String
alfabeto [] = ""
alfabeto (a:b) = (chr (a+64)):(alfabeto b)

-- Exercício 25)
-- 25. Sabendo que [1..7] é equivalente à lista [1,2,3,4,5,6,7], complete as correspondências abaixo:
-- (a) ['a'..'g'] = "abcdefg"
-- (b) [0.1 ..0.9] = [0.1, 1.1]
-- (c) [0.1,0.3 .. 0.9] = [0.1, 0.3, 0.5, 0.7, 0.9]
-- (d) [0.1,0.3 ..1.8] = [0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9]
-- (e) [0.4,0.2 ..0.8] = []
-- (f) [1,4..15] = [1, 4, 7, 10, 13]
teste1 = ['a'..'g']
teste2 = [0.1 ..0.9]
teste3 = [0.1,0.3 .. 0.9]
teste4 = [0.1,0.3 ..1.8]
teste5 = [0.4,0.2 ..0.8]   
teste6 = [1,4..15]

-- Exercício 26)
-- Função que recebe uma lista de caracteres, um caracter 'a' e retorna quantos caracteres são iguais a 'a'.
caracterIgual :: [Char] -> Char -> Int
caracterIgual [] _ = 0
caracterIgual (a:b) x
    | a==x = 1 + caracterIgual b x
    | otherwise = caracterIgual b x

-- Exercício 27)
-- Função que recebe uma lista de inteiros com elementos repetidos e retorna uma lista sem os elementos repetidos.
purificada :: [Int] -> [Int]
purificada [] = []
purificada (a:[]) = [a]
purificada (a:(b:c))
    | a==b = purificada (b:c)
    | otherwise = a:(purificada (b:c))

-- Exercício 28)
-- Função auxiliar que recebe dois parâmetros, 'a' e 'b', e retorna uma lista de b elementos composta de a. Ex: 3 -> 4 -> [3, 3, 3, 3]
listaDe :: a -> Int -> [a]
listaDe _ 0 = []
listaDe a b 
    | b>0 = a:listaDe a (b-1)

-- Função que recebe uma lista de inteiros e retorna uma lista com a repetição de cada elemento de acordo com o seu valor.
plorifera :: [Int] -> [Int]
plorifera [] = []
plorifera [0] = []
plorifera (a:b) = (listaDe a a) ++ plorifera b

-- Exercício 29)
valorAlfabeto :: Char -> Int
valorAlfabeto a = (ord a)-64

-- Função que recebe uma lista de caracteres maiúsculos e retorna uma lista com a repetição (definida pelo seu valor na ordem do alfabeto) de cada caracter .
plorifera2 :: [Char] -> [Char]
plorifera2 [] = []
plorifera2 (a:b) = (listaDe a (valorAlfabeto a)) ++ plorifera2 b