module Programa where
import Data.Char

-- Exercício 1)
-- Função que retorna uma tupla-3 contendo o caractere fornecido com entrada,
-- o mesmo caracter em letras minúsculas ou maiúsculas, e o seu número da tabela ASCII
-- Ex: b (b,B,98)

entradaConvertida :: Char -> (Char, Char, Int)
entradaConvertida x
    | isLower x = (x, chr ((ord x)-32), ord x)
    | otherwise = (x,chr ((ord x)+32), ord x)

-- Exercício 2)
pessoa :: Int -> ([Char], Int, Char)
pessoa rg 
    | rg == 1 = ("Joao Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza", 51, 'm')
    | rg == 3 = ("Adriana Avila", 27, 'f')
    | rg == 4 = ("Rodrigo Pagliares", 40, 'm')
    | rg == 5 = ("Eliseu Cesar", 35, 'm')
    | rg == 6 = ("Jose Paulo", 70, 'm')
    | rg == 7 = ("Mariane", 33, 'f')
    | rg == 8 = ("Tiago Jose Arruda", 36, 'm')
    | rg == 9 = ("Luis Silva", 53, 'm')
    | rg == 10 = ("Jocileide Strauss", 21, 'f')
    | otherwise = ("Nao ha ninguem mais", 9999, 'x')

-- Funções auxiliares para extrair os dados das tuplas:
obterNome :: ([Char], Int, Char) -> String
obterNome (a,b,c) = a

obterIdade :: ([Char], Int, Char) -> Int
obterIdade (a,b,c) = b

obterGen :: ([Char], Int, Char) -> Char
obterGen (a,b,c) = c

-- a) O nome da pessoa de menor idade até um determinado registro.

-- Função auxiliar que compara a idade de duas tuplas e retorna a tupla com a menor idade.
eMaisNovo :: ([Char], Int, Char) -> ([Char], Int, Char) -> ([Char], Int, Char)
eMaisNovo (a,b,c) (d,e,f)
    | b<e = (a,b,c)
    | otherwise = (d,e,f)

-- Função auxiliar que retorna a tupla contendo os dados da pessoa com a menor idade até um determinado registro.
menorIdadePeriodo :: Int -> ([Char], Int, Char)
menorIdadePeriodo 0 = ("Invalido", 9999,'x')
menorIdadePeriodo x 
    | x>0 = eMaisNovo (pessoa x) (menorIdadePeriodo (x-1))

-- Função que retorna nome da pessoa de menor idade até um determinado registro.
nomeMenorIdade :: Int -> [Char]
nomeMenorIdade x = obterNome (menorIdadePeriodo x)

-- b) A média das idades de todas as pessoas até um dado registro.

-- Função auxiliar que soma a idade de todas as pessoas até um determinado registro.
somaIdadesPeriodo :: Int -> Int
somaIdadesPeriodo 0 = 0
somaIdadesPeriodo x = obterIdade (pessoa x) + somaIdadesPeriodo (x-1)

-- Função que retorna a média das idades até um determinado registro.
mediaIdadePeriodo :: Int -> Float
mediaIdadePeriodo x = fromIntegral (somaIdadesPeriodo x)/fromIntegral x

-- c) Quantidade de homens
-- Função que retorna a quantidade de homens até um determinado registro.
gore :: Int -> Int
gore 0 = 0
gore x
    | (obterGen (pessoa x))=='m' = 1 + gore (x-1) 
    | otherwise = gore (x-1)

-- d) Registro da pessoa mais velha
-- Função auxiliar que retorna a pessoa mais velha até um determinado registro.
eMaisVelho :: ([Char], Int, Char) -> ([Char], Int, Char) -> ([Char], Int, Char)
eMaisVelho (a,b,c) (d,e,f)
    | b>e = (a,b,c)
    | otherwise = (d,e,f)

-- Função que retorna o índice do registro da pessoa mais velha.
maiorIdadePeriodo :: Int -> ([Char], Int, Char)
maiorIdadePeriodo 0 = ("Invalido",-9999,'x')
maiorIdadePeriodo x = eMaisVelho (pessoa x) (maiorIdadePeriodo (x-1))

-- Função que retorna o registro da pessoa mais velha.
rgMaiorIdade :: Int -> Int
rgMaiorIdade 0 = 0
rgMaiorIdade x 
    | (pessoa x) == maiorIdadePeriodo x = x
    | otherwise = rgMaiorIdade (x-1)

-- Função que retorna o registro da pessoa mais nova.
rgMenorIdade :: Int -> Int
rgMenorIdade 0 = 0
rgMenorIdade x 
    | (pessoa x) == menorIdadePeriodo x = x
    | otherwise = rgMenorIdade (x-1)

-- Exercício 3)
-- JÁ FOI FEITO, IDÊNTICO AO EXERCÍCIO 1 

-- Exercício 4)
-- Função que recebe 4 números inteiros e retorna uma lista com esses elementos.
criaLista4Int :: Int -> Int -> Int -> Int -> [Int]
criaLista4Int a b c d = [a,b,c,d]

criaTupla4Int :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
criaTupla4Int a b c d = (a,b,c,d)

-- Função que retorna o menor número de uma lista de inteiros.
eMenor :: [Int] -> Int
eMenor [] = 0
eMenor (a:[]) = a
eMenor (a:b)
    | a<(eMenor b) = a
    | otherwise = eMenor b

-- Função que remove um elemento de uma lista de inteiros.
removeElemento :: Int -> [Int] -> [Int]
removeElemento _ [] = [] 
removeElemento x (a:b)
    | x==a = b
    | otherwise = a:(removeElemento x b)

-- Função que ordena uma lista de inteiros
ordenaListaInt :: [Int] -> [Int]
ordenaListaInt [] = []
ordenaListaInt (a:[]) = [a]
ordenaListaInt (a:b) = (eMenor (a:b)):ordenaListaInt((removeElemento (eMenor (a:b)) (a:b)))

-- Função que retira elementos de uma lista de acordo com a posição indicada
retiraDaLista :: Int -> [Int] -> Int
retiraDaLista 0 (a:b) = a 
retiraDaLista x (a:b) = retiraDaLista (x-1) b

-- Função que recebe 4 números inteiros e retorna uma lista ordenada
listaOrdenada4Int :: Int -> Int -> Int -> Int -> [Int]
listaOrdenada4Int a b c d = ordenaListaInt (criaLista4Int a b c d)

-- Função que cria uma tupla-4 de inteiros a partir de elementos de uma lista com 5 elementos
viraTupla :: [Int] -> (Int, Int, Int, Int)
viraTupla (a:b) = (retiraDaLista 0 (a:b), retiraDaLista 1 (a:b), retiraDaLista 2 (a:b), retiraDaLista 3 (a:b))

-- Função que recebe 4 números inteiros e retorna uma tupla ordenada 
tuplaOrdenada4Int :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
tuplaOrdenada4Int a b c d = viraTupla (ordenaListaInt (criaLista4Int a b c d))

-- Exercício 5)
-- Função que recebe duas datas (dia, mes, ano), sendo a primeira menor do que a segunda, e retorna quantos dias existem entre estas duas datas.
-- NÃO FIZ

-- Exercício 6)
delta :: (Int, Int, Int) -> Int
delta (a,b,c) = b*b - 4*a*c

raizes :: (Int, Int, Int) -> [Double] 
raizes (a,b,c) = [(fromIntegral (-b) + sqrt (fromIntegral (delta (a,b,c)))) / (fromIntegral (2*a))] ++ [(fromIntegral (-b) - sqrt (fromIntegral (delta (a,b,c)))) / (fromIntegral (2*a))]

-- Função que receba os coeficientes de uma equação do segundo grau, ax²+bx+c=0, na forma (a,b,c) e retorna as raízes dessa equação.
eqSegundoGrau :: (Int, Int, Int) -> [Double]
eqSegundoGrau (a,b,c)
    | (delta (a,b,c))<0 = [9999999.9999999]
    | otherwise = raizes (a,b,c)

-- Exercício 7)
-- Função que receba três valores e verifique se forma um triângulo. Se sim, retornar uma tupla-2 com o tipo do triângulo formado (com relação às arestas)
-- e o perímetro do mesmo.

eTriangulo :: (Int, Int, Int) -> Bool
eTriangulo (a,b,c) = (a+b)>c && (a+c)>b && (b+c)>a

perimetroTriangulo :: (Int, Int, Int) -> Int
perimetroTriangulo (a,b,c) = a+b+c

tipoTriangulo :: (Int, Int, Int) -> String
tipoTriangulo (a,b,c)
    | a==b && a==c = "Equilatero"
    | a==b || a==c || b==c = "Isosceles"
    | otherwise = "Escaleno"

triangulo :: (Int, Int, Int) -> (String, Int)
triangulo (a,b,c) 
    | eTriangulo (a,b,c) = ((tipoTriangulo (a,b,c)), (perimetroTriangulo (a,b,c)))
    | otherwise = ("Nao forma triangulo", 0)

-- Exercício 8)
base :: Int -> (Int, String, String, Char)
base x
    | x == 0 = (1793, "Pedro Paulo", "MESTRE", 'M')
    | x == 1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
    | x == 2 = (1534, "Joao de Medeiros", "DOUTOR", 'M')
    | x == 3 = (1267, "Claudio Cesar de Sa", "DOUTOR", 'M')
    | x == 4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
    | x == 5 = (1888, "Rita de Matos", "MESTRE", 'F')
    | x == 9 = (1968, "Tereza Cristina Andrade", "MESTRE", 'F')
    | x == 10 = (0, "", "", 'O')

-- Funções auxiliares para extrair dados da tupla
getAno :: (Int, String, String, Char) -> Int
getAno (a,b,c,d) = a

getNome :: (Int, String, String, Char) -> String
getNome (a,b,c,d) = b

getTitulo :: (Int, String, String, Char) -> String
getTitulo (a,b,c,d) = c

getGenero :: (Int, String, String, Char) -> Char
getGenero (a,b,c,d) = d

-- a) Função que retorne a quantidade de doutores
eDoutor :: (Int, String, String, Char) -> Bool
eDoutor (a,b,c,d) = (c=="DOUTOR") || (c=="DOUTORA")

mudaX :: Int -> Int
mudaX x 
    | (x==6) || (x==7) || (x==8) = 5
    | otherwise = x

quantidadeDoutorPeriodo :: Int -> Int
quantidadeDoutorPeriodo (-1) = 0
quantidadeDoutorPeriodo x
    | (eDoutor (base (mudaX x))) = 1 + quantidadeDoutorPeriodo ((mudaX x)-1)
    | otherwise = quantidadeDoutorPeriodo ((mudaX x)-1)

-- b) Quantidade de mulheres
eMulher :: (Int, String, String, Char) -> Bool
eMulher (a,b,c,d) = (d== 'F')

quantidadeMulherPeriodo :: Int -> Int
quantidadeMulherPeriodo (-1) = 0
quantidadeMulherPeriodo x 
    | (eMulher (base (mudaX x))) = 1 + quantidadeMulherPeriodo ((mudaX x)-1)
    | otherwise = quantidadeMulherPeriodo ((mudaX x)-1)

-- c) Quantidade de mestres do sexo masculino
eHomem :: (Int, String, String, Char) -> Bool
eHomem (a,b,c,d) = d=='M' 

eMestre :: (Int, String, String, Char) -> Bool
eMestre (a,b,c,d) = c=="MESTRE"

eHomemMestrePeriodo :: Int -> Int
eHomemMestrePeriodo (-1) = 0
eHomemMestrePeriodo x
    | (eHomem (base (mudaX x))) && (eMestre (base (mudaX x))) = 1 + (eHomemMestrePeriodo ((mudaX x)-1))
    | otherwise = eHomemMestrePeriodo ((mudaX x)-1)

-- d) O nome do professor mais antigo (número de menor matrícula)
matriculaMaisAntiga :: (Int, String, String, Char) -> (Int, String, String, Char) -> (Int, String, String, Char)
matriculaMaisAntiga (a,b,c,d) (e,f,g,h)
    | a<e = (a,b,c,d)
    | otherwise = (e,f,g,h)

-- Função que recebe um período e retorna uma tupla com o professor de registro mais antigo
matriculaPeriodo :: Int -> (Int, String, String, Char)
matriculaPeriodo (-1) = (999999, "base", "base", 'X')
matriculaPeriodo x
    | x==10 = matriculaPeriodo 9
    | otherwise = matriculaMaisAntiga (base (mudaX x)) (matriculaPeriodo ((mudaX x)-1))

-- Funçâo que retorna o ano do professor que possui a matricula mais antiga
nomeMatriculaMaisAntiga :: Int -> Int
nomeMatriculaMaisAntiga x = getAno (matriculaPeriodo x)
