library(tidyverse)
library(purrr)

f <- function(x,y, z = 1){
  resultado <- x + (2*y) + (3*z)
  return(resultado)
}
 
# Para saber quais são os argumentos da função.
args(f)


# Observar como as variáveis, isto é, os argumentos podem ser dispostos.
# nomeando ou por posição ou por exclusão.
f(2,3,4)
f(2,3)
f(x=2,y=3)
f(z=4, y = 2, 3)

# Não entendi bem essa função!
trim <- function(p){
  trimit <- function(x){
    n <- length(x)
    lo <- floor(n*p) + 1
    hi <- n + 1- lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  trimit
}

# Indo dos scripts às funções (R for Dummies, pg. 151)
# fazendo o script para transformar frações em % com uma casa decimal.
x <- c(0.458, 1.6653, 0.83112)
percent <- round(x * 100, digits = 1) 
result <- paste(percent, "%", sep = "") 
print(result) 

# transformando em função.
addPercent <- function(x){
  percent <- round(x * 100, digits = 1)
  result <- paste(percent, "%", sep = "")
  return(result) 
} 
addPercent(0.569)
addPercent(c(1, 0.36))
addPercent(22.69)

new.numbers <- c(0.8223, 0.02487, 1.62, 0.4)
addPercent(new.numbers) 

# no R a função é um objeto como qualquer outro.
# abaixo a função ppaste faz o mesmo que a addPercent
ppaste <- addPercent
ppaste(new.numbers)

# em alguns casos não precisamos usar as chaves {}.
# fica parecendo aqueles funções que estudamos no colégio.
odds <- function(x) x / (1-x) 
odds(.8)
odds(20)

# o argumento pode ser qualquer coisa? R. Sim.
odds_qqc <- function(asdf) asdf / (1-asdf)
odds_qqc(0.8)
odds_qqc(20)

# outra função de uma linha
addPercent_2 <- function(x) paste(round(x * 100, digits = 1), "%", sep = "")
addPercent_2(new.numbers)

# se o vetor já se referir a percentual basta dividir o argumento por 100.
percentagem <- c(58.23, 120.4, 33)
  addPercent(percentagem/100)

# podemos sofisticar a função addPercentage adicionabndo o argumento mult
  addPercent <- function(x, mult){
  percent <- round(x * mult, digits = 1)
  result <- paste(percent, "%", sep = "") 
  return(result)
  }  
  
# mult = 100 tanforma números em percentagem,
# mult = 1 usa-se quando os números já represntam porcentagem mas ainda estão sem o sinal %. 
addPercent(x = new.numbers, mult = 100)
addPercent(x = new.numbers, mult = 1) 

# usando o default value.
addPercent_3 <- function(x, mult = 100){
  percent <- round(x * mult, digits = 1)
  result <- paste(percent, "%", sep = "") 
  return(result)
} 

addPercent_3(new.numbers)
addPercent_3(new.numbers, mult = 1)

# podemos botar mais argumentos, no caso 'casas decimais'.
addPercent <- function(x, mult = 100, casas_decimais = 1){
  percent <- round(x * mult, digits = casas_decimais)
  paste(percent, "%", sep = "")
} 
addPercent(new.numbers, mult = 100, casas_decimais = 4)
# por que não ficou com 4 casas decimais? R porque não tinha. vamos tentar outros números
# se a 4ª casa decimal é zero fica com 3
out_num <- c(1.236590212, .0150256987, 12.52618000008974)
addPercent(out_num, mult = 100, casas_decimais = 4)

# area de circulo de raio 1.
pi*1^2

# função area do circulo
circ_area <- function(r){
  area <- pi*r^2
  return(area)
}

x <- 1:5
circ_area(x)

# função area e circunferência
are_circun <- function(r){
  area <- pi*r^2
  circun <- 2*pi*r
  return(c(Área = area, Circunferência = circun))
}
are_circun(1)
are_circun(x)

# outra alternativa de apresentação do resultado, usando lista
are_circun <- function(r){
  area <- pi*r^2
  circun <- 2*pi*r
  return(list(Área = area, Circunferência = circun))
}
are_circun(x)

# outra alternativa
are_circun <- function(r){
  area <- pi*r^2
  circun <- 2*pi*r
  return(data.frame(Área = area, Circunferência = circun))
}
are_circun(x)

r <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
# ainda pode retornar um gráfico com x = r e y = area
grafico <- function(r){
  area <- pi*r^2
    return(plot(x = r, y = area, xlab = "Raio", ylab = "Área"))
}

vol_cilindro <- function(r,h){
  vol <- pi*r*r*h
  return(vol)
}

# tanto faz estas 3 formas de aplicar a função
vol_cilindro(r = 1, h =3)
vol_cilindro(h = 3, r = 1)
vol_cilindro(1,3)

# Define a simple function
myFirstFun<-function(n)
{
  # Compute the square of integer `n`
  n*n   
}

# Assign `10` to `k`
k <- 10

# Call `myFirstFun` with that value
m <- myFirstFun(k)

# Call `m` 
m
myFirstFun(12)
myFirstFun(15)


#  define-se uma função e fixa-se o y. sequência de 0.05 até 1 de 0.01 em 0.01
MyThirdFun <- function(n, y = seq(0.05, 1, by = 0.01))
{
# faz-se o 1º elemento elevado ao segundo.
  n^y  
}

# se especificarmos os 2 elementos obtêm-se um resultado, no caso 8
MyThirdFun(2,3)

# se especificarmos só o 1º, ele é elevado a cada elemento do 2º
 2^0.05
 2^0.06
 2^0.07
 
MyThirdFun(2)  

# função anônima. é quando não a nomeamos
fun<-function(x) x * 10
fun(9)
fun(458)

# convertendo graus fahrenheit para kelvin
fahrenheit_to_kelvin <- function(temp_F) {
temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
return(temp_K)
}
fahrenheit_to_kelvin(65)

# convertendo kelvin para celsius.
kelvin_to_celsius <- function(temp_K) {
  temp_C <- temp_K - 273.15
  return(temp_C)
}
kelvin_to_celsius(0)
kelvin_to_celsius(273)

# agora convertendo fahrenheit para celsius.
fahrenheit_to_celsius <- function(temp_F) {
  temp_K <- fahrenheit_to_kelvin(temp_F)
  temp_C <- kelvin_to_celsius(temp_K)
  return(temp_C)
}
fahrenheit_to_celsius(65)
fahrenheit_to_celsius(90)

# também podemos fazer tipo função composta: f(g(x)).
kelvin_to_celsius(fahrenheit_to_kelvin(65))
kelvin_to_celsius(fahrenheit_to_kelvin(90))

#
fence <- function(original, wrapper) {
  answer <- c(wrapper, original, wrapper)
  return(answer)
}
fence(c("casa", "chaminé", "título", "era uma vez"), "branca")
fence(seq(0:20), "Quanto número!")

# tomando o 1º e o último elementos de um vetor.
outside <- function(v) {
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first, last)
  return(answer)
}  

outside(c("Don't", "repeat", "yourself", "or", "others"))
outside(c(0:25))


# função reescalar. transforma os valores de um vetor colocando-os entre 0 e 1 proporcionalmente.
rescale <- function(v) {
  # Rescales a vector, v, to lie in the range 0 to 1.
  menor <- min(v)
  maior <- max(v)
  result <- (v - menor) / (maior - menor)
  return(result)
}

v <- c(1, 39, 56, 26, 1, 24, 58)
rescale(v)
j <- c(3,6,9,5,7,1,12,14)
rescale(j)

# esta função dá nome aos valores, no caso 1, 2 e 3.
nomear <- function(a = 1, b = 2, c = 3) {
  result <- c(a, b, c)
  names(result) <- c("lucia", "carlos", "mauro")  # This names each element of the vector
  return(result)
}

nomear()
nomear(20,36,14)
nomear(b = 34)
nomear(b = 12, a = 34, c = 2)

# função que escreve uma frase no final
pow <- function(x, y) {
  # function to print x raised to the power y
  result <- x^y
  print(paste(x,"elevado a", y, "é", result))
}
pow(9, 2)
pow(9, 0.5)
pow(27, 1/3)

# x é positivo, negativo ou zero?
check <- function(x) {
  if (x > 0) {
    result <- "Positivo"
  }
  else if (x < 0) {
    result <- "Negativo"
  }
  else {
    result <- "Zero"
  }
  return(paste(x, "é", result))
}
check(0)
check(6)
check(-6)






# no arguments
display()
# função para criar gráfico.
analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}