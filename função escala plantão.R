library(tidyverse)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(DT)
library(flexdashboard)


escala_de_plantao <- function(aud_1 = "Helder", aud_2 = "Fernando", aud_3 = "Alexandre", 
                              aud_4 = "Mauro", aud_5 = "Mario", aud_6 = "Philipe", aud_7 = "Raphael", 
                                data_seg_feira = "2018-04-02", num_dias = 180){

# criando os tipos de escala na semana,
# esta ordem foi decidida pela chefia, chamo de padrªo atual
# na a trabalha-se 2ª, 3ª e 4ª
# na e trabalha-se 3ª, 4ª e 5ª
# na c trabalha-se 2ª, 5ª e 6ª

# pensei um padrªo alternativo, sempre 3 dias seguidos
# no x trabalha-se 4ª, 5ª e 6ª
# no z trabalha-se 2ª, 3ª e 4ª

a <- c(1,1,1,NA,NA,NA,NA) # periodicidade 5. a cada 5 semanas volta  a posição inicial.
b <- c(1,1,NA,NA,1,NA,NA)
c <- c(1,NA,NA,1,1,NA,NA)
d <- c(NA,NA,1,1,1,NA,NA)
e <- c(NA,1,1,1,NA,NA,NA)

x <- c(NA,NA,1,1,1,NA,NA) # periodicidade 3.
y <- c(NA,1,1,1,NA,NA,NA)
z <- c(1,1,1,NA,NA,NA,NA)

# num_dias, é o número de dias que terá a tabela. 

# criando as colunas com o nome de cada auditor
afre_1 <- recode(rep(c(a,b,c,d,e), len = num_dias), '1' = aud_1)
afre_2 <- recode(rep(c(b,c,d,e,a), len = num_dias), '1' = aud_2)
afre_2_i <- recode(rep(c(b,c,d,e,a), len = num_dias), '1' = aud_3)
afre_3 <- recode(rep(c(c,d,e,a,b), len = num_dias), '1' = aud_4)
afre_4 <- recode(rep(c(d,e,a,b,c), len = num_dias), '1' = aud_5)
afre_5 <- recode(rep(c(e,a,b,c,d), len = num_dias), '1' = aud_6)
afre_5_i <- recode(rep(c(e,a,b,c,d), len = num_dias), '1' = aud_7)

# coluna dia, começando em 2 de abril de 2018, sempre uma segunda-feira
data <- seq(as.Date(data_seg_feira), by = "day", length.out = num_dias)

# coluna dia da semana sem s?bado e domingo
dia_semana <- wday(as.Date(data), label=TRUE, abbr = FALSE)


escala_plantao <- data_frame(data, dia_semana, afre_1, afre_2, afre_2_i, afre_3,
                             afre_4, afre_5, afre_5_i) %>% 
  
  # colocando todos afre numa coluna chamada Auditor
  gather(AFRE, Auditor, c(3:9)) %>% 
  # retirando os dias em que um auditor não vai
  filter(!is.na(Auditor)) %>% 
  # retirando os dias em que estou de férias: de 2018-07-16 até 2018-08-05
  filter(!(Auditor == "Mario" & data %in% ymd("2018-07-18", "2018-07-19", "2018-07-20", "2018-07-24",
                                              "2018-07-25", "2018-07-26", "2018-07-30", "2018-07-31", 
                                              "2018-08-01")))
# fazendo a tabela no DT
tabela <- DT::datatable(select (escala_plantao, -c(3)), filter = 'top',
              rownames = FALSE, class = 'cell-border stripe',
              colnames = c('Data', 'Dia da semana', 'Auditor'),
              options = list(bPaginate = FALSE)) %>% 
  formatStyle(columns = c(1, 2, 3), fontSize = '250%') 
return(tabela)
}

escala_de_plantao(aud_1 = "Helder", aud_2 = "Fernando", aud_3 = "Alexandre", 
                              aud_4 = "Mauro", aud_5 = "Mario", aud_6 = "Philipe", aud_7 = "Raphael", 
                              data_seg_feira = "2018-04-02", num_dias = 180)


