
# Solucion para la ayudantia web scraping 


# Parte 1 -----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(wordcloud)

pagina <- read_html("https://www.mercadolibre.cl/ofertas?promotion_type=DEAL_OF_THE_DAY&page=1")

# Numero de pagina 
n_pagina <- pagina %>% 
  html_node(xpath = '//ul[@class="andes-pagination"]') %>% 
  html_children() %>% 
  html_text2() %>% 
  as.numeric() %>% 
  suppressWarnings() %>% 
  max(na.rm = T)

# Nombre del producto
nombre <- pagina %>% 
  html_nodes(xpath = '//p[@class="promotion-item__title"]') %>% 
  html_text2()

# Precio del producto 
precio <- pagina %>% 
  html_nodes(xpath = '//span[@class="promotion-item__price"]') %>% 
  html_text2() %>% 
  str_remove_all("\\$") %>% 
  str_trim() %>% 
  str_remove_all("\\.") %>% 
  as.numeric()

# url del producto

url <- pagina %>% 
  html_nodes(xpath = '//a[@class="promotion-item__link-container"]') %>% 
  html_attr("href")

# Vendedor del producto

# Opcion 1
pagina %>% 
  html_nodes(xpath = '//span[@class="promotion-item__seller"]') %>% 
  html_text2() %>% 
  str_remove_all("por") %>% 
  str_trim() # como no todos los productos tienen vendedor, no sabemos a cual corresponde

# Opcion 2
vendedor <- 1:length(nombre) %>% 
  map_dfr(.f = function(x){
    pagina %>% 
      html_nodes(xpath = paste0('//ol[@class="items_container"]/li[',x,']/a/div/div/span[@class ="promotion-item__seller"]')) %>% 
      html_text2() %>% 
      str_remove_all("por") %>% 
      str_trim() %>% 
      ifelse(identical(character(0), .), NA, .) %>% 
      tibble(Vendedor = .)
    })


# Generalizar para todas las paginas 
df <- 1:n_pagina %>% 
  map_dfr(.f = function(y){
    
    pagina <- read_html(paste0("https://www.mercadolibre.cl/ofertas?promotion_type=DEAL_OF_THE_DAY&page=",y))
    
    # Nombre del producto
    nombre <- pagina %>% 
      html_nodes(xpath = '//p[@class="promotion-item__title"]') %>% 
      html_text2()
    
    Sys.sleep(runif(1,0,0.5))
    
    # Precio del producto 
    precio <- pagina %>% 
      html_nodes(xpath = '//span[@class="promotion-item__price"]') %>% 
      html_text2() %>% 
      str_remove_all("\\$") %>% 
      str_trim() %>% 
      str_remove_all("\\.") %>% 
      as.numeric()
    
    Sys.sleep(runif(1,0,0.5))
    
    # url del producto
    
    url <- pagina %>% 
      html_nodes(xpath = '//a[@class="promotion-item__link-container"]') %>% 
      html_attr("href")
    
    Sys.sleep(runif(1,0,0.5))
    
    # vendedor del producto 
    vendedor <- 1:length(nombre) %>% 
      map_dfr(.f = function(x){
        pagina %>% 
          html_nodes(xpath = paste0('//ol[@class="items_container"]/li[',x,']/a/div/div/span[@class ="promotion-item__seller"]')) %>% 
          html_text2() %>% 
          str_remove_all("por") %>% 
          str_trim() %>% 
          ifelse(identical(character(0), .), NA, .) %>% 
          tibble(Vendedor = .)
      })
    
    Sys.sleep(runif(1,0,0.5))
    
    tibble(nombre, precio, vendedor, url)
    
  })

# stopwords = lista de exclusión

unas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

frecuencias <- df$nombre %>%
  str_c(., collapse = " ") %>% 
  tibble(nombres = .) %>% 
  unnest_tokens(input = nombres, output = palabra, strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) %>% 
  anti_join(unas_stopwords) 

wordcloud(words = frecuencias$palabra,
          freq = frecuencias$n,
          min.freq = 2,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(6, "Spectral"))

table(df$Vendedor) %>% 
  tibble(nombre = names(.),
         n = .) %>% 
  ggplot(aes(y = reorder(nombre, n), n)) +
  geom_col(fill = "#56bc8a") + # código hexadecimal 
  geom_text(aes(label = n), hjust = -0.5) +
  labs(y = NULL,
       title = "Vendedores con mayores productos en oferta") +
  theme_minimal() 




# Vamos a buscar bigramas (n-grama)

bigramas <- df$nombre %>%
  str_c(., collapse = " ") %>% 
  tibble(nombres = .)  %>% 
  unnest_ngrams(input = nombres, output = bigrama, n = 2) %>% 
  count(bigrama, sort = TRUE) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra2 %in% unas_stopwords$palabra, 
         !palabra1 %in% unas_stopwords$palabra) %>% 
  unite(col = "bigrama", c(palabra1, palabra2), sep = " ")


bigramas %>% 
  slice_max(n, n = 12) %>% 
  ggplot(aes(y = reorder(bigrama, n), n)) +
  geom_col(fill = "#56bc8a") + # código hexadecimal 
  geom_text(aes(label = n), hjust = -0.5) +
  labs(y = NULL,
       title = "bigramas mas frecuentes de productos en oferta") +
  theme_minimal() 




# Parte 2 -----------------------------------------------------------------

  

