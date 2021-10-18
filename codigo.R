
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

library("data.table")
library("RSelenium")

# se finalizan los procesos Java (para finalizar los procesos que estén en java ocultos)
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# para establecer la conexion con google chrome
driver <- RSelenium::rsDriver(browser = "chrome",
                              chromever = "94.0.4606.61")

# remote_driver se define para poder establecer la conexion de interaccion con
# una pagina web
# cliente (lo que se ve por fuera) 
# vs servidor (por dentro lo que esta)

remote_driver <- driver[["client"]]

# trabajaremos con la pagina de seguros ripley porque tiene listas desplegables

# indicamos cual es la pagina con la cual queremos trabajar
remote_driver$navigate("https://www.segurosripley.cl/cotizar")

# observamos que debemos cargar varios valores, para ello primero averiguaremos como ingresar cada uno 

# MARCA DEL VEHICULO

# primero realizamos un click en la casilla
x <- remote_driver$findElement('xpath','//div[@class = "column is-8"]/div/div/div[1]/div/div/select')$selectTag()

# cuando yo me paro en un "select" debo indicarle parte selectTag para poder 
# extraer todo lo proveniente de esa lista


# seleccionamos las opciones de texto de la casilla de vehiculo
opcion <- tibble(Marca = x$text, # tibble para poder aplicar tidyverse
                 valor = x$value) %>% 
  filter(Marca == "JEEP") %>% # existe homologacion en la forma en la cual se escriben las marcas de los vehiculos
  pull(valor) # es para regresar una variable como vector


# entonces para poder aperturar la lista desplegable e ingresar el valor que queremos
remote_driver$findElement('xpath',
                          paste0('//div[@class = "column is-8"]/div/div/div[1]/div/div/select/option[@value = "',opcion,'"]'))$clickElement()


# MODELO DEL VEHICULO
y <- remote_driver$findElement('xpath','//div[@class = "column is-8"]/div/div/div[2]/div/div/select')$selectTag()

opcion <- tibble(Modelo = y$text,
                 valor = y$value) %>% 
  filter(Modelo == "WRANGLER") %>% 
  slice(1) %>% # porque tengo dos opciones pero me quedo con la primera (cualquiera funciona)
  pull(valor)


# entonces para poder aperturar la lista desplegable e ingresar el valor que queremos
remote_driver$findElement('xpath',
                          paste0('//div[@class = "column is-8"]/div/div/div[2]/div/div/select/option[@value = "',opcion,'"]'))$clickElement()


# AÑO DEL VEHICULO
# en el año tambien se puede hacer lo mismo, pero pondremos el ejemplo de un año fijo

remote_driver$findElement('xpath',
                          paste0('//div[@class = "column is-8"]/div/div/div[3]/div/div/select/option[@value = "2020"]'))$clickElement()


# RELLENAR RUT
RUT = "19630884-9"
# encontrar que es lo caracteristico de la casilla del rut
# el place holder es informacion traslucida que se mostrara y es representativo de la cajita del rut
remote_driver$findElement('xpath','//input[@placeholder = "12345678-9"]')$clickElement()
Sys.sleep(runif(1,0.2,0.7)) # para no ingresar el rapido el rut

rut <- RUT %>% # para dejar por separado los digitos del rut
  str_split("") %>% 
  unlist() # lo saco de la lista en la cual quedo guardado

for(i in rut){
  remote_driver$findElement('xpath','//input[@placeholder = "12345678-9"]')$sendKeysToElement(list(i)) 
  Sys.sleep(runif(1,1,1.5)) 
}



# FECHA DE NACIMIENTO
# lo mismo que el rut
# encontrar que es lo caracteristico de la casilla de nacimiento

FechaN = "10011997"

remote_driver$findElement('xpath','//input[@placeholder = "dd/mm/aaaa"]')$clickElement()

fecha_n <- FechaN %>% 
  str_split("") %>% 
  unlist()

for(i in fecha_n){
  remote_driver$findElement('xpath','//input[@placeholder = "dd/mm/aaaa"]')$sendKeysToElement(list(i))
  Sys.sleep(runif(1,1,1.5))
}


# CELULAR 
celular = "93946426"

remote_driver$findElement('xpath','//input[@placeholder = "+56991234567"]')$clickElement()

for(i in celular %>% str_split("") %>% unlist()){
  remote_driver$findElement('xpath','//input[@placeholder = "+56991234567"]')$sendKeysToElement(list(i))
  Sys.sleep(runif(1,1,1.5))
}


# CORREO
correo = "diego@gmail.com"

remote_driver$findElement('xpath','//input[@placeholder = "nombre@correo.com"]')$clickElement()

for(i in correo %>% str_split("") %>% unlist()){
  remote_driver$findElement('xpath','//input[@placeholder = "nombre@correo.com"]')$sendKeysToElement(list(i))
  Sys.sleep(runif(1,1,1.5))
}


# seleccionamos la casilla de cotizar y la presionamos

remote_driver$findElement('xpath','//button[@class = "button is-accent"]')$clickElement()

