
setwd("~/Diplomado 2021/Ayudantia 23 Octubre")

# descargar la pagina -> hacer un scraping para mercado libre 
# interaccion con la pagina web
# extraccion masiva paginas web -> sitemap (mapa de la pagina web)


library(tidyverse)       # Conjunto de paquetes de DS
library(rvest)           # Libreria para hacer web scraping de forma estatica

# descargar lo que veo  ---------------------------------------------------

web <- read_html("https://www.mercadolibre.cl/ofertas?promotion_type=DEAL_OF_THE_DAY&page=1")

# ¿Cuantas paginas tiene esta pagina web?
# inspeccionar -> con F12

n_paginas <- web %>% 
  html_nodes(xpath = "//li[@class = 'andes-pagination__button']") %>% 
  html_text2() %>% 
  as.numeric() %>% 
  max()

# Extraer el precio del producto 
web %>% 
  html_nodes(xpath ="//div[@class = 'promotion-item__description']/div[2]/span") %>% 
  html_text2()

web %>% 
  html_nodes(xpath ="//div[@class = 'promotion-item__description']/p")
  

