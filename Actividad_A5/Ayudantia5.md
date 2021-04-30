Ayudantia 5 Clusters
================

# Actividad Ayudantia 5

Realizar análisis de clustering (K-means, incluye preprocesamiento de la
data) e índices de evaluación para el archivo “sandwiches.csv” tomando
las columnas de nota y precio. Hacer análisis para diferentes K y/o
medidas de distancia para que vean cómo se comporta el clustering (En
caso de tener algún problema con ese csv, pueden utilizar el csv de
Pokémon también para la actividad)

# Algoritmo de clustering base:

## K-Medias

Para el análisis de clusters vamos a analizar la data de “pokemon.csv”
que contiene la información de los pokemones de 7 de sus generaciones,
echaremos un vistazo a las variables presentes.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.5
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
data_pok  <- read.csv(file='C:/Users/Javiera/Desktop/RAMOS 2021/Minería/proyectos_mineria/Actividad_A5/sanguchez.csv',sep=";")

head(data_pok)
```

    ##                                                               url
    ## 1          https://365sanguchez.com/abocado-cantina-buenos-aires/
    ## 2                   https://365sanguchez.com/alba-hotel-matanzas/
    ## 3   https://365sanguchez.com/albedrio-restaurant-santiago-centro/
    ## 4 https://365sanguchez.com/albedrio-restaurant-santiago-centro-2/
    ## 5              https://365sanguchez.com/aldea-nativa-providencia/
    ## 6            https://365sanguchez.com/aleman-experto-providencia/
    ##                 Local                                         Direccion  Precio
    ## 1     Abocado Cantina   C1125AAE, French 2316, C1125AAF CABA, Argentina $5.210.
    ## 2          Alba Hotel   Carlos Ibañez del Campo s/n – Matanzas, Navidad  $7.000
    ## 3 Albedrio Restaurant     Huérfanos 640, Santiago, Región Metropolitana  $7.290
    ## 4 Albedrío Restaurant Pasaje Huerfanos 640 edificio B local 5, Santiago  $8.690
    ## 5        Aldea Nativa  Tobalaba 1799, Providencia, Región Metropolitana  $4.900
    ## 6      Alemán Experto Av. Pedro de Valdivia 1683, Providencia, Santiago  $6.500
    ##                                                                                                                  Ingredientes
    ## 1                                               Suprema de pollo dulce, espinaca, crema ácida, repollo avinagrado y guacamole
    ## 2                     Carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada y queso derretido
    ## 3                          Mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada
    ## 4                          Queso Mozzarella, Rúcula, Champiñon portobello relleno de cheddar y luego apanado en panko y frito
    ## 5 Tofu asado no transgénico, palta, tomate, champiñones, mix de hojas verdes orgánicas,  mayonesa de zanahoria vegana casera,
    ## 6                           Hamburguesa, queso Cheddar, cebolla caramelizada, berros, pepinillos y salsa Jack Daniel’s Honey.
    ##   nota
    ## 1    3
    ## 2    3
    ## 3    4
    ## 4    4
    ## 5    4
    ## 6    3
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               texto
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Ojo acá! En la sanguchería “Abocado” (@AbocadoCantina) de Recoleta, más que un sándwich exquisito (que igual estaba bueno), descubrí una maravilla para copiar: acá el apanado, el frito del pollo, era dulce. Y bien crocante. Exquisito. Les juro que es el mejor apanado que he probado en mi vida. A esta suprema de pollo dulce, la acompañaban con espinaca (yo la hubiese puesto a la crema), crema ácida, repollo avinagrado y guacamole. Lamentablemente, la palta acá en Argentina no es como la chilena. Es más aguachenta. Y el pan, nuevamente sigue la línea que me ha tocado en este país, que no logra ser del nivel que tenemos en Chile. Pero insisto: ese batido hay que exportarlo. Estaba exquisito. Y sigo pensando en él.
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Aprovechando que me escapé a Matanzas con @catabarra_ a canjear mi regalo de cumpleaños (clases de surf), quise probar algunos sanguchitos de la zona. Y como hace un año me quedé a alojar en @albahotelboutique y tuve una muy buena experiencia, hoy quise darle una oportunidad a su carta de comida. Y a pesar de que en general nos fue bastante mal (3 de los platos andaban muuuy bajos), mi sanguchito salvó muy bien. Y es que la mezcla de carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada en marraqueta (y le sumé queso derretido), es demasiado buena. No falla. Así que de 1 a 5, este se lleva 3 narices de chancho. Es decir, es un buen sándwich. Vaya a probarlo con confianza. Una marrquetita crujiente y de poca miga, una mechada muy suave y harto queso son sus puntas de lanzas. Sí, hay cosas por mejorar. Por ejemplo, las “mechas” de la carne como que se pegaban unas a otras, entonces a veces de un mordisco te llevabas casi toda la carne. O el caldo lo haría más intenso. Porque lo que chorreaba aquí eran los champiñones más que la carne. Pero apaña harto, además que estás comiendo EN la playa misma.
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Sin duda, uno de los lugares que me ENCANTA visitar. Lejos la mejor hamburguesa que tienen es la Portobello (con un champiñón frito relleno de Cheddar y todo), pero como no estamos en temporada de hongos, no había ahora. Esa, sin duda, se lleva cinco narices. Hoy vine a @RestoAlbedrio con@MaxKbzon y nos dimos la torta comiendo. Él fue por un sándwich de prieta con manzana verde, papas hilo y mayo de ají verde. Yo, una burger “Picante”, con mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada. Solo les adelanto una cosa: tienen una salsa de reducción de cerveza con jugo de piña y azúcar rubia, que debiesen venderla en bidones!! Es EXQUISITA!
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Con @nitanzorron fuimos a probar esta maravilla de @albedrio_resto. Anoten: hamburguesa casera, queso mozzarella, rúcula y champiñon portobello relleno de cheddar y luego apanado en panko y frito . Una maravilla! Es que los champiñones rellenos ya son atómicos… Pero ahora que vienen fritos, tienes un sabor estratosférico. La mejor idea del mundo. Es una verdura muy “carnosa” y rica, realzada por el queso y el apanado. El toque amargo de la rúcula viene bien, y la hamburguesa en sí, creo que es la más jugosa que he probado. Me recordó a la de Ciudad Vieja. Anda perfecta. El pan Brioche, bien dulce, y de miga consistente. No tan aireada. Mi único punto a mejorar es que sentí que era muy “aguado” (los champiñones tienen alto porcentaje de agua), por lo que me faltó malicia. Un picante, o una salsa de ajo… No sé. Algo que te vuele la cabeza. De hecho, Albedrío tiene dos salsas que creo que pondrían a esta hamburguesa en el top chileno: la de la casa, que es una reducción de cerveza, pulpa de piña y azúcar rubia, y una mayonesa con cilantro y ajo que es perfecta. Con @nitanzorron conversamos que agregando esa salsa, el sandwich sube de nivel a SS3. Muy buena. Vayan a ver nuestra visita a mi canal de YouTube (link en mi perfil) para todos los detalles y comenten si les tinca porque encuentro que es mega creativa y muuuuy rica.
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Ojo los vegetarianos!! Porque gracias a@genoveva_tenaillon (síganla si quieren ver unas recetas exquisitas pero saludables al mismo tiempo) que me pasó el dato, encontré el templo de los sándwiches vegetarianos y jugos naturales wenos wenos wenos. Es Aldea Nativa, en Tobalaba, y a pesar de que es 99% más probable que prefiera un sándwich carnívoro, creo que los que probé acá son de los mejorcitos que me han tocado (hasta ahora, en La Tegualda están los mejores). El Barros Luco de la Geno estaba bien bueno (con carne de libre pastoreo, sin hormonas ni antibióticos… Y no, claramente este no era veggie jaja), pero me gustó más el mío: tofu asado no transgénico, palta, tomate, champiñones, mix de hojas verdes orgánicas, y le sumé una mayonesa de zanahoria vegana casera, que viene con todos los sándwiches (échensela entera). A ver. Era rico, pero la nota se la lleva principalmente porque es el mejor tofu que he probado en Chile. En general lo cocinan muy fome, pero este estaba marinado en soya y asado a la plancha, así que tenía un gustito distinto al típico “quesillo sin sabor” jajaj. Además, venía como con un cevichito de champiñones que también se lo puse adentro  y agarró una jugosidad que el pan agradeció. Con estos dos ingredientes que le puse, las verduras agarraron un aliño exquisito. De los vegetarianos que he probado, es de los más ricos. Pero si no te gusta el Tofu, también puedes probar alguna de las hamburguesas vegetarianas que tienen. Me gustó harto el lugar, además de que también es un mercado donde venden miles de productos orgánicos, vegetarianos y de esa onda.
    ## 6 Salsa de bourbon: checkAlemán ExpertoCómo llegué a Alemán ExpertoYa había venido un par de veces al Alemán Experto. Tanto al local de Santa Magdalena, como a este de Pedro de Valdivia. En todas las visitas tuve suerte dispar. En algunos me gustó harto, otras no tanto.La cosa es que hoy tuve que hacer trámites cerca, y como tenía poco tiempo para buscar una sanguchería, preferí ir al Alemán Experto, que aún no lo sumaba a 365 Sánguchez.Fotos tomadas con mi celular Sony Xperia XRestaurante y sanguchería Alemán ExpertoAlemán Experto es una sanguchería que cuenta con dos locales. El primero está en Santa Magdalena, y el otro en Pedro de Valdivia, en la esquina con Francisco Bilbao. Ojo, que también están abriendo uno en La Dehesa.Este restaurante es, tal como lo dice su nombre, bien alemán. Es decir, abundan los sánguches grandes y la cerveza.Hablando del local de Pedro de Valdivia, siento que hicieron un gran trabajo con su fachada exterior. Si no me creen, miren la foto de más arriba. Y es que la terraza que sacaron está increíble. Además, por su ubicación, siempre hay gente, por lo que me tinca que se arma buen ambiente para los after office.Les dejo su pagina web. Carta de sándwiches Alemán ExpertoLa carta de sándwiches del Alemán Experto es amplia, tanto en sus bases, como también en sus combinaciones gourmet y clásicas.Por el lado más jugado, la sanguchería Alemán Experto cuenta con hamburguesas y mechadas BBQ. De las primeras, destacan una que tiene camarones y queso azul ($6.400), y la que pedí yo. Se llama Jack Daniel’s Honey, y tiene una salsa basada en ese licor, además de queso Cheddar, berros, cebolla caramelizada y pepinillos.En las mechadas BBQ, hay dos opciones. una con tocino crispy, y la otra con queso Azul y aros de cebolla.Luego de esta sección más “gourmet”, Alemán Experto también cuenta con hamburguesas, churrascos, lomitos, aves, salchichas y mechadas para poder armarlas como italianos, lucos y chacareros.Para terminar, hay una sección de sándwiches vegetarianos. Son hamburguesas de quinoa, y tiene cuatro combinaciones distintas. Hamburguesa Jack Daniel’s Honey en Alemán ExpertoA pesar de no ser un fanático del bourbon, admito que sí me gusta esta variante con toques de miel. Y en una salsa, mejor aún.Tengo que decir que es un sándwich correcto. La salsa no se roba el protagonismo, y aporta un toque justo de dulzor y también de “malicia”.La cebolla caramelizada estaba suave, y los berros perfectos para contrastar el frescor con lo dulce de la cebolla y la salsa.Lo que no me gustó tanto, es que la hamburguesa estaba un poco seca. Tuvo suerte, eso sí, de que venía acompañada con harta salsa, por lo que lograba pasar piola. Pero si nos quedamos en la carne, le falta.Y el otro punto negativo, y esto ya parece que es una maldición que me persigue, fue el queso Cheddar. Primero, porque no estaba derretido. Cueck. Y segundo, porque su sabor era demasiado plástico. Les prometo que tengo ganas de hacer una cata de quesos Cheddar, quizás con Daniel Greve, para poderles recomendar cuáles son buenos. Pero este, no.Maridaje con Cerveza Austral LagerEn resumen: Alemán Experto puede ser experto en otras cosas, pero no en hamburguesasRecién ahora, que estoy escribiendo estas líneas, me doy cuenta que quizás hice una movida tonta. Si voy a un lugar que se llama Alemán Experto, lo normal sería haber pedido un lomito. Con chucrut, con pepinillos… algo por ahí.Se supone que los alemanes también le pegan a las fricandelas, pero este no fue el caso. De hecho, la carne no era tan especiada como suele serlo en ese país. Pero aún así, me tinca que el lomito aquí puede ser un gran acierto.Quedé con ganas de volver. Volver y probar otra proteína, como el lomito o la mechada. Así que nos vemos pronto, Alemán Experto.

``` r
summary(data_pok)
```

    ##      url               Local            Direccion            Precio         
    ##  Length:410         Length:410         Length:410         Length:410        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Ingredientes            nota          texto          
    ##  Length:410         Min.   :1.000   Length:410        
    ##  Class :character   1st Qu.:3.000   Class :character  
    ##  Mode  :character   Median :3.000   Mode  :character  
    ##                     Mean   :3.167                     
    ##                     3rd Qu.:4.000                     
    ##                     Max.   :5.000                     
    ##                     NA's   :8

Para clusterizar vamos a seleccionar las variables Nota y precio

Antes de clusterizar debemos preparar la data:

Pasar el precio a numero

Eliminar datos faltantes (NAs).

Escalar la data

``` r
data <- subset(data_pok, select = -c(texto, url, Local, Direccion, Ingredientes))
#summary(data)
#head(data)

data$Precio <- as.numeric(gsub('[$.]', '', data$Precio))
```

    ## Warning: NAs introducidos por coerción

``` r
#head(data)
#summary(data)
data <- data[complete.cases(data), ]
summary(data)
```

    ##      Precio           nota      
    ##  Min.   :  600   Min.   :1.000  
    ##  1st Qu.: 4892   1st Qu.:3.000  
    ##  Median : 5990   Median :3.000  
    ##  Mean   : 6061   Mean   :3.154  
    ##  3rd Qu.: 7100   3rd Qu.:4.000  
    ##  Max.   :14500   Max.   :5.000

Ya con la columna precios limpia podemos proceder a escalar los datos y
luego aplicar el algoritmo

``` r
numdata <- data[, colnames(data) %in% c("Precio","nota")]

escal_data = scale(numdata) %>% as_tibble()

escal_data %>% summary()
```

    ##      Precio              nota        
    ##  Min.   :-2.93234   Min.   :-1.9565  
    ##  1st Qu.:-0.62728   1st Qu.:-0.1396  
    ##  Median :-0.03792   Median :-0.1396  
    ##  Mean   : 0.00000   Mean   : 0.0000  
    ##  3rd Qu.: 0.55815   3rd Qu.: 0.7689  
    ##  Max.   : 4.53193   Max.   : 1.6774

# Analisis Cluster K = 10

``` r
modelo_kmeans <- kmeans(escal_data, centers = 10)
modelo_kmeans2 <- kmeans(numdata, centers = 10)

# creo la variable cluster en la tabla escal_data_pokda
escal_data$clus <- modelo_kmeans$cluster %>% as.factor()
numdata$clus <- modelo_kmeans2$cluster %>% as.factor()

ggplot(escal_data, aes(Precio, nota, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(numdata, aes(Precio, nota, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
info_clus <- modelo_kmeans$centers
info_clus2 <- modelo_kmeans2$centers

info_clus
```

    ##        Precio        nota
    ## 1   0.7065548  1.67739075
    ## 2  -0.1969180 -0.13957109
    ## 3   0.3608325  0.76890983
    ## 4  -1.5139762 -0.02309918
    ## 5  -0.5797998  1.26444487
    ## 6  -0.7371848 -1.30479662
    ## 7   2.9209397  0.20110925
    ## 8   0.9638799 -1.04805201
    ## 9   0.9485772 -0.05050433
    ## 10  0.6610727 -1.95653293

``` r
info_clus2
```

    ##       Precio     nota
    ## 1  13433.333 3.333333
    ## 2   4966.250 3.031250
    ## 3   3759.412 2.882353
    ## 4   2300.000 3.111111
    ## 5   7780.435 3.326087
    ## 6   4417.097 2.838710
    ## 7   6847.969 3.234375
    ## 8   5529.355 3.451613
    ## 9   6018.939 3.257576
    ## 10  9075.758 2.969697

# Evolución suma de cuadrados intra-cluster en la medida que aumentamos el numero de k

``` r
SSinterior <- numeric(30)

for(k in 1:30){
  modelo <- kmeans(escal_data, centers = k)
  SSinterior[k] <- modelo$tot.withinss
}

plot(SSinterior)
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Evaluacion

## Inspeccion visual

``` r
escal_data$clus <- as.numeric(escal_data$clus)
numdata$clus <- as.numeric(numdata$clus)

# uso distancia euclidiana
tempDist <- dist(escal_data) %>% as.matrix()

#reordeno filas y columnas en base al cluster obtenido
index <- sort(modelo_kmeans$cluster, index.return=TRUE)
tempDist <- tempDist[index$ix,index$ix]
rownames(tempDist) <- c(1:nrow(escal_data))
colnames(tempDist) <- c(1:nrow(escal_data))

image(tempDist)
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> \#
Estadistico de Hopkins.

``` r
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.0.5

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
res <- get_clust_tendency(escal_data, n = 30, graph = FALSE)
res2 <- get_clust_tendency(numdata, n = 30, graph = FALSE)

print(res)
```

    ## $hopkins_stat
    ## [1] 0.955539
    ## 
    ## $plot
    ## NULL

# Indice de correlacion

``` r
tempMatrix <- matrix(0, nrow = nrow(numdata), ncol = nrow(numdata))
tempMatrix[which(index$x==1), which(index$x==1)]  <- 1
tempMatrix[which(index$x==2), which(index$x==2)]  <- 1
tempMatrix[which(index$x==3), which(index$x==3)]  <- 1
tempMatrix[which(index$x==4), which(index$x==4)]  <- 1
tempMatrix[which(index$x==5), which(index$x==5)]  <- 1
tempMatrix[which(index$x==6), which(index$x==6)]  <- 1
tempMatrix[which(index$x==7), which(index$x==7)]  <- 1
tempMatrix[which(index$x==8), which(index$x==8)]  <- 1
tempMatrix[which(index$x==9), which(index$x==9)]  <- 1
tempMatrix[which(index$x==10), which(index$x==10)] <- 1

#construyo matriz de disimilitud
tempDist2 <- 1/(1+tempDist)

#Calcula correlacion 
cor <- cor(tempMatrix[upper.tri(tempMatrix)],tempDist2[upper.tri(tempDist2)])

print(cor)
```

    ## [1] 0.8316983

# Indice de cohesión y el de separación

``` r
library(flexclust)
```

    ## Warning: package 'flexclust' was built under R version 4.0.5

    ## Loading required package: grid

    ## Loading required package: lattice

    ## Loading required package: modeltools

    ## Loading required package: stats4

``` r
withinCluster <- numeric(10)
for (i in 1:10){
  tempdata <- escal_data[which(modelo_kmeans$cluster == i),]
  withinCluster[i] <- sum(dist2(tempdata,colMeans(tempdata))^2)
}
cohesion = sum(withinCluster)
#es equivalente a model$tot.withinss en k-means
print(c(cohesion, modelo_kmeans$tot.withinss))
```

    ## [1] 100.3463 100.3463

# Coeficiente de silueta

``` r
library(cluster)

coefSil <- silhouette(modelo_kmeans$cluster,dist(escal_data))
summary(coefSil)
```

    ## Silhouette of 358 units in 10 clusters from silhouette.default(x = modelo_kmeans$cluster, dist = dist(escal_data)) :
    ##  Cluster sizes and average silhouette widths:
    ##        22        69        43        39        44        46         8        18 
    ## 0.8036289 0.7515028 0.6960289 0.6003214 0.6144470 0.6828715 0.4842137 0.6094235 
    ##        51        18 
    ## 0.6266702 0.7406177 
    ## Individual silhouette widths:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2955  0.6317  0.6852  0.6745  0.7461  0.8498

``` r
fviz_silhouette(coefSil) + coord_flip()
```

    ##    cluster size ave.sil.width
    ## 1        1   22          0.80
    ## 2        2   69          0.75
    ## 3        3   43          0.70
    ## 4        4   39          0.60
    ## 5        5   44          0.61
    ## 6        6   46          0.68
    ## 7        7    8          0.48
    ## 8        8   18          0.61
    ## 9        9   51          0.63
    ## 10      10   18          0.74

![](Ayudantia5_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> \#\#
Utilizamos el coeficiente de silueta para encontrar el mejor valor de K

``` r
coefSil=numeric(30)
for (k in 2:30){
  modelo <- kmeans(escal_data, centers = k)
  temp <- silhouette(modelo$cluster,dist(escal_data))
  coefSil[k] <- mean(temp[,3])
}
tempDF=data.frame(CS=coefSil,K=c(1:30))

ggplot(tempDF, aes(x=K, y=CS)) + 
  geom_line() +
  scale_x_continuous(breaks=c(1:30))
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> \# 2do
Análisis Cluster En este nuevo caso con K = 4

``` r
numdata2 <- data[, colnames(data) %in% c("Precio", "nota")]
escal_data2 = scale(numdata2) %>% as_tibble()

modelo_kmean <- kmeans(escal_data2, centers = 4)
modelo_kmean2 <- kmeans(numdata2, centers = 4)

# creo la variable cluster en la tabla escal_data_pokda
escal_data2$clus2 <- modelo_kmean$cluster %>% as.factor()
numdata2$clus2 <- modelo_kmean2$cluster %>% as.factor()

ggplot(escal_data2, aes(Precio, nota, color=clus2)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot(numdata2, aes(Precio, nota, color=clus2)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
info_clusters <- modelo_kmean$centers
info_clusters2 <- modelo_kmean2$centers

info_clusters
```

    ##       Precio       nota
    ## 1  1.2376683 -0.2377852
    ## 2 -0.1735463 -0.8539322
    ## 3 -1.2395099  0.0523615
    ## 4  0.1741944  1.1852969

``` r
info_clusters2
```

    ##     Precio     nota
    ## 1 7306.562 3.260417
    ## 2 3700.595 2.916667
    ## 3 9384.211 3.052632
    ## 4 5720.143 3.250000

# Evaluacion

## Inspeccion visual

``` r
escal_data2$clus <- as.numeric(escal_data2$clus2)
numdata2$clus <- as.numeric(numdata2$clus2)

# uso distancia euclidiana
tempDist_2 <- dist(escal_data2) %>% as.matrix()

#reordeno filas y columnas en base al cluster obtenido
index <- sort(modelo_kmean$cluster, index.return=TRUE)
tempDist_2 <- tempDist_2[index$ix,index$ix]
rownames(tempDist_2) <- c(1:nrow(data))
colnames(tempDist_2) <- c(1:nrow(data))

image(tempDist_2)
```

![](Ayudantia5_files/figure-gfm/unnamed-chunk-16-1.png)<!-- --> \#\#
Estadistico de hopkins

``` r
library(factoextra)

escal_data2$clus2 <- NULL
numdata2$clus2 <- NULL

#Calcula el hopkins statistic 
res_1 <- get_clust_tendency(escal_data2, n = 30, graph = FALSE)
res_2 <- get_clust_tendency(numdata2, n = 30, graph = FALSE)

print(res_1)
```

    ## $hopkins_stat
    ## [1] 0.9400224
    ## 
    ## $plot
    ## NULL

``` r
print(res_2)
```

    ## $hopkins_stat
    ## [1] 0.9448255
    ## 
    ## $plot
    ## NULL

## Indice de correlacion

``` r
tempMatrix2 <- matrix(0, nrow = nrow(escal_data2), ncol = nrow(escal_data2))
tempMatrix2[which(index$x==1), which(index$x==1)]  <- 1
tempMatrix2[which(index$x==2), which(index$x==2)]  <- 1
tempMatrix2[which(index$x==3), which(index$x==3)]  <- 1
tempMatrix2[which(index$x==4), which(index$x==4)]  <- 1
tempMatrix2[which(index$x==5), which(index$x==5)]  <- 1

#construyo matriz de disimilitud
tempDist_22 <- 1/(1+tempDist_2)

#Calcula correlacion 
cor2 <- cor(tempMatrix2[upper.tri(tempMatrix2)],tempDist_2[upper.tri(tempDist_2)])

print(cor2)
```

    ## [1] -0.6898495

# Coeficiente de silueta

``` r
library(cluster)

coefSil2 <- silhouette(modelo_kmean$cluster,dist(escal_data2))
summary(coefSil2)
```

    ## Silhouette of 358 units in 4 clusters from silhouette.default(x = modelo_kmean$cluster, dist = dist(escal_data2)) :
    ##  Cluster sizes and average silhouette widths:
    ##        74       117        71        96 
    ## 0.4709823 0.4031023 0.4851278 0.5838082 
    ## Individual silhouette widths:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.1240  0.3933  0.4717  0.4819  0.5945  0.6772

``` r
fviz_silhouette(coefSil2) + coord_flip()
```

    ##   cluster size ave.sil.width
    ## 1       1   74          0.47
    ## 2       2  117          0.40
    ## 3       3   71          0.49
    ## 4       4   96          0.58

![](Ayudantia5_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
