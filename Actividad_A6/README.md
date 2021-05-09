Ayudantia 6: Clusters Jerárquicos
================

# Actividad de Ayudantia 6

Javiera Bustos

## Importar Librerias

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
library(cluster)
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.0.5

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(janitor)
```

    ## Warning: package 'janitor' was built under R version 4.0.5

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(dplyr)
```

## Cargar Datos:

``` r
wd = setwd("C:/Users/Javiera/Desktop/RAMOS 2021/Minería/proyectos_mineria/Actividad_A6")
load("beats.Rdata")
#tomamos como sample solo 10000 datos
data <- sample_n(beats, 10000)
summary(data)
```

    ##  artist_name         artist_id           album_id          album_type       
    ##  Length:10000       Length:10000       Length:10000       Length:10000      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  album_release_date album_release_year album_release_date_precision
    ##  Length:10000       Min.   :1919       Length:10000                
    ##  Class :character   1st Qu.:2010       Class :character            
    ##  Mode  :character   Median :2018       Mode  :character            
    ##                     Mean   :2013                                   
    ##                     3rd Qu.:2020                                   
    ##                     Max.   :2021                                   
    ##                     NA's   :9                                      
    ##   danceability        energy             key            loudness      
    ##  Min.   :0.0000   Min.   :0.00000   Min.   : 0.000   Min.   :-60.000  
    ##  1st Qu.:0.2580   1st Qu.:0.07708   1st Qu.: 2.000   1st Qu.:-24.448  
    ##  Median :0.3730   Median :0.20600   Median : 5.000   Median :-19.365  
    ##  Mean   :0.3935   Mean   :0.33766   Mean   : 5.074   Mean   :-18.644  
    ##  3rd Qu.:0.5140   3rd Qu.:0.56900   3rd Qu.: 8.000   3rd Qu.:-11.524  
    ##  Max.   :0.9650   Max.   :1.00000   Max.   :11.000   Max.   : -0.963  
    ##                                                                       
    ##       mode        speechiness       acousticness    instrumentalness 
    ##  Min.   :0.000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.000   1st Qu.:0.03770   1st Qu.:0.4078   1st Qu.:0.00142  
    ##  Median :1.000   Median :0.04420   Median :0.9250   Median :0.69700  
    ##  Mean   :0.683   Mean   :0.06786   Mean   :0.7007   Mean   :0.50266  
    ##  3rd Qu.:1.000   3rd Qu.:0.05780   3rd Qu.:0.9870   3rd Qu.:0.90100  
    ##  Max.   :1.000   Max.   :0.96900   Max.   :0.9960   Max.   :0.99800  
    ##                                                                      
    ##     liveness         valence            tempo          track_id        
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :  0.00   Length:10000      
    ##  1st Qu.:0.0976   1st Qu.:0.09345   1st Qu.: 82.67   Class :character  
    ##  Median :0.1235   Median :0.28500   Median :106.61   Mode  :character  
    ##  Mean   :0.2186   Mean   :0.34190   Mean   :109.06                     
    ##  3rd Qu.:0.2480   3rd Qu.:0.54200   3rd Qu.:131.64                     
    ##  Max.   :0.9980   Max.   :0.98900   Max.   :240.46                     
    ##                                                                        
    ##  analysis_url       time_signature   disc_number     duration_ms     
    ##  Length:10000       Min.   :0.000   Min.   : 1.00   Min.   :   3328  
    ##  Class :character   1st Qu.:4.000   1st Qu.: 1.00   1st Qu.: 121896  
    ##  Mode  :character   Median :4.000   Median : 1.00   Median : 193820  
    ##                     Mean   :3.725   Mean   : 1.21   Mean   : 227993  
    ##                     3rd Qu.:4.000   3rd Qu.: 1.00   3rd Qu.: 271172  
    ##                     Max.   :5.000   Max.   :26.00   Max.   :4777826  
    ##                                                                      
    ##   explicit        track_href         is_local        track_name       
    ##  Mode :logical   Length:10000       Mode :logical   Length:10000      
    ##  FALSE:9875      Class :character   FALSE:10000     Class :character  
    ##  TRUE :125       Mode  :character                   Mode  :character  
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##  track_preview_url   track_number        type            track_uri        
    ##  Length:10000       Min.   :  1.00   Length:10000       Length:10000      
    ##  Class :character   1st Qu.:  5.00   Class :character   Class :character  
    ##  Mode  :character   Median : 12.00   Mode  :character   Mode  :character  
    ##                     Mean   : 46.52                                        
    ##                     3rd Qu.: 30.00                                        
    ##                     Max.   :533.00                                        
    ##                                                                           
    ##  external_urls.spotify  album_name          key_name          mode_name        
    ##  Length:10000          Length:10000       Length:10000       Length:10000      
    ##  Class :character      Class :character   Class :character   Class :character  
    ##  Mode  :character      Mode  :character   Mode  :character   Mode  :character  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##    key_mode        
    ##  Length:10000      
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

``` r
head(data)
```

    ##              artist_name              artist_id               album_id
    ## 1  Johann Sebastian Bach 5aIqB5nVVvmFsvSdExz408 3Tk0clV8RnlytGZgBF9XYD
    ## 2   Ludwig van Beethoven 2wOqMjp9TyABvtHdOSOTUS 1TbwtdjaSkprz76cgE4s3h
    ## 3 Mother Nature Sound FX 4lrOh2SR7H3guHky2lAYOk 2loLE4JcxwwlSkttXqjmcd
    ## 4   Ludwig van Beethoven 2wOqMjp9TyABvtHdOSOTUS 5GISuKNCfAqdSmJZB2rRf7
    ## 5 Mother Nature Sound FX 4lrOh2SR7H3guHky2lAYOk 4ZA6QTymQQbmBNKKzZP6UO
    ## 6        Frédéric Chopin 7y97mc3bZRFXzT2szRM4L4 1M2zQlptpDYVQAdYu2trSj
    ##   album_type album_release_date album_release_year album_release_date_precision
    ## 1      album         2019-01-11               2019                          day
    ## 2      album         2018-07-06               2018                          day
    ## 3      album         2020-10-30               2020                          day
    ## 4      album         2020-06-09               2020                          day
    ## 5      album         2020-12-22               2020                          day
    ## 6      album               2002               2002                         year
    ##   danceability  energy key loudness mode speechiness acousticness
    ## 1        0.484 0.14200   7  -17.999    1      0.0406       0.9940
    ## 2        0.426 0.00539   2  -28.464    1      0.0431       0.9940
    ## 3        0.286 0.63900   9  -24.923    1      0.0605       0.0537
    ## 4        0.444 0.01060   4  -27.760    1      0.0560       0.9930
    ## 5        0.201 0.92800   7  -20.099    1      0.1790       0.1800
    ## 6        0.324 0.24500   2  -18.889    0      0.0317       0.9880
    ##   instrumentalness liveness valence   tempo               track_id
    ## 1           0.8470   0.1040  0.6030  69.936 4t8junU9an9m9GYjKq76bo
    ## 2           0.8940   0.0696  0.2000  77.290 2SBrZ2WratwnuZIFScXsuE
    ## 3           0.4490   0.3780  0.0361 135.222 0Gk0kh0QZqT346NhdNgQCx
    ## 4           0.9450   0.1490  0.0443 129.185 64L1bpndAkMNqHSyZlKONX
    ## 5           0.0505   0.4860  0.0362  91.886 4wbASrs14MUwGP13pbmPLv
    ## 6           0.9050   0.1690  0.1620 132.637 658NhXJTYiuNW9DdYYzCYa
    ##                                                       analysis_url
    ## 1 https://api.spotify.com/v1/audio-analysis/4t8junU9an9m9GYjKq76bo
    ## 2 https://api.spotify.com/v1/audio-analysis/2SBrZ2WratwnuZIFScXsuE
    ## 3 https://api.spotify.com/v1/audio-analysis/0Gk0kh0QZqT346NhdNgQCx
    ## 4 https://api.spotify.com/v1/audio-analysis/64L1bpndAkMNqHSyZlKONX
    ## 5 https://api.spotify.com/v1/audio-analysis/4wbASrs14MUwGP13pbmPLv
    ## 6 https://api.spotify.com/v1/audio-analysis/658NhXJTYiuNW9DdYYzCYa
    ##   time_signature disc_number duration_ms explicit
    ## 1              1           1      119173    FALSE
    ## 2              4           1      416573    FALSE
    ## 3              3           1       77740    FALSE
    ## 4              3           1      165960    FALSE
    ## 5              1           1      207996    FALSE
    ## 6              4           2      152120    FALSE
    ##                                                 track_href is_local
    ## 1 https://api.spotify.com/v1/tracks/4t8junU9an9m9GYjKq76bo    FALSE
    ## 2 https://api.spotify.com/v1/tracks/2SBrZ2WratwnuZIFScXsuE    FALSE
    ## 3 https://api.spotify.com/v1/tracks/0Gk0kh0QZqT346NhdNgQCx    FALSE
    ## 4 https://api.spotify.com/v1/tracks/64L1bpndAkMNqHSyZlKONX    FALSE
    ## 5 https://api.spotify.com/v1/tracks/4wbASrs14MUwGP13pbmPLv    FALSE
    ## 6 https://api.spotify.com/v1/tracks/658NhXJTYiuNW9DdYYzCYa    FALSE
    ##                                                                                                   track_name
    ## 1                      Goldberg Variations, BWV 988 (Arr. D. Sitkovetsky for String Trio): Var. 1, à 1 Clav.
    ## 2                                                                 Piano Sonata No. 2 in A Major, Op. 2 No. 2
    ## 3                                                                                                  Cosy Rain
    ## 4 Piano Sonata No. 28 in A Major, Op. 101: 3. Langsam und sehnsuchtsvoll (Adagio ma non troppo, con affetto)
    ## 5                                                                                             Lunar Elements
    ## 6                                                             Chopin: 24 Preludes, Op. 28: No. 24 in D Minor
    ##                                                                                             track_preview_url
    ## 1 https://p.scdn.co/mp3-preview/38718e69b84644ad53c61c58ab064a96cb3c9286?cid=ac26d97eca664234ab133e5208ea5737
    ## 2 https://p.scdn.co/mp3-preview/ef22976e59c162f4462df1e7bb7f21533ab3a90d?cid=ac26d97eca664234ab133e5208ea5737
    ## 3 https://p.scdn.co/mp3-preview/0ab1607a49fccd656ee21c31b8cc05f451961a46?cid=ac26d97eca664234ab133e5208ea5737
    ## 4                                                                                                        <NA>
    ## 5 https://p.scdn.co/mp3-preview/b782fea8ba0e942619674c789053cc98f6502b1d?cid=ac26d97eca664234ab133e5208ea5737
    ## 6 https://p.scdn.co/mp3-preview/cf1717bd1ca46aa682177b7691d5390cb7834b5f?cid=ac26d97eca664234ab133e5208ea5737
    ##   track_number  type                            track_uri
    ## 1           31 track spotify:track:4t8junU9an9m9GYjKq76bo
    ## 2            6 track spotify:track:2SBrZ2WratwnuZIFScXsuE
    ## 3           26 track spotify:track:0Gk0kh0QZqT346NhdNgQCx
    ## 4          210 track spotify:track:64L1bpndAkMNqHSyZlKONX
    ## 5           27 track spotify:track:4wbASrs14MUwGP13pbmPLv
    ## 6           30 track spotify:track:658NhXJTYiuNW9DdYYzCYa
    ##                                   external_urls.spotify
    ## 1 https://open.spotify.com/track/4t8junU9an9m9GYjKq76bo
    ## 2 https://open.spotify.com/track/2SBrZ2WratwnuZIFScXsuE
    ## 3 https://open.spotify.com/track/0Gk0kh0QZqT346NhdNgQCx
    ## 4 https://open.spotify.com/track/64L1bpndAkMNqHSyZlKONX
    ## 5 https://open.spotify.com/track/4wbASrs14MUwGP13pbmPLv
    ## 6 https://open.spotify.com/track/658NhXJTYiuNW9DdYYzCYa
    ##                              album_name key_name mode_name key_mode
    ## 1   Anniversary Box: Jörg Demus, Vol. 8        G     major  G major
    ## 2 Beethoven: The Complete Piano Sonatas        D     major  D major
    ## 3              ! ! ! ! : Land : ! ! ! !        A     major  A major
    ## 4           Summer Classical: Beethoven        E     major  E major
    ## 5       ! ! ! ! ! ! Cascade ! ! ! ! ! !        G     major  G major
    ## 6          Chopin: Nocturnes & Preludes        D     minor  D minor

# Pre Procesamiento de los Datos

## Limpieza Datos:

Para este dataset el proceso de limpieza de datos sera un poco mas
extensa por lo que debemos ir por partes

-   Primero verificar la existencia de valores NA o faltantes

``` r
data[data == ""] <- NA

# Verificamos donde hay valores NAs
data %>% 
  summarise_all(list(name = ~sum(is.na(.))))
```

    ##   artist_name_name artist_id_name album_id_name album_type_name
    ## 1                0              0             0               0
    ##   album_release_date_name album_release_year_name
    ## 1                       0                       9
    ##   album_release_date_precision_name danceability_name energy_name key_name
    ## 1                                 0                 0           0        0
    ##   loudness_name mode_name speechiness_name acousticness_name
    ## 1             0         0                0                 0
    ##   instrumentalness_name liveness_name valence_name tempo_name track_id_name
    ## 1                     0             0            0          0             0
    ##   analysis_url_name time_signature_name disc_number_name duration_ms_name
    ## 1                 0                   0                0                0
    ##   explicit_name track_href_name is_local_name track_name_name
    ## 1             0               0             0               0
    ##   track_preview_url_name track_number_name type_name track_uri_name
    ## 1                   3964                 0         0              0
    ##   external_urls.spotify_name album_name_name key_name_name mode_name_name
    ## 1                          0               0             0              0
    ##   key_mode_name
    ## 1             0

``` r
# De existir eliminamos todas las observaciones que presenten estos datos
data_pre <- data %>% 
  filter(!(is.na(track_name)|is.na(artist_name)|is.na(album_name)|is.na(duration_ms)))
# Corroboramos que no queden datos NA
data_pre %>% 
  summarise_all(list(name = ~sum(is.na(.))))
```

    ##   artist_name_name artist_id_name album_id_name album_type_name
    ## 1                0              0             0               0
    ##   album_release_date_name album_release_year_name
    ## 1                       0                       9
    ##   album_release_date_precision_name danceability_name energy_name key_name
    ## 1                                 0                 0           0        0
    ##   loudness_name mode_name speechiness_name acousticness_name
    ## 1             0         0                0                 0
    ##   instrumentalness_name liveness_name valence_name tempo_name track_id_name
    ## 1                     0             0            0          0             0
    ##   analysis_url_name time_signature_name disc_number_name duration_ms_name
    ## 1                 0                   0                0                0
    ##   explicit_name track_href_name is_local_name track_name_name
    ## 1             0               0             0               0
    ##   track_preview_url_name track_number_name type_name track_uri_name
    ## 1                   3964                 0         0              0
    ##   external_urls.spotify_name album_name_name key_name_name mode_name_name
    ## 1                          0               0             0              0
    ##   key_mode_name
    ## 1             0

-   Segundo filtrar y remover datos duplicados

``` r
data_pre <- data_pre[!duplicated(data_pre$track_id),]
```

## Revisar Estructura Datos

``` r
# Character
data_char <- c("artist_name", "artist_id", "album_id", "album_type", "album_release_date", "track_id", "track_href", "track_name", "type", "album_name", "key_name", "mode_name", "key_mode")
# Double
data_dou <- c("danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")
# Volvemos a borrar los datos que puedan haber quedado como NA con el cambio de tipo de variable
data_pre <- data_pre %>% 
  filter(!(is.na(key)|is.na(danceability)))
summary(data_pre)
```

    ##  artist_name         artist_id           album_id          album_type       
    ##  Length:9998        Length:9998        Length:9998        Length:9998       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  album_release_date album_release_year album_release_date_precision
    ##  Length:9998        Min.   :1919       Length:9998                 
    ##  Class :character   1st Qu.:2010       Class :character            
    ##  Mode  :character   Median :2018       Mode  :character            
    ##                     Mean   :2013                                   
    ##                     3rd Qu.:2020                                   
    ##                     Max.   :2021                                   
    ##                     NA's   :9                                      
    ##   danceability        energy             key            loudness      
    ##  Min.   :0.0000   Min.   :0.00000   Min.   : 0.000   Min.   :-60.000  
    ##  1st Qu.:0.2580   1st Qu.:0.07702   1st Qu.: 2.000   1st Qu.:-24.451  
    ##  Median :0.3730   Median :0.20600   Median : 5.000   Median :-19.369  
    ##  Mean   :0.3935   Mean   :0.33765   Mean   : 5.075   Mean   :-18.646  
    ##  3rd Qu.:0.5140   3rd Qu.:0.56900   3rd Qu.: 8.000   3rd Qu.:-11.531  
    ##  Max.   :0.9650   Max.   :1.00000   Max.   :11.000   Max.   : -0.963  
    ##                                                                       
    ##       mode         speechiness       acousticness    instrumentalness  
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.000000  
    ##  1st Qu.:0.0000   1st Qu.:0.03770   1st Qu.:0.4073   1st Qu.:0.001422  
    ##  Median :1.0000   Median :0.04420   Median :0.9250   Median :0.697000  
    ##  Mean   :0.6829   Mean   :0.06787   Mean   :0.7007   Mean   :0.502672  
    ##  3rd Qu.:1.0000   3rd Qu.:0.05780   3rd Qu.:0.9870   3rd Qu.:0.901000  
    ##  Max.   :1.0000   Max.   :0.96900   Max.   :0.9960   Max.   :0.998000  
    ##                                                                        
    ##     liveness          valence            tempo          track_id        
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :  0.00   Length:9998       
    ##  1st Qu.:0.09763   1st Qu.:0.09335   1st Qu.: 82.67   Class :character  
    ##  Median :0.12350   Median :0.28500   Median :106.63   Mode  :character  
    ##  Mean   :0.21855   Mean   :0.34192   Mean   :109.06                     
    ##  3rd Qu.:0.24800   3rd Qu.:0.54200   3rd Qu.:131.64                     
    ##  Max.   :0.99800   Max.   :0.98900   Max.   :240.46                     
    ##                                                                         
    ##  analysis_url       time_signature   disc_number     duration_ms     
    ##  Length:9998        Min.   :0.000   Min.   : 1.00   Min.   :   3328  
    ##  Class :character   1st Qu.:4.000   1st Qu.: 1.00   1st Qu.: 121876  
    ##  Mode  :character   Median :4.000   Median : 1.00   Median : 193764  
    ##                     Mean   :3.725   Mean   : 1.21   Mean   : 227951  
    ##                     3rd Qu.:4.000   3rd Qu.: 1.00   3rd Qu.: 271106  
    ##                     Max.   :5.000   Max.   :26.00   Max.   :4777826  
    ##                                                                      
    ##   explicit        track_href         is_local        track_name       
    ##  Mode :logical   Length:9998        Mode :logical   Length:9998       
    ##  FALSE:9873      Class :character   FALSE:9998      Class :character  
    ##  TRUE :125       Mode  :character                   Mode  :character  
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##  track_preview_url   track_number        type            track_uri        
    ##  Length:9998        Min.   :  1.00   Length:9998        Length:9998       
    ##  Class :character   1st Qu.:  5.00   Class :character   Class :character  
    ##  Mode  :character   Median : 12.00   Mode  :character   Mode  :character  
    ##                     Mean   : 46.53                                        
    ##                     3rd Qu.: 30.00                                        
    ##                     Max.   :533.00                                        
    ##                                                                           
    ##  external_urls.spotify  album_name          key_name          mode_name        
    ##  Length:9998           Length:9998        Length:9998        Length:9998       
    ##  Class :character      Class :character   Class :character   Class :character  
    ##  Mode  :character      Mode  :character   Mode  :character   Mode  :character  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##    key_mode        
    ##  Length:9998       
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

``` r
str(data_pre)
```

    ## 'data.frame':    9998 obs. of  36 variables:
    ##  $ artist_name                 : chr  "Johann Sebastian Bach" "Ludwig van Beethoven" "Mother Nature Sound FX" "Ludwig van Beethoven" ...
    ##  $ artist_id                   : chr  "5aIqB5nVVvmFsvSdExz408" "2wOqMjp9TyABvtHdOSOTUS" "4lrOh2SR7H3guHky2lAYOk" "2wOqMjp9TyABvtHdOSOTUS" ...
    ##  $ album_id                    : chr  "3Tk0clV8RnlytGZgBF9XYD" "1TbwtdjaSkprz76cgE4s3h" "2loLE4JcxwwlSkttXqjmcd" "5GISuKNCfAqdSmJZB2rRf7" ...
    ##  $ album_type                  : chr  "album" "album" "album" "album" ...
    ##  $ album_release_date          : chr  "2019-01-11" "2018-07-06" "2020-10-30" "2020-06-09" ...
    ##  $ album_release_year          : num  2019 2018 2020 2020 2020 ...
    ##  $ album_release_date_precision: chr  "day" "day" "day" "day" ...
    ##  $ danceability                : num  0.484 0.426 0.286 0.444 0.201 0.324 0.211 0.622 0 0.123 ...
    ##  $ energy                      : num  0.142 0.00539 0.639 0.0106 0.928 0.245 0.996 0.762 0.000326 0.0706 ...
    ##  $ key                         : int  7 2 9 4 7 2 9 5 4 8 ...
    ##  $ loudness                    : num  -18 -28.5 -24.9 -27.8 -20.1 ...
    ##  $ mode                        : int  1 1 1 1 1 0 1 1 0 1 ...
    ##  $ speechiness                 : num  0.0406 0.0431 0.0605 0.056 0.179 0.0317 0.0474 0.0384 0 0.0359 ...
    ##  $ acousticness                : num  0.994 0.994 0.0537 0.993 0.18 0.988 0.766 0.0219 0.968 0.95 ...
    ##  $ instrumentalness            : num  0.847 0.894 0.449 0.945 0.0505 0.905 0.983 0 0.998 0.978 ...
    ##  $ liveness                    : num  0.104 0.0696 0.378 0.149 0.486 0.169 0.921 0.0809 0.0891 0.119 ...
    ##  $ valence                     : num  0.603 0.2 0.0361 0.0443 0.0362 0.162 0.00001 0.645 0 0.19 ...
    ##  $ tempo                       : num  69.9 77.3 135.2 129.2 91.9 ...
    ##  $ track_id                    : chr  "4t8junU9an9m9GYjKq76bo" "2SBrZ2WratwnuZIFScXsuE" "0Gk0kh0QZqT346NhdNgQCx" "64L1bpndAkMNqHSyZlKONX" ...
    ##  $ analysis_url                : chr  "https://api.spotify.com/v1/audio-analysis/4t8junU9an9m9GYjKq76bo" "https://api.spotify.com/v1/audio-analysis/2SBrZ2WratwnuZIFScXsuE" "https://api.spotify.com/v1/audio-analysis/0Gk0kh0QZqT346NhdNgQCx" "https://api.spotify.com/v1/audio-analysis/64L1bpndAkMNqHSyZlKONX" ...
    ##  $ time_signature              : int  1 4 3 3 1 4 3 4 0 3 ...
    ##  $ disc_number                 : int  1 1 1 1 1 2 1 1 2 1 ...
    ##  $ duration_ms                 : int  119173 416573 77740 165960 207996 152120 110080 231904 72666 168000 ...
    ##  $ explicit                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ track_href                  : chr  "https://api.spotify.com/v1/tracks/4t8junU9an9m9GYjKq76bo" "https://api.spotify.com/v1/tracks/2SBrZ2WratwnuZIFScXsuE" "https://api.spotify.com/v1/tracks/0Gk0kh0QZqT346NhdNgQCx" "https://api.spotify.com/v1/tracks/64L1bpndAkMNqHSyZlKONX" ...
    ##  $ is_local                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ track_name                  : chr  "Goldberg Variations, BWV 988 (Arr. D. Sitkovetsky for String Trio): Var. 1, à 1 Clav." "Piano Sonata No. 2 in A Major, Op. 2 No. 2" "Cosy Rain" "Piano Sonata No. 28 in A Major, Op. 101: 3. Langsam und sehnsuchtsvoll (Adagio ma non troppo, con affetto)" ...
    ##  $ track_preview_url           : chr  "https://p.scdn.co/mp3-preview/38718e69b84644ad53c61c58ab064a96cb3c9286?cid=ac26d97eca664234ab133e5208ea5737" "https://p.scdn.co/mp3-preview/ef22976e59c162f4462df1e7bb7f21533ab3a90d?cid=ac26d97eca664234ab133e5208ea5737" "https://p.scdn.co/mp3-preview/0ab1607a49fccd656ee21c31b8cc05f451961a46?cid=ac26d97eca664234ab133e5208ea5737" NA ...
    ##  $ track_number                : int  31 6 26 210 27 30 17 9 6 18 ...
    ##  $ type                        : chr  "track" "track" "track" "track" ...
    ##  $ track_uri                   : chr  "spotify:track:4t8junU9an9m9GYjKq76bo" "spotify:track:2SBrZ2WratwnuZIFScXsuE" "spotify:track:0Gk0kh0QZqT346NhdNgQCx" "spotify:track:64L1bpndAkMNqHSyZlKONX" ...
    ##  $ external_urls.spotify       : chr  "https://open.spotify.com/track/4t8junU9an9m9GYjKq76bo" "https://open.spotify.com/track/2SBrZ2WratwnuZIFScXsuE" "https://open.spotify.com/track/0Gk0kh0QZqT346NhdNgQCx" "https://open.spotify.com/track/64L1bpndAkMNqHSyZlKONX" ...
    ##  $ album_name                  : chr  "Anniversary Box: Jörg Demus, Vol. 8" "Beethoven: The Complete Piano Sonatas" "! ! ! ! : Land : ! ! ! !" "Summer Classical: Beethoven" ...
    ##  $ key_name                    : chr  "G" "D" "A" "E" ...
    ##  $ mode_name                   : chr  "major" "major" "major" "major" ...
    ##  $ key_mode                    : chr  "G major" "D major" "A major" "E major" ...

## Separo Datos

``` r
datanum <- data_pre %>% 
  select(data_dou)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(data_dou)` instead of `data_dou` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

``` r
datachar <- data_pre %>% 
  select(data_char)
```

    ## Note: Using an external vector in selections is ambiguous.
    ## i Use `all_of(data_char)` instead of `data_char` to silence this message.
    ## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This message is displayed once per session.

## Escalar Datos

``` r
data_sca <- sapply(datanum, scale)
```

# Procesamiento de los Datos

## Clustering Jerarquico

-   Matriz de Distancias

``` r
#Distancia Euclideana
d = dist(data_sca, method = "euclidean")
#Distancia Manhattan
d1 = dist(data_sca, method = "manhattan")
#Distancia Minkowski
d2 = dist(data_sca, method = "minkowski")
hist(d, main = "Histograma Distancia Euclideana")
```

![](Ayudantia6_files/figure-gfm/matriz%20distancia-1.png)<!-- -->

``` r
hist(d1, main = "Histograma Distancia Manhattan")
```

![](Ayudantia6_files/figure-gfm/matriz%20distancia-2.png)<!-- -->

``` r
hist(d2, main = "Histograma Distancia Minkowski")
```

![](Ayudantia6_files/figure-gfm/matriz%20distancia-3.png)<!-- --> \#\#
Clustering Aglomerativo

Utilizando la funcion de R base hclust, aplicamos hierarchical
clustering, a partir de la matriz de distancias d, y utilizamos el
criterio complete linkage

-   Complete Model

``` r
# Fijamos una seed para que cada vez que ejecutemos el codigo obtengamos los mismos valores y no vayan cambiado cada vez que ejecutamos el script
set.seed(369)
model_complete <- hclust(d, method = "complete")
summary(model_complete)
```

    ##             Length Class  Mode     
    ## merge       19994  -none- numeric  
    ## height       9997  -none- numeric  
    ## order        9998  -none- numeric  
    ## labels          0  -none- NULL     
    ## method          1  -none- character
    ## call            3  -none- call     
    ## dist.method     1  -none- character

-   Ward Model

``` r
set.seed(369)
model_ward <- hclust(d, method = "ward.D")
summary(model_ward)
```

    ##             Length Class  Mode     
    ## merge       19994  -none- numeric  
    ## height       9997  -none- numeric  
    ## order        9998  -none- numeric  
    ## labels          0  -none- NULL     
    ## method          1  -none- character
    ## call            3  -none- call     
    ## dist.method     1  -none- character

-   Comparacion de los coeficientes de aglomeracion para cada metodo

``` r
models <- c("complete", "ward")
names(models) <- c("complete", "ward")
agcoef <- function(x) {
  agnes(data_sca, method = x)$ac
}
```

Generamos un dendrograma para visualizar la jerarquia. La libreria
‘ggdendro’ permite hacer estos diagramas en una sintaxis equivalente a
ggplot.

``` r
library("ggdendro")
```

    ## Warning: package 'ggdendro' was built under R version 4.0.5

``` r
ggdendrogram(model_complete, rotate = TRUE, theme_dendro = TRUE) 
```

![](Ayudantia6_files/figure-gfm/grafico%20dendrograma-1.png)<!-- -->

## Corte

``` r
# Determinamos un valor para h lo que nos entregara un valor distinto de k para cada h que escogamos, tambien podemos definir el k desde un inicio
groups <- cutree(model_complete, h = 9)
# Se imprimen los tamaños de cada cluster
table(groups)
```

    ## groups
    ##    1    2    3    4    5    6    7    8    9 
    ## 5854 1232 1882  148  690  110   66    2   14

``` r
# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion (tanto en data_pre y datanum)
data_pre$clust <- as.factor(groups)
datanum$clust <- as.factor(groups)
# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = data_sca, cluster = groups))
```

![](Ayudantia6_files/figure-gfm/corte%20arbol-1.png)<!-- -->

## Caracteristicas de los clusters encontrados

``` r
datanum$clust <- as.numeric(as.character(datanum$clust))
# Generamos una tabla que almacenara los valores promedios para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos
infoclusters <- aggregate(datanum, by=list(cluster=datanum$clust), mean)
# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclusters$clust <- NULL
# Transformamos el tiempo de la cancion a minutos
infoclusters <- infoclusters %>% mutate(duration_min = infoclusters$duration_ms/60000)
# Borramos la columna de la duracion en milisegundoss
infoclusters$duration_ms <- NULL
infoclusters
```

    ##   cluster danceability    energy      key   loudness      mode speechiness
    ## 1       1    0.3679710 0.1598650 5.051247 -21.921482 0.7166040  0.04840400
    ## 2       2    0.2962717 0.7384683 5.250812 -16.759149 0.5470779  0.08672208
    ## 3       3    0.5865193 0.7198252 5.089267  -7.326446 0.7051010  0.07615186
    ## 4       4    0.1858682 0.0852317 5.216216 -32.231689 0.1351351  0.03504054
    ## 5       5    0.2743420 0.1142554 4.692754 -21.820959 0.7217391  0.04243232
    ## 6       6    0.4057545 0.5437771 6.027273 -21.273664 0.4818182  0.45584545
    ## 7       7    0.6726515 0.2998127 5.757576 -18.072061 0.7424242  0.89809091
    ## 8       8    0.5085000 0.7260000 7.000000 -12.193500 0.0000000  0.03915000
    ## 9       9    0.3293571 0.2112357 3.785714 -19.575429 0.8571429  0.07576429
    ##   acousticness instrumentalness  liveness    valence    tempo duration_min
    ## 1    0.9027102       0.61792702 0.1451461 0.33866363 106.7785     3.004367
    ## 2    0.3938424       0.40117816 0.6084420 0.17072040 108.1931     3.355459
    ## 3    0.1699652       0.12151193 0.2134562 0.56106291 122.9133     4.105051
    ## 4    0.8795149       0.70099216 0.1713331 0.07560541  56.0727     2.355141
    ## 5    0.9466710       0.77249314 0.1319778 0.13100942 104.8834    10.553156
    ## 6    0.6742260       0.36149347 0.3981173 0.23494909 103.0318     2.586460
    ## 7    0.7117045       0.04942692 0.3115409 0.59840455 105.2253     1.668176
    ## 8    0.0116000       0.75050000 0.1092500 0.19465000 131.0630    69.980458
    ## 9    0.8212143       0.29535866 0.2209071 0.17468571 106.4869    26.587757

## Filtremos por clusters con mas datos

``` r
# 1er Cluster con mas datos
data_c1 <- data_pre %>% 
  filter(data_pre$clust == 1)
# 2do Cluster con mas datos
data_c2 <- data_pre %>% 
  filter(data_pre$clust == 4)
# 3er Cluster con mas datos
data_c3 <- data_pre %>% 
  filter(data_pre$clust == 2)
```

## Tomemos a c2

``` r
# Borramos la columna clust para escalar la datanum de c2
data_c2$clust <- NULL
# Selecciono las variables numericas, se escalan las variables y se almacenan los datos en una tabla
datanumc2 <- data_c2 %>% 
  select(data_dou) %>% 
  scale() %>% 
  as_tibble()
```

Ahora a C2 le aplicaremos un clustering divisivo

## Clustering Divisivo

``` r
# Generamos un modelo divisvo mediante la funcion diana de clusters
modelo_div <- diana(datanumc2)
# Le pedimos el coeficiente de divisivilidad al modelo
modelo_div$dc
```

    ## [1] 0.8734917

``` r
# Graficamos nuestro dendrograma divisivo
pltree(modelo_div, cex = 0.8, hang = -1.5, main = "Dendrogram of diana")
```

![](Ayudantia6_files/figure-gfm/clustering%20divisivo-1.png)<!-- -->

## Cantidad Clusters

``` r
# Para el caso divisivo le entregaremos el numero de clusters con los que queremos agrupar nuestros datos
groupsc2 <- cutree(modelo_div, k = 10)
# Se imprimen los tamaños de cada cluster
table(groupsc2)
```

    ## groupsc2
    ##  1  2  3  4  5  6  7  8  9 10 
    ##  1 44 14  9 20 32 12  5 10  1

``` r
# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion de data_c2
data_c2$clust <- as.factor(groupsc2)
# Graficamos las observaciones agrupadas por su cluster
fviz_cluster(list(data = datanumc2, cluster = groupsc2))
```

![](Ayudantia6_files/figure-gfm/division%20arbol-1.png)<!-- -->

``` r
# Generamos una nueva columna para almacenar a que cluster pertenece cada observacion de datanumc2
datanumc2$clust <- as.factor(groupsc2)
```

## Caracteristicas Clusters encontrados

``` r
datanumc2$clust <- as.numeric(as.character(datanumc2$clust))
# Generamos una tabla que almacenara los valores promedios para cada uno de los clusters encontrados lo que nos permitira caracterizar a cada uno de ellos
infoclustersc2 <- aggregate(datanumc2, by=list(cluster=datanumc2$clust), mean)
# Borramos la columna clust ya que se repite esa informacion en la tabla generada
infoclustersc2$clust <- NULL
# Transformamos el tiempo de la cancion a minutos
infoclustersc2 <- infoclustersc2 %>% mutate(duration_min = infoclustersc2$duration_ms/60000)
# Borramos la columna de la duracion en milisegundoss
infoclustersc2$duration_ms <- NULL
infoclustersc2
```

    ##    cluster danceability      energy        key    loudness       mode
    ## 1        1  -1.25833666 -0.56747088 -0.3622410 -1.00496982 -0.3939470
    ## 2        2   0.07759355 -0.38194955 -0.8022357 -0.15139405 -0.3939470
    ## 3        3  -0.11806739 -0.32001938  0.3610910 -1.01494301 -0.3939470
    ## 4        4  -1.25833666  0.39315180  0.3658187  1.06971347 -0.3939470
    ## 5        5   1.69272663 -0.47312264  0.6057474 -0.04012055 -0.3939470
    ## 6        6   0.38128399 -0.51013861  0.6988232 -0.57734254 -0.3939470
    ## 7        7  -1.25833666  1.72503342 -0.6600836  1.40263130  2.5212609
    ## 8        8  -1.25833666  0.05872364  0.8291293  0.28467452  2.5212609
    ## 9        9  -1.25833666  2.36776749 -0.1835354  1.66566632  0.4806154
    ## 10      10  -1.25833666 -0.56964972 -1.5536113 -3.38614995 -0.3939470
    ##    speechiness acousticness instrumentalness    liveness    valence      tempo
    ## 1   -1.5647128   0.39055326        0.9065109 -0.41163549 -1.0667679 -1.6096799
    ## 2    0.3801874   0.31331224        0.1580144 -0.25645833 -0.1143967  0.5556931
    ## 3    1.1480394  -2.35411967        0.1083751 -0.10092362  0.3137633  0.8812465
    ## 4   -1.5647128  -0.10526052       -0.7671052 -0.14077070 -1.0667679 -1.6096799
    ## 5    0.6811781   0.49957344        0.6396008 -0.40084817  1.5074723  0.4410705
    ## 6    0.4073352   0.38296709        0.5071726 -0.43728977  0.3446408  0.4862025
    ## 7   -1.5647128   0.19818634       -0.6637827  0.23944115 -1.0667679 -1.6096799
    ## 8   -1.5647128  -0.18058891       -1.5907538  0.08963679 -1.0667679 -1.6096799
    ## 9   -1.5647128  -0.01110004       -1.3435296  3.39220719 -1.0667679 -1.6096799
    ## 10  -1.5647128  -3.88197856       -2.1395294 -0.85764467 -1.0667679 -1.6096799
    ##     duration_min
    ## 1  -1.269956e-05
    ## 2   1.222297e-05
    ## 3   9.378287e-07
    ## 4  -1.745696e-05
    ## 5  -2.324968e-06
    ## 6   6.447350e-06
    ## 7  -1.786078e-05
    ## 8  -2.351645e-05
    ## 9  -1.838238e-05
    ## 10 -2.520867e-05
