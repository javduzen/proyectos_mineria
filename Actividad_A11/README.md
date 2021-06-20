Ayudantia 11 Arboles de Decision
================

## Actividad Ayudantia 11

Usé “Hotel Bookings”  
determinar si la reserva del hotel sera o no cancelada. (Comparen los
resultados obtenidos mediante arboles de decision con los modelos de
regresion logistica, naive bayes y KNN)

## Cargamos las librerias

``` r
library(plyr)
library(ggplot2)
library(tidyverse)
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.0.5

    ## Warning: package 'broom' was built under R version 4.0.5

    ## Warning: package 'dials' was built under R version 4.0.5

    ## Warning: package 'infer' was built under R version 4.0.5

    ## Warning: package 'modeldata' was built under R version 4.0.5

    ## Warning: package 'parsnip' was built under R version 4.0.5

    ## Warning: package 'recipes' was built under R version 4.0.5

    ## Warning: package 'rsample' was built under R version 4.0.5

    ## Warning: package 'tune' was built under R version 4.0.5

    ## Warning: package 'workflows' was built under R version 4.0.5

    ## Warning: package 'workflowsets' was built under R version 4.0.5

    ## Warning: package 'yardstick' was built under R version 4.0.5

``` r
library(discrim)
```

    ## Warning: package 'discrim' was built under R version 4.0.5

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.0.5

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.0.5

## Cargamos los datos

``` r
hotel <- read.csv("C:/Users/Javiera/Desktop/RAMOS 2021/Minería/proyectos_mineria/Actividad_A11/hotel_bookings.csv", na.strings = c("","NA"," ","?"))
```

``` r
head(hotel)
```

    ##          hotel is_canceled lead_time arrival_date_year arrival_date_month
    ## 1 Resort Hotel           0       342              2015               July
    ## 2 Resort Hotel           0       737              2015               July
    ## 3 Resort Hotel           0         7              2015               July
    ## 4 Resort Hotel           0        13              2015               July
    ## 5 Resort Hotel           0        14              2015               July
    ## 6 Resort Hotel           0        14              2015               July
    ##   arrival_date_week_number arrival_date_day_of_month stays_in_weekend_nights
    ## 1                       27                         1                       0
    ## 2                       27                         1                       0
    ## 3                       27                         1                       0
    ## 4                       27                         1                       0
    ## 5                       27                         1                       0
    ## 6                       27                         1                       0
    ##   stays_in_week_nights adults children babies meal country market_segment
    ## 1                    0      2        0      0   BB     PRT         Direct
    ## 2                    0      2        0      0   BB     PRT         Direct
    ## 3                    1      1        0      0   BB     GBR         Direct
    ## 4                    1      1        0      0   BB     GBR      Corporate
    ## 5                    2      2        0      0   BB     GBR      Online TA
    ## 6                    2      2        0      0   BB     GBR      Online TA
    ##   distribution_channel is_repeated_guest previous_cancellations
    ## 1               Direct                 0                      0
    ## 2               Direct                 0                      0
    ## 3               Direct                 0                      0
    ## 4            Corporate                 0                      0
    ## 5                TA/TO                 0                      0
    ## 6                TA/TO                 0                      0
    ##   previous_bookings_not_canceled reserved_room_type assigned_room_type
    ## 1                              0                  C                  C
    ## 2                              0                  C                  C
    ## 3                              0                  A                  C
    ## 4                              0                  A                  A
    ## 5                              0                  A                  A
    ## 6                              0                  A                  A
    ##   booking_changes deposit_type agent company days_in_waiting_list customer_type
    ## 1               3   No Deposit  NULL    NULL                    0     Transient
    ## 2               4   No Deposit  NULL    NULL                    0     Transient
    ## 3               0   No Deposit  NULL    NULL                    0     Transient
    ## 4               0   No Deposit   304    NULL                    0     Transient
    ## 5               0   No Deposit   240    NULL                    0     Transient
    ## 6               0   No Deposit   240    NULL                    0     Transient
    ##   adr required_car_parking_spaces total_of_special_requests reservation_status
    ## 1   0                           0                         0          Check-Out
    ## 2   0                           0                         0          Check-Out
    ## 3  75                           0                         0          Check-Out
    ## 4  75                           0                         0          Check-Out
    ## 5  98                           0                         1          Check-Out
    ## 6  98                           0                         1          Check-Out
    ##   reservation_status_date
    ## 1              2015-07-01
    ## 2              2015-07-01
    ## 3              2015-07-02
    ## 4              2015-07-02
    ## 5              2015-07-03
    ## 6              2015-07-03

``` r
summary(hotel)
```

    ##     hotel            is_canceled       lead_time   arrival_date_year
    ##  Length:119390      Min.   :0.0000   Min.   :  0   Min.   :2015     
    ##  Class :character   1st Qu.:0.0000   1st Qu.: 18   1st Qu.:2016     
    ##  Mode  :character   Median :0.0000   Median : 69   Median :2016     
    ##                     Mean   :0.3704   Mean   :104   Mean   :2016     
    ##                     3rd Qu.:1.0000   3rd Qu.:160   3rd Qu.:2017     
    ##                     Max.   :1.0000   Max.   :737   Max.   :2017     
    ##                                                                     
    ##  arrival_date_month arrival_date_week_number arrival_date_day_of_month
    ##  Length:119390      Min.   : 1.00            Min.   : 1.0             
    ##  Class :character   1st Qu.:16.00            1st Qu.: 8.0             
    ##  Mode  :character   Median :28.00            Median :16.0             
    ##                     Mean   :27.17            Mean   :15.8             
    ##                     3rd Qu.:38.00            3rd Qu.:23.0             
    ##                     Max.   :53.00            Max.   :31.0             
    ##                                                                       
    ##  stays_in_weekend_nights stays_in_week_nights     adults      
    ##  Min.   : 0.0000         Min.   : 0.0         Min.   : 0.000  
    ##  1st Qu.: 0.0000         1st Qu.: 1.0         1st Qu.: 2.000  
    ##  Median : 1.0000         Median : 2.0         Median : 2.000  
    ##  Mean   : 0.9276         Mean   : 2.5         Mean   : 1.856  
    ##  3rd Qu.: 2.0000         3rd Qu.: 3.0         3rd Qu.: 2.000  
    ##  Max.   :19.0000         Max.   :50.0         Max.   :55.000  
    ##                                                               
    ##     children           babies              meal             country         
    ##  Min.   : 0.0000   Min.   : 0.000000   Length:119390      Length:119390     
    ##  1st Qu.: 0.0000   1st Qu.: 0.000000   Class :character   Class :character  
    ##  Median : 0.0000   Median : 0.000000   Mode  :character   Mode  :character  
    ##  Mean   : 0.1039   Mean   : 0.007949                                        
    ##  3rd Qu.: 0.0000   3rd Qu.: 0.000000                                        
    ##  Max.   :10.0000   Max.   :10.000000                                        
    ##  NA's   :4                                                                  
    ##  market_segment     distribution_channel is_repeated_guest
    ##  Length:119390      Length:119390        Min.   :0.00000  
    ##  Class :character   Class :character     1st Qu.:0.00000  
    ##  Mode  :character   Mode  :character     Median :0.00000  
    ##                                          Mean   :0.03191  
    ##                                          3rd Qu.:0.00000  
    ##                                          Max.   :1.00000  
    ##                                                           
    ##  previous_cancellations previous_bookings_not_canceled reserved_room_type
    ##  Min.   : 0.00000       Min.   : 0.0000                Length:119390     
    ##  1st Qu.: 0.00000       1st Qu.: 0.0000                Class :character  
    ##  Median : 0.00000       Median : 0.0000                Mode  :character  
    ##  Mean   : 0.08712       Mean   : 0.1371                                  
    ##  3rd Qu.: 0.00000       3rd Qu.: 0.0000                                  
    ##  Max.   :26.00000       Max.   :72.0000                                  
    ##                                                                          
    ##  assigned_room_type booking_changes   deposit_type          agent          
    ##  Length:119390      Min.   : 0.0000   Length:119390      Length:119390     
    ##  Class :character   1st Qu.: 0.0000   Class :character   Class :character  
    ##  Mode  :character   Median : 0.0000   Mode  :character   Mode  :character  
    ##                     Mean   : 0.2211                                        
    ##                     3rd Qu.: 0.0000                                        
    ##                     Max.   :21.0000                                        
    ##                                                                            
    ##    company          days_in_waiting_list customer_type           adr         
    ##  Length:119390      Min.   :  0.000      Length:119390      Min.   :  -6.38  
    ##  Class :character   1st Qu.:  0.000      Class :character   1st Qu.:  69.29  
    ##  Mode  :character   Median :  0.000      Mode  :character   Median :  94.58  
    ##                     Mean   :  2.321                         Mean   : 101.83  
    ##                     3rd Qu.:  0.000                         3rd Qu.: 126.00  
    ##                     Max.   :391.000                         Max.   :5400.00  
    ##                                                                              
    ##  required_car_parking_spaces total_of_special_requests reservation_status
    ##  Min.   :0.00000             Min.   :0.0000            Length:119390     
    ##  1st Qu.:0.00000             1st Qu.:0.0000            Class :character  
    ##  Median :0.00000             Median :0.0000            Mode  :character  
    ##  Mean   :0.06252             Mean   :0.5714                              
    ##  3rd Qu.:0.00000             3rd Qu.:1.0000                              
    ##  Max.   :8.00000             Max.   :5.0000                              
    ##                                                                          
    ##  reservation_status_date
    ##  Length:119390          
    ##  Class :character       
    ##  Mode  :character       
    ##                         
    ##                         
    ##                         
    ## 

``` r
str(hotel)
```

    ## 'data.frame':    119390 obs. of  32 variables:
    ##  $ hotel                         : chr  "Resort Hotel" "Resort Hotel" "Resort Hotel" "Resort Hotel" ...
    ##  $ is_canceled                   : int  0 0 0 0 0 0 0 0 1 1 ...
    ##  $ lead_time                     : int  342 737 7 13 14 14 0 9 85 75 ...
    ##  $ arrival_date_year             : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ arrival_date_month            : chr  "July" "July" "July" "July" ...
    ##  $ arrival_date_week_number      : int  27 27 27 27 27 27 27 27 27 27 ...
    ##  $ arrival_date_day_of_month     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ stays_in_weekend_nights       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ stays_in_week_nights          : int  0 0 1 1 2 2 2 2 3 3 ...
    ##  $ adults                        : int  2 2 1 1 2 2 2 2 2 2 ...
    ##  $ children                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ babies                        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ meal                          : chr  "BB" "BB" "BB" "BB" ...
    ##  $ country                       : chr  "PRT" "PRT" "GBR" "GBR" ...
    ##  $ market_segment                : chr  "Direct" "Direct" "Direct" "Corporate" ...
    ##  $ distribution_channel          : chr  "Direct" "Direct" "Direct" "Corporate" ...
    ##  $ is_repeated_guest             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_cancellations        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_bookings_not_canceled: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ reserved_room_type            : chr  "C" "C" "A" "A" ...
    ##  $ assigned_room_type            : chr  "C" "C" "C" "A" ...
    ##  $ booking_changes               : int  3 4 0 0 0 0 0 0 0 0 ...
    ##  $ deposit_type                  : chr  "No Deposit" "No Deposit" "No Deposit" "No Deposit" ...
    ##  $ agent                         : chr  "NULL" "NULL" "NULL" "304" ...
    ##  $ company                       : chr  "NULL" "NULL" "NULL" "NULL" ...
    ##  $ days_in_waiting_list          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ customer_type                 : chr  "Transient" "Transient" "Transient" "Transient" ...
    ##  $ adr                           : num  0 0 75 75 98 ...
    ##  $ required_car_parking_spaces   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ total_of_special_requests     : int  0 0 0 0 1 1 0 1 1 0 ...
    ##  $ reservation_status            : chr  "Check-Out" "Check-Out" "Check-Out" "Check-Out" ...
    ##  $ reservation_status_date       : chr  "2015-07-01" "2015-07-01" "2015-07-02" "2015-07-02" ...

## Transformamos variables

Como la variable is\_canceled ya es 0-1 no es necesario cambiarla o
mapearla nuevamente.  
Eliminar NA

``` r
hotel %>% 
  summarise_all(funs(sum(is.na(.))))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ##   hotel is_canceled lead_time arrival_date_year arrival_date_month
    ## 1     0           0         0                 0                  0
    ##   arrival_date_week_number arrival_date_day_of_month stays_in_weekend_nights
    ## 1                        0                         0                       0
    ##   stays_in_week_nights adults children babies meal country market_segment
    ## 1                    0      0        4      0    0       0              0
    ##   distribution_channel is_repeated_guest previous_cancellations
    ## 1                    0                 0                      0
    ##   previous_bookings_not_canceled reserved_room_type assigned_room_type
    ## 1                              0                  0                  0
    ##   booking_changes deposit_type agent company days_in_waiting_list customer_type
    ## 1               0            0     0       0                    0             0
    ##   adr required_car_parking_spaces total_of_special_requests reservation_status
    ## 1   0                           0                         0                  0
    ##   reservation_status_date
    ## 1                       0

``` r
hotel <- hotel %>% filter(!(is.na(children)))

hotel %>% 
  summarise_all(funs(sum(is.na(.))))
```

    ##   hotel is_canceled lead_time arrival_date_year arrival_date_month
    ## 1     0           0         0                 0                  0
    ##   arrival_date_week_number arrival_date_day_of_month stays_in_weekend_nights
    ## 1                        0                         0                       0
    ##   stays_in_week_nights adults children babies meal country market_segment
    ## 1                    0      0        0      0    0       0              0
    ##   distribution_channel is_repeated_guest previous_cancellations
    ## 1                    0                 0                      0
    ##   previous_bookings_not_canceled reserved_room_type assigned_room_type
    ## 1                              0                  0                  0
    ##   booking_changes deposit_type agent company days_in_waiting_list customer_type
    ## 1               0            0     0       0                    0             0
    ##   adr required_car_parking_spaces total_of_special_requests reservation_status
    ## 1   0                           0                         0                  0
    ##   reservation_status_date
    ## 1                       0

``` r
hotel$is_canceled <- as.factor(hotel$is_canceled)
str(hotel)
```

    ## 'data.frame':    119386 obs. of  32 variables:
    ##  $ hotel                         : chr  "Resort Hotel" "Resort Hotel" "Resort Hotel" "Resort Hotel" ...
    ##  $ is_canceled                   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ lead_time                     : int  342 737 7 13 14 14 0 9 85 75 ...
    ##  $ arrival_date_year             : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ arrival_date_month            : chr  "July" "July" "July" "July" ...
    ##  $ arrival_date_week_number      : int  27 27 27 27 27 27 27 27 27 27 ...
    ##  $ arrival_date_day_of_month     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ stays_in_weekend_nights       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ stays_in_week_nights          : int  0 0 1 1 2 2 2 2 3 3 ...
    ##  $ adults                        : int  2 2 1 1 2 2 2 2 2 2 ...
    ##  $ children                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ babies                        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ meal                          : chr  "BB" "BB" "BB" "BB" ...
    ##  $ country                       : chr  "PRT" "PRT" "GBR" "GBR" ...
    ##  $ market_segment                : chr  "Direct" "Direct" "Direct" "Corporate" ...
    ##  $ distribution_channel          : chr  "Direct" "Direct" "Direct" "Corporate" ...
    ##  $ is_repeated_guest             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_cancellations        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_bookings_not_canceled: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ reserved_room_type            : chr  "C" "C" "A" "A" ...
    ##  $ assigned_room_type            : chr  "C" "C" "C" "A" ...
    ##  $ booking_changes               : int  3 4 0 0 0 0 0 0 0 0 ...
    ##  $ deposit_type                  : chr  "No Deposit" "No Deposit" "No Deposit" "No Deposit" ...
    ##  $ agent                         : chr  "NULL" "NULL" "NULL" "304" ...
    ##  $ company                       : chr  "NULL" "NULL" "NULL" "NULL" ...
    ##  $ days_in_waiting_list          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ customer_type                 : chr  "Transient" "Transient" "Transient" "Transient" ...
    ##  $ adr                           : num  0 0 75 75 98 ...
    ##  $ required_car_parking_spaces   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ total_of_special_requests     : int  0 0 0 0 1 1 0 1 1 0 ...
    ##  $ reservation_status            : chr  "Check-Out" "Check-Out" "Check-Out" "Check-Out" ...
    ##  $ reservation_status_date       : chr  "2015-07-01" "2015-07-01" "2015-07-02" "2015-07-02" ...

## Exploracion de los datos

``` r
table(hotel$is_canceled)
```

    ## 
    ##     0     1 
    ## 75166 44220

``` r
hist(as.numeric(hotel$is_canceled))
```

![](Ayudantia11_files/figure-gfm/plot%20count-1.png)<!-- -->

## Implementacion Decision Trees, separar data en Test y Train

``` r
library(tidymodels)

data_split <- initial_split(hotel, prop = 0.8)

# Create data frames for the two sets:
train_data <- training(data_split) 
test_data <- testing(data_split)

str(train_data)
```

    ## 'data.frame':    95508 obs. of  32 variables:
    ##  $ hotel                         : chr  "City Hotel" "City Hotel" "Resort Hotel" "City Hotel" ...
    ##  $ is_canceled                   : Factor w/ 2 levels "0","1": 1 1 1 1 2 1 1 2 2 1 ...
    ##  $ lead_time                     : int  101 25 83 3 156 0 7 178 100 178 ...
    ##  $ arrival_date_year             : int  2016 2015 2016 2016 2017 2015 2016 2017 2016 2017 ...
    ##  $ arrival_date_month            : chr  "November" "November" "November" "December" ...
    ##  $ arrival_date_week_number      : int  48 48 46 52 17 41 50 15 17 28 ...
    ##  $ arrival_date_day_of_month     : int  26 22 12 19 26 9 4 14 23 14 ...
    ##  $ stays_in_weekend_nights       : int  2 2 2 1 0 0 2 1 1 2 ...
    ##  $ stays_in_week_nights          : int  1 3 5 0 3 1 2 2 1 6 ...
    ##  $ adults                        : int  2 2 1 2 2 1 2 2 2 2 ...
    ##  $ children                      : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ babies                        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ meal                          : chr  "BB" "BB" "HB" "BB" ...
    ##  $ country                       : chr  "ROU" "FRA" "DEU" "PRT" ...
    ##  $ market_segment                : chr  "Online TA" "Offline TA/TO" "Online TA" "Online TA" ...
    ##  $ distribution_channel          : chr  "TA/TO" "TA/TO" "TA/TO" "TA/TO" ...
    ##  $ is_repeated_guest             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_cancellations        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_bookings_not_canceled: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ reserved_room_type            : chr  "A" "A" "E" "A" ...
    ##  $ assigned_room_type            : chr  "A" "A" "E" "A" ...
    ##  $ booking_changes               : int  0 0 1 0 0 0 0 0 0 1 ...
    ##  $ deposit_type                  : chr  "No Deposit" "No Deposit" "No Deposit" "No Deposit" ...
    ##  $ agent                         : chr  "9" "12" "314" "9" ...
    ##  $ company                       : chr  "NULL" "NULL" "NULL" "NULL" ...
    ##  $ days_in_waiting_list          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ customer_type                 : chr  "Transient" "Transient-Party" "Transient" "Transient" ...
    ##  $ adr                           : num  90 65 57.6 97.5 100 ...
    ##  $ required_car_parking_spaces   : int  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ total_of_special_requests     : int  2 0 0 1 0 0 0 1 0 1 ...
    ##  $ reservation_status            : chr  "Check-Out" "Check-Out" "Check-Out" "Check-Out" ...
    ##  $ reservation_status_date       : chr  "2016-11-29" "2015-11-27" "2016-11-19" "2016-12-20" ...

``` r
str(test_data)
```

    ## 'data.frame':    23878 obs. of  32 variables:
    ##  $ hotel                         : chr  "Resort Hotel" "Resort Hotel" "Resort Hotel" "Resort Hotel" ...
    ##  $ is_canceled                   : Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 1 ...
    ##  $ lead_time                     : int  23 68 72 72 78 69 16 70 107 93 ...
    ##  $ arrival_date_year             : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ arrival_date_month            : chr  "July" "July" "July" "July" ...
    ##  $ arrival_date_week_number      : int  27 27 27 27 27 27 27 27 27 27 ...
    ##  $ arrival_date_day_of_month     : int  1 1 1 1 1 2 2 2 2 2 ...
    ##  $ stays_in_weekend_nights       : int  0 0 2 2 2 2 2 2 2 3 ...
    ##  $ stays_in_week_nights          : int  4 4 4 4 5 4 3 3 5 8 ...
    ##  $ adults                        : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ children                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ babies                        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ meal                          : chr  "BB" "BB" "BB" "BB" ...
    ##  $ country                       : chr  "PRT" "IRL" "PRT" "PRT" ...
    ##  $ market_segment                : chr  "Online TA" "Online TA" "Direct" "Direct" ...
    ##  $ distribution_channel          : chr  "TA/TO" "TA/TO" "Direct" "Direct" ...
    ##  $ is_repeated_guest             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_cancellations        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ previous_bookings_not_canceled: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ reserved_room_type            : chr  "E" "D" "A" "D" ...
    ##  $ assigned_room_type            : chr  "E" "E" "A" "D" ...
    ##  $ booking_changes               : int  0 0 1 1 0 0 0 0 0 0 ...
    ##  $ deposit_type                  : chr  "No Deposit" "No Deposit" "No Deposit" "No Deposit" ...
    ##  $ agent                         : chr  "240" "240" "250" "250" ...
    ##  $ company                       : chr  "NULL" "NULL" "NULL" "NULL" ...
    ##  $ days_in_waiting_list          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ customer_type                 : chr  "Transient" "Transient" "Transient" "Transient" ...
    ##  $ adr                           : num  123 97 84.7 99.7 63.6 ...
    ##  $ required_car_parking_spaces   : int  0 0 0 0 1 0 0 0 0 0 ...
    ##  $ total_of_special_requests     : int  0 3 1 1 0 0 0 1 2 0 ...
    ##  $ reservation_status            : chr  "Canceled" "Check-Out" "Check-Out" "Check-Out" ...
    ##  $ reservation_status_date       : chr  "2015-06-23" "2015-07-05" "2015-07-07" "2015-07-07" ...

## Seleccion de Atributos

-   Hay atributos que no tienen mucha importancia para el analisis por
    lo tanto los eliminaremos.

``` r
train <- subset(train_data, select = - c(arrival_date_week_number, arrival_date_day_of_month,arrival_date_year,adr, reservation_status,reservation_status_date,deposit_type,assigned_room_type,meal,country,distribution_channel,required_car_parking_spaces,total_of_special_requests   ))

test <- subset(test_data, select = - c(arrival_date_week_number, arrival_date_day_of_month,arrival_date_year,adr, reservation_status,reservation_status_date,deposit_type,assigned_room_type,meal,country,distribution_channel,required_car_parking_spaces,total_of_special_requests   ))
```

## Crear Modelo

-   Primero creamos la receta de nuestro modelo

``` r
receta <- 
  recipe(is_canceled ~ ., data = train)

receta
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor         18

-   Luego procedemos a crear nuestro modelo de arbol de decision con 5
    capas de decision, y un minimo numero de entidades por hoja (poda)
    de 10. La libreria que se utiliza para calcular este modelo sera la
    de rpart, que viene precargada en los paquetes que estamos
    utilizando. Con este paso solo definimos el modelo, aun no lo
    calculamos.

``` r
modelo_trees <-
  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

modelo_trees
```

    ## Decision Tree Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   tree_depth = 5
    ##   min_n = 10
    ## 
    ## Computational engine: rpart

-   Ahora hacemos el fit del modelo, calculamos sus predicciones y
    calculamos el valor de AUC

``` r
fit_mod <- function(mod){
  
  modelo_fit <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(receta) %>% 
  fit(data = train)

model_pred <- 
  predict(modelo_fit, test, type = "prob") %>% 
  bind_cols(test) 

return(model_pred %>% 
  roc_auc(truth = is_canceled, .pred_0))
}

fit_mod(modelo_trees)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.638

-   Ahora compararemos con otros modelos (regresion logistica, naive
    bayes o KNN), aprovechando que la libreria tidymodels nos facilita
    realizar esta comparacion. Lo unico que debemos cambiar es el
    modelo, ya que utilizamos la misma receta y el mismo flujo de
    validacion para el modelo. Por lo que podemos reutilizar lo que
    hicimos arriba

## Regresion Logistica

este no lo pude ejecutar pq se quedaba pegado

``` r
#modelo_rl <- 
 # logistic_reg() %>% 
  #set_engine("glm")

#fit_mod(modelo_rl)
```

## Naive Bayes

``` r
library(naivebayes)
```

    ## Warning: package 'naivebayes' was built under R version 4.0.5

    ## naivebayes 0.9.7 loaded

``` r
modelo_nb <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("naivebayes")

fit_mod(modelo_nb)
```

    ## Warning: naive_bayes(): Feature reserved_room_type - zero probabilities are
    ## present. Consider Laplace smoothing.

    ## Warning: naive_bayes(): Feature agent - zero probabilities are present. Consider
    ## Laplace smoothing.

    ## Warning: naive_bayes(): Feature company - zero probabilities are present.
    ## Consider Laplace smoothing.

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.790

## KNN

Tampoco pude ejecutarlo, se me quedaba pegado tod el rato

``` r
#library(kknn)

#modelo_knn <-
 # nearest_neighbor(neighbors = 5) %>% 
  #set_engine("kknn") %>% 
  #set_mode("classification")

#fit_mod(modelo_knn)
```

-   Podemos ver que en este caso el modelo de Naive Bayes y
    clasificacion son los que obtienen los mejores resultados al
    clasificar con un AUC de .79 - 0.63

``` r
library(rpart)
```

    ## 
    ## Attaching package: 'rpart'

    ## The following object is masked from 'package:dials':
    ## 
    ##     prune

``` r
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 4.0.5

``` r
censo <- rpart(is_canceled~., data = train, method = "class")

rpart.plot(censo)
```

![](Ayudantia11_files/figure-gfm/plot%20tree%20-1.png)<!-- -->

## Predict

No pude ejecutar

``` r
#pred_income <- predict(censo, newdata = test, type = "class")
#pred_income %>% as.data.frame() %>% head()
#pred_income %>% as.data.frame() %>% tail()

#test_data$predictedincome <- pred_income
```

``` r
## Prob para curva ROC

#pred_incom_roc <- predict(censo, newdata = test, type = "prob")
#pred_incom_roc %>% as.data.frame() %>% head()
#pred_incom_roc %>% as.data.frame() %>% tail()
#pred_incom_roc <- pred_incom_roc %>% as.data.frame()
#prob <- pred_incom_roc$"1"
```
