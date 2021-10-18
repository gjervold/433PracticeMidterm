midterm
================
Karl Rohe

## Midterm.

#### 1. Map the delay by destination.

Compute the average delay by destination, then join on the airports data
frame so you can show the spatial distribution of delays. Here’s an easy
way to draw a map of the United States. You are welcome to use this code
or some other code.

``` r
#install.packages("tidyverse")
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

You might want to use the size or colour of the points to display the
average delay for each airport.

``` r
flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
planes
```

    ## # A tibble: 3,322 x 9
    ##    tailnum  year type          manufacturer   model  engines seats speed engine 
    ##    <chr>   <int> <chr>         <chr>          <chr>    <int> <int> <int> <chr>  
    ##  1 N10156   2004 Fixed wing m~ EMBRAER        EMB-1~       2    55    NA Turbo-~
    ##  2 N102UW   1998 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ##  3 N103US   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ##  4 N104UW   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ##  5 N10575   2002 Fixed wing m~ EMBRAER        EMB-1~       2    55    NA Turbo-~
    ##  6 N105UW   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ##  7 N107US   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ##  8 N108UW   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ##  9 N109UW   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ## 10 N110UW   1999 Fixed wing m~ AIRBUS INDUST~ A320-~       2   182    NA Turbo-~
    ## # ... with 3,312 more rows

``` r
airlines
```

    ## # A tibble: 16 x 2
    ##    carrier name                       
    ##    <chr>   <chr>                      
    ##  1 9E      Endeavor Air Inc.          
    ##  2 AA      American Airlines Inc.     
    ##  3 AS      Alaska Airlines Inc.       
    ##  4 B6      JetBlue Airways            
    ##  5 DL      Delta Air Lines Inc.       
    ##  6 EV      ExpressJet Airlines Inc.   
    ##  7 F9      Frontier Airlines Inc.     
    ##  8 FL      AirTran Airways Corporation
    ##  9 HA      Hawaiian Airlines Inc.     
    ## 10 MQ      Envoy Air                  
    ## 11 OO      SkyWest Airlines Inc.      
    ## 12 UA      United Air Lines Inc.      
    ## 13 US      US Airways Inc.            
    ## 14 VX      Virgin America             
    ## 15 WN      Southwest Airlines Co.     
    ## 16 YV      Mesa Airlines Inc.

``` r
airports
```

    ## # A tibble: 1,458 x 8
    ##    faa   name                             lat    lon   alt    tz dst   tzone    
    ##    <chr> <chr>                          <dbl>  <dbl> <dbl> <dbl> <chr> <chr>    
    ##  1 04G   Lansdowne Airport               41.1  -80.6  1044    -5 A     America/~
    ##  2 06A   Moton Field Municipal Airport   32.5  -85.7   264    -6 A     America/~
    ##  3 06C   Schaumburg Regional             42.0  -88.1   801    -6 A     America/~
    ##  4 06N   Randall Airport                 41.4  -74.4   523    -5 A     America/~
    ##  5 09J   Jekyll Island Airport           31.1  -81.4    11    -5 A     America/~
    ##  6 0A9   Elizabethton Municipal Airport  36.4  -82.2  1593    -5 A     America/~
    ##  7 0G6   Williams County Airport         41.5  -84.5   730    -5 A     America/~
    ##  8 0G7   Finger Lakes Regional Airport   42.9  -76.8   492    -5 A     America/~
    ##  9 0P2   Shoestring Aviation Airfield    39.8  -76.6  1000    -5 U     America/~
    ## 10 0S9   Jefferson County Intl           48.1 -123.    108    -8 A     America/~
    ## # ... with 1,448 more rows

``` r
weather
```

    ## # A tibble: 26,115 x 15
    ##    origin  year month   day  hour  temp  dewp humid wind_dir wind_speed
    ##    <chr>  <int> <int> <int> <int> <dbl> <dbl> <dbl>    <dbl>      <dbl>
    ##  1 EWR     2013     1     1     1  39.0  26.1  59.4      270      10.4 
    ##  2 EWR     2013     1     1     2  39.0  27.0  61.6      250       8.06
    ##  3 EWR     2013     1     1     3  39.0  28.0  64.4      240      11.5 
    ##  4 EWR     2013     1     1     4  39.9  28.0  62.2      250      12.7 
    ##  5 EWR     2013     1     1     5  39.0  28.0  64.4      260      12.7 
    ##  6 EWR     2013     1     1     6  37.9  28.0  67.2      240      11.5 
    ##  7 EWR     2013     1     1     7  39.0  28.0  64.4      240      15.0 
    ##  8 EWR     2013     1     1     8  39.9  28.0  62.2      250      10.4 
    ##  9 EWR     2013     1     1     9  39.9  28.0  62.2      260      15.0 
    ## 10 EWR     2013     1     1    10  41    28.0  59.6      260      13.8 
    ## # ... with 26,105 more rows, and 5 more variables: wind_gust <dbl>,
    ## #   precip <dbl>, pressure <dbl>, visib <dbl>, time_hour <dttm>

``` r
# answer
flights2 = flights %>%
  group_by(dest) %>%
  summarize(mean_delay = mean(arr_delay), na.rm=T)

airports %>%
  left_join(flights2, c("faa" = "dest")) %>%
  filter(complete.cases(mean_delay)) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point(aes(col=mean_delay)) +
  coord_quickmap()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### 2. Do planes trade ownership?

You might expect that there’s an implicit relationship between plane and
airline, because each plane is flown by a single airline. Explore this
conjecture using data. (Let’s assume that the tail number of a plane
does not change.)

``` r
# answer
flights %>%
  group_by(tailnum, carrier) %>%
  summarize(n=n()) %>%
  group_by(tailnum) %>%
  summarize(n=n()) %>%
  filter(n>1)
```

    ## `summarise()` has grouped output by 'tailnum'. You can override using the `.groups` argument.

    ## # A tibble: 18 x 2
    ##    tailnum     n
    ##    <chr>   <int>
    ##  1 N146PQ      2
    ##  2 N153PQ      2
    ##  3 N176PQ      2
    ##  4 N181PQ      2
    ##  5 N197PQ      2
    ##  6 N200PQ      2
    ##  7 N228PQ      2
    ##  8 N232PQ      2
    ##  9 N933AT      2
    ## 10 N935AT      2
    ## 11 N977AT      2
    ## 12 N978AT      2
    ## 13 N979AT      2
    ## 14 N981AT      2
    ## 15 N989AT      2
    ## 16 N990AT      2
    ## 17 N994AT      2
    ## 18 <NA>        7

#### 3a. Plane’s average speed.

Notice that `flights$air_time` is in minutes. Make a new column that is
the air time in hours.

``` r
# answer
flights %>%
  mutate(time_h = air_time/60)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   time_h <dbl>

#### 4b. Average speed

For each flight, compute the average speed of that flight (in miles per
hour). Then, for each plane, compute the average of those average
speeds. Display it in a histogram. You can use a base R histogram `hist`
or ggplot’s `geom_histogram`.

``` r
# answer
flights %>%
  mutate(time_h = air_time/60,
    avg_mph = distance/time_h) %>%
  group_by(tailnum) %>%
  summarize(mean_mph = mean(avg_mph)) %>%
  ggplot(aes(x=mean_mph)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1855 rows containing non-finite values (stat_bin).

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### 5. Bonus

Make a table where each row is a destination, each column is a carrier,
and each element is the number of times that the carrier has flown to
that destination. Ensure that you only count flights that arrived at the
destination.

``` r
# answer
table <- flights %>%
  filter(complete.cases(arr_time)) %>%
  group_by(carrier, dest) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = dest,
              values_from = n,
              values_fill = 0)
```

    ## `summarise()` has grouped output by 'carrier'. You can override using the `.groups` argument.

``` r
table
```

    ## # A tibble: 16 x 105
    ## # Groups:   carrier [16]
    ##    carrier   ATL   AUS   AVL   BNA   BOS   BTV   BUF   BWI   CAE   CHS   CLE
    ##    <chr>   <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 9E         56     2    10   452   853     2   790   816     3   332   322
    ##  2 AA          0   359     0     0  1430     0     0     0     0     0     0
    ##  3 AS          0     0     0     0     0     0     0     0     0     0     0
    ##  4 B6          0   744     0     0  4326  1348  2773     0     0   612     0
    ##  5 DL      10476   353     0     1   962     0     3     0     0     0     0
    ##  6 EV       1660     0   253  2059   156  1162  1005   339   103  1825   556
    ##  7 F9          0     0     0     0     0     0     0     0     0     0     0
    ##  8 FL       2284     0     0     0     0     0     0     0     0     0     0
    ##  9 HA          0     0     0     0     0     0     0     0     0     0     0
    ## 10 MQ       2237     0     0  2306     0     0     0   333     0     0  1636
    ## 11 OO          0     0     0     0     0     0     0     0     0     0    21
    ## 12 UA        102   664     0     0  3299     0     0     0     0     1  1864
    ## 13 US          0     0     0     0  4002     0     0     0     0     0     0
    ## 14 VX          0     0     0     0     0     0     0     0     0     0     0
    ## 15 WN         58   295     0  1273     0     0     0   200     0     0     0
    ## 16 YV          0     0     0     0     0     0     0     0     0     0     0
    ## # ... with 93 more variables: CLT <int>, CMH <int>, CVG <int>, DAY <int>,
    ## #   DCA <int>, DFW <int>, DSM <int>, DTW <int>, GRR <int>, GSO <int>,
    ## #   GSP <int>, IAD <int>, IND <int>, JAX <int>, LEX <int>, MCI <int>,
    ## #   MEM <int>, MHT <int>, MKE <int>, MSN <int>, MSP <int>, MSY <int>,
    ## #   MVY <int>, ORD <int>, ORF <int>, PHL <int>, PIT <int>, RDU <int>,
    ## #   RIC <int>, ROC <int>, RSW <int>, SAT <int>, SDF <int>, SRQ <int>,
    ## #   SYR <int>, TPA <int>, TYS <int>, EGE <int>, FLL <int>, IAH <int>, ...
