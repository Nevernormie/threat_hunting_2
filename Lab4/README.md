# Lab4
Поташук Антон БИСО-01-20

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  ОС Windows
2.  RStudio
3.  “dns.log”
4.  header.csv

## План

1.  Запускаем RStudio
2.  Качаем пакет dplyr
3.  Импортируем и подготавливаем данные DNS
4.  Анализ данных
5.  Обогащение данных

## Ход работы

Устанавливаем пакет dplyr с помощью команды install.packages(“dplyr”)

Загружаем библиотеку dplyr

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

### Импорт и подготовка данных DNS

#### 1. Импортируйте данные DNS

``` r
dns_data <- read.table("dns.log", header = FALSE, sep = "\t", quote = "", encoding="UTF-8")
```

#### 2. Добавьте пропущенные данные о структуре данных (назначении столбцов)

В файле header.csv не хватает некоторых столбцов и данных о них, с
помощью открытых источников добавим их вручную.

``` r
colnames(dns_data) <- read.csv("header.csv", header = FALSE, skip = 1)$V1
```

#### 3. Просмотрите общую структуру данных с помощью функции glimpse()

``` r
dns_data %>%
  glimpse()
```

    Rows: 427,935
    Columns: 23
    $ ts          <dbl> 1331901006, 1331901015, 1331901016, 1331901017, 1331901006…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bs…
    $ id.orig_h   <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "19…
    $ id.orig_p   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ id.resp_h   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "1…
    $ id_resp_p   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ proto       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "u…
    $ trans_id    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 62…
    $ query       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass      <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"…
    $ qclass_name <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C…
    $ qtype       <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "32"…
    $ qtype_name  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB…
    $ rcode       <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rcode_name  <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-…
    $ AA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ TC          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ RD          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    $ RA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ Z           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0…
    $ answers     <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ TTLs        <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rejected    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

### Анализ данных

#### 4. Сколько участников информационного обмена в сети Доброй Организации?

``` r
orig_h <- unique(dns_data$`id.orig_h`)
resp_h <- unique(dns_data$`id.resp_h`)

hosts <- c(orig_h, resp_h)

hosts %>%
  unique() %>%
  length()
```

    [1] 1359

#### 5. Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?

``` r
internal_ip_pattern <- c("192.168.", "10.", "100.([6-9]|1[0-1][0-9]|12[0-7]).", "172.((1[6-9])|(2[0-9])|(3[0-1])).")
internal_ips <- hosts[grep(paste(internal_ip_pattern, collapse = "|"), hosts)]
internal_ips_cnt <- sum(hosts %in% internal_ips)
external_ips_cnt <- length(hosts) - internal_ips_cnt

ratio <- internal_ips_cnt / external_ips_cnt
ratio
```

    [1] 16.24419

#### 6. Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.

``` r
top_hosts <- dns_data %>%
  group_by(id.orig_h) %>%
  summarise(request_count = n()) %>%
  arrange(desc(request_count)) %>%
  top_n(10, request_count)

print(top_hosts)
```

    # A tibble: 10 × 2
       id.orig_h       request_count
       <chr>                   <int>
     1 10.10.117.210           75943
     2 192.168.202.93          26522
     3 192.168.202.103         18121
     4 192.168.202.76          16978
     5 192.168.202.97          16176
     6 192.168.202.141         14967
     7 10.10.117.209           14222
     8 192.168.202.110         13372
     9 192.168.203.63          12148
    10 192.168.202.106         10784

#### 7. Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

``` r
top_domains <- dns_data %>%
  group_by(query) %>%
  summarise(request_count = n()) %>%
  arrange(desc(request_count)) %>%
  top_n(10, request_count)

print(top_domains)
```

    # A tibble: 10 × 2
       query                                                           request_count
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00…         10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

#### 8. Опеределите базовые статистические характеристики (функция summary()) интервала времени между последовательным обращениями к топ-10 доменам.

``` r
top_domains_filtered <- dns_data %>% 
  filter(tolower(query) %in% top_domains$query) %>%
  arrange(ts)
time_intervals <- diff(top_domains_filtered$ts)

summary(time_intervals)
```

        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.00     0.10     1.07     0.54 49677.59 

#### 9. Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?

``` r
ip_domain_counts <- dns_data %>%
  group_by(ip = `id.orig_h`, query) %>%
  summarise(request_count = n()) %>%
  filter(request_count > 1)
```

    `summarise()` has grouped output by 'ip'. You can override using the `.groups`
    argument.

``` r
unique_ips_with_periodic_requests <- unique(ip_domain_counts$ip)

unique_ips_with_periodic_requests %>% length()
```

    [1] 240

``` r
unique_ips_with_periodic_requests %>% head()
```

    [1] "10.10.10.10"     "10.10.117.209"   "10.10.117.210"   "128.244.37.196" 
    [5] "169.254.109.123" "169.254.228.26" 

### Обогащение данных

#### 10. Определите местоположение (страну, город) и организацию-провайдера для топ-10 доменов. Для этого можно использовать сторонние сервисы, например <https://v4.ifconfig.co/.>

``` r
top_domains
```

    # A tibble: 10 × 2
       query                                                           request_count
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00…         10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

1.  teredo.ipv6.microsoft.com

-   IP: 20.112.250.133
-   Country: United States
-   Timezone: America/Chicago
-   Organization: Microsoft

1.  tools.google.com

-   IP: 173.194.222.100
-   Country: United States
-   Timezone: America/Chicago
-   Organization: Google

1.  www.apple.com

-   IP: 17.253.144.10
-   Country: United States
-   Timezone: America/Chicago
-   Organization: Apple-Engineering

1.  safebrowsing.clients.google.com

-   IP: 64.233.164.100
-   Country: United States
-   Timezone: America/Chicago
-   Organization: Google

1.  44.206.168.192.in-addr.arpa

-   IP: 44.206.168.192
-   Country: United States
-   City: Ashburn
-   Timezone: America/New_York
-   Organization: Amazon

## Вывод

В ходе лабораторной работы были импортированы, подготовлены,
проанализированы и обогащены данные трафика DNS.
