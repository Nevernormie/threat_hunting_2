# Lab5
Поташук Антон БИСО-01-20

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  ОС Windows
2.  RStudio
3.  “mir.csv-01.csv”

## План

1.  Запускаем RStudio
2.  Качаем пакеты dplyr и lubridate
3.  Анализируем данные
4.  Отвечаем на вопросы
5.  Создаём отчет

## Ход работы

Устанавливаем пакет dplyr с помощью команды install.packages(“dplyr”) и
пакет lubridate командой install.packages(“lubridate”)

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(lubridate)
```


    Присоединяю пакет: 'lubridate'

    Следующие объекты скрыты от 'package:base':

        date, intersect, setdiff, union

Импортируем данные из общего файла файла в 2 разных датасета

Датасет 1 – анонсы беспроводных точек доступа (\`dataset1\`)

``` r
dataset1 = read.csv("mir.csv-01.csv", nrows = 167)
dataset1 %>% glimpse()
```

    Rows: 167
    Columns: 15
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ First.time.seen <chr> " 2023-07-28 09:13:03", " 2023-07-28 09:13:03", " 2023…
    $ Last.time.seen  <chr> " 2023-07-28 11:50:50", " 2023-07-28 11:55:12", " 2023…
    $ channel         <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ Speed           <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ Privacy         <chr> " WPA2", " WPA2", " WPA2", " WPA2", " WPA2", " OPN", "…
    $ Cipher          <chr> " CCMP", " CCMP", " CCMP", " CCMP", " CCMP", " ", " CC…
    $ Authentication  <chr> " PSK", " PSK", " PSK", " PSK", " PSK", "   ", " PSK",…
    $ Power           <int> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ X..beacons      <int> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ X..IV           <int> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ LAN.IP          <chr> "   0.  0.  0.  0", "   0.  0.  0.  0", "   0.  0.  0.…
    $ ID.length       <int> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ ESSID           <chr> " C322U13 3965", " Cnet", " KC", " POCO X5 Pro 5G", " …
    $ Key             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` dataset1
  mutate_at(vars(BSSID, Privacy, Cipher, Authentication, LAN.IP, ESSID), trimws) %>% mutate_at(vars(BSSID, Privacy, Cipher, Authentication, LAN.IP, ESSID), na_if, "") %>%  mutate_at(vars(First.time.seen, Last.time.seen), as.POSIXct, format = "%Y-%m-%d %H:%M:%S")

dataset1 %>% head
```

Датасет 2 – запросы на подключение клиентов к известным им точкам
доступа (dataset2)

``` r
dataset2 = read.csv("mir.csv-01.csv", skip = 170)
dataset2 %>% glimpse()
```

    Rows: 12,269
    Columns: 7
    $ Station.MAC     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ First.time.seen <chr> " 2023-07-28 09:13:03", " 2023-07-28 09:13:03", " 2023…
    $ Last.time.seen  <chr> " 2023-07-28 10:59:44", " 2023-07-28 09:13:03", " 2023…
    $ Power           <chr> " -33", " -65", " -39", " -61", " -53", " -43", " -31"…
    $ X..packets      <chr> "      858", "        4", "      432", "      958", " …
    $ BSSID           <chr> " BE:F1:71:D5:17:8B", " (not associated) ", " BE:F1:71…
    $ Probed.ESSIDs   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

``` r
dataset2 <- dataset2 %>% 
  mutate_at(vars(Station.MAC, BSSID, Probed.ESSIDs),trimws) %>% mutate_at(vars(Station.MAC, BSSID, Probed.ESSIDs),na_if, "")

dataset2 <- dataset2 %>% mutate_at(vars(First.time.seen, Last.time.seen), as.POSIXct, format = "%Y-%m-%d %H:%M:%S") %>% mutate_at(vars(Power, X..packets), as.integer) %>% filter(!is.na(BSSID))
```

    Warning: There were 2 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `Power = .Primitive("as.integer")(Power)`.
    Caused by warning:
    ! в результате преобразования созданы NA
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
dataset2 %>% head
```

            Station.MAC     First.time.seen      Last.time.seen Power X..packets
    1 CA:66:3B:8F:56:DD 2023-07-28 09:13:03 2023-07-28 10:59:44   -33        858
    2 96:35:2D:3D:85:E6 2023-07-28 09:13:03 2023-07-28 09:13:03   -65          4
    3 5C:3A:45:9E:1A:7B 2023-07-28 09:13:03 2023-07-28 11:51:54   -39        432
    4 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16   -61        958
    5 5E:8E:A6:5E:34:81 2023-07-28 09:13:04 2023-07-28 09:13:04   -53          1
    6 10:51:07:CB:33:E7 2023-07-28 09:13:05 2023-07-28 11:56:06   -43        344
                  BSSID Probed.ESSIDs
    1 BE:F1:71:D5:17:8B  C322U13 3965
    2  (not associated)  IT2 Wireless
    3 BE:F1:71:D6:10:D7  C322U21 0566
    4 BE:F1:71:D5:17:8B  C322U13 3965
    5  (not associated)          <NA>
    6  (not associated)          <NA>

### Шаг 2. Анализ данных

#### Точки доступа

#### Задание 1. Определить небезопасные точки доступа (без шифрования – OPN)

``` r
open_wifi <- dataset1 %>% 
  filter(grepl("OPN", Privacy)) %>%
  select(BSSID, ESSID) %>%
  arrange(BSSID) %>%
  distinct

open_wifi
```

                   BSSID          ESSID
    1  00:03:7A:1A:03:56        MT_FREE
    2  00:03:7F:12:34:56        MT_FREE
    3  00:25:00:FF:94:73               
    4  00:26:99:F2:7A:E0               
    5  00:26:99:F2:7A:EF               
    6  00:3E:1A:5D:14:45        MT_FREE
    7  00:53:7A:99:98:56        MT_FREE
    8  00:AB:0A:00:10:10               
    9  02:67:F1:B0:6C:98        MT_FREE
    10 02:BC:15:7E:D5:DC        MT_FREE
    11 02:CF:8B:87:B4:F9        MT_FREE
    12 E0:D9:E3:48:FF:D2               
    13 E0:D9:E3:49:00:B1               
    14 E8:28:C1:DC:0B:B2               
    15 E8:28:C1:DC:33:12               
    16 E8:28:C1:DC:B2:40  MIREA_HOTSPOT
    17 E8:28:C1:DC:B2:41   MIREA_GUESTS
    18 E8:28:C1:DC:B2:42               
    19 E8:28:C1:DC:B2:50   MIREA_GUESTS
    20 E8:28:C1:DC:B2:51               
    21 E8:28:C1:DC:B2:52  MIREA_HOTSPOT
    22 E8:28:C1:DC:BD:50   MIREA_GUESTS
    23 E8:28:C1:DC:BD:52  MIREA_HOTSPOT
    24 E8:28:C1:DC:C6:B0   MIREA_GUESTS
    25 E8:28:C1:DC:C6:B1               
    26 E8:28:C1:DC:C6:B2               
    27 E8:28:C1:DC:C8:30   MIREA_GUESTS
    28 E8:28:C1:DC:C8:31               
    29 E8:28:C1:DC:C8:32  MIREA_HOTSPOT
    30 E8:28:C1:DC:FF:F2               
    31 E8:28:C1:DD:04:40  MIREA_HOTSPOT
    32 E8:28:C1:DD:04:41   MIREA_GUESTS
    33 E8:28:C1:DD:04:42               
    34 E8:28:C1:DD:04:50   MIREA_GUESTS
    35 E8:28:C1:DD:04:51               
    36 E8:28:C1:DD:04:52  MIREA_HOTSPOT
    37 E8:28:C1:DE:47:D0   MIREA_GUESTS
    38 E8:28:C1:DE:47:D1               
    39 E8:28:C1:DE:47:D2  MIREA_HOTSPOT
    40 E8:28:C1:DE:74:30               
    41 E8:28:C1:DE:74:31               
    42 E8:28:C1:DE:74:32  MIREA_HOTSPOT

#### Задание 2. Определить производителя для каждого обнаруженного устройства

\- 00:03:7A Taiyo Yuden Co., Ltd.

\- 00:03:7F Atheros Communications, Inc.

\- 00:25:00 Apple, Inc.

\- 00:26:99 Cisco Systems, Inc

\- E0:D9:E3 Eltex Enterprise Ltd.

\- E8:28:C1 Eltex Enterprise Ltd.

#### Задание 3. Выявить устройства, использующие последнюю версию протокола шифрования WPA3, и названия точек доступа, реализованных на этих устройствах

``` r
dataset1 %>%

  filter(grepl("WPA3", Privacy)) %>% select(BSSID, ESSID, Privacy)
```

                  BSSID               ESSID    Privacy
    1 26:20:53:0C:98:E8                      WPA3 WPA2
    2 A2:FE:FF:B8:9B:C9          Christie’s  WPA3 WPA2
    3 96:FF:FC:91:EF:64                      WPA3 WPA2
    4 CE:48:E7:86:4E:33  iPhone (Анастасия)  WPA3 WPA2
    5 8E:1F:94:96:DA:FD  iPhone (Анастасия)  WPA3 WPA2
    6 BE:FD:EF:18:92:44            Димасик   WPA3 WPA2
    7 3A:DA:00:F9:0C:02   iPhone XS Max 🦊🐱🦊  WPA3 WPA2
    8 76:C5:A0:70:08:96                      WPA3 WPA2

#### Задание 4. Отсортировать точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию

``` r
dataset1_with_intervals <- dataset2 %>% 
  mutate(Time.Interval = Last.time.seen - First.time.seen)

dataset1_with_intervals %>% 
  arrange(desc(Time.Interval)) %>% mutate(Time.Interval = seconds_to_period(Time.Interval)) %>% select(BSSID, First.time.seen, Last.time.seen, Time.Interval) %>% head
```

                  BSSID     First.time.seen      Last.time.seen Time.Interval
    1  (not associated) 2023-07-28 09:13:13 2023-07-28 11:56:17     2H 43M 4S
    2  (not associated) 2023-07-28 09:13:11 2023-07-28 11:56:13     2H 43M 2S
    3  (not associated) 2023-07-28 09:13:15 2023-07-28 11:56:17     2H 43M 2S
    4  (not associated) 2023-07-28 09:13:05 2023-07-28 11:56:06     2H 43M 1S
    5 8A:A3:03:73:52:08 2023-07-28 09:13:17 2023-07-28 11:56:16    2H 42M 59S
    6  (not associated) 2023-07-28 09:13:11 2023-07-28 11:56:07    2H 42M 56S

#### Задание 5. Обнаружить топ-10 самых быстрых точек доступа

``` r
top_10_fastest_spots <- dataset1 %>%

  arrange(desc(Speed)) %>%

  select(BSSID, ESSID, Speed, Privacy) %>%

  head(10)

top_10_fastest_spots
```

                   BSSID               ESSID Speed    Privacy
    1  26:20:53:0C:98:E8                       866  WPA3 WPA2
    2  96:FF:FC:91:EF:64                       866  WPA3 WPA2
    3  CE:48:E7:86:4E:33  iPhone (Анастасия)   866  WPA3 WPA2
    4  8E:1F:94:96:DA:FD  iPhone (Анастасия)   866  WPA3 WPA2
    5  9A:75:A8:B9:04:1E                  KC   360       WPA2
    6  4A:EC:1E:DB:BF:95      POCO X5 Pro 5G   360       WPA2
    7  56:C5:2B:9F:84:90          OnePlus 6T   360       WPA2
    8  E8:28:C1:DC:B2:41        MIREA_GUESTS   360        OPN
    9  E8:28:C1:DC:B2:40       MIREA_HOTSPOT   360        OPN
    10 E8:28:C1:DC:B2:42                       360        OPN

#### Задание 6. Отсортировать точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию

``` r
dataset1 %>% select(BSSID,ESSID,X..beacons) %>% arrange(desc(X..beacons))
```

                    BSSID                        ESSID X..beacons
    1   BE:F1:71:D6:10:D7                 C322U21 0566       1647
    2   1E:93:E3:1B:3C:F4                   Galaxy A71       1390
    3   0A:C5:E1:DB:17:7B                AndroidAP177B       1251
    4   BE:F1:71:D5:17:8B                 C322U13 3965        846
    5   6E:C7:EC:16:DA:1A                         Cnet        750
    6   AA:F4:3F:EE:49:0B             Redmi Note 8 Pro        738
    7   38:1A:52:0D:84:D7     EBFCD57F-EE81fI_DL_1AO2T        704
    8   9A:75:A8:B9:04:1E                           KC        694
    9   D2:6D:52:61:51:5D                                     647
    10  BE:F1:71:D5:0E:53                 C322U06 9080        617
    11  4A:EC:1E:DB:BF:95               POCO X5 Pro 5G        510
    12  4A:86:77:04:B7:28            iPhone (Искандер)        361
    13  56:C5:2B:9F:84:90                   OnePlus 6T        317
    14  E8:28:C1:DC:B2:51                                     279
    15  E8:28:C1:DC:B2:50                 MIREA_GUESTS        260
    16  76:70:AF:A4:D2:AF                         витя        253
    17  E8:28:C1:DC:B2:52                MIREA_HOTSPOT        251
    18  8E:55:4A:85:5B:01                     Vladimir        248
    19  3A:70:96:C6:30:2C            iPhone (Искандер)        145
    20  1C:7E:E5:8E:B7:DE                                     142
    21  38:1A:52:0D:85:1D     EBFCD5C1-EE81fI_DN11AOcY        130
    22  38:1A:52:0D:90:A1     EBFCD597-EE81fI_DMN1AOe1        112
    23  48:5B:39:F9:7A:48                                     109
    24  38:1A:52:0D:8F:EC     EBFCD6C2-EE81fI_DR21AOa0        107
    25  38:1A:52:0D:90:5D     EBFCD615-EE81fI_DOL1AO8w         90
    26  5E:C7:C0:E4:D7:D4                    realme 10         85
    27  00:26:99:F2:7A:E2                         GIVC         84
    28  36:46:53:81:12:A0           GGWPRedmi Note 10S         82
    29  00:26:99:F2:7A:E1                          IKB         65
    30  00:26:99:BA:75:80                         GIVC         61
    31  A2:64:E8:97:58:EE                     Mi Phone         52
    32  9E:A3:A9:D6:28:3C                                      51
    33  00:23:EB:E3:81:FE                          IKB         47
    34  00:23:EB:E3:81:FD                         GIVC         46
    35  0C:80:63:A9:6E:EE                                      42
    36  9E:A3:A9:DB:7E:01                                      40
    37  12:51:07:FF:29:D6         DESKTOP-KITJO8R 5262         32
    38  E8:28:C1:DD:04:40                MIREA_HOTSPOT         30
    39  38:1A:52:0D:97:60     EBFCD593-EE81fI_DMJ1AOI4         28
    40  A6:02:B9:73:2F:76                 C239U51 0701         26
    41  E8:28:C1:DD:04:41                 MIREA_GUESTS         25
    42  E8:28:C1:DD:04:42                                      23
    43  9A:9F:06:44:24:5B        Long Huong Galaxy M12         22
    44  E8:28:C1:DD:04:50                 MIREA_GUESTS         20
    45  00:23:EB:E3:81:F1                          IKB         19
    46  A6:02:B9:73:81:47                 C239U53 6056         19
    47  92:F5:7B:43:0B:69                     Redmi 12         18
    48  A6:02:B9:73:83:18                 C239U52 6706         17
    49  E8:28:C1:DC:C8:32                MIREA_HOTSPOT         12
    50  8E:1F:94:96:DA:FD           iPhone (Анастасия)         12
    51  86:DF:BF:E4:2F:23         DESKTOP-Q7R0KRV 2433         11
    52  E8:28:C1:DD:04:51                                       9
    53  9E:A3:A9:BF:12:C0                                       9
    54  96:FF:FC:91:EF:64                                       9
    55  CE:48:E7:86:4E:33           iPhone (Анастасия)          9
    56  E8:28:C1:DC:C8:31                                       8
    57  00:23:EB:E3:81:F2                         GIVC          7
    58  E8:28:C1:DC:C8:30                 MIREA_GUESTS          7
    59  B6:C4:55:B5:53:24                      Redmi 9          7
    60  F2:30:AB:E9:03:ED              iPhone (Uliana)          6
    61  1E:C2:8E:D8:30:91                         Damy          6
    62  E8:28:C1:DC:B2:41                 MIREA_GUESTS          5
    63  E8:28:C1:DC:B2:40                MIREA_HOTSPOT          5
    64  E8:28:C1:DC:B2:42                                       5
    65  E8:28:C1:DC:C6:B1                                       5
    66  D2:25:91:F6:6C:D8                        Саня           5
    67  E8:28:C1:DC:BD:50                 MIREA_GUESTS          5
    68  3A:DA:00:F9:0C:02            iPhone XS Max 🦊🐱🦊          5
    69  E8:28:C1:DD:04:52                MIREA_HOTSPOT          4
    70  E8:28:C1:DC:C6:B0                 MIREA_GUESTS          4
    71  B2:CF:C0:00:4A:60          Михаил's Galaxy M32          4
    72  26:20:53:0C:98:E8                                       3
    73  E8:28:C1:DE:47:D2                MIREA_HOTSPOT          3
    74  7E:3A:10:A7:59:4E                     vivo Y21          3
    75  9C:A5:13:28:D5:89               Galaxy M012063          3
    76  E8:28:C1:DE:74:31                                       2
    77  00:26:CB:AA:62:71                          IKB          2
    78  10:50:72:00:11:08               MGTS_GPON_B563          2
    79  0A:24:D8:D9:24:70                    IgorKotya          2
    80  2E:FE:13:D0:96:51             Redmi Note 8 Pro          2
    81  E8:28:C1:DE:74:32                MIREA_HOTSPOT          1
    82  76:E4:ED:B0:5C:9A               Инет от Путина          1
    83  56:99:98:EE:5A:4E                tementy-phone          1
    84  02:BC:15:7E:D5:DC                      MT_FREE          1
    85  C2:B5:D7:7F:07:A8  DIRECT-a8-HP M227f LaserJet          1
    86  E8:28:C1:DE:47:D1                                       1
    87  A2:FE:FF:B8:9B:C9                   Christie’s          1
    88  EA:D8:D1:77:C8:08      DIRECT-08-HP M428fdw LJ          1
    89  BA:2A:7A:DD:38:3E                 Айфон (Oleg)          1
    90  00:03:7A:1A:03:56                      MT_FREE          1
    91  76:5E:F3:F9:A5:1C                 Redmi 9C NFC          1
    92  00:03:7F:12:34:56                      MT_FREE          1
    93  00:3E:1A:5D:14:45                      MT_FREE          1
    94  E0:D9:E3:49:00:B1                                       1
    95  E8:28:C1:DC:BD:52                MIREA_HOTSPOT          1
    96  00:26:CB:AA:62:72                         GIVC          1
    97  6A:B0:1A:C2:DF:49                                       1
    98  02:67:F1:B0:6C:98                      MT_FREE          1
    99  76:C5:A0:70:08:96                                       1
    100 EA:7B:9B:D8:56:34                     POCO C40          1
    101 02:CF:8B:87:B4:F9                      MT_FREE          1
    102 E8:28:C1:DE:47:D0                 MIREA_GUESTS          1
    103 E8:28:C1:DC:FF:F2                                       0
    104 00:25:00:FF:94:73                                       0
    105 08:3A:2F:56:35:FE                                       0
    106 C6:BC:37:7A:67:0D            Mura's Galaxy A52          0
    107 E8:28:C1:DE:74:30                                       0
    108 E0:D9:E3:48:FF:D2                                       0
    109 12:48:F9:CF:58:8E                                       0
    110 00:26:99:F2:7A:E0                                       0
    111 2A:E8:A2:02:01:73                Redmi Note 9S          0
    112 E8:28:C1:DC:3C:92                                       0
    113 14:EB:B6:6A:76:37              Gnezdo_lounge 2          0
    114 E0:D9:E3:48:FF:D0                                       0
    115 E2:37:BF:8F:6A:7B                                       0
    116 8A:4E:75:44:5A:F6                  quiksmobile          0
    117 CE:B3:FF:84:45:FC                                       0
    118 00:03:7A:1A:18:56                                       0
    119 E8:28:C1:DC:54:72                                       0
    120 00:AB:0A:00:10:10                                       0
    121 00:09:9A:12:55:04                                       0
    122 E8:28:C1:DC:3A:B0                                       0
    123 E8:28:C1:DC:C6:B2                                       0
    124 E8:28:C1:DB:F5:F2                                       0
    125 BE:FD:EF:18:92:44                     Димасик           0
    126 E8:28:C1:DC:0B:B2                                       0
    127 E8:28:C1:DC:3C:80                                       0
    128 00:23:EB:E3:49:31                                       0
    129 00:23:EB:E3:44:31                                       0
    130 A6:F7:05:31:E8:EE                                       0
    131 12:54:1A:C6:FF:71                       Amuler          0
    132 CE:C3:F7:A4:7E:B3                 DIRECT-A6-HP          0
    133 E8:28:C1:DC:33:12                                       0
    134 E8:28:C1:DB:FC:F2                                       0
    135 00:26:99:BA:75:8F                                       0
    136 DC:09:4C:32:34:9B               kak_vashi_dela          0
    137 E8:28:C1:DC:F0:90                                       0
    138 E0:D9:E3:49:04:52                                       0
    139 E0:D9:E3:49:04:50                                       0
    140 E8:28:C1:DC:03:30                                       0
    141 E0:D9:E3:49:04:40                                       0
    142 B2:1B:0C:67:0A:BD                                       0
    143 E8:28:C1:DC:54:B0                                       0
    144 E0:D9:E3:49:00:B0                                       0
    145 E8:28:C1:DC:33:10                                       0
    146 E8:28:C1:DB:F5:F0                 MIREA_GUESTS          0
    147 E8:28:C1:DE:72:D0                                       0
    148 E0:D9:E3:49:04:41                                       0
    149 00:26:99:F1:1A:E1                          IKB          0
    150 8A:A3:03:73:52:08              Galaxy A30s5208          0
    151 00:23:EB:E3:44:32                                       0
    152 E0:D9:E3:48:B4:D2                                       0
    153 AE:3E:7F:C8:BC:8E                     Igorek✨          0
    154 02:B3:45:5A:05:93                     HONOR 30          0
    155 00:00:00:00:00:00                                       0
    156 E8:28:C1:DC:3C:90                                       0
    157 30:B4:B8:11:C0:90                                       0
    158 00:26:99:F2:7A:EF                                       0
    159 22:C9:7F:A9:BA:9C                                       0
    160 92:12:38:E5:7E:1E                                       0
    161 E8:28:C1:DC:0B:B0                                       0
    162 82:CD:7D:04:17:3B                                       0
    163 E8:28:C1:DC:03:32                                       0
    164 E8:28:C1:DC:54:B2                                       0
    165 00:53:7A:99:98:56                      MT_FREE          0
    166 00:03:7F:10:17:56                                       0
    167 00:0D:97:6B:93:DF                                       0

#### Данные клиентов

#### Задание 1. Определить производителя для каждого обнаруженного устройства

``` r
dataset2 %>%

  filter(grepl("(..:..:..:)(..:..:..)", BSSID)) %>%

  distinct(BSSID)
```

                   BSSID
    1  BE:F1:71:D5:17:8B
    2  BE:F1:71:D6:10:D7
    3  1E:93:E3:1B:3C:F4
    4  E8:28:C1:DC:FF:F2
    5  00:25:00:FF:94:73
    6  00:26:99:F2:7A:E2
    7  0C:80:63:A9:6E:EE
    8  E8:28:C1:DD:04:52
    9  0A:C5:E1:DB:17:7B
    10 9A:75:A8:B9:04:1E
    11 8A:A3:03:73:52:08
    12 4A:EC:1E:DB:BF:95
    13 BE:F1:71:D5:0E:53
    14 08:3A:2F:56:35:FE
    15 6E:C7:EC:16:DA:1A
    16 2A:E8:A2:02:01:73
    17 E8:28:C1:DC:B2:52
    18 E8:28:C1:DC:C6:B2
    19 E8:28:C1:DC:C8:32
    20 56:C5:2B:9F:84:90
    21 9A:9F:06:44:24:5B
    22 12:48:F9:CF:58:8E
    23 E8:28:C1:DD:04:50
    24 AA:F4:3F:EE:49:0B
    25 3A:70:96:C6:30:2C
    26 E8:28:C1:DC:3C:92
    27 8E:55:4A:85:5B:01
    28 5E:C7:C0:E4:D7:D4
    29 E2:37:BF:8F:6A:7B
    30 96:FF:FC:91:EF:64
    31 CE:B3:FF:84:45:FC
    32 00:26:99:BA:75:80
    33 76:70:AF:A4:D2:AF
    34 E8:28:C1:DC:B2:50
    35 00:AB:0A:00:10:10
    36 E8:28:C1:DC:C8:30
    37 8E:1F:94:96:DA:FD
    38 E8:28:C1:DB:F5:F2
    39 E8:28:C1:DD:04:40
    40 EA:7B:9B:D8:56:34
    41 BE:FD:EF:18:92:44
    42 7E:3A:10:A7:59:4E
    43 00:26:99:F2:7A:E1
    44 00:23:EB:E3:49:31
    45 E8:28:C1:DC:B2:40
    46 E0:D9:E3:49:04:40
    47 3A:DA:00:F9:0C:02
    48 E8:28:C1:DC:B2:41
    49 E8:28:C1:DE:74:32
    50 E8:28:C1:DC:33:12
    51 92:F5:7B:43:0B:69
    52 DC:09:4C:32:34:9B
    53 E8:28:C1:DC:F0:90
    54 E0:D9:E3:49:04:52
    55 22:C9:7F:A9:BA:9C
    56 E8:28:C1:DD:04:41
    57 92:12:38:E5:7E:1E
    58 B2:1B:0C:67:0A:BD
    59 E8:28:C1:DC:33:10
    60 E0:D9:E3:49:04:41
    61 1E:C2:8E:D8:30:91
    62 A2:64:E8:97:58:EE
    63 A6:02:B9:73:2F:76
    64 A6:02:B9:73:81:47
    65 AE:3E:7F:C8:BC:8E
    66 B6:C4:55:B5:53:24
    67 86:DF:BF:E4:2F:23
    68 02:67:F1:B0:6C:98
    69 36:46:53:81:12:A0
    70 E8:28:C1:DC:0B:B0
    71 82:CD:7D:04:17:3B
    72 E8:28:C1:DC:54:B2
    73 00:03:7F:10:17:56
    74 00:0D:97:6B:93:DF

\- 00:03:7F Atheros Communications, Inc.

\- 00:0D:97 Hitachi Energy USA Inc.

\- 00:23:EB Cisco Systems, Inc

\- 00:25:00 Apple, Inc.

\- 00:26:99 Cisco Systems, Inc

\- 08:3A:2F Guangzhou Juan Intelligent Tech Joint Stock Co.,Ltd

\- 0C:80:63 Tp-Link Technologies Co.,Ltd.

\- DC:09:4C Huawei Technologies Co.,Ltd

\- E0:D9:E3 Eltex Enterprise Ltd.

\- E8:28:C1 Eltex Enterprise Ltd.

#### Задание 2. Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
dataset2 %>%

  filter(grepl("(..:..:..:)(..:..:..)", BSSID) & !is.na(Probed.ESSIDs)) %>%

  select(BSSID, Probed.ESSIDs) %>%

  group_by(BSSID, Probed.ESSIDs) %>%

  filter(n() > 1) %>%

  arrange(BSSID) %>%

  unique()
```

    # A tibble: 16 × 2
    # Groups:   BSSID, Probed.ESSIDs [16]
       BSSID             Probed.ESSIDs   
       <chr>             <chr>           
     1 00:26:99:BA:75:80 GIVC            
     2 00:26:99:F2:7A:E2 GIVC            
     3 0C:80:63:A9:6E:EE KOTIKI_XXX      
     4 1E:93:E3:1B:3C:F4 Galaxy A71      
     5 6E:C7:EC:16:DA:1A Cnet            
     6 8E:55:4A:85:5B:01 Vladimir        
     7 AA:F4:3F:EE:49:0B Redmi Note 8 Pro
     8 BE:F1:71:D5:17:8B C322U13 3965    
     9 E8:28:C1:DC:B2:50 MIREA_HOTSPOT   
    10 E8:28:C1:DC:B2:50 MIREA_GUESTS    
    11 E8:28:C1:DC:B2:52 MIREA_HOTSPOT   
    12 E8:28:C1:DC:C6:B2 MIREA_HOTSPOT   
    13 E8:28:C1:DC:F0:90 MIREA_GUESTS    
    14 E8:28:C1:DD:04:40 MIREA_HOTSPOT   
    15 E8:28:C1:DD:04:52 MIREA_HOTSPOT   
    16 E8:28:C1:DE:74:32 MIREA_HOTSPOT   

#### Задание 3. Кластеризовать запросы от устройств к точкам доступа по их именам. Определить время появления устройства в зоне радиовидимости и время выхода его из нее

``` r
clustered_data <- dataset2 %>%

  filter(!is.na(Probed.ESSIDs)) %>%

  group_by(Station.MAC, Probed.ESSIDs) %>%

  arrange(First.time.seen)

cluster_summary <- clustered_data %>%

  summarise(Cluster_Start_Time = min(First.time.seen),

            Cluster_End_Time = max(Last.time.seen),

            Total_Power = sum(Power))
```

    `summarise()` has grouped output by 'Station.MAC'. You can override using the
    `.groups` argument.

``` r
cluster_summary %>% head(10)
```

    # A tibble: 10 × 5
    # Groups:   Station.MAC [10]
       Station.MAC Probed.ESSIDs Cluster_Start_Time  Cluster_End_Time    Total_Power
       <chr>       <chr>         <dttm>              <dttm>                    <int>
     1 00:90:4C:E… Redmi         2023-07-28 09:16:59 2023-07-28 10:21:15         -65
     2 00:95:69:E… nvripcsuite   2023-07-28 09:13:11 2023-07-28 11:56:13         -55
     3 00:95:69:E… nvripcsuite   2023-07-28 09:13:15 2023-07-28 11:56:17         -33
     4 00:95:69:E… nvripcsuite   2023-07-28 09:13:11 2023-07-28 11:56:07         -69
     5 00:F4:8D:F… Redmi 12      2023-07-28 10:45:04 2023-07-28 11:43:26         -73
     6 02:00:00:0… xt3 w64dtgv5… 2023-07-28 09:54:40 2023-07-28 11:55:36         -67
     7 02:06:2B:A… Avenue611     2023-07-28 09:55:12 2023-07-28 09:55:12         -65
     8 02:1D:0F:A… iPhone (Дима… 2023-07-28 09:57:08 2023-07-28 09:57:08         -61
     9 02:32:DC:5… Timo Resort   2023-07-28 10:58:23 2023-07-28 10:58:24         -84
    10 02:35:E9:C… iPhone (Макс… 2023-07-28 10:00:55 2023-07-28 10:00:55         -88

#### Задание 4. Оценить стабильность уровня сигнала внури кластера во времени. Выявить наиболее стабильный кластер. Для оценки стабильности оценить математическое ожидание и среднеквадратичное отклонение для каждого найденного кластера.

``` r
stability_metrics <- clustered_data %>%

  group_by(Station.MAC, Probed.ESSIDs) %>%

  summarise(Mean_Power = mean(Power))
```

    `summarise()` has grouped output by 'Station.MAC'. You can override using the
    `.groups` argument.

``` r
stability_metrics %>%

  arrange((Mean_Power)) %>% head(1)
```

    # A tibble: 1 × 3
    # Groups:   Station.MAC [1]
      Station.MAC       Probed.ESSIDs  Mean_Power
      <chr>             <chr>               <dbl>
    1 8A:45:77:F9:7F:F4 iPhone (Дима )        -89

## Вывод

В ходе выполнения практической работы мной были закреплены навыки работы
с пакетом `dplyr` и языком R
