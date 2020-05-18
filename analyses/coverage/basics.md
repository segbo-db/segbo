SegBo: basic descriptive stats
================
Steven Moran
18 May, 2020

``` r
# Load the data
segbo <- read.csv('../../data/segbo_with_glottolog.csv')
```

``` r
# Number of observations, i.e. number of borrowed sounds across the languages in SegBo
head(segbo %>% select(LanguageName, BorrowedSound) %>% distinct()) %>% kable()
```

| LanguageName | BorrowedSound |
|:-------------|:--------------|
| Abau         | d̠ʒ            |
| Abau         | t             |
| Abui         | ɡ             |
| Abui         | ɟ             |
| Abui         | ts            |
| Abun         | d̠ʒ            |

``` r
nrow(segbo %>% select(LanguageName, BorrowedSound) %>% distinct())
```

    ## [1] 1666

``` r
# Number of distinct donor languages or dialects (via Glottocodes)
head(segbo %>% select(SourceLanguageGlottocode) %>% distinct()) %>% kable() 
```

| SourceLanguageGlottocode |
|:-------------------------|
| tokp1240                 |
| stan1306, indo1316       |
| stan1306                 |
| arab1395                 |
| unknown                  |
| stan1288, huam1247       |

``` r
nrow(segbo %>% select(SourceLanguageGlottocode) %>% distinct())
```

    ## [1] 211

``` r
# Number of distinct borrowing languages or dialects (via Glottocodes)
head(segbo %>% select(BorrowingLanguageGlottocode) %>% distinct()) %>% kable()
```

| BorrowingLanguageGlottocode |
|:----------------------------|
| abau1245                    |
| abui1241                    |
| abun1252                    |
| achi1257                    |
| agua1253                    |
| musa1266                    |

``` r
nrow(segbo %>% select(BorrowingLanguageGlottocode) %>% distinct())
```

    ## [1] 514

``` r
# Number of distinct borrowing languages (as marked via ISO 639-3 codes)
head(segbo %>% select(iso639P3code) %>% distinct()) %>% kable()
```

| iso639P3code |
|:-------------|
| aau          |
| abz          |
| kgr          |
| ace          |
| agr          |
| mmq          |

``` r
nrow(segbo %>% select(iso639P3code) %>% distinct())
```

    ## [1] 469

``` r
# Number of families for borrowing languages (note the empty field because we don't know for many data points where the sound was borrowed from)
x <- segbo %>% select(SourceLanguageGlottocode, family_id)
y <- x %>% group_by(family_id) %>% summarize(count=n())
head(y) %>% kable()
```

| family\_id |  count|
|:-----------|------:|
|            |     41|
| abkh1242   |      1|
| afro1255   |    144|
| algi1248   |      1|
| anga1289   |      2|
| arau1255   |      7|

``` r
nrow(y)
```

    ## [1] 96

``` r
# And number of families for source languages -- this isn't going to work now because the language family is tied to the BorrowingLanguage...
x <- segbo %>% select(LanguageName, family_id)
y <- x %>% group_by(family_id) %>% summarize(count=n())
head(y) %>% kable()
```

| family\_id |  count|
|:-----------|------:|
|            |     41|
| abkh1242   |      1|
| afro1255   |    144|
| algi1248   |      1|
| anga1289   |      2|
| arau1255   |      7|

``` r
nrow(y)
```

    ## [1] 96
