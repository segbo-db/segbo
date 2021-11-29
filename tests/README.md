Data checks for SegBo data tables
================
Steven Moran
(29 November, 2021)

Load libraries.

``` r
library(tidyverse)
library(testthat)
library(bib2df)
```

Load raw SegBo metadata and phoneme tables.

``` r
metadata <- read.csv('../data/SegBo database - Metadata.csv', stringsAsFactors = F)
phonemes <- read.csv('../data/SegBo database - Phonemes.csv', stringsAsFactors = F, na.strings = "")
```

Eyeball if there are any typos listed in various columns.

``` r
# table(metadata$Contributor)
table(phonemes$OnlyInLoanwords)
```

    ## 
    ##  mostly      no unknown     yes 
    ##     115     275     130    1142

``` r
table(phonemes$Result)
```

    ## 
    ##                  new phoneme  other distributional change 
    ##                          867                          221 
    ## phonologization of allophone                to be checked 
    ##                          105                          376 
    ##                      unknown 
    ##                           82

``` r
table(phonemes$NewDistinction)
```

    ## 
    ##                  affricate affricate, palato-alveolar 
    ##                         85                          2 
    ##            alveolo-palatal                approximant 
    ##                          2                         10 
    ##                 aspiration                back vowels 
    ##                          8                          1 
    ##           close-mid vowels                     dental 
    ##                          1                          3 
    ##                 diphthongs               flap, liquid 
    ##                          3                          1 
    ##                  fricative                    glottal 
    ##                          4                         18 
    ##                  implosive                 labialized 
    ##                          2                          1 
    ##                labiodental                 labiovelar 
    ##                         54                          3 
    ##                    lateral                 lax vowels 
    ##                         20                          2 
    ##                 mid vowels                         no 
    ##                         27                        534 
    ##                       null                    palatal 
    ##                          1                          6 
    ##         palatal, affricate            palato-alveolar 
    ##                          2                          7 
    ##                 pharyngeal               postalveolar 
    ##                          7                          4 
    ##               prenasalized                  retroflex 
    ##                          4                          1 
    ##                     rhotic                        tap 
    ##                         21                          3 
    ##                tap, rhotic              to be checked 
    ##                          1                        587 
    ##                      trill                    unknown 
    ##                          7                        135 
    ##                     uvular                      velar 
    ##                          6                          1 
    ##                  velarized       voiceless fricatives 
    ##                          3                          3 
    ##                    voicing      voicing in fricatives 
    ##                         30                         14 
    ##        voicing in plosives               vowel length 
    ##                         15                          1

Make sure there are no duplicate borrowed phonemes.

``` r
dups <- phonemes %>% group_by(InventoryID, BorrowedSound) %>% filter(n()>1) %>% select(InventoryID, BorrowingLanguageGlottocode, BorrowedSound)
expect_equal(nrow(dups), 0)
```

Check the bibliography.

``` r
path <- '../bibliography/bibliography.bib'
bib <- bib2df(path)
```

Some entries have multiple comma separated IDs.

``` r
# which(!(metadata$BibTexKey %in% bib$BIBTEXKEY))
# metadata[which(!(metadata$BibTexKey %in% bib$BIBTEXKEY)),]
```

Split them and get a list of all IDs.

``` r
keys <- bib$BIBTEXKEY
split.keys <- str_split(keys, ",")
split.keys <- split.keys %>% unlist()
split.keys <- str_trim(split.keys)
```

All accounted for?

``` r
expect_equal(nrow(metadata[which(!(split.keys %in% bib$BIBTEXKEY)),]), 0)
```

Matches both ways?

``` r
which(!(bib$BIBTEXKEY %in% split.keys))
```

    ## integer(0)

Inventory IDs in metadata and phoneme tables? Make sure all metadata
Inventory IDs have rows in phonemes.

``` r
length(which(!(metadata$InventoryID %in% phonemes$InventoryID)))
```

    ## [1] 0

``` r
metadata[which(!(metadata$InventoryID %in% phonemes$InventoryID)),]
```

    ## [1] InventoryID      Glottocode       Dialect          LanguageName    
    ## [5] BibTexKey        Filename         Contributor      MetadataComments
    ## [9] PhoibleID       
    ## <0 rows> (or 0-length row.names)

``` r
expect_equal(length(which(!(metadata$InventoryID %in% phonemes$InventoryID))), 0)
```

Make sure every phoneme ID is in metadata table.

``` r
phoneme.ids <- unique(phonemes$InventoryID)
expect_true(all(phoneme.ids %in% metadata$InventoryID))
```

Do the Glottocodes follow the correct format in the metadata?

``` r
glottocode <- "([a-z0-9]{4})([0-9]{4})"
expect_equal(length(which(!(str_detect(metadata$Glottocode, glottocode)))), 0)
which(!(str_detect(metadata$Glottocode, glottocode)))
```

    ## integer(0)

``` r
metadata[which(!(str_detect(metadata$Glottocode, glottocode))), ]
```

    ## [1] InventoryID      Glottocode       Dialect          LanguageName    
    ## [5] BibTexKey        Filename         Contributor      MetadataComments
    ## [9] PhoibleID       
    ## <0 rows> (or 0-length row.names)

Do the Glottocodes follow the correct format in the phonemes?

``` r
# expect_equal(length(which(!(str_detect(phonemes$BorrowingLanguageGlottocode, glottocode)))), 0)
which(!(str_detect(phonemes$BorrowingLanguageGlottocode, glottocode)))
```

    ## integer(0)

``` r
metadata[which(!(str_detect(phonemes$BorrowingLanguageGlottocode, glottocode))), ]
```

    ## [1] InventoryID      Glottocode       Dialect          LanguageName    
    ## [5] BibTexKey        Filename         Contributor      MetadataComments
    ## [9] PhoibleID       
    ## <0 rows> (or 0-length row.names)

Let’s check that all Glottocodes are at the language level in the
metadata file.

``` r
glottolog <- read_csv('https://cdstar.eva.mpg.de/bitstreams/EAEA0-E62D-ED67-FD05-0/languages_and_dialects_geo.csv')
metadata[which(!(metadata$Glottocode %in% glottolog$glottocode)),]
```

    ## [1] InventoryID      Glottocode       Dialect          LanguageName    
    ## [5] BibTexKey        Filename         Contributor      MetadataComments
    ## [9] PhoibleID       
    ## <0 rows> (or 0-length row.names)

``` r
expect_true(all(metadata$Glottocode %in% glottolog$glottocode))
```

Let’s check that all Glottocodes are at the language level at the source
level in the phonemes file. First we need to unlist the column, which
may have multiple language codes in it.

``` r
source_lang_codes <- phonemes %>% 
  select(SourceLanguageGlottocode) %>%
  mutate(SourceLanguageGlottocode = strsplit(SourceLanguageGlottocode, ",")) %>%
  unnest(SourceLanguageGlottocode)

source_lang_codes$SourceLanguageGlottocode <- str_trim(source_lang_codes$SourceLanguageGlottocode)
```

Which data points do not have language level coverage in Glottolog?
These should only be “unknown”.

``` r
expect_equal(nrow(source_lang_codes %>% filter(is.na(SourceLanguageGlottocode))),0)
x <- source_lang_codes[which(!(source_lang_codes$SourceLanguageGlottocode %in% glottolog$glottocode)),]
table(x$SourceLanguageGlottocode)
```

    ## 
    ## gree1276  unknown 
    ##        2      472

To check whether the donor language is reported as the borrowing
language in terms of Glottocodes, we first get the mappings from source
language to borrowing language. And then test that none are the same.

``` r
lang_mappings <- phonemes %>% select(BorrowingLanguageGlottocode, SourceLanguageGlottocode)

lang_mappings <- lang_mappings %>% 
  mutate(SourceLanguageGlottocode = strsplit(as.character(SourceLanguageGlottocode), ",")) %>% 
  unnest(SourceLanguageGlottocode) %>% 
  arrange(SourceLanguageGlottocode, BorrowingLanguageGlottocode) %>%
  select(SourceLanguageGlottocode, BorrowingLanguageGlottocode)

expect_equal(nrow(lang_mappings %>% filter(SourceLanguageGlottocode == BorrowingLanguageGlottocode)), 0)
```

Check whether the segments in SegBo are also reported in
[PHOIBLE](https://phoible.org). At the current time, this rhotic segment
reported by Mahanta (2012) in Assamese (ID 285, assa1263) is under
investigation (it is reported as a aspirated rhotic from Sanskrit).

``` r
phoible <- read_csv('https://raw.githubusercontent.com/phoible/dev/master/data/phoible.csv')
phoible_segments <- phoible %>% select(Phoneme) %>% distinct()
segbo_phonemes <- phonemes %>% select(BorrowedSound) %>% distinct()
segbo_phonemes[which(!(segbo_phonemes$BorrowedSound %in% phoible_segments$Phoneme)),]
```

    ## [1] "ɹ̤"

How many languages in SegBo are not in PHOIBLE?

``` r
tmp1 <- metadata %>% select(Glottocode) %>% distinct()
tmp2 <- phoible %>% select(Glottocode) %>% distinct()
```

There are this many languages (glottocodes) in SegBo.

``` r
nrow(tmp1)
```

    ## [1] 497

There are this many languages in SegBo not in PHOIBLE.

``` r
length(tmp1[which(!(tmp1$Glottocode %in% tmp2$Glottocode)),])
```

    ## [1] 198

These are the languages that are not in PHOIBLE.

``` r
tmp1[which(!(tmp1$Glottocode %in% tmp2$Glottocode)),]
```

    ##   [1] "abui1241" "abun1252" "musa1266" "akaj1239" "amap1240" "wara1294"
    ##   [7] "lish1245" "asil1242" "assy1241" "bela1254" "lish1247" "bier1244"
    ##  [13] "boht1238" "cahu1264" "coat1238" "copt1239" "dame1241" "dusn1237"
    ##  [19] "edol1239" "gant1244" "gura1251" "gran1245" "hert1241" "itza1241"
    ##  [25] "kala1373" "kaur1271" "kaza1248" "kend1254" "kham1281" "khva1239"
    ##  [31] "nort3142" "kipu1237" "doro1266" "kome1238" "kona1242" "kora1294"
    ##  [37] "kute1249" "louu1245" "maka1316" "mana1298" "payn1244" "nucl1706"
    ##  [43] "mauw1238" "mehe1243" "mian1256" "miga1241" "mlah1239" "mong1330"
    ##  [49] "mudu1242" "muss1246" "sout2857" "naka1262" "ngka1235" "noga1249"
    ##  [55] "ocot1243" "oira1263" "pina1252" "pipi1250" "rian1263" "hula1244"
    ##  [61] "sawe1240" "nort3139" "shor1247" "slav1254" "sooo1254" "sout2961"
    ##  [67] "sril1245" "suab1238" "sout2808" "tafi1243" "bunu1267" "tauy1241"
    ##  [73] "thao1240" "tond1251" "turk1303" "turo1239" "ughe1237" "vafs1240"
    ##  [79] "wano1243" "wara1302" "west2548" "moro1287" "wutu1241" "yima1243"
    ##  [85] "high1276" "iron1242" "ukaa1243" "caml1239" "chop1243" "midd1357"
    ##  [91] "mart1256" "kolc1235" "abua1245" "agut1237" "akoy1238" "alor1247"
    ##  [97] "amar1273" "anem1249" "apma1240" "aral1243" "baik1238" "bala1315"
    ## [103] "balu1257" "beka1241" "blag1240" "bobo1255" "boto1242" "buli1255"
    ## [109] "buna1278" "buru1303" "cent2100" "cent2101" "chek1238" "ciac1237"
    ## [115] "daak1235" "dehu1237" "domm1246" "dumb1241" "ende1246" "foii1241"
    ## [121] "ford1242" "gadd1244" "galo1243" "helo1243" "hiww1237" "inon1237"
    ## [127] "jamb1236" "kaga1255" "kaga1256" "kana1286" "kapr1245" "kemb1249"
    ## [133] "keri1250" "komo1261" "kove1237" "laiy1246" "lala1268" "lama1277"
    ## [139] "lamp1242" "lamp1243" "lari1255" "lewo1244" "lund1271" "mama1276"
    ## [145] "mans1262" "mati1250" "maya1282" "mini1251" "molb1237" "mort1237"
    ## [151] "nali1244" "nang1262" "napu1241" "naue1237" "neng1238" "nese1235"
    ## [157] "nucl1594" "pagu1249" "papu1250" "patt1249" "pend1242" "pile1238"
    ## [163] "puka1242" "rata1244" "saar1237" "sahu1245" "samo1305" "sawi1256"
    ## [169] "siwa1245" "sout2883" "sumb1241" "tagb1258" "taji1246" "teop1238"
    ## [175] "tetu1246" "tido1248" "timu1262" "tiri1258" "toab1237" "tobe1252"
    ## [181] "tobi1239" "tuam1242" "tugu1245" "unua1237" "urak1238" "vinm1237"
    ## [187] "vure1239" "waim1252" "wand1267" "woli1241" "wotu1240" "wutu1244"
    ## [193] "yabe1254" "yaum1237" "yuag1237" "rapa1244" "kana1281" "puma1239"
