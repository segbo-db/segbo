Data checks for SegBo data tables
================
Steven Moran
06 November, 2021

Load libraries.

Load raw SegBo metadata and phoneme tables.

``` r
metadata <- read.csv('../data/SegBo database - Metadata.csv', stringsAsFactors = F)
phonemes <- read.csv('../data/SegBo database - Phonemes.csv', stringsAsFactors = F, na.strings = "")
```

Eyeball if there are any typos listed in various columns.

``` r
table(metadata$Contributor)
```

    ## 
    ##       Aigul Zakirova        Anna Sorokina          Antti Laine 
    ##                    1                    1                    1 
    ##  Chingduang Yurayong        Daniel Wilson         Daniil Levin 
    ##                    3                    1                    1 
    ##        Daria Zhornik         Dasha Bikina        David Ginebra 
    ##                    2                    1                   21 
    ##      Dmitry Nikolaev        Einav Levanon       Eitan Grossman 
    ##                   41                  191                   44 
    ##           Elad Eisen       Ezequiel Koile       Haggai Kovatch 
    ##                   78                    2                    6 
    ##         Heini Arjava   Konstantin Filatov          KurdistanDB 
    ##                    1                    1                   25 
    ##       Linda Konnerth         Lotta Jalava         Nikita Kuzin 
    ##                    1                    2                    1 
    ##       Pavel Duryagin       Polina Pleshak        Rammie Cahlon 
    ##                    2                    2                    4 
    ##       Ruslan Idrisov         Steven Moran          Vanya Levin 
    ##                    2                   63                    3 
    ## Vasilisa Zhigulskaya       Yael Assouline           Yoav Yosef 
    ##                    2                    1                    3 
    ##          Yuval Sarig 
    ##                    3

``` r
table(phonemes$OnlyInLoanwords)
```

    ## 
    ##  mostly      no unknown     yes 
    ##     115     276     130    1143

``` r
table(phonemes$Result)
```

    ## 
    ##                  new phoneme  other distributional change 
    ##                          867                          221 
    ## phonologization of allophone                to be checked 
    ##                          105                          377 
    ##                      unknown 
    ##                           83

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
    ##                          1                        595 
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
# metadata[which(!(metadata$Glottocode %in% glottolog$glottocode)),]
# expect_true(all(metadata$Glottocode %in% glottolog$glottocode))
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
    ## unknown 
    ##     475
