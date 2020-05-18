Data checks for SegBo data tables
================
Steven Moran
18 May, 2020

Data
----

``` r
# Load SegBo metadata and phonemes
metadata <- read.csv('../data/SegBo database - Metadata.csv', stringsAsFactors = F) # Dumped from the Google Sheets
phonemes <- read.csv('../data/SegBo database - Phonemes.csv', stringsAsFactors = F, na.strings = "") # Dumped from the Google Sheets
```

Column types
------------

``` r
# Eyeball if there are any lookalikes in the typos listed in various columns
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
    ##                   79                    2                    6 
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
    ##     115     277     130    1144

``` r
table(phonemes$Result)
```

    ## 
    ##                  new phoneme  other distributional change 
    ##                          867                          221 
    ## phonologization of allophone                to be checked 
    ##                          105                          379 
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
    ##                          1                        597 
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

Duplicate phoneme check
-----------------------

``` r
# Make sure there are no duplcate borrowed phonemes
dups <- phonemes %>% group_by(InventoryID, BorrowedSound) %>% filter(n()>1) %>% select(InventoryID, BorrowingLanguageGlottocode, BorrowedSound)
expect_equal(nrow(dups), 0)
```

Bibliography check
------------------

``` r
# Check the bibliography
path <- '../bibliography/bibliography.bib'
bib <- bib2df(path)
```

    ## Warning: `as_data_frame()` is deprecated as of tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning in bib2df_tidy(bib, separate_names): NAs introduced by coercion

    ## Column `YEAR` contains character strings.
    ##               No coercion to numeric applied.

``` r
# Some entries have multiple comma separated IDs
# which(!(metadata$BibTexKey %in% bib$BIBTEXKEY))
# metadata[which(!(metadata$BibTexKey %in% bib$BIBTEXKEY)),]

# Split them and get a list of all IDs
keys <- bib$BIBTEXKEY
split.keys <- str_split(keys, ",")
split.keys <- split.keys %>% unlist()
split.keys <- str_trim(split.keys)

# All accounted for?
expect_equal(nrow(metadata[which(!(split.keys %in% bib$BIBTEXKEY)),]), 0)

# Matches both ways?
which(!(bib$BIBTEXKEY %in% split.keys))
```

    ## integer(0)

Inventory IDs in metadata and phoneme tables?
---------------------------------------------

``` r
# Make sure all metadata Inventory IDs have rows in phonemes
length(which(!(metadata$InventoryID %in% phonemes$InventoryID)))
```

    ## [1] 0

``` r
metadata[which(!(metadata$InventoryID %in% phonemes$InventoryID)),]
```

    ## [1] InventoryID      Glottocode       LanguageName     BibTexKey       
    ## [5] Filename         Contributor      MetadataComments PhoibleID       
    ## [9] ClosestNeighbor 
    ## <0 rows> (or 0-length row.names)

``` r
# expect_equal(length(which(!(metadata$InventoryID %in% phonemes$InventoryID))), 0)
```

``` r
# Make sure every phoneme ID is in metadata
phoneme.ids <- unique(phonemes$InventoryID)
expect_true(all(phoneme.ids %in% metadata$InventoryID))
```

Glottocodes check
-----------------

``` r
# Do the Glottocodes follow the correct format in the metadata?
glottocode <- "([a-z0-9]{4})([0-9]{4})"
expect_equal(length(which(!(str_detect(metadata$Glottocode, glottocode)))), 0)
which(!(str_detect(metadata$Glottocode, glottocode)))
```

    ## integer(0)

``` r
metadata[which(!(str_detect(metadata$Glottocode, glottocode))), ]
```

    ## [1] InventoryID      Glottocode       LanguageName     BibTexKey       
    ## [5] Filename         Contributor      MetadataComments PhoibleID       
    ## [9] ClosestNeighbor 
    ## <0 rows> (or 0-length row.names)

``` r
# Do the Glottocodes follow the correct format in the phonemes?
# expect_equal(length(which(!(str_detect(phonemes$BorrowingLanguageGlottocode, glottocode)))), 0)
which(!(str_detect(phonemes$BorrowingLanguageGlottocode, glottocode)))
```

    ## integer(0)

``` r
metadata[which(!(str_detect(phonemes$BorrowingLanguageGlottocode, glottocode))), ]
```

    ## [1] InventoryID      Glottocode       LanguageName     BibTexKey       
    ## [5] Filename         Contributor      MetadataComments PhoibleID       
    ## [9] ClosestNeighbor 
    ## <0 rows> (or 0-length row.names)
