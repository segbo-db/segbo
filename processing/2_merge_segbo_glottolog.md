Merge SegBo and Glottolog metadata
================
Steven Moran &lt;<steven.moran@uzh.ch>&gt;
24 November, 2019

``` r
# First run `1_merge_segbo_tables.Rmd to merge the SegBo Metadata and Phoneme tables
segbo <- read.csv('../data/segbo_merged.csv', na.strings = "", stringsAsFactors = FALSE)
```

``` r
# Glottolog 3.3 data (TODO: update)
# https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/glottolog_languoid.csv.zip
languoids <- read.csv('../data/glottolog_languoid.csv/languoid.csv', stringsAsFactors = FALSE) 
geo <- read.csv(url("https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/languages_and_dialects_geo.csv"), stringsAsFactors = FALSE)
```

``` r
# SegBo data checks

# Are all SegBo Glottocodes in Glottolog?
expect_true(all(segbo$Glottocode %in% languoids$id))
expect_true(all(segbo$BorrowingLanguageGlottocode %in% languoids$id))

# We have NAs (unknowns) in the SourceLanguage field
# expect_true(all(segbo$SourceLanguageGlottocode %in% languoids$id))

# Some Glottocodes aren't in the geo data... so we can't get those coordinates.
kable(segbo[which(!(segbo$Glottocode %in% geo$glottocode)),] %>% select(InventoryID, BorrowingLanguageGlottocode) %>% distinct())
```

|  InventoryID| BorrowingLanguageGlottocode |
|------------:|:----------------------------|
|           38| azer1255                    |
|          154| mong1331                    |
|          240| ttgr1237                    |
|          254| uzbe1247                    |
|          310| qelt1235                    |
|          538| mait1255                    |
|          542| pash1269                    |

``` r
# Merge the datasets
segbo <- left_join(segbo, languoids, by=c("Glottocode"="id"))
segbo <- left_join(segbo, geo)
```

    ## Joining, by = c("name", "level", "latitude", "longitude")

``` r
# Write SegBo with merged Glottolog metadata to disk
write.csv(segbo, file='../data/segbo_with_glottolog.csv', row.names = F)
```
