# Processing

First put the CSV dumps from Google Sheets (private) into the `segbo/data` directory.

Then run `1_merge_segbo_tables.Rmd` to merge the metadata and phonemes tables.

Then run `2_merge_segbo_glottolog.Rmd` to merge in metadata from Glottolog (e.g. language area, genealogy).

After this you should run the various tests in the `segbo/tests` directory.
