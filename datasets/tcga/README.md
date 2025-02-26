# TCGA

Raw data (preprocessed with both Kallisto and Salmon) and different versions as used in the paper: TCGBiolinks (GDC), Recount3 and Toil project.

Full files are in in D:\RNA_seq\TCGA_BLCA
To use all versions I saved them together into R objects.

- This R object includes the versions I used for the analysis: D:\TCGA_object407_new.RData

407 samples is the set I got when trying to match sample names across all versions + metadata + excluding replicates and non-tumor samples.

- Originally the "raw" set contained more samples (around 412 if I remember correctly). Full sample set from each version is in TCGA_object_full: D:\TCGA_object_full.rds
- The unique sample set with matching metadata from our locally preprocessed version includes 409 samples:  D:\TCGA_object409new.RData

If you need the path to each dataset outside of the object let me know. At this point I was saving everything into R objects for some reason and they were the quickest to find.

- Metadata: D:/RNA_seq/TCGA_BLCA/TableS1Robertson.csv
