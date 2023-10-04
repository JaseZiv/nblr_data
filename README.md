# nblr_data
Data repository for the {nblR} rstats library


## Usage

GitHub Actions are scheduled to run daily and update data throughout the season.

**Steps:**

1. First, need to run `update/update_results.R`
2. Once that has run, then run `update/update_matches.R`
3. Finally, once 1 and 2 have run, then run `update/update_stats.R`
4. Once step 3 has run, then run `update/save_as_csv.R` to write all the rds files to a csv in the same releases


**Once a new season begins, the `current_season` variable in each of the above three files needs to be re-set to the new season. Additionally, some URLs may need to be modified to ensure the correct season's matches are being loaded.**