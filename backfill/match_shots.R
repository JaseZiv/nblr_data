library(nblscrapeR)
library(dplyr)

#======================
#----- 2015/2016 -----#
matches_16 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2015_16.rds"))
shots16 <- nblscrapeR::parse_match_shot(matches_16)
save_nblr(df=shots16, file_name = "shots_2015-2016", release_tag = "shots")

#======================
#----- 2016/2017 -----#
matches_17 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2016_17.rds"))
shots17 <- nblscrapeR::parse_match_shot(matches_17)
save_nblr(df=shots16, file_name = "shots_2016-2017", release_tag = "shots")


#======================
#----- 2017/2018 -----#
matches_18 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2017_18.rds"))
shots18 <- nblscrapeR::parse_match_shot(matches_18)
save_nblr(df=shots18, file_name = "shots_2017-2018", release_tag = "shots")


#======================
#----- 2018/2019 -----#
matches_19 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2018_19.rds"))
shots19 <- nblscrapeR::parse_match_shot(matches_19)
save_nblr(df=shots19, file_name = "shots_2018-2019", release_tag = "shots")


#======================
#----- 2019/2020 -----#
matches_20 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2019_20.rds"))
shots20 <- nblscrapeR::parse_match_shot(matches_20)
save_nblr(df=shots20, file_name = "shots_2019-2020", release_tag = "shots")


#======================
#----- 2020/2021 -----#
matches_21 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2020_21.rds"))
shots21 <- nblscrapeR::parse_match_shot(matches_21)
save_nblr(df=shots21, file_name = "shots_2020-2021", release_tag = "shots")


#======================
#----- 2021/2022 -----#
matches_22 <- readRDS(url("https://github.com/JaseZiv/nblr_data/releases/download/match_lists/match_lists_2021_22.rds"))
shots22 <- nblscrapeR::parse_match_shot(matches_22)
save_nblr(df=shots22, file_name = "shots_2021-2022", release_tag = "shots")
