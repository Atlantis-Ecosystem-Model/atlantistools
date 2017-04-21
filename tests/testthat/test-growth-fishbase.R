# context("test extraction of data from fishbase.")
#
# df1 <- get_growth_fishbase("Sprattus sprattus")
#
# test_that("test extraction for sprat", {
#   expect_equal(dim(df1), c(25, 17))
#   expect_equivalent(df1$linf[5], 13.4)
#   expect_equivalent(df1$k[12],  0.298)
#   expect_equivalent(df1$locality[23], "Kattegat")
# })
#
# df2 <- get_growth_fishbase("Anguilla anguilla")
#
# test_that("test extraction for eel", {
#   expect_equal(dim(df2), c(43, 17))
#   expect_equivalent(df2$linf[10], 50.1)
#   expect_equivalent(df2$k[19],  0.078)
#   expect_equivalent(df2$locality[4], "Monaci lagoon")
# })
#
#
# df3 <- get_ref_fishbase(df1)
#
# test_that("test extraction of references for sprat", {
#   expect_true(stringr::str_detect(df3$ref[df3$ref_id == 312], "A preliminary compilation of fish length growth parameters."))
#   expect_true(stringr::str_detect(df3$ref[df3$ref_id == 56764], "Some Biological characteristics of sprat"))
#   expect_true(stringr::str_detect(df3$ref[df3$ref_id == 1771], "Growth and age composition of sprat stock"))
# })
#
# # use this for internal testing only
# # fish <- read.csv("Z:/my_data_alex/fish_species_names_from_ibts.csv", stringsAsFactors = FALSE)[, 1]
# # url <- get_growth_fishbase(fish)
# # test_url <- urls$ref_url
# # use the following ids to test the function. the have 1-5 listed references.
# # 1, 2, 3, 16, 697
#
# # use this to check the package.
# test_url <- "/popdyn/FishPopGrowthSummary.php?ID=861&pref=312&sex=unsexed&loo=56.80000&k=0.53000&id2=845&genusname=Scyliorhinus&speciesname=canicula&fc=183&gm_loo=75.682521749986&gm_lm=49.520370700602&gm_m=0.184&gm_k=0.20812285885834&vautoctr=2040&gm_lm_rl=0.62928533390764"
#
# tests <- purrr::map_chr(test_url, ~paste0("http://www.fishbase.se/", .)) %>%
#   purrr::map(., xml2::read_html) %>%
#   purrr::map(., rvest::html_text)
#
# test_that("test extraction of references for sprat", {
#   expect_true(all(stringr::str_detect(tests, "Main Ref. :")))
#   expect_true(all(stringr::str_detect(tests, "Data Ref. :")))
#   expect_true(all(stringr::str_detect(tests, "Data Type :")))
#   expect_equal(url_to_refid(test_url), c(312, 1231))
# })
#
# # test NA handling
# df <- get_growth_fishbase(c("Alosa agone", "Scyliorhinus canicula"))
#
# test_that("error and NA handling", {
#   expect_error(get_growth_fishbase("Alosa agone"), "None of the species have information about growth.")
#   expect_equal(sum(is.na(df[df$species == "Alosa agone", ])), 16)
#   expect_equal(nrow(is.na(df[df$species == "Alosa agone", ])), 1)
# })
#
#
#
#
#
