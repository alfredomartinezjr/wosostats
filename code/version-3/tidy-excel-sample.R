source("code/version-3/tidy-excel-functions.R")

# tidies match log for the 2018 SheBelieves Cup
# match between USA and France.
sample.one.path <- "source/excel/shebelieves-cup-2018/shebelieves-cup-2018-usa-fra-030418.xlsx"
shibaleaves_usafra <- TidyMatchExcel(sample.one.path)

# tidies match logs for all 2018 SheBelieves
# Cup matches (in the path, which are USA-only).
sample.two.path <- "source/excel/shebelieves-cup-2018/"
shibaleaves_2018 <- TidyMultiMatchExcels(sample.two.path)