## Download data for project
##
## This script downloads the data needed for the project and writes it to CSV
## files for processing in other scripts. All downloaded data are saved in the
## "./data-downloaded" directory.


library(googlesheets)

if(!dir.exists("./data-downloaded/"))
    dir.create("./data-downloaded/")


## Master brood table --------------------------------------
## doi:10.5063/F18S4N6X
bt.url <- "https://cn.dataone.org/cn/v2/resolve/urn:uuid:c747eee7-89fd-44ba-bce1-a14d0670792d"
download.file(bt.url, "./data-downloaded/raw_brood_table_2019_03_31.csv")

bi.url <- "https://cn.dataone.org/cn/v2/resolve/urn:uuid:178c61cc-6119-42c2-9387-5b0b602324d4"
download.file(bi.url, "./data-downloaded/raw_stock_info_2019_03_31.csv")


## Pink abundance ------------------------------------------

## Get pink data google sheet
gs.pink <- gs_title("pink-abundance-data-2017-12-08")

## List work sheets
gs_ws_ls(ss = gs.pink)

## Download work sheet
gs_download(gs.pink, ws = "pinks", to = "./data-downloaded/pink_abundance_2017_12_08.csv")



## Rob's second year indices -------------------------------

## Get data google sheet
gs.pink <- gs_title("rob-second-year-indices-2018-02-16")

## List work sheets
gs_ws_ls(ss = gs.pink)

## Download work sheet
gs_download(gs.pink, ws = "data", to = "./data-downloaded/rob-second-year-indices-2018-02-16.csv")



## SST raw -------------------------------------------------
ersst::sst_download(years = 1950:2016,
                    months = 1:12,
                    save.dir = "./data-downloaded/sst_raw/",
                    version = 5)

sst.raw.full <- ersst::sst_load(years = 1950:2016,
                                months = 1:12,
                                read.dir = "./data-downloaded/sst_raw/",
                                version = 5)

sst.raw.np <- ersst::sst_subset_space(sst.raw.full,
                                      lat.min = 36,
                                      lat.max = 80,
                                      lon.min = 170,
                                      lon.max = 250)

sst.raw.df <- ersst::sst_dataframe(sst.raw.np)

write.csv(sst.raw.df, "./data-downloaded/sst_raw.csv", row.names = FALSE)



## PDO + NPGO ----------------------------------------------
years <- 1950:2016
pdo <- get.pdo(years)
npgo <- get.npgo(years)

write.csv(pdo, "./data-downloaded/pdo.csv", row.names = FALSE)
write.csv(npgo, "./data-downloaded/npgo.csv", row.names = FALSE)

