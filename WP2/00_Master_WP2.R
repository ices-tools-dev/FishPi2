## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## This script shows examples of how to run each function, their inputs and their outputs ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# load new libraries
install.packages("fishPiCodes_1.0.18.zip", repos = NULL, type = "win.binary")
install.packages("FishPi2WP2_0.1.6.zip", repos = NULL, type = "win.binary")
#devtools::install("FishPi2WP2")
#devtools::install("fishPiCodes")
library(FishPi2WP2)
library(fishPiCodes)


# load  "DeriveData" built with fishpi2 routines and check
load("2015_DeriveData.rData")
load("2016_DeriveData.rData")
ls()

# create datasets from the "DeriveData"
mylist                  <- list(BEL15,BEL16,DEU15,DEU16,DNK15,DNK16,ESP15,ESP16,IRL15,IRL16,NLD15,NLD16,PRT15,PRT16,SWE15,SWE16,UK15,UK16)
# 2015 subset of data for testing functions
mylist_2015             <- list(BEL15,DEU15,DNK15,ESP15,IRL15,NLD15,PRT15,SWE15,UK15) # list
db15                    <- data.table::rbindlist(mylist_2015,use.names=TRUE)          # data.table
db15_df                 <- as.data.frame(db15)                                        # dataframe


## ~~~~~~~~~~~~~~~~~~ ##
## Diversity function ##
## ~~~~~~~~~~~~~~~~~~ ##

# see helpfile for more on this
?stocks_diversity

# run the function "stocks_diversity" using either a dataframe or a data.table (does not work with lists)
stocks_diversity_output <- stocks_diversity(db15)

#the result of the function is a dataframe
View(stocks_diversity_output)
head(stocks_diversity_output)


## ~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Plot stock data function ##
## ~~~~~~~~~~~~~~~~~~~~~~~~ ##

# see helpfile for more on this
?plot_stock_data

# run some examples of barplots that can be made
plot_stock_data(db15, stockname="mac.27.nea" ,x="vslFlgCtry",y ="landWt",  z ="landCtry")
plot_stock_data(db15, stockname="mac.27.nea" ,x="landMonth",y ="landWt",  z ="gear")
plot_stock_data(db15, stockname="all.stocks" ,x="landMonth",y ="landWt",  z ="gear")
plot_stock_data(db15, stockname=c("mac.27.nea","lez.27.4a6a" ,"hke.27.8c9a","ane.27.9a","alf.27.nea") ,x="landCtry",y ="landWt",  z ="gear")
plot_stock_data(db15, stockname="all.stocks" ,x="gear",y ="landWt", z ="landCtry",  facetgrid ="vslFlgCtry")


## ~~~~~~~~~~~~~~ ##
## Map pie charts ##
## ~~~~~~~~~~~~~~ ##

# see helpfile for more on this
?map_pie_landings

# Your directory - set to an existing directory to store maps
YOUR_DIR <- "maps/"

# THIS IS THE ONLY FUNCTION OF THE PACKAGE THAT WILL SPLIT THE RESULTS PER YEAR

areas =c("27.4.a", "27.4.b", "27.4.c", "27.3.a", "27.7.d")

# couple of example stocks
map_pie_landings(db15, YOUR_DIR, stock= c("thr.27.nea","sol.27.8c9a"))

# this will map ALL STOCKS!!
map_pie_landings(db15, YOUR_DIR, areas= areas)


## ~~~~~~~~~~~~~~ ##
##    Heatmap     ## ##
## ~~~~~~~~~~~~~~ ##

# see helpfile for more on this
?heatmap

stocks   <- as.vector(t(dplyr::distinct(dplyr::filter(db15_df, substr(area, 1, 4) == "27.4"), stock)))
stocks_1 <- as.vector(t(dplyr::distinct(dplyr::filter(db15_df, substr(area, 1, 2) == "27"), stock)))
stocks_2 <- stocks[1:20]

windows()
heatmap(db15_df, stocks = stocks, x = "area", z = "landWt")

## ~~~~~~~~~~~~~~ ##
##    Threshold   ## ##
## ~~~~~~~~~~~~~~ ##

# see helpfile for more on this
?threshold

stocks   <- as.vector(t(dplyr::distinct(dplyr::filter(db15_df, substr(area, 1, 4) == "27.4"), stock)))
stocks_1 <- as.vector(t(dplyr::distinct(dplyr::filter(db15_df, substr(area, 1, 2) == "27"), stock)))
stocks_2 <- stocks[1:20]

windows()
threshold(db15_df, stocks, y = "landWt", z = "vslFlgCtry")
threshold(db15_df, stocks, y = "landWt", z = "landCtry")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##    Landings summaries by variable   ## ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# see helpfile for more on this
?stock_landings_by_id

# Splitting of 3 stocks by vslId
stocks <- c("cod.27.47d20","gug.27.3a47d","ple.27.420")
output <- stock_landings_by_id(db15, id_var = "vslId", stocks = stocks)
head(output$landWt); head(output$propWt)

# Illustrate results
windows()
par(mfrow=c(2,2))
for (stock in stocks){
plot(cumsum(sort(output[[1]][[stock]], decreasing=T)), main=stock, xlab="vessel rank", ylab="landWt")}

windows()
par(mfrow=c(2,2))
for ( stock in stocks){
plot(cumsum(sort(output[[2]][[stock]], decreasing=T)), main=stock, xlab="vessel rank", ylab="propWt")}
