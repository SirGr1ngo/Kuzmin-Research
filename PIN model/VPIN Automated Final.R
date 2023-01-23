#VPIN calculation without normal dist buck classification as 
#buyers and sellers are already identified in the order flow 
# unifying all days in one dataset and calculating total trades within one day (summing all trades)
rm(list = ls())

library(plyr)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(openair)
library(stringr)

directory <- ""
setwd(directory)

library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(bit64)

files_to_loop <- list.files()
cols <- c("timestamp","symbol", "side", "size", "price")

#analysis for each symbol
# smb_list <- c("ADAUSD", "BCHUSD", "DOGEUSD", "DOTUSD","DOTUSDTZ20", "ETHUSD", "XRPUSD", "LTCUSD", "LINKUSD", 
#   "TRXUSD", "ETH7D","XRP7D","EOSUSD", "EOSUSDTH21", "BNBUSD", "BNBUSDTZ20","DOTUSD", "DOTUSDTZ20", 
#   "YFIUSD", "YFIUSDTZ20", "XTZUSD", "XTZUSDTZ20", "LINKUSDTZ20", "DOTUSDTZ20")

# smb_list <- "XBTUSD"

#2018
# smb_list <- c("ADAH19", "BCHH19", "ETHUSD","EOSH19", "LTCH19",
#               "TRXH19", "XBTUSD", "XRPH19")

#2019
# smb_list <- c("ADAH20", "BCHH20", "ETHUSD", "LTCH20", "BNBUSDTH21", "BNBUSDTZ20","DOTUSD", "DOTUSDTZ20",
#               "TRXH20","YFIUSDTH21", "XTZUSD", "XTZUSDTH21", "LINKUSDTH21", "DOTUSDTZ20")

smb_list <- c("ADAUSD", "BCHUSD",  "DOTUSD","DOTUSDTZ20", "ETHUSD", "XBTUSD", "XRPUSD", "LTCUSD", "LINKUSD",
              "TRXUSD", "ETH7D","XRP7D","EOSUSD", "EOSUSDTH21")


#function to find ticker that might change its name through the time 
func_extr_smb <- function(x,y){
  if (y %in% x$symbol) {
    y
  }else if (paste(y, "T", sep = "") %in% x$symbol){
    y <- paste(y, "T", sep = "")
    y
  }else{
    y <- substr(y, 1, nchar(y)-3)
    smb <- unique(str_extract(x$symbol, paste(y, ".\\d{2}", sep = "")))
    smb <- subset(smb, !is.na(smb))
    smb
  }
}

#data analysis 
files_mth <- list()
name <- vector()
for (i in (1:5)) {
  if(i<10){
    name[i] <- paste("2021","0",i, sep="")
  }else{
    name[i] <- paste("2021", i, sep="")
  }
  files_mth[[i]] <- str_extract(files_to_loop, paste(name[i],"\\d{2}",".csv.gz", sep=""))
  files_mth[[i]] <- subset(files_mth[[i]], !is.na(files_mth[[i]]))
}

for (smb in smb_list) {
  vpin_output <- data.frame()
  j <- 1
  for (i in (1:length(files_mth))){
    #form a files
    df <- lapply(files_mth[[i]], fread)
    df <- rbindlist(df)
    df <- df[, .SD, .SDcols=cols]
    df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d D %H:%M:%S", 
                               origin="1970-01-01", tz="UTC")
    
    smb_i <- func_extr_smb(df, smb)
    # smb_i <- smb
    
    n_days <- df %>% mutate(day= day(timestamp)) %>% summarize(max(day))
    
    #Time bucketing 
    time_bucket <- df[which(df$symbol==smb_i),] %>% 
      mutate(interval= floor_date(timestamp, "minute")) %>%
      group_by(interval) %>% summarize(total=sum(size),
                                       buy_side = sum(size[side=="Buy"]),
                                       sell_side = sum(size[side=="Sell"])) %>% 
      ungroup() %>% complete(interval, fill = list(total=0)) 
    
    time_bucket <- data.frame(time_bucket)
    sum_trades <- sum(time_bucket$total)
    
    #Average daily trades (in crypto units)
    avg_trades <- (sum_trades/n_days) %>% as.numeric()
    
    #Volume buckets 
    VBS <- avg_trades/50
    
    #Time bucketing
    j <-1
    a <-1
    n <-1440
    bucket_df <- matrix(ncol = 5,nrow = 1)
    bucket_df[1,1:5] <- 0
    bucket_df <- data.frame(bucket_df)
    colnames(bucket_df) <- c("time", "total_trades", "buy_trades", "sell_trades",
                             "order_imbalance")
    while (a<n && a<nrow(time_bucket)) {
      bucket_df[j,2] <- bucket_df[j,2] + time_bucket$total[a]
      bucket_df[j,3] <- bucket_df[j,3] + time_bucket$buy_side[a]
      bucket_df[j,4] <- bucket_df[j,4] + time_bucket$sell_side[a]
      if(bucket_df[j,2] >= VBS){
        dif <- bucket_df[j,2]-VBS
        bucket_df[j,2] <- bucket_df[j,2] - dif
        bucket_df[j,1] <- time_bucket$interval[a]
        bucket_df[j+1,2] <- dif
        bucket_df[j+1,3] <- 0
        bucket_df[j+1,4] <- 0
        bucket_df[j+1,5] <- 0
        bucket_df[j,5] <- abs(bucket_df[j,3]-bucket_df[j,4])
        j <- j+1
      }
      a <- a+1
      if(a>= n){
        bucket_df[j,1] <- time_bucket$interval[a]
        n <- n+1440
      }
    }
    bucket_df$time <- as.POSIXct(bucket_df$time, format="%Y-%m-%d", 
                                 origin="1970-01-01", tz="UTC")
    
    
    # VPIN series (no rolling over): bucket by bucket with final monthly averaging  
    vpin_df_s <- matrix(ncol = 4, nrow=1)
    vpin_df_s <- data.frame(vpin_df_s)
    colnames(vpin_df_s) <- c("date", "VPIN", "Initial bucket", "Final bucket")
    i <- 1
    j <-1
    while(j<=n_days){
      vpin_df_s[j,2] <- sum(bucket_df[i:i+49,5])/(50*VBS)
      vpin_df_s[j,1] <- bucket_df[i+49,1]
      vpin_df_s[j,3] <- i
      vpin_df_s[j,4] <- i+49
      j <- j+1
      i <- i+50
    }
    
    for (i in (1:nrow(vpin_df_s))) {
      j <- vpin_df_s[i,1]
      vpin_df_s[i,1] <- unique(df$timestamp)[i]  
    }
   vpin_output <- rbind(vpin_output, vpin_df_s)
   assign(paste("vpin_1m", smb_i, sep = "_"), vpin_output)
    }
  }

    
    