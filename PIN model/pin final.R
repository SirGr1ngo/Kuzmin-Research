flt <- files_to_loop[1]
pin <- function(data, factor=c("EHO","LK"), aggr = c("1d", "2h", "1h", "30m"), smb){
  df <- fread(data)
  df <- df[, .SD, .SDcols=cols]
  df$timestamp <- as.POSIXct(df$timestamp, format="%Y-%m-%d D %H:%M:%S", 
                             origin="1970-01-01", tz="UTC")
  
  smb_i <- smb
  
  df_Buy <- df[which(df$symbol==smb_i & df$side=="Buy"),]
  df_Sell <- df[which(df$symbol==smb_i & df$side=="Sell"),]
  
  if(aggr=="1d"){
    
    total_buy <- df_Buy %>% mutate(interval= floor_date(timestamp, "day")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_buy=sum(size)) %>% 
      ungroup() %>% complete(interval, fill = list(total_buy=0))
    
    total_sell <- df_Sell  %>% mutate(interval= floor_date(timestamp, "day")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_sell=sum(size))%>% 
      ungroup() %>% complete(interval, fill = list(total_sell=0))
    
    total_buy <- data.table(total_buy)
    total_sell <- data.table(total_sell)
    total_trades <- merge(total_buy, total_sell, all=TRUE)
    
    total_trades[is.na(total_trades)] <- 0
    data <- total_trades[,2:3]
    data <- data.frame(data) 
    
   output <- pin_fun(data, "LK")
   output 

  }else if (aggr=="2h"){
    
    total_buy <- df_Buy %>% mutate(interval= floor_date(timestamp, "2hours")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_buy=sum(size)) %>% 
      ungroup() %>% complete(interval, fill = list(total_buy=0))
    
    total_sell <- df_Sell  %>% mutate(interval= floor_date(timestamp, "2hours")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_sell=sum(size))%>% 
      ungroup() %>% complete(interval, fill = list(total_sell=0))
    
    total_buy <- data.table(total_buy)
    total_sell <- data.table(total_sell)
    total_trades <- merge(total_buy, total_sell, by="interval" ,all=TRUE)
    
    total_trades[is.na(total_trades)] <- 0
    total_trades <- data.frame(total_trades)
    data <- total_trades[,2:3]
    
    output <- pin_fun(data, "LK")
    output 
    
  }else if (aggr=="1h"){
    
    total_buy <- df_Buy %>% mutate(interval= floor_date(timestamp, "hour")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_buy=sum(size)) %>% 
      ungroup() %>% complete(interval, fill = list(total_buy=0))
    
    total_sell <- df_Sell  %>% mutate(interval= floor_date(timestamp, "hour")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_sell=sum(size))%>% 
      ungroup() %>% complete(interval, fill = list(total_sell=0))
    
    total_buy <- data.table(total_buy)
    total_sell <- data.table(total_sell)
    total_trades <- merge(total_buy, total_sell, all=TRUE)
    
    total_trades[is.na(total_trades)] <- 0
    data <- total_trades[,2:3]
    data <- data.frame(data) 
    
    output <- pin_fun(data, "LK")[2,] 
    output 
    
  }else if (aggr=="30min"){
    
    total_buy <- df_Buy %>% mutate(interval= floor_date(timestamp, "30minutes")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_buy=sum(size)) %>% 
      ungroup() %>% complete(interval, fill = list(total_buy=0))
    
    total_sell <- df_Sell  %>% mutate(interval= floor_date(timestamp, "30minutes")) %>% 
      group_by(interval, .drop=FALSE) %>% summarize(total_sell=sum(size))%>% 
      ungroup() %>% complete(interval, fill = list(total_sell=0))
    
    total_buy <- data.table(total_buy)
    total_sell <- data.table(total_sell)
    total_trades <- merge(total_buy, total_sell, all=TRUE)
    
    total_trades[is.na(total_trades)] <- 0
    data <- total_trades[,2:3]
    data <- data.frame(data) 
    
    output <- pin_fun(data, "LK")[2,]
    output 
  }
}