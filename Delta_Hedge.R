require("pbapply");require("data.table");require("quantmod")
# ***********************************************************************
#               read in options data from TD Ameritrade
# ***********************************************************************
FILES = list.files("/Volumes/3TB/TDAPI/OptionChains/ALL-DF",full.names = TRUE)
ops = pblapply(as.list(FILES),function(file){
  tmp = readRDS(file)
  tmp = subset(tmp,tmp$StockSymbol == "AMC")
  tmp = subset(tmp, tmp$expirationDate == "2021-03-19 13:00:00 PST")
  tmp = tmp[,c("putCall","mark","quoteTimeInLong","netChange","volatility",
               "delta","gamma","theta","vega","rho","strikePrice",
               "daysToExpiration","expirationDate","StockSymbol","StockClose")]
  tmp$quoteTimeInLong = as.Date(tmp$quoteTimeInLong)
  tmp$expirationDate = as.Date(tmp$expirationDate)
  tmp
  })
ops2 = ops[lapply(ops,nrow)>0]
ops2 = rbindlist(ops2,use.names=TRUE)
ops2 = ops2[,c("putCall","mark","quoteTimeInLong","netChange","volatility",
             "delta","strikePrice","daysToExpiration","expirationDate",
             "StockSymbol","StockClose")]
saveRDS(ops2,"ops_ALL.rds") # ALL OPTIONS
# ***********************************************************************
# ***********************************************************************
ops2 = readRDS("ops_ALL.rds")                       # ALL OPTIONS
ops2 = subset(ops2,ops2$daysToExpiration <= 14)     # subset options with less than 14 days2exp
ops2$ID = paste0(ops2$strikePrice,"-",ops2$putCall) # create an option ID to easily ID options

# combinations of all strikes
comb = unique(expand.grid(ops2$ID,ops2$ID))
comb = subset(comb,comb$Var1 != comb$Var2)
comb$Var1 = as.character(comb$Var1)
comb$Var2 = as.character(comb$Var2)

# test function
strk1 = comb[1000,1];strk2 = comb[1000,2]

# create spreads
sprds = function(strk1, strk2){
  
  # select strikes from options data
  tmp = subset(ops2, ops2$ID == strk1 | ops2$ID == strk2)
  
  # reorganize columns
  tmp = tmp[,c("quoteTimeInLong","putCall","strikePrice","mark","delta",
               "StockClose","daysToExpiration","ID")]
  
  
  # short 1st strike
  tmp$delta[is.nan(tmp$delta)]<-0
  strks = unique(tmp$ID)
  s1 = subset(tmp,tmp$ID == strks[1])
  s1$mark <- -s1$mark 
  s1$delta <- -s1$delta 
  s1 = rbind(s1,subset(tmp,tmp$ID == strks[2]))
  
  # short 2nd strike
  s2 = subset(tmp,tmp$ID == strks[2])
  s2$mark <- -s2$mark 
  s2$delta <- -s2$delta 
  s2 = rbind(s2,subset(tmp,tmp$ID == strks[1]))
  
  # extract unique days 2 expirations
  d = unique(tmp$daysToExpiration)
  d = d[order(d,decreasing = TRUE)]
  
  # calculate delta-hedge results for 
  short1 = lapply(as.list(d),function(days2exp){
    one = subset(s1, s1$daysToExpiration == days2exp)
    one = one[order(one$mark),] %>% as.data.frame()
    prc     = sum(one$mark)          # net price of options (negative == credit)
    dlta    = sum(one$delta)*100     # net delta between strikes
    stk     = -dlta                  # num of shares to offset delta
    stkVal  = stk*one$strikePrice[1] # value of position
    # combine data
    one <- as.data.frame(cbind(as.character(one$quoteTimeInLong[1]), 
          one$ID[1],one$putCall[1],one$strikePrice[1], 
          one$mark[1], one$delta[1],one$ID[2],
          one$putCall[2],one$strikePrice[2], one$mark[2], 
          one$delta[2],prc,dlta,stk,NA,one$StockClose[1],stkVal,
          one$daysToExpiration[1]))
    # change column names
    colnames(one) <- c("Date","ID1","type1","strike1","premium1","delta1",
                       "ID2","type2","strike2","premium2","delta2",
                       "netPremium","optDelta","stkShares","netDelta",
                       "stkPrice","stkValue","days2exp")
    # fix column classes
    one$netDelta   = as.numeric(one$optDelta) + as.numeric(one$stkShares)
    one$strike1    = one$strike1 %>% as.numeric()
    one$strike2    = one$strike2 %>% as.numeric()
    one$premium1   = one$premium1 %>% as.numeric()
    one$premium2   = one$premium2 %>% as.numeric()
    one$delta1     = one$delta1 %>% as.numeric()
    one$delta2     = one$delta2 %>% as.numeric()
    one$netPremium = one$netPremium %>% as.numeric()
    one$optDelta   = one$optDelta %>% as.numeric()
    one$stkShares  = one$stkShares %>% as.numeric()
    one$netDelta   = one$netDelta %>% as.numeric()
    one$stkPrice   = one$stkPrice %>% as.numeric()
    one$stkValue   = one$stkValue %>% as.numeric()
    one$days2exp   = one$days2exp %>% as.numeric()
    one
  })
  # row bind results
  short1 = do.call(rbind,short1)
  # complete cases
  short1 <- na.omit(short1)
  # get Daily PnL on Stock
  stkPnL = suppressWarnings(c(0,diff(short1$stkPrice)*short1$stkShares))
  # create column for stock PnL
  short1$stkPnL = stkPnL[1:(length(stkPnL)-1)] 
  # create column for option PnL
  short1$optPnL <- c(0,diff(short1$netPremium))*100
  # create column for gross PnL
  short1$grossPnL <- round(short1$stkPnL + short1$optPnL,2)
  # cumulative sum of PnL
  short1$cSumPnL <- cumsum(short1$grossPnL)
  # ***********************************************************************
  #             Repeat but short 2nd strike
  # ***********************************************************************
  # calculate delta-hedge results for 
  short2 = lapply(as.list(d),function(days2exp){
    one = subset(s2, s2$daysToExpiration == days2exp)
    one = one[order(one$mark),] %>% as.data.frame()
    prc     = sum(one$mark)          # net price of options (negative == credit)
    dlta    = sum(one$delta)*100     # net delta between strikes
    stk     = -dlta                  # num of shares to offset delta
    stkVal  = stk*one$strikePrice[1] # value of position
    # combine data
    one <- as.data.frame(cbind(as.character(one$quoteTimeInLong[1]), 
                               one$ID[1],one$putCall[1],one$strikePrice[1], 
                               one$mark[1], one$delta[1],one$ID[2],
                               one$putCall[2],one$strikePrice[2], one$mark[2], 
                               one$delta[2],prc,dlta,stk,NA,one$StockClose[1],stkVal,
                               one$daysToExpiration[1]))
    # change column names
    colnames(one) <- c("Date","ID1","type1","strike1","premium1","delta1",
                       "ID2","type2","strike2","premium2","delta2",
                       "netPremium","optDelta","stkShares","netDelta",
                       "stkPrice","stkValue","days2exp")
    # fix column classes
    one$netDelta   = as.numeric(one$optDelta) + as.numeric(one$stkShares)
    one$strike1    = one$strike1 %>% as.numeric()
    one$strike2    = one$strike2 %>% as.numeric()
    one$premium1   = one$premium1 %>% as.numeric()
    one$premium2   = one$premium2 %>% as.numeric()
    one$delta1     = one$delta1 %>% as.numeric()
    one$delta2     = one$delta2 %>% as.numeric()
    one$netPremium = one$netPremium %>% as.numeric()
    one$optDelta   = one$optDelta %>% as.numeric()
    one$stkShares  = one$stkShares %>% as.numeric()
    one$netDelta   = one$netDelta %>% as.numeric()
    one$stkPrice   = one$stkPrice %>% as.numeric()
    one$stkValue   = one$stkValue %>% as.numeric()
    one$days2exp   = one$days2exp %>% as.numeric()
    one
  })
  # row bind results
  short2 = do.call(rbind,short2)
  # complete cases
  short2 <- na.omit(short2)
  # get Daily PnL on Stock
  stkPnL = suppressWarnings(c(0,diff(short2$stkPrice)*short2$stkShares))
  # create column for stock PnL
  short2$stkPnL = stkPnL[1:(length(stkPnL)-1)] 
  # create column for option PnL
  short2$optPnL <- c(0,diff(short2$netPremium))*100
  # create column for gross PnL
  short2$grossPnL <- round(short2$stkPnL + short2$optPnL,2)
  # cumulative sum of PnL
  short2$cSumPnL <- cumsum(short2$grossPnL)
  
  # combine short1 & short2
  dta = rbind(short1,short2)
  
  # return combined df
  dta
}
# test the function
df = sprds(strk1 = comb[2000,1], strk2 = comb[2000,2])

# calculate results for all combinations
ALL = pblapply(as.list(1:nrow(comb)), function(ii){
  sprds(strk1 = comb[ii,1], strk2 = comb[ii,2])
})
# rbind all options
ALL <- rbindlist(ALL,use.names = TRUE)
#ALL <- readRDS("delta_hdge_AMC.rds")
# subset results by extracting days2exp == 0
comps = subset(ALL, ALL$days2exp == 0)
# order by PnL
comps = unique(comps[order(comps$cSumPnL,decreasing = TRUE),])
# plot histogram of PnL
hist(comps$cSumPnL)
# review best strikes
bst = unique(subset(ALL, ALL$ID1 == "14-PUT" & ALL$ID2 == "7-CALL"))


