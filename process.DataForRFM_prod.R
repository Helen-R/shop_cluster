shop.id <- 2131
dt <- NULL
for (yr in 2014:2017) {
    dt1 <- fread(file.path("data/raw_input/", sprintf("DataForRFMProd_%s.csv", yr)))
    # scan(file.path("data/raw_input/", "GaSourceMedium.csv"), skip=44028, nlines = 1, what="character")
    setnames(dt1, colnames(dt1), c("DateId", "ShopId", "MemberId", "SalePageId", 
                                 "TotalPayment","PromotionDiscount","ECouponDiscount",
                                 "TotalDiscount","TgCounts","Quantity",
                                 "SalesOrderSlaveStatusDef","CreatedDateTime"))
    dt <- rbind(dt, dt1[ShopId==shop.id])
}
# save(dt, file=file.path("data/raw_input/", sprintf("DataForRFMProd_%s.RData", yr)))

# product feature
date.seq <- dt$DateId %>% as.character(.) %>% as.Date(., format="%Y%m%d")
dt[, order.date:=date.seq]
dt1 <- dt[, .(Qty=sum(Quantity),
              n.purchase.member=uniqueN(MemberId),
              gross.sales=sum(TotalPayment),
              promo.discnt=sum(PromotionDiscount),
              ecoup.discnt=sum(ECouponDiscount),
              ttl.discnt=sum(TotalDiscount)), by = .(order.date, SalePageId)]
setorder(dt1, SalePageId, order.date)
dt1[, `:=`(nth.days=1:.N,
           n.days=.N,
           min.order.date=min(order.date),
           max.order.date=max(order.date),
           ttl.purchase.member=sum(n.purchase.member, na.rm=T),
           ttl.gross.sales=sum(gross.sales),
           ttl.promo.discnt=sum(promo.discnt),
           ttl.ecoup.discnt=sum(ecoup.discnt),
           ttlttl.discnt=sum(ttl.discnt)
           ), by = SalePageId]
dt1[, `:=` (nth.duration=as.integer(order.date-min.order.date+1),
            duration=as.integer(max.order.date-min.order.date+1))]
dt1 <- dt1[, -c("min.order.date", "max.order.date")]
    
