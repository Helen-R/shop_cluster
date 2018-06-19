
dt <- fread("data/output/shop_cluster_processed.csv")
dt[, AvgNetSalesSixMonths:=as.integer(gsub(",", "", AvgNetSalesSixMonths))]
tmp <- dt[, .(AvgNetSalesSixMonths=mean(AvgNetSalesSixMonths), 
                      mAvgNetSalesSixMonths=median(AvgNetSalesSixMonths), 
                      AvgNetSalesSixMonths.sd=sd(AvgNetSalesSixMonths),
                      WebConversion=mean(WebConversion), 
                      mWebConversion=median(WebConversion), 
                      AppConversion=mean(AppConversion),
                      mAppConversion=median(AppConversion),
                      web.ga.perc=mean(web.ga.perc),
                      mweb.ga.perc=median(web.ga.perc),
                      return.perc=mean(return.perc),
                      mreturn.perc=median(return.perc),
                      arpu=mean(arpu),
                      marpu=median(arpu),
                      # been.milli=mean(BeenMillionLastSixMonths),
                      # D1Plus=mean(D1Plus),
                      # D2Plus=mean(D2Plus), 
                      .N), by = .(fit.cluster)]
setorder(tmp, fit.cluster)
tmp
View(tmp)

# try lm
df.raw <- read.csv("data/output/shop_cluster_for_lm.csv")
df.raw$AvgNetSalesSixMonths <- as.integer(gsub(",","",df.raw$AvgNetSalesSixMonths))
df.raw$AvgNetSalesSixMonths <- df.raw$AvgNetSalesSixMonths#%/%100000
fit.lm <- lm(formula = AvgNetSalesSixMonths ~ arpu+return.perc+web.ga, data = df.raw)
summary(fit.lm)
df <- scale(df.raw)
df <- as.data.frame(df)
lm(formula = AvgNetSalesSixMonths ~., data = df)
lm(formula = AvgNetSalesSixMonths ~ WebConversion+AppConversion+web.ga+arpu, data = df)
