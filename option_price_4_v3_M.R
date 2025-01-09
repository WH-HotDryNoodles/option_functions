#CRR
#区分欧美
#区分认购认沽

#MC
#不分欧美
#区分认购认沽    

###########定价函数，之后再考虑单独塞到一个文件里用来调用################
CRRBinomialTreeOption =
  #暂时设定股息默认为0
  function(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, Time, r, b=0, sigma, n,
           title = NULL, description = NULL)
  { 
    TypeFlag = TypeFlag[1]
    z = NA
    if (TypeFlag == "ce" || TypeFlag == "ca") z = +1
    if (TypeFlag == "pe" || TypeFlag == "pa") z = -1
    if (is.na(z)) stop("TypeFlag misspecified: ce|ca|pe|pa")
    dt = Time/n
    u  = exp(sigma*sqrt(dt))
    d  = 1/u
    p  = (exp(b*dt)-d)/(u-d)
    Df = exp(-r*dt)
    
    # Iteration:
    OptionValue = z*(S*u^(0:n)*d^(n:0) - X)
    OptionValue = (abs(OptionValue) + OptionValue) / 2
    
    # European Option:
    if (TypeFlag == "ce" || TypeFlag == "pe") {
      for ( j in seq(from = n-1, to = 0, by = -1) ) 
        for ( i in 0:j )         
          OptionValue[i+1] = 
            (p*OptionValue[i+2] + (1-p)*OptionValue[i+1]) * Df }
    
    # American Option:
    if (TypeFlag == "ca" || TypeFlag == "pa") {
      for ( j in seq(from = n-1, to = 0, by = -1) )  
        for ( i in 0:j )  
          OptionValue[i+1] = max((z * (S*u^i*d^(abs(i-j)) - X)), 
                                 (p*OptionValue[i+2] + (1-p)*OptionValue[i+1]) * Df) }
    
    param = list()
    param$TypeFlag = TypeFlag
    param$S = S
    param$X = X
    param$Time = Time
    param$r = r
    param$b = b
    param$sigma = sigma
    param$n = n
    
    # Add title and description:
    if (is.null(title)) title = "CRR Binomial Tree Option"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    return(OptionValue[1])
  }

MonteCarlo <- function(type_MC, S, X, t, r, sigma, n) {
  z <- rnorm(n)  # 从标准正态分布中生成n个样本
  St <- S * exp((r - 0.5 * sigma^2) * t + sigma * z * sqrt(t))  # 计算股票价格路径
  if (type_MC == "p") {
    price <- mean(pmax(0, X - St)) * exp(-r * t)
  }
  if (type_MC == "c") {
    price <- mean(pmax(0, St - X)) * exp(-r * t)
  }
  return(price) # 计算看跌期权的价值
}

black_scholes <- function(type, S0, K, t, r, q=0, sigma) {
  #暂定股息率为0
  # 计算d1和d2
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  # 计算期权价格
  if (type == "c") {
    option_price <- S0 * exp(-q * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  } else if (type == "p") {
    option_price <- K * exp(-r * t) * pnorm(-d2) - S0 * exp(-q * t) * pnorm(-d1)
  }
  return(option_price)
}

black_76 <- function(type, F0, K, t, r, sigma) {
  d1 <- (log(F0 / K) + (r + 0.5 * sigma^2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c") {
    option_price <- exp(-r * t) * (F0 * pnorm(d1) - K * pnorm(d2))
  } else if (type == "p") {
    option_price <- exp(-r * t) * (K * pnorm(-d2) - F0 * pnorm(-d1))
  }
  return(option_price)
}

##############将option_functions.R放到此文件的同一目录下######################
library(stats)
library(curl)
library(data.table)
#source("option_functions_v4.R")
###################用户调参数界面######################

# #用户输入-期权代码
#option_code = "10007139"

#用户输入-欧式/美式
#option_eu_us = "e"

#用户输入-无风险利率
rf_rate = 0.0156

#用户自己分别去调-n(迭代步数/样本数etc)
CRR_n = 2000
MC_n = 20000

#之后我们再想办法算-sigma(波动率)
#现在先自己在某些网站找数据输进去
sigma = 0.35

###################爬虫函数#################

#看涨看跌判断 & 期权类型合并
option_c_p = 'p'
option_type = 'pe'

#标的当前价格 & 期权执行价
price_S = 76000
price_X = 73000

#到期时间（以年为单位）
expire_time = 1

#################调用定价函数#####################

CRR_option_price <- CRRBinomialTreeOption(
  TypeFlag = option_type,  # 例如，欧式看涨期权
  S = price_S,         # 股票当前价格
  X = price_X,         # 执行价格
  Time = expire_time,        # 到期时间（以年为单位）
  r = rf_rate,        # 无风险利率
  sigma = sigma,     # 波动率
  b = 0,
  n = CRR_n         # 二叉树的步数
)

MC_option_price <- MonteCarlo(
  type_MC = option_c_p, 
  S = price_S, 
  X = price_X, 
  t = expire_time, 
  r = rf_rate, 
  sigma = sigma, 
  n = MC_n
)

BS_option_price <- black_scholes(
  type = option_c_p, 
  S0 = price_S, 
  K = price_X, 
  t = expire_time, 
  r = rf_rate, 
  q = 0, 
  sigma = sigma
)

B76_option_price <- black_76(
  type = option_c_p, 
  F0 = price_S, 
  K = price_X, 
  t = expire_time, 
  r = rf_rate, 
  sigma = sigma
)

###################输出结果#################
cat(sprintf("CRR: %.4f" , CRR_option_price))
cat(sprintf("MonteCarlo: %.4f" , MC_option_price))
cat(sprintf("black scholes: %.4f" , BS_option_price))
#cat(sprintf("black 76: %.4f" , B76_option_price))
#cat(sprintf("price now: %.4f" , option_latest_price(option_code)))






#option_code = "10008547"
# df <- option_history_price_sohu(option_code)
# print(df)


