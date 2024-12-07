source("option_functions_v4.R")


##########Akshare改R函数##########
option_cffex_spot_price_sina <- function(symbol = "mo2501C5700") {
  # 构造URL
  url <- paste0("https://hq.sinajs.cn/list=P_OP_", symbol)
  # 设置HTTP请求头
  headers <- c(
    "Accept" = "*/*",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "zh-CN,zh;q=0.9,en;q=0.8",
    "Cache-Control" = "no-cache",
    "Connection" = "keep-alive",
    "Host" = "hq.sinajs.cn",
    "Pragma" = "no-cache",
    "Referer" = "https://stock.finance.sina.com.cn/",
    "sec-ch-ua" = '" Not;A Brand";v="99", "Google Chrome";v="97", "Chromium";v="97"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"Windows"',
    "Sec-Fetch-Dest" = "script",
    "Sec-Fetch-Mode" = "no-cors",
    "Sec-Fetch-Site" = "cross-site",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
  )
  
  # 使用curl下载数据
  h <- new_handle()
  handle_setheaders(h, .list = headers)
  response <- curl_fetch_memory(url, handle = h)
  data_text <- rawToChar(response$content)
  data_text <- iconv(data_text, from = "GBK", to = "UTF-8")
  
  start_pos <- find(data_text, '\"') + 1
  end_pos <- rfind(data_text, '\"') - 1
  # 提取双引号之间的内容
  data_string <- substr(data_text, start_pos, end_pos)
  # 按逗号分割数据并返回
  data_list <- strsplit(data_string, ",")[[1]]
  # 定义字段名
  field_list <- c(
    "买量", "买价", "最新价", "卖价", "卖量", "持仓量", "涨幅", "行权价", "昨收价", "开盘价",
    "涨停价", "跌停价", "申卖价五", "申卖量五", "申卖价四", "申卖量四", "申卖价三", "申卖量三",
    "申卖价二", "申卖量二", "申卖价一", "申卖量一", "申买价一", "申买量一", "申买价二",
    "申买量二", "申买价三", "申买量三", "申买价四", "申买量四", "申买价五", "申买量五",
    "行情时间", "主力合约标识", "状态码", "标的证券类型", "标的股票", "期权合约简称", "振幅",
    "最高价", "最低价", "成交量", "成交额"
  )
  # 将数据和字段名组合成data.table并返回
  data_df <- data.table("字段" = field_list, "值" = data_list)
  return(data_df)
  # 输出内容自期权合约简称后无效，不过不影响前面的准确数据
}

option_cffex_underlying_spot_price_sina <- function(symbol = "io2506C3800") {
  option_spot_price_all <- option_cffex_spot_price_sina(symbol = symbol)
  underlying_spot <- option_spot_price_all[[37, "值"]]

  
  url <- paste0("https://hq.sinajs.cn/list=", underlying_spot)
  headers <- c(
    "Accept"= "*/*",
    "Accept-Encoding"= "gzip, deflate",
    "Accept-Language"= "zh-CN,zh;q=0.9,en;q=0.8",
    "Cache-Control"= "no-cache",
    "Host"= "hq.sinajs.cn",
    "Pragma"= "no-cache",
    "Proxy-Connection"= "keep-alive",
    "Referer"= "https://vip.stock.finance.sina.com.cn/",
    "User-Agent"= "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
  )
  h <- new_handle()
  handle_setheaders(h, .list = headers)
  response <- curl_fetch_memory(url, handle = h)
  data_text <- rawToChar(response$content)
  data_text <- iconv(data_text, from = "GBK", to = "UTF-8")
  
  start_pos <- find(data_text, '"') + 1
  end_pos <- rfind(data_text, '"') - 1
  data_string <- substr(data_text, start_pos, end_pos)
  data_list <- strsplit(data_string, ",")[[1]]
  field_list <- c("证券简称", "今日开盘价", "昨日收盘价", "最近成交价", "最高成交价", "最低成交价", "买入价", "卖出价", "成交数量", "成交金额", "买数量一", "买价位一", "买数量二", "买价位二", "买数量三", "买价位三", "买数量四", "买价位四", "买数量五", "买价位五", "卖数量一", "卖价位一", "卖数量二", "卖价位二", "卖数量三", "卖价位三", "卖数量四", "卖价位四", "卖数量五", "卖价位五", "行情日期", "行情时间", "停牌状态")
  data_df <- data.frame(字段 = field_list, 值 = data_list, stringsAsFactors = FALSE)
  data_df_2 <- data_df[c(1, 4, 31, 32), , drop = FALSE]
  return(data_df_2)
}

######################################################
option_cffex_expire_day_sina <- function(option_code) {
  headers <- c(
    "Accept" = "*/*",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "zh-CN,zh;q=0.9,en;q=0.8",
    "Cache-Control" = "no-cache",
    "Connection" = "keep-alive",
    "Host" = "hq.sinajs.cn",
    "Pragma" = "no-cache",
    "Referer" = "https://stock.finance.sina.com.cn/",
    "sec-ch-ua" = '" Not;A Brand";v="99", "Google Chrome";v="97", "Chromium";v="97"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"Windows"',
    "Sec-Fetch-Dest" = "script",
    "Sec-Fetch-Mode" = "no-cors",
    "Sec-Fetch-Site" = "cross-site",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
  )
  url <- paste0('https://hq.sinajs.cn/list=P_OP_', option_code)
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, .list = headers)
  curl::handle_setopt(handle, timeout = 6)
  conn <- curl::curl(url, handle = handle)
  data_1 <- readLines(conn, warn = FALSE)
  data_1 <- iconv(data_1, from = "GBK", to = "UTF-8")
  close(conn)
  data_2 <- unlist(strsplit(data_1, ","))
  expire_day <- data_2[length(data_2) - 4]
  return(expire_day)    #网站算法导致可能会有±1天误差，不过对计算影响不大
}


#读取标的物现价（float数据）
option_cffex_underlying_price_sina <- function(option_code) {
  df_1 <- option_cffex_underlying_spot_price_sina(option_code)
  underlying_price <- as.numeric(df_1[2, "值"])
  return(underlying_price)
}

#标的代码爬100天数据
cffex_history_price_sohu <- function(underlying) {
  if (underlying == "io") { 
    underlying_spot = "000300"
  } else if (underlying == "ho") {
    underlying_spot = "000016"
  } else if  (underlying == "mo") {
    underlying_spot = "000852"
  } else {
    underlying_spot = ""
  }
  headers <- c(
    "Accept" = "*/*",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "zh-CN,zh;q=0.9,en;q=0.8",
    "Cache-Control" = "no-cache",
    "Connection" = "keep-alive",
    "Host" = "q.stock.sohu.com",
    "Pragma" = "no-cache",
    "Referer" = "https://q.stock.sohu.com/",
    "sec-ch-ua" = '" Not;A Brand";v="99", "Google Chrome";v="97", "Chromium";v="97"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = '"Windows"',
    "Sec-Fetch-Dest" = "script",
    "Sec-Fetch-Mode" = "no-cors",
    "Sec-Fetch-Site" = "same-origin",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
  )
  url <- paste0('https://q.stock.sohu.com/hisHq?code=zs_', underlying_spot, '&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp')
  # 定义重试机制（感谢gpt）
  fetch_with_retry <- function(url, headers, max_retries = 3, min_delay = 1, max_delay = 3) {
    retries <- 0
    while (retries < max_retries) {
      h <- curl::new_handle()
      curl::handle_setheaders(h, .list = headers)
      result <- tryCatch({
        data <- curl::curl_fetch_memory(url, handle = h)
        content <- rawToChar(data$content)
        if (grepl("503 Service Temporarily Unavailable", content)) {
          stop("503 Service Temporarily Unavailable")
        }
        return(content)
      }, error = function(e) {
        message(sprintf("Attempt %d failed: %s", retries + 1, e$message))
        NULL
      })
      if (!is.null(result)) {
        return(result)
      }
      retries <- retries + 1
      if (retries < max_retries) {
        delay <- runif(1, min_delay, max_delay) # 随机延迟
        message(sprintf("Retrying in %.2f seconds...", delay))
        Sys.sleep(delay)
      }
    }
    stop("Failed to fetch data after multiple retries.")
  }
  data_text <- fetch_with_retry(url, headers)
  data_text <- iconv(data_text, from = "GBK", to = "UTF-8")
  start_pos <- find(data_text, 'hq\":') + 4
  end_pos <- rfind(data_text, ',\"code\"') - 1
  data_string <- substr(data_text, start_pos, end_pos)
  
  data_string <- gsub("\\[|\\]", "", data_string) # 移除方括号
  data_string <- gsub('\\"', "", data_string) # 移除引号
  data_string <- strsplit(data_string, ",")[[1]] # 按逗号分割
  
  df_1 <- data.frame(matrix(data_string, ncol = 10, byrow = TRUE))
  colnames(df_1) <- c('Date', 'Open', 'Close', '涨跌额', '涨跌幅', 'Low', 'High', '成交量(手)', '成交金额(万)', '换手率')
  # df_2 <- df_1[, 1:3]
  return(df_1)
}
##################################
# -----执行价、看涨/跌均为用户输入，定价相关数据已完备-----
# option_cffex_expire_day_sina(option_code)#剩余到期天数
# option_cffex_underlying_price_sina(option_code)#标的物现价
# cffex_history_price_sohu(underlying)#标的物100天历史数据
# 
# 11月末新增的部分函数依赖于先前的函数，尤其是以option_code为参数的
# 中金部分仍需修改部分内容来对接
# 定义的函数名也必须有所区别
# 

# 
# 其他功能直接调用主函数文件内的函数就能正常用


##############用户输入-测试############

# underlying = "io"
# expire_date = "2506"
# #看涨or看跌
# option_c_p = "c"
# #执行价
# price_X = "3800"
# 
# option_code = paste0(underlying, expire_date, toupper(option_c_p), price_X)
# 
# print(cffex_history_price_sohu(underlying))

#可以仿照新浪财经的填写形式：标的物+行权时间+看涨/看跌+行权价格