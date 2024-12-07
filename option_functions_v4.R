if (!require(curl)) install.packages("curl", dependencies = TRUE)

library(curl)
library(data.table)
library(stats)
library(plotly)
library(ggplot2)


options(width = 1000)   #取消自动换行
options(digits = 16)    #提高计算精度
options(warn = - 1)    #关闭警告
#################Py转R辅助函数###########################
#R语言这正则表达式累死个人，还是自己定义python里的函数直接用方便
find <- function(text, substring) {
  match_position <- regexpr(substring, text)
  if (match_position == -1) {
    return(-1)
  }
  return(match_position)
}

rfind <- function(text, substring) {
  matches <- gregexpr(substring, text)
  positions <- matches[[1]]
  if (length(positions) > 0 && positions[1] != -1) {
    return(tail(positions, 1))  
  } else {
    return(-1) 
  }
}

################Akshare改R语言#######################

#输出期权完整信息
option_sse_spot_price_sina <- function(symbol = "10008107") {
  # 构造URL
  url <- paste0("https://hq.sinajs.cn/list=CON_OP_", symbol)
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
}

#输出标的物部分相关信息
option_sse_underlying_spot_price_sina <- function(symbol = "10007139") {
  option_spot_price_all <- option_sse_spot_price_sina(symbol = symbol)
  underlying_spot <- option_spot_price_all[[37, "值"]]
  if (substr(underlying_spot, 1, 2) == "15") {
    underlying_spot <- paste0("sz", underlying_spot)
  } else {
    underlying_spot <- paste0("sh", underlying_spot)
  }
  
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

###################自制函数转R################################
#期权实时价格（简化处理）
option_spot_price_sina <- function(option_code) {
  option_spot_price_all <- option_sse_spot_price_sina(symbol = option_code)
  option_spot_price <- option_spot_price_all[c(38, 3, 37, 33), , drop = FALSE]
  rownames(option_spot_price) <- NULL  # 重置索引
  return(option_spot_price)
}

#期权到期时间与剩余天数,输出列表[到期时间,剩余天数]
option_expire_day_sina <- function(option_code) {
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
  url <- paste0('https://hq.sinajs.cn/list=CON_OP_', option_code)
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, .list = headers)
  curl::handle_setopt(handle, timeout = 6)
  conn <- curl::curl(url, handle = handle)
  data_1 <- readLines(conn, warn = FALSE)
  data_1 <- iconv(data_1, from = "GBK", to = "UTF-8")
  close(conn)
  data_2 <- unlist(strsplit(data_1, ","))
  end_date <- data_2[length(data_2) - 4]
  expire_day <- data_2[length(data_2) - 3]
  return(c(end_date, expire_day))
}

#标的物历史价格（100天）
option_history_price_sohu <- function(option_code) {
  option_spot_price_all <- option_sse_spot_price_sina(symbol = option_code)
  option_spot_price <- option_spot_price_all[c(38, 3, 37, 33), , drop = FALSE]
  underlying_spot <- option_spot_price[3, "值"]
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
  url <- paste0('https://q.stock.sohu.com/hisHq?code=cn_', underlying_spot, '&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp')
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
  #搞了半个小时这里，gpt也不懂怎么整，还好有find和rfind函数在
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


###################整合所需的简化版函数（避免反复进行复杂运算或调用）################################
#判断期权看涨还是看跌
option_type_judge_c_p <- function(option_code) {
  option_type <- ""
  option_spot_price_all <- option_sse_spot_price_sina(symbol = option_code)
  if (grepl("购", option_spot_price_all[38, "值"])) {
    option_type <- "c"
  } else if (grepl("沽", option_spot_price_all[38, "值"])) {
    option_type <- "p"
  } else {
    option_type <- "期权类型报错"
  }
  return(option_type)
}

#读取期权执行价（int*0.001）
option_strike_price <- function(option_code) {
  option_spot_price_all <- option_sse_spot_price_sina(symbol = option_code)
  deal_code <- option_spot_price_all[[38, "值"]]
  strike_price <- strsplit(deal_code, "月")[[1]][2]
  strike_price = as.numeric(gsub(".*?([0-9]+).*", "\\1", strike_price))
  strike_price <- as.numeric(strike_price) * 0.001
  return(strike_price)
}

#读取标的物现价（float数据）
option_underlying_price_sina <- function(option_code) {
  df_1 <- option_sse_underlying_spot_price_sina(option_code)
  underlying_price <- as.numeric(df_1[2, "值"])
  return(underlying_price)
}

#期权到期时间（以年为单位）
option_expire_years <- function(option_code) {
  expire_days <- option_expire_day_sina(option_code)[2]
  expire_years <- as.integer(expire_days) / 365
  return(expire_years)
}

##########11月末新增函数###########

#直接用标的代码爬100天数据
history_price_sohu <- function(underlying_spot) {
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
  url <- paste0('https://q.stock.sohu.com/hisHq?code=cn_', underlying_spot, '&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp')
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

candlestick_underlying <- function(option_code) {
  history_price = option_history_price_sohu(option_code)[c("Open","High","Low","Close","Date")]
  plot_candlestick_with_crosshair <- function(data) {
    # 数据验证：检查必要列是否存在
    required_columns <- c("Date", "Open", "High", "Low", "Close")
    if (!all(required_columns %in% colnames(data))) {
      stop("数据框必须包含以下列：'Date', 'Open', 'High', 'Low', 'Close'。")
    }
    # 使用plotly创建交互式K线图
    fig <- plot_ly(
      data,
      x = ~Date, type = "candlestick",
      open = ~Open, high = ~High, low = ~Low, close = ~Close,
      increasing = list(line = list(color = "green")),  # 上涨线颜色
      decreasing = list(line = list(color = "red"))     # 下跌线颜色
    )
    # 添加横纵轴的鼠标悬停指引线
    fig <- fig %>% layout(
      title = "标的资产K线图",
      xaxis = list(
        title = "日期",
        rangeslider = list(visible = FALSE),  # 时间范围滑块
        showline = TRUE,
        zeroline = FALSE,
        showspikes = TRUE,  # 启用垂直参考线
        spikemode = "across+toaxis",  # 延伸参考线到坐标轴
        spikesnap = "cursor",  # 捕捉到鼠标指针
        spikecolor = "black",  # 设置参考线颜色
        spikewidth = 1,  # 设置参考线的宽度（细线）
        showticks = TRUE,  # 显示坐标轴上的值
        tickangle = -45,  # 旋转坐标轴标签
        tickmode = "array"  # 确保显示刻度
      ),
      yaxis = list(
        title = "价格",
        showline = TRUE,
        zeroline = FALSE,
        showspikes = TRUE,  # 启用垂直参考线
        spikemode = "across+toaxis",  # 延伸参考线到坐标轴
        spikesnap = "cursor",  # 捕捉到鼠标指针
        spikecolor = "black",  # 设置参考线颜色
        spikewidth = 1,  # 设置参考线的宽度（细线）
        showticks = TRUE,  # 显示坐标轴上的值
        tickangle = -45,  # 旋转坐标轴标签
        tickmode = "linear",  # 启用线性刻度
        dtick = 0.1,  # 设置刻度间隔，细化纵坐标的显示
        showticklabels = TRUE  # 显示纵坐标标签
      ),
      hovermode = "closest" # 显示横纵指引线和对齐的悬停提示
    )
    return(fig)
  }
  return(plot_candlestick_with_crosshair(history_price))
}

volatility_latest <- function(underlying_spot, window_size) {
  
  df_history_price = history_price_sohu(underlying_spot)[c("Date","Close")]
  
  calculate_volatility_latest <- function(df, window_size) {
    df$Date <- as.Date(df$Date)
    df$Close <- as.numeric(gsub(",", "", df$Close))
    df$LogReturn <- NA
    for (i in 1:nrow(df)) {
      df$LogReturn[i] <- log(df$Close[i]) - log(df$Close[i+1])
    }
    df$Volatility <- NA
    for (i in 1:(nrow(df))) {
      end_date <- df$Date[i]
      start_date <- end_date - window_size
      trading_days_in_range <- df[df$Date >= start_date & df$Date <= end_date, ]
      trading_returns <- trading_days_in_range$LogReturn
      if (start_date >= tail(df$Date, 1)) {
        volatility <- sd(trading_returns, na.rm = TRUE)
        AnnualizedVolatility <- volatility * sqrt(242)
        df$Volatility[i] <- AnnualizedVolatility
      }
    }
    return(df$Volatility[1])
  }
  window_size = window_size
  volatility = calculate_volatility_latest(df_history_price, window_size)
  return(volatility)
}

volatility_plot <- function(underlying_spot, window_size) {
  df = history_price_sohu(underlying_spot)
  plot_volatility <- function(df, window_size) {
    calculate_volatility <- function(df, window_size) {
      df$Date <- as.Date(df$Date)
      df$Close <- as.numeric(gsub(",", "", df$Close))
      df$LogReturn <- NA
      for (i in 1:(nrow(df) - 1)) {  # 注意：避免最后一行访问超出范围
        df$LogReturn[i] <- log(df$Close[i]) - log(df$Close[i+1])
      }
      df$Volatility <- NA
      for (i in 1:(nrow(df))) {
        end_date <- df$Date[i]
        start_date <- end_date - window_size
        trading_days_in_range <- df[df$Date >= start_date & df$Date <= end_date, ]
        trading_returns <- trading_days_in_range$LogReturn
        if (start_date >= tail(df$Date, 1)) {
          volatility <- sd(trading_returns, na.rm = TRUE)
          AnnualizedVolatility <- volatility * sqrt(242)
          df$Volatility[i] <- AnnualizedVolatility
        }
      }
      return(df)
    }
    
    df <- calculate_volatility(df, window_size)
    df_volatility <- df[!is.na(df$Volatility), ]
    p <- ggplot(df_volatility, aes(x = Date)) +
      geom_line(aes(y = Volatility), color = "blue", size = 1, alpha = 0.7) +
      
      # 美化图表
      labs(title = "历史波动率曲线") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转日期标签并加粗
        plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
        axis.title.x = element_blank(),  # 去掉 x 轴标题
        axis.title.y = element_text(size = 12, face = "bold"),  # 坐标轴标题加粗
        panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
        panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线变淡
        panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线更淡
      ) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month", expand = c(0, 0))
    vol_plot <- ggplotly(p)
    return(vol_plot)
  }
  window_size = window_size
  v_plot = plot_volatility(df,window_size)
  return(v_plot)
}

volatility_df <- function(underlying_spot, window_size) {
  df = history_price_sohu(underlying_spot)
  calculate_volatility <- function(df, window_size) {
    df$Date <- as.Date(df$Date)
    df$Close <- as.numeric(gsub(",", "", df$Close))
    df$LogReturn <- NA
    for (i in 1:(nrow(df) - 1)) {  # 注意：避免最后一行访问超出范围
      df$LogReturn[i] <- log(df$Close[i]) - log(df$Close[i+1])
    }
    df$Volatility <- NA
    for (i in 1:(nrow(df))) {
      end_date <- df$Date[i]
      start_date <- end_date - window_size
      trading_days_in_range <- df[df$Date >= start_date & df$Date <= end_date, ]
      trading_returns <- trading_days_in_range$LogReturn
      if (start_date >= tail(df$Date, 1)) {
        volatility <- sd(trading_returns, na.rm = TRUE)
        AnnualizedVolatility <- volatility * sqrt(242)
        df$Volatility[i] <- AnnualizedVolatility
      }
    }
    return(df)
  }
  df <- calculate_volatility(df, window_size)[,c(1,12)]
  df <- na.omit(df)
  return(df)
}

option_latest_price <- function(option_code) {
  latest_price <- option_sse_spot_price_sina(option_code)[[3,2]]
  latest_price <- as.numeric(latest_price)
  return(latest_price)
}

option_vola_df <- function(option_code) {
  expire_date <- option_expire_day_sina(option_code)[1]
  trade_date = paste0( substr(expire_date,1,4), substr(expire_date,6,7))
  underlying = option_sse_spot_price_sina(option_code)[[37,2]]
  option_sse_codes_sina <- function(symbol = "看涨期权", trade_date = "202402", underlying = "510050") {
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
    if (symbol == "看涨期权") {
      url <- paste0("https://hq.sinajs.cn/list=OP_UP_", underlying, substr(trade_date, 3, 6))
    } else {
      url <- paste0("https://hq.sinajs.cn/list=OP_DOWN_", underlying, substr(trade_date, 3, 6))
    }
    handle <- curl::new_handle()
    curl::handle_setheaders(handle, .list = headers)
    curl::handle_setopt(handle, timeout = 6)
    conn <- curl::curl(url, handle = handle)
    data_text <- readLines(conn, warn = FALSE)
    data_temp <- unlist(strsplit(data_text, ","))
    temp_list <- data_temp[grep("^CON_OP_", data_temp)]
    option_codes <- substr(temp_list, 8, nchar(temp_list))
    # temp_df <- data.frame(
    #   序号 = 1:length(option_codes),
    #   期权代码 = option_codes,
    #   stringsAsFactors = FALSE
    # )
    return(option_codes)
  }
  
  result <- option_sse_codes_sina(trade_date = trade_date, underlying = underlying)
  price_X <- c()
  latest_price <- c()
  for (i in result) {
    price_X <- append(price_X, option_strike_price(i))
    latest_price <- append(latest_price, option_latest_price(i))
  }
  df_1 = data.frame(
    行权价格 = price_X ,
    最新价 = latest_price
  )
  return(df_1)
}

##############已有函数列表##################
# Akshare基础爬虫代码：
# option_sse_spot_price_sina(option_code)               #输出期权所有相关信息
# option_sse_underlying_spot_price_sina(option_code)   #输出标的物部分相关信息
# 
# 进阶数据处理 & 爬虫：
# option_spot_price_sina(option_code)         #期权实时价格等信息（简化处理）
# option_expire_day_sina(option_code)         #期权到期时间与剩余天数,输出列表[到期时间,剩余天数]
# option_history_price_sohu(option_code)      #标的物历史价格（100天）
# 
# 定价直接获取数据用的：
# option_type_judge_c_p(option_code)            #判断期权看涨还是看跌
# option_strike_price(option_code)              #读取期权执行价（int*0.001）
# option_underlying_price_sina(option_code)     #读取标的物现价（float数据）
# option_expire_years(option_code)              #期权到期时间（以年为单位）
# 
# 11月末新增，用于对接数据或新功能：
# history_price_sohu(underlying_spot)     #直接用标的代码爬100天历史价格数据
# candlestick_underlying(option_code)     #画期权对应标的资产的K线
# volatility_latest(underlying_spot,window_size)      #计算标的历史波动率，直接用标的代码
# volatility_plot(underlying_spot,window_size)        #画出标的历史波动率曲线，直接用标的代码
# volatility_df(underlying_spot,window_size)          #输出标的历史波动率表格
# option_latest_price(option_code)        #读取期权最新价
# option_vola_df(option_code)             #获取同行权日、同标的所有期权的行权价与现价列表
# 

################测试函数####################

# window_size = 30

# option_code = "10008107"
# # underlying_spot = "510050"
# df <- option_spot_price_sina(option_code)[[4,2]]
# print(class(df))

# df <- volatility_latest(option_spot_price_sina(option_code)[[3,2]],window_size)
# print(class(df))


# 
# underlying_spot = 510050
# df <- history_price_sohu(underlying_spot)
# print(df)

