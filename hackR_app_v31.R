library(DT)
library(shiny)
library(bslib)
library(curl)
library(data.table)
library(stats)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(flexdashboard)
library(curl)

CRR_n = 100
MC_n = 5000
# 使用 10008107 10008105 10007677 作为默认option_code测试
# 使用 510050 作为默认标的code测试

##########函数定义区--建议折叠################

options(width = 1000)   #取消自动换行
options(digits = 16)    #提高计算精度
options(warn = - 1)    #关闭警告

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

#给ggplot保留4位小数用的
custom_format <- function(x) sprintf("%.4f", x)

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
  if (underlying_spot=="000300"){
    url <- paste0('https://q.stock.sohu.com/hisHq?code=zs_', underlying_spot, '&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp')
  }
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
  if (underlying_spot=="000300"){
    url <- paste0('https://q.stock.sohu.com/hisHq?code=zs_', underlying_spot, '&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp')
  }
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
        df$Volatility[i] <- round(AnnualizedVolatility,4)
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
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month", expand = c(0, 0)) +      
      scale_y_continuous(
        labels = function(y) sprintf("%.4f", y)  # 设置 Y 轴刻度保留 4 位小数
      )
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
        df$Volatility[i] <- round(AnnualizedVolatility,4)
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

plot_iv <- function(option_code,rf_rate) {
  df_iv = option_vola_df(option_code)
  S = option_underlying_price_sina(option_code)
  r = rf_rate
  T = option_expire_years(option_code)
  #计算理论价格的函数
  black_scholes <- function(S, K, r, T, sigma) {
    d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)
    C <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    return(C)
  }
  #计算隐含波动率的函数
  implied_volatility <- function(S, K, r, T, market_price) {
    # 初始波动率猜测
    sigma_guess <- 0.
    # 目标函数：期权价格与市场价格的误差
    objective_function <- function(sigma) {
      bs_price <- black_scholes(S, K, r, T, sigma)
      return((bs_price - market_price)^2)
    }
    # 最小化误差函数，寻找隐含波动率
    result <- optimize(objective_function, c(0.0001, 5), tol = 1e-8)
    return(result$minimum)  # 返回最小化误差的波动率
  }
  
  # 计算每个期权的隐含波动率
  df_iv$IV <- mapply(function(K, market_price) {
    iv <- implied_volatility(S, K, r, T, market_price)
    return(iv)
  }, df_iv$行权价格, df_iv$最新价)
  
  #print(df_iv) #检视波动率表格
  # 拟合隐含波动率曲线（最小二乘法）
  
  model <- lm(IV ~ poly(行权价格, 2), data = df_iv)
  
  # 创建 ggplot2 图形
  p <- ggplot(df_iv, aes(x = 行权价格, y = IV)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = FALSE) +
    labs(title = "隐含波动率曲线") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转标签并加粗
      plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
      axis.title.x = element_text(size = 12, face = "bold"),  # x轴标题加粗
      axis.title.y = element_text(size = 12, face = "bold"),  # y轴标题加粗
      panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
      panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线
      panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线
    ) +      
    scale_y_continuous(
      labels = function(y) sprintf("%.4f", y)  # 设置 Y 轴刻度保留 4 位小数
    ) +      
    scale_x_continuous(
      labels = function(x) sprintf("%.4f", x)  # 设置 Y 轴刻度保留 4 位小数
    )
  
  # 将 ggplot 图形转换为交互式图形
  iv_plot <- ggplotly(p)
  
  # 返回交互式图形
  return(iv_plot)
}

iv_latest <- function(option_code,rf_rate) {
  df_iv = option_vola_df(option_code)
  S = option_underlying_price_sina(option_code)
  r = rf_rate
  T = option_expire_years(option_code)
  K = option_strike_price(option_code)
  market_price = option_latest_price(option_code)
  #计算理论价格的函数
  black_scholes <- function(S, K, r, T, sigma) {
    d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)
    C <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    return(C)
  }
  #计算隐含波动率的函数
  implied_volatility <- function(S, K, r, T, market_price) {
    # 初始波动率猜测
    sigma_guess <- 0.2
    # 目标函数：期权价格与市场价格的误差
    objective_function <- function(sigma) {
      bs_price <- black_scholes(S, K, r, T, sigma)
      return((bs_price - market_price)^2)
    }
    # 最小化误差函数，寻找隐含波动率
    result <- optimize(objective_function, c(0.0001, 5), tol = 1e-8)
    return(result$minimum)  # 返回最小化误差的波动率
  }
  latest_iv <- implied_volatility(S, K, r, T, market_price)
  latest_iv <- round(latest_iv,4)
  latest_date <- option_spot_price_sina(option_code)[[4,2]]
  dates <- c(substr(latest_date, 1, 10))  # 日期向量
  iv_data <- c(latest_iv)  # 数据向量
  # 创建数据框
  df_latest_iv <- data.frame(Date = dates, IV = iv_data)
  return(df_latest_iv)
}

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

#生成希腊字母图
xilazimu <- function(s, k, r, T, sigma, n) {
  d1 <- (log(s / k) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  d <- c(d1, d2)
  d1 <- d[1]
  if(n == "c"){n = 1 }
  if(n == "p"){n = -1}
  delta <- n * pnorm(n * d1)
  gamma <- dnorm(d1) / (s * sigma * sqrt(T))
  vega <- (s * dnorm(d1) * sqrt(T)) / 100
  d2 <- d[2]
  theta <- (-1 * (s * dnorm(d1) * sigma) / (2 * sqrt(T)) - n * r * k * exp(-r * T) * pnorm(n * d2)) / 365
  rho <- n * s * T * exp(-r * T) * dnorm(d2) / 100  # 计算 rho 并除以100，以便结果在百分比范围内
  return(list(delta = delta, gamma = gamma, vega = vega, theta = theta, rho = rho))
}

####################################

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "期权定价APP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("期权信息",tabName="Information", icon = icon("calculator")),
      menuItem("期权定价", tabName = "option_pricing", icon = icon("bitcoin")),
      menuItem("历史波动率", tabName = "historical_volatility", icon = icon("chart-line")),
      menuItem("交易策略", tabName = "trading_strategy", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .modal-dialog {
          width: 90% !important; /* 设置弹窗宽度 */
          max-width: 1200px !important; /* 最大宽度 */
        }
        .modal-body img {
          max-width: 100% !important; /* 图片宽度自适应弹窗 */
          height: auto !important; /* 高度自适应 */
        }
        .table-wrapper {
          background-color: white; /* 背景色 */
        }
        table.dataTable {
          background-color: white; /* 表格内容透明，背景统一 */
        }
        
        body {
          font-family: 'Times New Roman', serif;  /* 设置默认字体为 Times New Roman */
        }
        
        /* 中文字体使用微软雅黑 */
        body, .modal-body, .sidebar-menu > li > a, .box-title, .box-header, .box-content, .shiny-input-container, .btn-primary, .btn-secondary, .btn-success, .btn-warning {
          font-family: 'Times New Roman', serif;
        }
        
        /* 针对中文设置字体为微软雅黑 */
        .zh-text {
          font-family: '微软雅黑', sans-serif;
        }
                      
  ),
      "))
    ),
    tabItems(
      tabItem(tabName = "Information",
              mainPanel(
                fluidRow(
                  column(5, textInput(inputId = "opcode", label = "期权代码:",placeholder = "请输入期权代码，例如：10007644")),
                  column(1, actionButton(inputId = "yess", label = "确认"))
                ),
                h4("期权信息", style = "font-family: '微软雅黑', serif;"),
                div(
                  style = "display: flex; justify-content: space-between;",  # 强制水平布局
                  div(style = "width: 48%;", DTOutput("期权信息表")),  # 左侧表格
                  div(style = "width: 48%;", DTOutput("期权五档买卖"))   # 右侧表格
                )
              )
      ),
      tabItem(tabName = "option_pricing",
              sidebarLayout(
                sidebarPanel(
                  tags$a(href="https://quote.eastmoney.com/center/gridlist.html#options_sh50etf_all",target = "_blank", "OptionInformation"),
                  selectInput(inputId = "exchange", label = "交易所:", choices = c("上交所", "深交所"), selected = "上交所"),
                  textInput(inputId = "code", label = "期权代码:", placeholder = "请输入期权代码，例如：10007644"),            
                  selectInput(inputId = "type", label = "期权类型（e表示欧式期权，a表示美式期权）:", choices = c("e", "a"), selected = "e"),
                  numericInput(inputId = "rf", label = "无风险利率", value = 0.018),
                  selectInput(inputId = "sigma", label = "波动率天数选择:", choices = c("30", "60", "90", "Other..." = "other"), selected = "30"),
                  conditionalPanel(condition = "input.sigma == 'other'", textInput(inputId = "sigma_1", label = "输入90天以内的天数:", placeholder = "输入天数")),
                  actionButton(inputId = "yes", label = "确认"),
                  actionButton(inputId = "showModal", label = "标的K线图")
                ),
                mainPanel(
                  actionButton("tip_button", label = "💡温馨提示💡", style = "font-size: 12px; padding: 4px 8px;"),                  
                  h4("结果", style = "font-family: '微软雅黑', serif;"),
                  DTOutput("定价结果表"),
            
                  title = "隐波+希腊",
                  status = "info",
                  tabsetPanel(
                    tabPanel("隐含波动率图", plotlyOutput("隐含波动率图"),DTOutput("隐含波动率表")),
                    tabPanel("delta", fluidRow(column(5, numericInput(inputId = "per1", label = "波动幅度:", value=0.1),offset=7)),
                             plotlyOutput("delta图"), DTOutput("delta表")),
                    tabPanel("gamma", fluidRow(column(5, numericInput(inputId = "per2", label = "波动幅度:", value=0.1),offset=7)),
                             plotlyOutput("gamma图"), DTOutput("gamma表")),
                    tabPanel("vega", fluidRow(column(5, numericInput(inputId = "per3", label = "波动幅度:", value=0.1),offset=7)),
                             plotlyOutput("vega图"), DTOutput("vega表")),
                    tabPanel("theta",fluidRow(column(5, numericInput(inputId = "per4", label = "波动幅度:", value=0.1),offset=7)),
                             plotlyOutput("theta图"), DTOutput("theta表")),
                    tabPanel("rho", fluidRow(column(5, numericInput(inputId = "per5", label = "波动幅度:", value=0.1),offset=7)),
                             plotlyOutput("rho图"), DTOutput("rho表"))
                  ),
                )
              )
      ),
      tabItem(tabName = "historical_volatility",
              mainPanel(
                fluidRow(
                  column(5, textInput(inputId = "underlying_code", label = "标的物代码:", value = "510050", placeholder = "请输入标的物代码")),
                  column(3, selectInput(inputId = "historical_volatility", label = "时间:", choices = c("30", "60", "90", "Other..." = "other"), selected = "30")),
                  column(3, conditionalPanel(condition = "input.historical_volatility == 'other'", textInput(inputId = "historical_volatility_1", label = "输入90天以内的天数:", placeholder = "输入天数"))),
                  column(1, actionButton(inputId = "confirm", label = "确认"))
                ),
                h4("历史波动率", style = "font-family: '微软雅黑', serif;"),
                plotlyOutput("历史波动率图"),
                DTOutput("历史波动率表")
              )
      ),
      tabItem(tabName = "trading_strategy",
              sidebarLayout(
                sidebarPanel(
                  numericInput(inputId = "n", label = "输入期权组合中的期权数量:", value = 1),
                  actionButton(inputId = "confirm", label = "确认"),
                  uiOutput("inputGrid")
                ),
                mainPanel(
                  actionButton("tip_button_2", label = "💡使用提示💡", style = "font-size: 14px; padding: 4px 8px;"),                  
                  h4("期权组合", style = "font-family: '微软雅黑', serif;"),
                  DTOutput("期权组合表"),
                  h4("交易策略图", style = "font-family: '微软雅黑', serif;"),
                  plotOutput("交易策略图")
                )
              )
      )
    )
  )
)

server <- function(input, output,session) {
  
  observeEvent(input$yess, {
    ##请输出期权信息表##
    output$期权信息表 <- renderDT({
      option_code <- input$opcode
      option_info_1 = option_sse_spot_price_sina(option_code)
      option_info = rbind(option_info_1[38],option_info_1[37],option_info_1[1:12],option_info_1[39:43])
      datatable(option_info, options = list(pageLength = 100, lengthChange = FALSE, searching = FALSE))
    })
    output$期权五档买卖 <- renderDT({
      option_code <- input$opcode
      option_info_1 = option_sse_spot_price_sina(option_code)
      option_info = rbind(option_info_1[13:32])
      datatable(option_info, options = list(pageLength = 100, lengthChange = FALSE, searching = FALSE))
    })
  })
  
  observeEvent(input$yes, {
    # 从用户输入中获取数据
    # 特别注意以下的玄学代码，参考群内部分消息，source的位置不能随意改动
    exchange <- input$exchange
    option_code <- input$code
    option_eu_us <- input$type
    rf_rate <- input$rf
    window_size <- if (input$sigma == "other") {
      as.numeric(input$sigma_1)
    } else {
      as.numeric(input$sigma)
    }
    
    sigma = volatility_latest(option_spot_price_sina(option_code)[[3,2]],window_size)
    
    option_c_p = option_type_judge_c_p(option_code)
    option_type = paste(option_c_p, option_eu_us, sep = "", collapse=NULL)
    
    #标的当前价格 & 期权执行价
    price_S = option_underlying_price_sina(option_code)
    price_X = option_strike_price(option_code)
    
    #到期时间（以年为单位）
    expire_time = option_expire_years(option_code)
    
    option_name = option_sse_spot_price_sina(option_code)[[38,2]]
    
    option_lp =  option_latest_price(option_code)
    
    #################调用定价函数####################
    
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
    
    option_data <- data.frame(
      指标 = c("期权名称",
             "实时期权价格",
             "二叉树模型期权价格", 
             "蒙特卡洛模拟期权价格", 
             "Black-Scholes模型期权价格"),
      值 = c(option_name, 
            sprintf("%.4f", option_lp),
            sprintf("%.4f", CRR_option_price), 
            sprintf("%.4f", MC_option_price), 
            sprintf("%.4f", BS_option_price)) 
    )

    
    # 将计算结果输出到主面板
    output$定价结果表 <- renderDataTable({
      datatable(option_data, options = list(dom = 't'))
    })
    # output$警示信息 <- renderText({
    #   "股市有风险，投资需谨慎!!!!!!!"
    # })
  })
  
  ####弹窗######################################################
  observeEvent(input$showModal, {
    option_code <- input$code
    showModal(modalDialog(
      easyClose = TRUE,
      candlestick_underlying(option_code),
    ))
  })
  observeEvent(input$tip_button, {
    # 弹出一个模态窗口
    showModal(
      modalDialog(
        title = "温馨提示",
        "股市有风险，投资需谨慎！请根据自身情况合理投资，理性操作，保护您的财产安全。",
        easyClose = TRUE,  # 点击空白处关闭弹窗
      )
    )
  })
  ####第一页补充内容#############################################

  observeEvent(input$yes, {
    exchange <- input$exchange
    option_code <- input$code
    option_eu_us <- input$type
    rf_rate <- input$rf
    window_size <- if (input$sigma == "other") {
      as.numeric(input$sigma_1)
    } else {
      as.numeric(input$sigma)
    }
    sigma = volatility_latest(option_spot_price_sina(option_code)[[3,2]],window_size)
    option_c_p = option_type_judge_c_p(option_code)
    option_type = paste(option_c_p, option_eu_us, sep = "", collapse=NULL)
    price_S = option_underlying_price_sina(option_code)
    price_X = option_strike_price(option_code)
    expire_time = option_expire_years(option_code)
    
    zimu <- xilazimu(
      s<- price_S,
      k = price_X, 
      T = expire_time, 
      r = rf_rate,
      sigma = sigma,
      n = option_c_p
    )
    
    s <- price_S  # 初始标的资产价格
    k <- price_X    # 执行价格
    r <- rf_rate  # 无风险利率
    T <- expire_time     # 到期时间（1年）
    sigma <- sigma  # 波动率
    n <- option_c_p    # 看涨期权 (1 为看涨，-1 为看跌)
    
    #####################
    per <- 0.2 #(标的资产的价格波动幅度) 
    per1 <- input$per1
    per2 <- input$per2
    per3 <- input$per3
    per4 <- input$per4
    per5 <- input$per5
    #####################
    
    price_changes1 <- seq(1-per1, 1+per1, by = 0.01)  # 标的资产价格的百分比变化
    price_changes2 <- seq(1-per2, 1+per2, by = 0.01)  # 标的资产价格的百分比变化
    price_changes3 <- seq(1-per3, 1+per3, by = 0.01)  # 标的资产价格的百分比变化
    price_changes4 <- seq(1-per4, 1+per4, by = 0.01)  # 标的资产价格的百分比变化
    price_changes5 <- seq(1-per5, 1+per5, by = 0.01)  # 标的资产价格的百分比变化
    
    prices1 <- s * price_changes1  # 标的资产的实际价格
    prices2 <- s * price_changes2  # 标的资产的实际价格
    prices3 <- s * price_changes3  # 标的资产的实际价格
    prices4 <- s * price_changes4  # 标的资产的实际价格
    prices5 <- s * price_changes5  # 标的资产的实际价格
    # 计算不同价格下的 Delta
    deltas <- sapply(prices1, function(s) xilazimu(s, k, r, T, sigma, n)$delta)
    gammas <- sapply(prices2, function(s) xilazimu(s, k, r, T, sigma, n)$gamma)
    vegas <- sapply(prices3, function(s) xilazimu(s, k, r, T, sigma, n)$vega)
    thetas <- sapply(prices4, function(s) xilazimu(s, k, r, T, sigma, n)$theta)
    rhos <- sapply(prices5, function(s) xilazimu(s, k, r, T, sigma, n)$rho)
    
    deltas <- round(deltas,6)
    gammas <- round(gammas,6)
    vegas <- round(vegas,6)
    thetas <- round(thetas,6)
    rhos <- round(rhos,6)
    
    # 创建数据框，方便绘制
    df_d <- data.frame(price = prices1, delta = deltas)
    df_g <- data.frame(price = prices2, gamma = gammas)
    df_v <- data.frame(price = prices3, vega = vegas)
    df_t <- data.frame(price = prices4, theta = thetas)
    df_r <- data.frame(price = prices5, rho = rhos)
    
    
    # 这里生成隐波图
    output$隐含波动率图 <- renderPlotly({
      plot_iv(option_code,rf_rate)
      #barplot(rnorm(5), main = "Plot 2", xlab = "X-axis", ylab = "Y-axis")
    })
    
    output$隐含波动率表 <- renderDT({
      datatable(iv_latest(option_code,rf_rate), options = list(pageLength = 100, lengthChange = FALSE, searching = FALSE))  # 设置每页显示5行
    })

    
    output$delta图 <- renderPlotly({
      # 生成第一个图形
      ggplotly(
        ggplot(df_d, aes(x = price, y = delta)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = s, linetype = "dashed", color = "gray", size = 1) +
          labs(title = "Option Delta vs. Underlying Asset Price") +
          xlab("Underlying Asset Price") +
          ylab("Delta") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转日期标签并加粗
            plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
            axis.title.x = element_text(size = 12, face = "bold"),  # x轴标题加粗
            axis.title.y = element_text(size = 12, face = "bold"),  # y轴标题加粗
            panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
            panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线变淡
            panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线更淡
          ) +
          scale_x_continuous(labels = custom_format) +  # x轴保留4位小数，custom_format定义在最前面
          scale_y_continuous(labels = custom_format))    # y轴保留4位小数
  })
    
    output$gamma图 <- renderPlotly({
      # 生成第二个图形
      ggplotly(
        ggplot(df_g, aes(x = price, y = gamma)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = s, linetype = "dashed", color = "gray", size = 1) +
          labs(title = "Option Gamma vs. Underlying Asset Price") +
          xlab("Underlying Asset Price") +
          ylab("Gamma") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转日期标签并加粗
            plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
            axis.title.x = element_text(size = 12, face = "bold"),  # x轴标题加粗
            axis.title.y = element_text(size = 12, face = "bold"),  # y轴标题加粗
            panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
            panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线变淡
            panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线更淡
          ) +
          scale_x_continuous(labels = custom_format) +  # x轴保留4位小数，custom_format定义在最前面
          scale_y_continuous(labels = custom_format))    # y轴保留4位小数
    })
    
    output$vega图 <- renderPlotly({
      # 生成第三个图形
      ggplotly(
        ggplot(df_v, aes(x = price, y = vega)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = s, linetype = "dashed", color = "gray", size = 1) +
          labs(title = "Option Vega vs. Underlying Asset Price") +
          xlab("Underlying Asset Price") +
          ylab("Vega") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转日期标签并加粗
            plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
            axis.title.x = element_text(size = 12, face = "bold"),  # x轴标题加粗
            axis.title.y = element_text(size = 12, face = "bold"),  # y轴标题加粗
            panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
            panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线变淡
            panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线更淡
          ) +
          scale_x_continuous(labels = custom_format) +  # x轴保留4位小数，custom_format定义在最前面
          scale_y_continuous(labels = custom_format))    # y轴保留4位小数
    })
    
    output$theta图 <- renderPlotly({
      # 生成第四个图形
      ggplotly(
        ggplot(df_t, aes(x = price, y = theta)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = s, linetype = "dashed", color = "gray", size = 1) +
          labs(title = "Option Theta vs. Underlying Asset Price") +
          xlab("Underlying Asset Price") +
          ylab("Theta") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转日期标签并加粗
            plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
            axis.title.x = element_text(size = 12, face = "bold"),  # x轴标题加粗
            axis.title.y = element_text(size = 12, face = "bold"),  # y轴标题加粗
            panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
            panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线变淡
            panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线更淡
          ) +
          scale_x_continuous(labels = custom_format) +  # x轴保留4位小数，custom_format定义在最前面
          scale_y_continuous(labels = custom_format))    # y轴保留4位小数
    })
    
    output$rho图 <- renderPlotly({
      # 生成第五个图形
      ggplotly(
        ggplot(df_r, aes(x = price, y = rho)) +
          geom_line(color = "blue", size = 1) +
          geom_vline(xintercept = s, linetype = "dashed", color = "gray", size = 1) +
          labs(title = "Option Rho vs. Underlying Asset Price") +
          xlab("Underlying Asset Price") +
          ylab("Rho") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # 旋转日期标签并加粗
            plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中并加粗
            axis.title.x = element_text(size = 12, face = "bold"),  # x轴标题加粗
            axis.title.y = element_text(size = 12, face = "bold"),  # y轴标题加粗
            panel.background = element_rect(fill = "gray", color = NA),  # 设置面板背景为灰色
            panel.grid.major = element_line(color = alpha("white", 0.5), size = 0.5),  # 主要网格线变淡
            panel.grid.minor = element_line(color = alpha("white", 0.5), size = 0.25)  # 次要网格线更淡
          ) +
          scale_x_continuous(labels = custom_format) +  # x轴保留4位小数，custom_format定义在最前面
          scale_y_continuous(labels = custom_format))    # y轴保留4位小数
    })
    
    # 这里生成希腊字母表
    output$delta表 <- renderDT({
      data <- data.frame(
        Column1 = 1:5,
        Column2 = letters[1:5]
      )
      datatable(df_d, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE))  # 设置每页显示5行
    })
    
    output$gamma表 <- renderDT({
      data <- data.frame(
        Column1 = 1:5,
        Column2 = letters[1:5]
      )
      datatable(df_g, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE))  # 设置每页显示5行
    })
    
    output$vega表 <- renderDT({
      data <- data.frame(
        Column1 = 1:5,
        Column2 = letters[1:5]
      )
      datatable(df_v, options = list(pageLength = 5))  # 设置每页显示5行
    })
    
    output$theta表 <- renderDT({
      data <- data.frame(
        Column1 = 1:5,
        Column2 = letters[1:5]
      )
      datatable(df_t, options = list(pageLength = 5))  # 设置每页显示5行
    })
    
    output$rho表 <- renderDT({
      data <- data.frame(
        Column1 = 1:5,
        Column2 = letters[1:5]
      )
      datatable(df_r, options = list(pageLength = 5))  # 设置每页显示5行
    })
  })
  
  ###第二张标签页：历史波动率绘图############################################
  observeEvent(input$confirm, {
    # 获取用户输入
    code <- input$underlying_code
    days <- as.numeric(input$historical_volatility)
    
    # 检查是否选择了"Other..."并获取相应的天数
    if (input$historical_volatility == 'other' && !is.null(input$historical_volatility_1)) {
      days <- as.numeric(input$historical_volatility_1)
    }
    
    # 过滤数据
    # filtered_data <- data.frame(
    #   Code = code,
    #   HistoricalVolatility = ifelse(input$historical_volatility == 'other', days, as.numeric(days))
    # )
    
    output$历史波动率图 <- renderPlotly({
      volatility_plot(code,days)
    })
    
    output$历史波动率表 <- renderDT({
      # volatility_df(code,days)
      # df_test = data.frame(
      #   cl1 = c(1,2,3,4),
      #   cl2 = c(2,6,4,8)
      # )
      datatable(volatility_df(code,days), options = list(pageLength = 5))  # 设置每页显示5行
    })
  })
  

  
  
  #####第三张标签页：交易策略#######################################
  output$inputGrid <- renderUI({
    req(input$n) # 确保n已输入
    # 使用lapply生成UI元素列表
    ui_list <- lapply(1:input$n, function(i) {
      list(
        fluidRow(
          column(4, numericInput(paste0("option_price", i), paste0("期权价格 ", "(" , i , ")") , value = NULL)),
          column(4, numericInput(paste0("K", i), paste0("行权价",  "(" , i , ")"), value = NULL)),
          column(4, radioButtons(paste0("option_type", i), paste0("Call/Put ",  "(" , i , ")"), c("Call" = "Call", "Put" = "Put"), inline = TRUE))
        ),
        fluidRow(
          column(4, radioButtons(paste0("action_type", i), paste0("买/卖 ",  "(" , i , ")"), c("Buy" = "Buy", "Sell" = "Sell"), inline = TRUE)),
          column(4, numericInput(paste0("quantity", i), paste0("数量 ",  "(" , i , ")"), value = 0))
        )
      )
    })
    # 将列表中的UI元素合并为一个整体
    do.call(tagList, ui_list)
  })
  
  # 添加一个“确定”按钮
  output$确定按钮 <- renderUI({
    actionButton("confirm", "确认")
  })
  
  # 收集输入框的数据并生成表格
  output$期权组合表 <- renderDT({
    req(input$confirm) # 确保确认按钮已被点击
    data <- data.frame(
      Option_price = sapply(1:input$n, function(i) input[[paste0("option_price", i )]]),
      K = sapply(1:input$n, function(i) input[[paste0("K",  i )]]),
      Option_type = sapply(1:input$n, function(i) input[[paste0("option_type",  i )]]),
      Action_type = sapply(1:input$n, function(i) input[[paste0("action_type",  i )]]),
      Quantity = sapply(1:input$n, function(i) input[[paste0("quantity",  i )]]),
      stringsAsFactors = FALSE
    )
    
    # 检查数据是否为空，如果为空则返回一个空表
    if (any(is.null(data$Option_price) | is.null(data$K) | is.null(data$Option_type) | 
            is.null(data$Action_type) | is.null(data$Quantity))) {
      return(NULL) # 如果有缺失值，返回空表
    }
    
    data
  })
  
  # 更新表格的事件
  observeEvent(input$confirm, {
    output$期权组合表 <- renderDT({
      req(input$n) # 确保n已输入
      data <- data.frame(
        Option_price = sapply(1:input$n, function(i) input[[paste0("option_price", i)]]),
        K = sapply(1:input$n, function(i) input[[paste0("K",   i )]]),
        Option_type = sapply(1:input$n, function(i) input[[paste0("option_type", i )]]),
        Action_type = sapply(1:input$n, function(i) input[[paste0("action_type",  i )]]),
        Quantity = sapply(1:input$n, function(i) input[[paste0("quantity",  i)]]),
        stringsAsFactors = FALSE
      )
      
      # 检查数据是否为空
      if (any(is.null(data$Option_price) | is.null(data$K) | is.null(data$Option_type) | 
              is.null(data$Action_type) | is.null(data$Quantity))) {
        return(NULL) # 如果有缺失值，返回空表
      }
      
      data
    })
  })
  
  ### 绘图函数（添加确认按钮后绘制图像）
  plot_multiple_option_profits <- function(options_data) {
    stock_prices <- seq(0, 2 * max(options_data$K, na.rm = TRUE), by = 1)
    all_profits <- data.frame()
    total_profits <- numeric(length(stock_prices))
    
    # 计算每个期权的利润和总利润
    for (i in 1:nrow(options_data)) {
      option_price <- options_data$Option_price[i]
      K <- options_data$K[i]
      option_type <- options_data$Option_type[i]
      action_type <- options_data$Action_type[i]
      n <- options_data$Quantity[i]
      
      # 计算每种期权的利润
      if (option_type == "Call") {
        if (action_type == "Buy") {
          profits <- pmax(stock_prices - K, 0) * n - option_price * n
        } else if (action_type == "Sell") {
          profits <- -(pmax(stock_prices - K, 0) * n - option_price * n)
        }
      } else if (option_type == "Put") {
        if (action_type == "Buy") {
          profits <- pmax(K - stock_prices, 0) * n - option_price * n
        } else if (action_type == "Sell") {
          profits <- -(pmax(K - stock_prices, 0) * n - option_price * n)
        }
      } else {
        stop("Invalid option type!")
      }
      
      total_profits <- total_profits + profits
      
      temp_data <- data.frame(
        StockPrice = stock_prices,
        Profit = profits,
        OptionGroup = paste("Option", i)
      )
      all_profits <- rbind(all_profits, temp_data)
    }
    
    # 计算盈亏平衡点：找出总利润为零的点
    tolerance <- 1e-3
    break_even_points <- stock_prices[which(abs(total_profits) < tolerance)]
    
    # 如果没有找到盈亏平衡点，尝试通过更细的步长计算
    if (length(break_even_points) == 0) {
      stock_prices_fine <- seq(0, 2 * max(options_data$K, na.rm = TRUE), by = 0.1)
      total_profits_fine <- numeric(length(stock_prices_fine))
      
      for (i in 1:nrow(options_data)) {
        option_price <- options_data$Option_price[i]
        K <- options_data$K[i]
        option_type <- options_data$Option_type[i]
        action_type <- options_data$Action_type[i]
        n <- options_data$Quantity[i]
        
        if (option_type == "Call") {
          if (action_type == "Buy") {
            profits_fine <- pmax(stock_prices_fine - K, 0) * n - option_price * n
          } else if (action_type == "Sell") {
            profits_fine <- -(pmax(stock_prices_fine - K, 0) * n - option_price * n)
          }
        } else if (option_type == "Put") {
          if (action_type == "Buy") {
            profits_fine <- pmax(K - stock_prices_fine, 0) * n - option_price * n
          } else if (action_type == "Sell") {
            profits_fine <- -(pmax(K - stock_prices_fine, 0) * n - option_price * n)
          }
        }
        
        total_profits_fine <- total_profits_fine + profits_fine
      }
      
      # 在细化的步长中再次寻找盈亏平衡点
      break_even_points <- stock_prices_fine[which(abs(total_profits_fine) < tolerance)]
    }
    
    # 绘制图像
    ggplot(all_profits, aes(x = StockPrice, y = Profit, color = OptionGroup)) +
      geom_line() +
      geom_line(data = data.frame(StockPrice = stock_prices, Profit = total_profits), 
                aes(x = StockPrice, y = Profit), color = "black", size = 1.5) +
      ggtitle("Option Profit Curves for Multiple Combinations and Total Profit") +
      xlab("Stock Price") +
      ylab("Profit") +
      theme_minimal() +
      scale_color_manual(values = rainbow(nrow(options_data))) +
      geom_line(y = 0, color = "black", linetype = "dashed", size = 0.5) +
      # 添加盈亏平衡点的标注
      geom_text(data = data.frame(x = break_even_points, y = rep(0, length(break_even_points))),
                aes(x = x, y = y, label = paste("BEP at:", round(x, 2))),
                vjust = -1, color = "red", angle = 0, hjust = 0.5)
  }
  
  
  # 确保点击“确认”按钮后绘制图像
  observeEvent(input$confirm, {
    data <- data.frame(
      Option_price = sapply(1:input$n, function(i) input[[paste0("option_price", i)]]),
      K = sapply(1:input$n, function(i) input[[paste0("K", i)]]),
      Option_type = sapply(1:input$n, function(i) input[[paste0("option_type", i)]]),
      Action_type = sapply(1:input$n, function(i) input[[paste0("action_type", i)]]),
      Quantity = sapply(1:input$n, function(i) input[[paste0("quantity", i)]]),
      stringsAsFactors = FALSE
    )
    
    # 检查数据是否为空
    if (any(is.null(data$Option_price) | is.null(data$K) | is.null(data$Option_type) | 
            is.null(data$Action_type) | is.null(data$Quantity))) {
      return(NULL) # 如果有缺失值，返回空表
    }
    
    output$交易策略图 <- renderPlot({
      plot_multiple_option_profits(data)
    })
  })
####弹窗######################################################
  observeEvent(input$tip_button_2, {
    # 弹出一个模态窗口
    showModal(
      modalDialog(
        title = "使用提示",
        "请确保在全部信息都输入完成后再点击确认；更改期权信息后，需要再次点击确认以更新交易策略图。",
        easyClose = TRUE,  # 点击空白处关闭弹窗
      )
    )
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)