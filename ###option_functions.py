import pandas as pd
import requests

########  修改了部分Akshare库函数  ###########

def option_sse_spot_price_sina(symbol: str = "10003720") -> pd.DataFrame:
    """
    新浪财经-期权-期权实时数据
    :param symbol: 期权代码
    :type symbol: str
    :return: 期权量价数据
    :rtype: pandas.DataFrame
    """
    url = f"https://hq.sinajs.cn/list=CON_OP_{symbol}"
    headers = {
        "Accept": "*/*",
        "Accept-Encoding": "gzip, deflate, br",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8",
        "Cache-Control": "no-cache",
        "Connection": "keep-alive",
        "Host": "hq.sinajs.cn",
        "Pragma": "no-cache",
        "Referer": "https://stock.finance.sina.com.cn/",
        "sec-ch-ua": '" Not;A Brand";v="99", "Google Chrome";v="97", "Chromium";v="97"',
        "sec-ch-ua-mobile": "?0",
        "sec-ch-ua-platform": '"Windows"',
        "Sec-Fetch-Dest": "script",
        "Sec-Fetch-Mode": "no-cors",
        "Sec-Fetch-Site": "cross-site",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/97.0.4692.71 Safari/537.36",
    }
    r = requests.get(url, headers=headers)
    data_text = r.text
    data_list = data_text[data_text.find('"') + 1 : data_text.rfind('"')].split(",")
    field_list = [
        "买量",
        "买价",
        "最新价",
        "卖价",
        "卖量",
        "持仓量",
        "涨幅",
        "行权价",
        "昨收价",
        "开盘价",
        "涨停价",
        "跌停价",
        "申卖价五",
        "申卖量五",
        "申卖价四",
        "申卖量四",
        "申卖价三",
        "申卖量三",
        "申卖价二",
        "申卖量二",
        "申卖价一",
        "申卖量一",
        "申买价一",
        "申买量一 ",
        "申买价二",
        "申买量二",
        "申买价三",
        "申买量三",
        "申买价四",
        "申买量四",
        "申买价五",
        "申买量五",
        "行情时间",
        "主力合约标识",
        "状态码",
        "标的证券类型",
        "标的股票",
        "期权合约简称",
        "振幅",
        "最高价",
        "最低价",
        "成交量",
        "成交额",
    ]
    data_df = pd.DataFrame(list(zip(field_list, data_list)), columns=["字段", "值"])
    return data_df

def option_sse_underlying_spot_price_sina(
    symbol: str = "sh510300",
) -> pd.DataFrame:
    """
    期权标的物的实时数据
    :param symbol: sh510050 or sh510300
    :type symbol: str
    :return: 期权标的物的信息
    :rtype: pandas.DataFrame
    """
    option_spot_price_all = option_sse_spot_price_sina(symbol= option_code)
    option_spot_price = option_spot_price_all.loc[[37,2,36,32]].reset_index(drop=True)
    underlying_spot = option_spot_price.loc[2,"值"]
    if underlying_spot[:2] == "15":
        symbol= "sz" + underlying_spot
    else:
        symbol= "sh" + underlying_spot

    
    url = f"https://hq.sinajs.cn/list={symbol}"
    headers = {
        "Accept": "*/*",
        "Accept-Encoding": "gzip, deflate",
        "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8",
        "Cache-Control": "no-cache",
        "Host": "hq.sinajs.cn",
        "Pragma": "no-cache",
        "Proxy-Connection": "keep-alive",
        "Referer": "https://vip.stock.finance.sina.com.cn/",
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/97.0.4692.71 Safari/537.36",
    }
    r = requests.get(url, headers=headers)
    data_text = r.text
    data_list = data_text[data_text.find('"') + 1 : data_text.rfind('"')].split(",")
    field_list = [
        "证券简称",
        "今日开盘价",
        "昨日收盘价",
        "最近成交价",
        "最高成交价",
        "最低成交价",
        "买入价",
        "卖出价",
        "成交数量",
        "成交金额",
        "买数量一",
        "买价位一",
        "买数量二",
        "买价位二",
        "买数量三",
        "买价位三",
        "买数量四",
        "买价位四",
        "买数量五",
        "买价位五",
        "卖数量一",
        "卖价位一",
        "卖数量二",
        "卖价位二",
        "卖数量三",
        "卖价位三",
        "卖数量四",
        "卖价位四",
        "卖数量五",
        "卖价位五",
        "行情日期",
        "行情时间",
        "停牌状态",
    ]
    data_df = pd.DataFrame(list(zip(field_list, data_list)), columns=["字段", "值"])
    data_df_2 = data_df.loc[[0,3,30,31]].reset_index(drop=True)
    return data_df_2

###########自制函数，有出bug的风险###############

#期权实时价格（简化处理）
def option_spot_price_sina(option_code):
    option_spot_price_all = option_sse_spot_price_sina(symbol= option_code)
    option_spot_price = option_spot_price_all.loc[[37,2,36,32]].reset_index(drop=True)
    return option_spot_price

#期权到期时间与剩余天数
def option_expire_day_sina(option_code):
    headers = {'referer': 'http://finance.sina.com.cn'}
    resp = requests.get('https://hq.sinajs.cn/list=CON_OP_' + option_code, headers=headers, timeout=6)
    data_1 = resp.text
    data_2 = data_1.split(",")
    end_date = data_2[-5]
    expire_day = data_2[-4]
    return [end_date,expire_day]

#标的物历史价格（100天）
def option_history_price_sohu(option_code):
    option_spot_price_all = option_sse_spot_price_sina(symbol= option_code)
    option_spot_price = option_spot_price_all.loc[[37,2,36,32]].reset_index(drop=True)
    underlying_spot = option_spot_price.loc[2,"值"]
    headers = {'referer': 'https://q.stock.sohu.com'}
    resp = requests.get('https://q.stock.sohu.com/hisHq?code=cn_' + underlying_spot + '&stat=1&order=D&period=d&callback=historySearchHandler&rt=jsonp', headers=headers, timeout=6)
    data_text = resp.text
    data_list_1 = data_text.split('"hq":', 1)[1]
    data_list_2 = data_list_1.split(',"code"', 1)[0]
    data_list_2 = eval(data_list_2)
    df_1 = pd.DataFrame(data_list_2, columns=['日期','开盘','收盘','涨跌额','涨跌幅','最低','最高','成交量(手)','成交金额(万)','换手率'])
    df_2 = df_1.iloc[:, 0:3]
    return df_2

#期权到期时间与剩余天数,输出列表[到期时间,剩余天数]
def option_expire_day_sina(option_code):
    headers = {'referer': 'http://finance.sina.com.cn'}
    resp = requests.get('https://hq.sinajs.cn/list=CON_OP_' + option_code, headers=headers, timeout=6)
    data_1 = resp.text
    data_2 = data_1.split(",")
    end_date = data_2[-5]
    expire_day = data_2[-4]
    return [end_date,expire_day]

#######整合所需的简化版函数（在python直接实现所需功能，避免在R内进行复杂运算或调用）######

#判断期权看涨还是看跌
def option_type_judge_shsz(option_code):
    option_type = ""
    option_spot_price_all = option_sse_spot_price_sina(symbol= option_code)
    option_spot_price = option_spot_price_all.loc[[37,2,36,32]].reset_index(drop=True)
    if "购" in option_spot_price.loc[0,"值"]:
        option_type = "c"
    elif "沽" in option_spot_price.loc[0,"值"]:
        option_type = "p"
    else:
        option_type = "期权类型报错"
    return option_type

#读取期权执行价（int*0.001）
def option_strike_price(option_code):
    option_spot_price_all = option_sse_spot_price_sina(symbol= option_code)
    option_spot_price = option_spot_price_all.loc[[37,2,36,32]].reset_index(drop=True)
    deal_code = option_spot_price.loc[0,"值"]
    strike_price = int(deal_code.split("月")[1])*0.001
    return strike_price

#读取标的物现价（仅float数据）
def option_underlying_price_sina(option_code):
    df_1 = option_sse_underlying_spot_price_sina(option_code)
    underlying_price = df_1.loc[1,"值"]
    underlying_price = float(underlying_price)
    return underlying_price

#期权到期时间（以年为单位）
def option_expire_years(option_code):
    expire_days = option_expire_day_sina(option_code)[1]
    expire_years = float(int(expire_days)/365)  #用自然日计算
    return expire_years

#################################

pd.set_option('display.max_rows', None) #Dataframe展示全内容
pd.set_option('display.max_column', None)


#输入编码而非合约交易代码，目前只支持上证和深证
#option_code=input("输入六位期权编码（非合约交易代码），目前只支持上证和深证：\n")
option_code = "10007139"

#期权实时价格数据

option_spot_price = option_spot_price_sina(option_code)
print("期权实时价格数据：")
print(option_spot_price)
print("")
#print(option_type_judge_shsz(option_code))
'''

#读取期权对对应标的物ETF代码并判断是沪还是深
#标的物实时数据

option_underlying_price = option_underlying_price_sina(option_code)

print("标的物当前价格：")
print(option_underlying_price)
print("")

#期权到期时间与剩余天数
expire_day_list = option_expire_day_sina(option_code)
end_date = expire_day_list[0]
expire_day = expire_day_list[1]
print('到期时间：' + end_date)
print('剩余天数：' + expire_day)
print("")

history_price = option_history_price_sohu(option_code)
print(history_price)
'''
print(option_type_judge_shsz(option_code))
