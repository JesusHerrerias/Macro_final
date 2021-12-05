# Librerias
library(tidyverse)
library(magrittr)
library(fixest)

# Datos ====

# Sector financiero
yields <- readxl::read_xlsx("data/raw/Datos.xlsx", sheet = "Yields", 
                            col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric")) %>% 
  select("Date", "Brazil", "Chile", "Colombia", "Mexico", "Peru")


stock_market <- readxl::read_xlsx("data/raw/Datos.xlsx", sheet = "Headline stock market indices") %>% 
  select("Date", "Brazil", "Chile", "Colombia", "Mexico", "Peru")

fx <- readxl::read_xlsx("data/raw/Datos.xlsx", sheet = "FX DATA") %>% 
  select("Date", "Brazil", "Chile", "Colombia", "Mexico", "Peru")

# Sector real

Debt_gdp <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "Debt_gdp")
CDS_5y <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "CDS_5Y")
CPI <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "CPI")
Current_account <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "Current_account")
GDP <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "GDP")
Reserves <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "Reserves")
Rate <- readxl::read_xlsx("data/raw/Datos_2.xlsx", sheet = "Rate")


# Limpieza ====

# Sector financiero

yields <- yields %>% 
  mutate(Change_yield_Brazil = lead(Brazil) - lag(Brazil), Change_yield_Chile = lead(Chile) - lag(Chile),
         Change_yield_Colombia = lead(Colombia) - lag(Colombia), Change_yield_Mexico = lead(Mexico) - lag(Mexico),
         Change_yield_Peru = lead(Peru) - lag(Peru)) %>% 
  select(- Mexico, - Brazil, -Chile, - Colombia, - Peru) %>% 
  pivot_longer(-Date, names_to = "Country", values_to = "Values")

yields$Date <- as.Date(yields$Date, format = '%Y-%m-%d') 

stock_market <- stock_market %>% 
  mutate(Change_sm_Brazil = lead(Brazil) - lag(Brazil), Change_sm_Chile = lead(Chile) - lag(Chile),
         Change_sm_Colombia = lead(Colombia) - lag(Colombia), Change_sm_Mexico = lead(Mexico) - lag(Mexico),
         Change_sm_Peru = lead(Peru) - lag(Peru)) %>% 
  select(- Mexico, - Brazil, -Chile, - Colombia, - Peru) %>% 
  pivot_longer(-Date, names_to = "Country", values_to = "Values") 
  
stock_market$Date <- as.Date(stock_market$Date, format = '%Y-%m-%d') 



fx <- fx %>% 
  mutate(Change_fx_Brazil = lead(Brazil) - lag(Brazil), Change_fx_Chile = lead(Chile) - lag(Chile),
         Change_fx_Colombia = lead(Colombia) - lag(Colombia), Change_fx_Mexico = lead(Mexico) - lag(Mexico),
         Change_fx_Peru = lead(Peru) - lag(Peru)) %>% 
  select(- Mexico, - Brazil, -Chile, - Colombia, - Peru) %>% 
  pivot_longer(-Date, names_to = "Country", values_to = "Values") 

fx$Date <- as.Date(fx$Date, format = '%Y-%m-%d') 



yields <- yields %>%
  mutate(D1 = case_when(Date == "2008-11-25" ~ 1, TRUE ~ 0), D2 = case_when(Date == "2008-12-02" ~ 1, TRUE ~ 0),
         D3 = case_when(Date == "2008-12-16" ~ 1, TRUE ~ 0), D4 = case_when(Date == "2009-01-28" ~ 1, TRUE ~ 0),
         D5 = case_when(Date == "2009-03-18" ~ 1, TRUE ~ 0), D6 = case_when(Date == "2010-08-10" ~ 1, TRUE ~ 0),
         D7 = case_when(Date == "2010-08-27" ~ 1, TRUE ~ 0), D8 = case_when(Date == "2010-09-21" ~ 1, TRUE ~ 0),
         D9 = case_when(Date == "2010-10-15" ~ 1, TRUE ~ 0), D10 = case_when(Date == "2010-11-03" ~ 1, TRUE ~ 0),
         D11 = case_when(Date == "2011-08-09" ~ 1, TRUE ~ 0), D12 = case_when(Date == "2011-08-26" ~ 1, TRUE ~ 0),
         D13 = case_when(Date == "2011-09-21" ~ 1, TRUE ~ 0), D14 = case_when(Date == "2012-08-22" ~ 1, TRUE ~ 0),
         D15 = case_when(Date == "2012-08-31" ~ 1, TRUE ~ 0), D16 = case_when(Date == "2012-09-13" ~ 1, TRUE ~ 0),
         D17 = case_when(Date == "2013-03-20" ~ 1, TRUE ~ 0), D18 = case_when(Date == "2013-05-01" ~ 1, TRUE ~ 0),
         D19 = case_when(Date == "2013-05-22" ~ 1, TRUE ~ 0), D20 = case_when(Date == "2013-06-19" ~ 1, TRUE ~ 0),
         D21 = case_when(Date == "2013-07-11" ~ 1, TRUE ~ 0), D22 = case_when(Date == "2013-10-30" ~ 1, TRUE ~ 0),
         D23 = case_when(Date == "2013-12-18" ~ 1, TRUE ~ 0), D24 = case_when(Date == "2014-09-17" ~ 1, TRUE ~ 0),
         D25 = case_when(Date == "2014-10-29" ~ 1, TRUE ~ 0))


stock_market <- stock_market %>%
  mutate(D1 = case_when(Date == "2008-11-25" ~ 1, TRUE ~ 0), D2 = case_when(Date == "2008-12-02" ~ 1, TRUE ~ 0),
         D3 = case_when(Date == "2008-12-16" ~ 1, TRUE ~ 0), D4 = case_when(Date == "2009-01-28" ~ 1, TRUE ~ 0),
         D5 = case_when(Date == "2009-03-18" ~ 1, TRUE ~ 0), D6 = case_when(Date == "2010-08-10" ~ 1, TRUE ~ 0),
         D7 = case_when(Date == "2010-08-27" ~ 1, TRUE ~ 0), D8 = case_when(Date == "2010-09-21" ~ 1, TRUE ~ 0),
         D9 = case_when(Date == "2010-10-15" ~ 1, TRUE ~ 0), D10 = case_when(Date == "2010-11-03" ~ 1, TRUE ~ 0),
         D11 = case_when(Date == "2011-08-09" ~ 1, TRUE ~ 0), D12 = case_when(Date == "2011-08-26" ~ 1, TRUE ~ 0),
         D13 = case_when(Date == "2011-09-21" ~ 1, TRUE ~ 0), D14 = case_when(Date == "2012-08-22" ~ 1, TRUE ~ 0),
         D15 = case_when(Date == "2012-08-31" ~ 1, TRUE ~ 0), D16 = case_when(Date == "2012-09-13" ~ 1, TRUE ~ 0),
         D17 = case_when(Date == "2013-03-20" ~ 1, TRUE ~ 0), D18 = case_when(Date == "2013-05-01" ~ 1, TRUE ~ 0),
         D19 = case_when(Date == "2013-05-22" ~ 1, TRUE ~ 0), D20 = case_when(Date == "2013-06-19" ~ 1, TRUE ~ 0),
         D21 = case_when(Date == "2013-07-11" ~ 1, TRUE ~ 0), D22 = case_when(Date == "2013-10-30" ~ 1, TRUE ~ 0),
         D23 = case_when(Date == "2013-12-18" ~ 1, TRUE ~ 0), D24 = case_when(Date == "2014-09-17" ~ 1, TRUE ~ 0),
         D25 = case_when(Date == "2014-10-29" ~ 1, TRUE ~ 0))

fx <- fx %>%
  mutate(D1 = case_when(Date == "2008-11-25" ~ 1, TRUE ~ 0), D2 = case_when(Date == "2008-12-02" ~ 1, TRUE ~ 0),
         D3 = case_when(Date == "2008-12-16" ~ 1, TRUE ~ 0), D4 = case_when(Date == "2009-01-28" ~ 1, TRUE ~ 0),
         D5 = case_when(Date == "2009-03-18" ~ 1, TRUE ~ 0), D6 = case_when(Date == "2010-08-10" ~ 1, TRUE ~ 0),
         D7 = case_when(Date == "2010-08-27" ~ 1, TRUE ~ 0), D8 = case_when(Date == "2010-09-21" ~ 1, TRUE ~ 0),
         D9 = case_when(Date == "2010-10-15" ~ 1, TRUE ~ 0), D10 = case_when(Date == "2010-11-03" ~ 1, TRUE ~ 0),
         D11 = case_when(Date == "2011-08-09" ~ 1, TRUE ~ 0), D12 = case_when(Date == "2011-08-26" ~ 1, TRUE ~ 0),
         D13 = case_when(Date == "2011-09-21" ~ 1, TRUE ~ 0), D14 = case_when(Date == "2012-08-22" ~ 1, TRUE ~ 0),
         D15 = case_when(Date == "2012-08-31" ~ 1, TRUE ~ 0), D16 = case_when(Date == "2012-09-13" ~ 1, TRUE ~ 0),
         D17 = case_when(Date == "2013-03-20" ~ 1, TRUE ~ 0), D18 = case_when(Date == "2013-05-01" ~ 1, TRUE ~ 0),
         D19 = case_when(Date == "2013-05-22" ~ 1, TRUE ~ 0), D20 = case_when(Date == "2013-06-19" ~ 1, TRUE ~ 0),
         D21 = case_when(Date == "2013-07-11" ~ 1, TRUE ~ 0), D22 = case_when(Date == "2013-10-30" ~ 1, TRUE ~ 0),
         D23 = case_when(Date == "2013-12-18" ~ 1, TRUE ~ 0), D24 = case_when(Date == "2014-09-17" ~ 1, TRUE ~ 0),
         D25 = case_when(Date == "2014-10-29" ~ 1, TRUE ~ 0))



# Sector Real


# Regresiones

reg.yield <- feols(Values ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + D12 + D13 + D14 + D15 + D16 + D17 + D18 + 
                     D19 + D20 + D21 + D22 + D23 + D24 + D25 | Country, data = yields, se = "standard")
summary(reg.yield)


reg.SI <- feols(Values ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + D12 + D13 + D14 + D15 + D16 + D17 + D18 + 
                     D19 + D20 + D21 + D22 + D23 + D24 + D25 | Country, data = stock_market, se = "standard")
summary(reg.SI)

reg.fx <- feols(Values ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + D12 + D13 + D14 + D15 + D16 + D17 + D18 + 
                     D19 + D20 + D21 + D22 + D23 + D24 + D25 | Country, data = fx, se = "standard")
summary(reg.fx)

# a <- plm(Values ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + D12 + D13 + D14 + D15 + D16 + D17 + D18 + 
#            D19 + D20 + D21 + D22 + D23 + D24 + D25 , index = "Country", data = fx, model = "within")
# summary(a)


etable(reg.yield, 
       reg.SI,
       reg.fx,
       title = "",
       float = TRUE,
       fitstat = c("n", "r2"),
       digits = 2,
       tex = TRUE)
