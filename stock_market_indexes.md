stock indexes
================
DanielH
December 29, 2018

-   [the Standard & Poor's 500 (S&P 500)](#the-standard-poors-500-sp-500)
-   [nasdaq composite (NASDAQ)](#nasdaq-composite-nasdaq)
-   [dow jones industrial average (DJIA)](#dow-jones-industrial-average-djia)
-   [Russell 2000 (RUT)](#russell-2000-rut)
-   [total returns 2009 - 2018](#total-returns-2009---2018)

``` r
library(tidyverse)
library(purrrlyr)
library(tidyquant)
library(huxtable)
library(gridExtra)
library(ggthemes)
library(ggthemes)
library(scales)
library(knitr)

# set theme
theme_set(theme_minimal())
```

the Standard & Poor's 500 (S&P 500)
-----------------------------------

<https://www.investopedia.com/terms/s/sp500.asp>

Standard & Poor's 500 Index (known commonly as the S&P 500) is a larger and more diverse index than the DJIA. Made up of 500 of the most widely traded stocks in the U.S., it represents about 80% of the total value of U.S. stock markets.

> In general, the S&P 500 index gives a good indication of movement in the U.S. marketplace as a whole.

Because **the S&P 500 index is** market weighted (also referred to as **capitalization weighted**), every stock in the index is represented in proportion to its total market capitalization. In other words, if the total market value of all 500 companies in the S&P 500 drops by 10%, the value of the index also drops by 10%.

A 10% movement in all stocks in the DJIA, by contrast, would not necessarily cause a 10% change in the index. Many people consider the market weighting used in the S&P 500 to be a better measure of the market's movement because two portfolios can be more easily compared when changes are measured in percentages rather than dollar amounts.

> The S&P 500 index includes companies in a variety of sectors, including energy, industrials, information technology, healthcare, financials and consumer staples.

``` r
# daily returns
sp500_daily_returns <-
  "^GSPC" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")


# daily plot
sp500_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "darkgreen") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "S&P 500 daily returns",
       subtitle = "2005-2018",
       x = "", y = "") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic"))
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# monthly returns
sp500_monthly_returns <-
  "^GSPC" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")

# monthly plot
sp500_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(), limits = c(-.15, .1)) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "S&P 500 monthly returns",
       subtitle = "2005-2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
# yearly returns
sp500_yearly_returns <-
  "^GSPC" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")

# plot yearly
sp500_yearly_returns %>%
  ggplot(aes(year( date), Rb)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  scale_x_continuous(breaks =  seq.int(2005, 2018, 1)) +
  scale_y_continuous(labels = percent_format()) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "S&P 500 yearly returns",
       subtitle = "2005-2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-2-3.png)

nasdaq composite (NASDAQ)
-------------------------

<https://www.investopedia.com/terms/n/nasdaqcompositeindex.asp>

Most investors know that the Nasdaq is the exchange on which technology stocks are traded.

> The Nasdaq Composite Index is a market-capitalization-weighted index of all stocks traded on the Nasdaq stock exchange.

This index includes some companies that are not based in the U.S.

Although this index is known for its large portion of technology stocks, the Nasdaq Composite also includes stocks from financial, industrial, insurance and transportation industries, among others.

**The Nasdaq Composite includes large and small firms but, unlike the Dow and the S&P 500, it also includes many speculative companies with small market capitalizations. Consequently, its movement generally indicates the performance of the technology industry as well as investors' attitudes toward more speculative stocks.**

``` r
# NASDAQ daily returns
nasdaq_daily_returns <-
  "^IXIC" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")

# plot daily returns
nasdaq_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "goldenrod4") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format()) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "NASDAQ daily returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# NASDAQ monthly returns
nasdaq_monthly_returns <-
  "^IXIC" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


# plot monthly returns
nasdaq_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "goldenrod4", size = .03) +
  geom_point(color = "goldenrod4",size = 1.25) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format(), limits = c(-.2, .16)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "NASDAQ monthly returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# NASDAQ yearly returns
nasdaq_yearly_returns <-
  "^IXIC" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb") 


# plot yearly returns
nasdaq_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "goldenrod4") +
  scale_x_continuous(breaks =  seq.int(2005, 2018, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(-.48, .48)) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "NASDAQ yearly returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
# plot growth
nasdaq_daily_returns %>%
  mutate(cumul = cumsum(Rb)) %>%
  ggplot(aes(date, cumul)) +
  geom_line(color = "goldenrod4") +
  geom_smooth(color =  "black", size = .75) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "NASDAQ performance",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-3-4.png)

dow jones industrial average (DJIA)
-----------------------------------

<https://www.investopedia.com/terms/d/djia.asp>

> The Dow Jones Industrial Average (*DJIA*) is one of the oldest, most well-known and most frequently used indices in the world. It includes the stocks of 30 of the largest and most influential companies in the United States.

The DJIA is what's known as a price-weighted index. It was originally computed by adding up the per-share price of the stocks of each company in the index and dividing this sum by the number of companies—that's why it's called an average. Unfortunately, it is no longer this simple to calculate. Over the years, stock splits, spin-offs, and other events have resulted in changes in the divisor, making it a very small number (less than 0.2).

The DJIA represents about a quarter of the value of the entire U.S. stock market, but a percent change in the Dow should not be interpreted as a definite indication that the entire market has dropped by the same percent. This is because of the Dow's price-weighted function. The basic problem is that a $1 change in the price of a $120 stock in the index will have a greater effect on the DJIA than a $1 change in the price of a $20 stock, even though the higher-priced stock may have changed by only 0.8% and the other by 5%.

A change in the Dow represents changes in investors' expectations of the earnings and risks of the large companies included in the average. Because the general attitude toward large-cap stocks often differs from the attitude toward small-cap stocks, international stocks or technology stocks, the Dow should not be used to represent sentiment in other areas of the marketplace. On the other hand, because the Dow is made up of some of the most well-known companies in the U.S., large swings in this index generally correspond to the movement of the entire market, although not necessarily on the same scale.

<https://www.macrotrends.net/2481/stock-market-performance-by-president>

``` r
# daily returns
dij_daily_returns <-
  "DJI" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")


# plot daily returns
dij_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "steelblue", size = .2) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-.10, .10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "DJIA daily returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# monthly returns
dij_monthly_returns <-
  "DJI" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


# plot monthly returns
dij_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(size = .03, color = "steelblue") +
  geom_point(size = 1.25, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red",
             size = .7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format()) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "DJIA monthly returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# yearly returns
dji_yearly_returns <-
  "DJI" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")


# plot yearly returns
dji_yearly_returns  %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks =  seq.int(2005, 2018, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(-0.4, 0.3),
                     breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "DJIA yearly returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-4-3.png)

Russell 2000 (RUT)
------------------

<https://www.investopedia.com/terms/r/russell2000.asp>

The Russell 2000 Index is a small-cap stock market index of the bottom 2,000 stocks in the Russell 3000 Index. The index is maintained by FTSE Russell, a subsidiary of the London Stock Exchange Group

> The Russell 2000 is by far the most common benchmark for mutual funds that identify themselves as "small-cap"

while the S&P 500 index is used primarily for large capitalization stocks. It is the most widely quoted measure of the overall performance of the small-cap to mid-cap company shares.

``` r
# RUSSELL 2000 daily returns
russ2000_daily_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")

# plot daily returns
russ2000_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "firebrick") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-.15, .10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "Russell 2000 daily returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# RUSSELL 2000 monthly returns
russ2000_monthly_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


# plot monthly returns
russ2000_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "firebrick") +
  geom_point(color = "firebrick") +
  geom_hline(yintercept = 0, size = 1) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "Russell 2000 monthly returns",
       subtitle = "2005 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
# RUSSELL 2000 yearly returns
russ2000_yearly_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "2005-01-01",
           to   = today()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")

# plot R2000 yearly returns
russ2000_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  scale_x_continuous(breaks =  seq.int(2005, 2018, 1)) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-0.4, 0.4)) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic")) +
  labs(title = "Russell 2000 yearly returns",
       subtitle = "2005 - 2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-5-3.png)

total returns 2009 - 2018
-------------------------

``` r
tibble(`stock index` = c("S&P 500", "NASDAQ", "DJIA", "Russell 2000"),
       ytd = c(sp500_yearly_returns %>% tail(1) %>% .[[2]],
               nasdaq_yearly_returns %>% tail(1) %>% .[[2]],
               dji_yearly_returns %>% tail(1) %>% .[[2]],
               russ2000_yearly_returns %>% tail(1) %>% .[[2]])) %>%
  mutate(ytd = percent(ytd, accuracy = .1)) %>% 
  spread(`stock index`, ytd) %>%
  select(4,2,1,3) %>%
  kable()
```

| S&P 500 | NASDAQ | DJIA  | Russell 2000 |
|:--------|:-------|:------|:-------------|
| -6.2%   | -3.9%  | -5.6% | -12.2%       |

``` r
#plot
tibble(`stock index` = c("S&P 500", "NASDAQ", "DJIA", "Russell 2000"),
       ytd = c(sp500_yearly_returns %>% tail(1) %>% .[[2]],
               nasdaq_yearly_returns %>% tail(1) %>% .[[2]],
               dji_yearly_returns %>% tail(1) %>% .[[2]],
               russ2000_yearly_returns %>% tail(1) %>% .[[2]])) %>%
  dmap_at(1, as_factor) %>%
  ggplot(aes(`stock index`, ytd, fill = `stock index`)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_tableau(palette = "Tableau 10") +
  scale_y_continuous(labels = percent_format()) +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 12,
                                     hjust = 0.5,
                                     face = "italic"),
        axis.text.x = element_text(face = "bold",
                                   color = "black",
                                   size = 10)) +
  labs(title = "Major Markets 2018 Performance ",
       subtitle = "all major indexes show negative returns",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# return tibble
return_tibble <-
  tribble(~index, ~return,
        "sp500", c(sp500_yearly_returns %>% .[[2]]),
        "nasdaq", c(nasdaq_yearly_returns %>% .[[2]]),
        "dowjones", c(dji_yearly_returns %>% .[[2]]),
        "russ2000", c(russ2000_yearly_returns %>% .[[2]])) %>%
  unnest(return) %>% 
  mutate(year = rep(2018:2005, 4)) %>%
  select(3,1,2) %>%
  spread(year, return)

##  long format tibble

# get names
column_names <-
  return_tibble %>%
  pull(index)

# long tibble
long_return_tbl <-
  return_tibble %>%
  t %>%
  as_tibble() %>%
  slice(-1)

colnames(long_return_tbl) <- column_names
  
# add year column and format values
indexes_return_table <-
  long_return_tbl %>%
  mutate(year = rep(2018:2005)) %>%
  select(5, everything()) %>%
  dmap_at(c(2:5), as.numeric) %>% 
  dmap_if(is_double, percent, accuracy = .1) 


indexes_return_table %>%
  kable()
```

|  year| dowjones | nasdaq | russ2000 | sp500  |
|-----:|:---------|:-------|:---------|:-------|
|  2018| -5.6%    | -3.9%  | -12.2%   | -6.2%  |
|  2017| 25.1%    | 28.2%  | 13.1%    | 19.4%  |
|  2016| 13.4%    | 7.5%   | 19.5%    | 9.5%   |
|  2015| -2.2%    | 5.7%   | -5.7%    | -0.7%  |
|  2014| 7.5%     | 13.4%  | 3.5%     | 11.4%  |
|  2013| 26.5%    | 38.3%  | 37.0%    | 29.6%  |
|  2012| 6.7%     | 15.9%  | 14.6%    | 13.4%  |
|  2011| 6.1%     | -1.8%  | -5.5%    | 0.0%   |
|  2010| 11.0%    | 16.9%  | 25.3%    | 12.8%  |
|  2009| 18.8%    | 43.9%  | 25.2%    | 23.5%  |
|  2008| -33.8%   | -40.5% | -34.8%   | -38.5% |
|  2007| 6.4%     | 9.8%   | -2.7%    | 3.5%   |
|  2006| 16.3%    | 9.5%   | 17.0%    | 13.6%  |
|  2005| -0.1%    | 2.5%   | 5.1%     | 3.8%   |

### format table

``` r
indexes_return_table2 <-
  long_return_tbl %>%
  mutate(year = rep(2018:2005)) %>%
  select(5, everything()) %>%
  dmap_at(c(2:5), as.numeric)



indexes_return_table2 %>%
  select(-1) %>% 
  as_hux() %>%
  map_text_color(by_quantiles(c(0.1, 0.9), c("red", "black", "green3")))
```

    ## Warning in knit_print.huxtable(x, ...): Unrecognized output format "markdown". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "md" or "screen".

-0.0563  -0.0388 -0.122  -0.0624  
0.251   0.282  0.131  0.194   
0.134   0.075  0.195  0.0954  
-0.0223  0.0573 -0.0571 -0.00727 
0.0752  0.134  0.0353 0.114   
0.265   0.383  0.37   0.296   
0.0665  0.159  0.146  0.134   
0.0613  -0.018  -0.0545 -3.18e-05
0.11    0.169  0.253  0.128   
0.188   0.439  0.252  0.235   
-0.338   -0.405  -0.348  -0.385   
0.0643  0.0981 -0.0275 0.0353  
0.163   0.0952 0.17   0.136   
-0.00111 0.0247 0.0512 0.0384  

Column names: dowjones, nasdaq, russ2000, sp500
