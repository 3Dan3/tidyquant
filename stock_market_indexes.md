---
title: "stock indexes"
author: "DanielH"
date: "December 29, 2018"
output:
  html_document:
    code_folding: hide
    keep_md: yes
    theme: readable
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: no
  pdf_document:
    toc: true
    df_print: kable
---


---



## Standard & Poor's 500

Standard & Poor's 500 Index (known commonly as the S&P 500) is a larger and more diverse index than the DJIA. Made up of 500 of the most widely traded stocks in the U.S., it represents about 80% of the total value of U.S. stock markets. 

>In general, the S&P 500 index gives a good indication of movement in the U.S. marketplace as a whole.

Because __the S&P 500 index is__ market weighted (also referred to as __capitalization weighted__), every stock in the index is represented in proportion to its total market capitalization. In other words, if the total market value of all 500 companies in the S&P 500 drops by 10%, the value of the index also drops by 10%.

A 10% movement in all stocks in the DJIA, by contrast, would not necessarily cause a 10% change in the index. Many people consider the market weighting used in the S&P 500 to be a better measure of the market's movement because two portfolios can be more easily compared when changes are measured in percentages rather than dollar amounts.

>The S&P 500 index includes companies in a variety of sectors, including energy, industrials, information technology, healthcare, financials and consumer staples.


```r
# daily returns
sp500_daily_returns <-
  "^GSPC" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")


# daily plot
sp500_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "steelblue") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "S&P 500 daily returns",
       subtitle = "1999-2018",
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

![](stock_market_indexes_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# monthly returns
sp500_monthly_returns <-
  "^GSPC" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")

# monthly plot
sp500_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(), limits = c(-.2, .14)) +
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
       subtitle = "1999-2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
# yearly returns
sp500_yearly_returns <-
  "^GSPC" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")

# plot yearly
sp500_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_continuous(breaks =  seq.int(1999, 2018, 1)) +
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
       subtitle = "1999-2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-2-3.png)<!-- -->


## NASDAQ Composite

Most investors know that the Nasdaq is the exchange on which technology stocks are traded. 

>The Nasdaq Composite Index is a market-capitalization-weighted index of all stocks traded on the Nasdaq stock exchange. 

This index includes some companies that are not based in the U.S.

Although this index is known for its large portion of technology stocks, the Nasdaq Composite also includes stocks from financial, industrial, insurance and transportation industries, among others. 

__The Nasdaq Composite includes large and small firms but, unlike the Dow and the S&P 500, it also includes many speculative companies with small market capitalizations. Consequently, its movement generally indicates the performance of the technology industry as well as investors' attitudes toward more speculative stocks.__


```r
# NASDAQ daily returns
nasdaq_daily_returns <-
  "^IXIC" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")

# plot daily returns
nasdaq_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "darkorange") +
  geom_hline(yintercept = 0) +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# NASDAQ monthly returns
nasdaq_monthly_returns <-
  "^IXIC" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


# plot monthly returns
nasdaq_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "darkorange", size = .03) +
  geom_point(color = "darkorange",size = 1.25) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format(), limits = c(-.3, .28)) +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# NASDAQ yearly returns
nasdaq_yearly_returns <-
  "^IXIC" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb") 


# plot yearly returns
nasdaq_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  scale_x_continuous(breaks =  seq.int(1999, 2018, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(-.48, .94)) +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

## Dow Jones Industrial Average

>The Dow Jones Industrial Average (_DJIA_) is one of the oldest, most well-known and most frequently used indices in the world. It includes the stocks of 30 of the largest and most influential companies in the United States. 

The DJIA is what's known as a price-weighted index. It was originally computed by adding up the per-share price of the stocks of each company in the index and dividing this sum by the number of companies—that's why it's called an average. Unfortunately, it is no longer this simple to calculate. Over the years, stock splits, spin-offs, and other events have resulted in changes in the divisor, making it a very small number (less than 0.2).

The DJIA represents about a quarter of the value of the entire U.S. stock market, but a percent change in the Dow should not be interpreted as a definite indication that the entire market has dropped by the same percent. This is because of the Dow's price-weighted function. The basic problem is that a $1 change in the price of a $120 stock in the index will have a greater effect on the DJIA than a $1 change in the price of a $20 stock, even though the higher-priced stock may have changed by only 0.8% and the other by 5%.

A change in the Dow represents changes in investors' expectations of the earnings and risks of the large companies included in the average. Because the general attitude toward large-cap stocks often differs from the attitude toward small-cap stocks, international stocks or technology stocks, the Dow should not be used to represent sentiment in other areas of the marketplace. On the other hand, because the Dow is made up of some of the most well-known companies in the U.S., large swings in this index generally correspond to the movement of the entire market, although not necessarily on the same scale.



```r
# daily returns
dij_daily_returns <-
  "DJI" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")


# plot daily returns
dij_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "firebrick4", size = .2) +
  geom_hline(yintercept = 0, color = "red") +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-.1, .12)) +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# monthly returns
dij_monthly_returns <-
  "DJI" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


# plot monthly returns
dij_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(size = .03, color = "firebrick4") +
  geom_point(size = 1.25, color = "firebrick4") +
  geom_hline(yintercept = 0) +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
# yearly returns
dji_yearly_returns <-
  "DJI" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")


# plot yearly returns
dji_yearly_returns  %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  scale_x_continuous(breaks =  seq.int(1999, 2018, 1)) +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-4-3.png)<!-- -->



## Russell 2000

The Russell 2000 Index is a small-cap stock market index of the bottom 2,000 stocks in the Russell 3000 Index. The index is maintained by FTSE Russell, a subsidiary of the London Stock Exchange Group

>The Russell 2000 is by far the most common benchmark for mutual funds that identify themselves as "small-cap" 

while the S&P 500 index is used primarily for large capitalization stocks. It is the most widely quoted measure of the overall performance of the small-cap to mid-cap company shares.


```r
# RUSSELL 2000 daily returns
russ2000_daily_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rb")

# plot daily returns
russ2000_daily_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "steelblue1") +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# RUSSELL 2000 monthly returns
russ2000_monthly_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")


# plot monthly returns
russ2000_monthly_returns %>%
  ggplot(aes(date, Rb)) +
  geom_line(color = "steelblue1") +
  geom_point(color = "steelblue1") +
  geom_hline(yintercept = 0, size = 1, color = "red") +
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
       subtitle = "1999 - 2018",
        x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
# RUSSELL 2000 yearly returns
russ2000_yearly_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "1999-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")

# plot R2000 yearly returns
russ2000_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "steelblue1") +
  scale_x_continuous(breaks =  seq.int(1999, 2018, 1)) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-0.4, 0.5)) +
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
       subtitle = "1999 - 2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-5-3.png)<!-- -->


## total returns 2018

Here we want to look at the performance of our four indexes for the year 2018 


```r
# plot
tibble(`stock index` = c("SP500", "NASDAQ", "DJIA", "RUT"),
       ytd = c(sp500_yearly_returns %>% tail(1) %>% .[[2]],
               nasdaq_yearly_returns %>% tail(1) %>% .[[2]],
               dji_yearly_returns %>% tail(1) %>% .[[2]],
               russ2000_yearly_returns %>% tail(1) %>% .[[2]])) %>%
  dmap_at(1, as_factor) %>%
  ggplot(aes(`stock index`, ytd, fill = `stock index`)) +
  geom_bar(stat = "identity", show.legend = F) +
  #scale_fill_tableau(palette = "Tableau 10") +
  scale_fill_manual(values=c("steelblue4", "darkorange", "firebrick", "steelblue1")) +
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

<img src="stock_market_indexes_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

As we can see in 2018 all four indexes had a negative return. The biggest drop, -12% was for the RUSSELL 2000 index. The S&P 500 lost 6.2%, the DJIA 5.63%. Interestingly enough, the NASDAQ, supposedly a pretty volatile market lost less than any other index.

## total returns 1999 - 2018


```r
# return table (tibble)
return_tibble <-
  tribble(~index, ~return,
        "SP500", c(sp500_yearly_returns %>% .[[2]]),
        "NASDAQ", c(nasdaq_yearly_returns %>% .[[2]]),
        "DJIA", c(dji_yearly_returns %>% .[[2]]),
        "RUT", c(russ2000_yearly_returns %>% .[[2]])) %>%
  unnest(return) %>% 
  mutate(year = rep(2018:1999, 4)) %>%
  select(3,1,2) %>%
  spread(year, return)

## ----------------------- long tibble -------------------

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

# -------------------mutate & format tibble ---------------

colnames(long_return_tbl) <- column_names 

# add year column and format values
indexes_return_table <-
  long_return_tbl %>%
  mutate(year = rep(2018:1999)) %>%
  select(5, everything()) %>%
  dmap_at(c(2:5), as.numeric) %>% 
  dmap_if(is_double, percent, accuracy = .01) %>%
  select(1,5,3,2,4) %>%
  rename(" " = year)

# format tibble
indexes_return_table %>%
  mutate(DJIA = cell_spec(DJIA, color = ifelse(DJIA < 0, "red", "black"),
                          format = "html", align = "c"),
         NASDAQ =  cell_spec(NASDAQ, color = ifelse(NASDAQ < 0, "red", "black"),
                          format = "html", align = "c"),
         RUT = cell_spec(RUT, color = ifelse(RUT < 0, "red", "black"),
                          format = "html", align = "c"),
         SP500 = cell_spec(SP500, color = ifelse(SP500 < 0, "red", "black"),
                          format = "html", align = "c")) %>%
  kable(format = "html",  escape = F, align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F) 
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;">   </th>
   <th style="text-align:right;"> SP500 </th>
   <th style="text-align:right;"> NASDAQ </th>
   <th style="text-align:right;"> DJIA </th>
   <th style="text-align:right;"> RUT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2018 </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-6.24%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-3.88%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-5.63%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-12.18%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2017 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">19.42%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">28.24%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">25.08%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">13.14%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2016 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">9.54%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">7.50%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">13.42%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">19.48%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2015 </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-0.73%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">5.73%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-2.23%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-5.71%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2014 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">11.39%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">13.40%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">7.52%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">3.53%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2013 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">29.60%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">38.32%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">26.50%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">37.00%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2012 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">13.41%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">15.91%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">6.65%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">14.63%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2011 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">0.00%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-1.80%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">6.13%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-5.45%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2010 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">12.78%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">16.91%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">11.02%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">25.31%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2009 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">23.45%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">43.89%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">18.82%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">25.22%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2008 </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-38.49%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-40.54%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-33.84%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-34.80%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2007 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">3.53%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">9.81%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">6.43%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-2.75%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">13.62%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">9.52%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">16.29%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">17.00%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">3.00%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">1.37%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-0.61%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">3.32%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2004 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">8.99%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">8.59%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">3.15%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">17.00%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2003 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">26.38%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">50.01%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">23.74%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">45.37%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-23.37%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-31.53%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-15.70%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-21.58%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-13.04%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-21.05%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-5.10%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">1.03%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2000 </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-10.14%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-39.29%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-8.15%</span> </td>
   <td style="text-align:right;"> <span style="     color: red;text-align: c;">-4.20%</span> </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1999 </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">19.64%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">84.29%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">25.18%</span> </td>
   <td style="text-align:right;"> <span style="     color: black;text-align: c;">19.82%</span> </td>
  </tr>
</tbody>
</table>

Looking at the table we can see that the years between 1999 and 2003 show a very high volatility with huge gains and lossess.

We also notice the great recession hitting in year 2008

Finally, last year, 2018, all indexes were negative


## summary statistics, 1999-2008


```r
# transposed tibble
summary_stats_tbl <-
  long_return_tbl %>%
  gather("index", "value") %>% 
  mutate_at(2, as.numeric) %>%
  group_by(index) %>%
  summarize(mean = mean(value),
            stdev = sd(value),
            max = max(value),
            min = min(value)) %>%
  rename(" " = index) %>%
  t() %>%
  as_tibble() %>%
  slice(-1) %>%
  set_colnames(c("DJIA", "NASDAQ", "RUT", "SP500")) %>%
  dmap(as.double)


# add col and highlight extreme values
summary_stats_tbl %>%
  dmap(percent, accuracy = .01) %>%
  mutate(" " = c("mean", "stdev", "max", "min")) %>%
  select(" ", 4,2,1,3) %>%
  kable(escape = F, align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F) %>%
  column_spec(3, bold = F, background = "yellow")
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;">   </th>
   <th style="text-align:right;"> SP500 </th>
   <th style="text-align:right;"> NASDAQ </th>
   <th style="text-align:right;"> DJIA </th>
   <th style="text-align:right;"> RUT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> mean </td>
   <td style="text-align:right;"> 5.14% </td>
   <td style="text-align:right;background-color: yellow;"> 9.77% </td>
   <td style="text-align:right;"> 5.93% </td>
   <td style="text-align:right;"> 7.76% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> stdev </td>
   <td style="text-align:right;"> 17.09% </td>
   <td style="text-align:right;background-color: yellow;"> 30.44% </td>
   <td style="text-align:right;"> 15.32% </td>
   <td style="text-align:right;"> 19.43% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> max </td>
   <td style="text-align:right;"> 29.60% </td>
   <td style="text-align:right;background-color: yellow;"> 84.29% </td>
   <td style="text-align:right;"> 26.50% </td>
   <td style="text-align:right;"> 45.37% </td>
  </tr>
  <tr>
   <td style="text-align:right;"> min </td>
   <td style="text-align:right;"> -38.49% </td>
   <td style="text-align:right;background-color: yellow;"> -40.54% </td>
   <td style="text-align:right;"> -33.84% </td>
   <td style="text-align:right;"> -34.80% </td>
  </tr>
</tbody>
</table>

As we can see the NASDAQ tend to be the market with the highest average return and the highest risk and this makes sense

---

##### extra


```r
# RUSSELL 2000 yearly returns
russ2000_yearly_returns <-
  "^RUT" %>%
  tq_get(get  = "stock.prices",
           from = "1989-01-01",
           to   = "2019-01-01") %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Rb")

# plot R2000 yearly returns
russ2000_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_bar(stat = "identity", fill = "steelblue1") +
  scale_x_continuous(breaks =  seq.int(1989, 2018, 1)) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-0.4, 0.5)) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic"),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Russell 2000 yearly returns",
       subtitle = "1999 - 2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
russ2000_yearly_returns %>%
  ggplot(aes(year(date), Rb)) +
  geom_line(color = "steelblue1", size = 1) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(breaks =  seq.int(1989, 2018, 1)) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(-0.4, 0.5)) +
  theme(plot.title = element_text(size = 20,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0.5,
                                  vjust = 2,
                                  lineheight = 2),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5,
                                     face = "italic"),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Russell 2000 yearly returns",
       subtitle = "1999 - 2018",
       x = "", y = "")
```

![](stock_market_indexes_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

Laso, try to use lollipop,naaa i don-t think so


https://uc-r.github.io/lollipop








