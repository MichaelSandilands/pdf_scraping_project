Advanced PDF Scraping
================

## Project Details

This project is a complicated PDF scraping project I undertook to
advance my data cleaning skills.

The data we’re interested in is stored in PDF tables.

The documents currently look like this:

![](00_images/data_before.png)

The data now looks like this:

``` r
readr::read_csv("00_data/crude_imports_by_month_year_country_type_of_crude.csv") %>% 
    head(10) %>% 
    knitr::kable()
```

    ## Rows: 9177 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): region, country, type_of_crude
    ## dbl  (4): volume, total_value, cif_price, prop_of_total_imports
    ## date (1): month_year
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

| month_year | region      | country      | type_of_crude       | volume | total_value | cif_price | prop_of_total_imports |
|:-----------|:------------|:-------------|:--------------------|-------:|------------:|----------:|----------------------:|
| 2005-01-01 | Middle East | Iran         | Other Iran Crude    |   4749 |      164384 |     34.62 |                0.0123 |
| 2005-01-01 | Middle East | Iran         | Iranian Heavy       |  13074 |      456183 |     34.89 |                0.0339 |
| 2005-01-01 | Middle East | Iran         | Iranian Light       |   7033 |      266102 |     37.83 |                0.0182 |
| 2005-01-01 | Middle East | Iraq         | Basrah Light        |   3111 |       89518 |     28.77 |                0.0081 |
| 2005-01-01 | Middle East | Kuwait       | Kuwait Blend        |   5377 |      193829 |     36.05 |                0.0139 |
| 2005-01-01 | Middle East | Saudi Arabia | Arab Light          |  24297 |      965545 |     39.74 |                0.0629 |
| 2005-01-01 | Middle East | Saudi Arabia | Arab Medium         |   3984 |      137463 |     34.51 |                0.0103 |
| 2005-01-01 | Middle East | Saudi Arabia | Arab Heavy          |   6613 |      211725 |     32.02 |                0.0171 |
| 2005-01-01 | Middle East | Saudi Arabia | Berri (Extra Light) |   2846 |      110202 |     38.72 |                0.0074 |
| 2005-01-01 | Middle East | Syria        | Souedie             |   1382 |       39738 |     28.76 |                0.0036 |

What made this project especially challenging is the inconsistent
presentation of data from PDF - PDF and the very large amount of missing
values in many of the cells.

But with advanced use of regular expressions and tidyverse functions the
data is extracted to a very high degree of accuracy.
