chronshape
================

The aim of `chronshape` is to provide tools for working with date or timestamp ranges, namely:

-   Collapsing,
-   Partitioning,
-   Expanding,
-   Filling the gaps between ranges.

It uses `data.table` in order to speed up the operations.

You can install it *via* `devtools::install_github("arg0naut91/chronshape")`.

However, please note that:

-   Currently, only the `collapse_ranges` function is available. More functions coming up soon;

-   Even the `collapse_ranges` function is in its very early state, and very much prone to bugs, especially when it comes to `POSIXct` formats.

collapse\_ranges
----------------

This function can be useful when we want to collapse several consecutive date or timestamp ranges.

Let's say we have a data frame of different financial actors and their credit ratings for different time ranges.

Now you can see that some of them are actually consecutive. Instead of 6 rows, we'd therefore like to get only 4.

        id rating start_date   end_date
    1 1111     A+ 2014-01-01 2014-12-31
    2 1111     AA 2015-01-01 2015-12-31
    3 1111     AA 2016-01-01 2016-03-01
    4 2222     B- 2017-01-01 2017-01-31
    5 2222     B- 2018-01-01 2018-12-31
    6 2222     B- 2019-01-01 2020-02-01

We can do this as below. A bit of explanation of some of the arguments:

-   `dimension` defines whether we are dealing with dates or timestamps; default to dates;

-   `max_gap` defines what is considered as consecutive. By default, this means no gap whatsoever. If `dimension` is `date`, this means 0 days; in case of `timestamp`, it refers to seconds;

-   `fmt` defines the format. By default, it uses `Y-M-D` for dates, and `Y-M-D H:M:OS` for timestamps. If your ranges are in another format, you need to modify that accordingly.

``` r
df_collapsed <- collapse_ranges(df, 
                                groups = c("id", "rating"), 
                                start_var = "start_date", 
                                end_var = "end_date",
                                max_gap = 0L,
                                fmt = "%Y-%m-%d",
                                dimension = "date")

df_collapsed
```

        id rating start_date   end_date
    1 1111     A+ 2014-01-01 2014-12-31
    2 1111     AA 2015-01-01 2016-03-01
    3 2222     B- 2017-01-01 2017-01-31
    4 2222     B- 2018-01-01 2020-02-01

We can address timestamps in a similar way, only now we need to specify the `dimension` as `timestamp`.

Let's say that now we're dealing with individuals and the way they spend their time:

        id       diary          start_time            end_time
    1 1111     reading 2014-01-01 14:00:00 2014-01-01 14:59:59
    2 1111 watching TV 2014-01-01 15:00:00 2014-01-01 16:29:59
    3 1111 watching TV 2014-01-01 16:30:00 2014-01-01 19:00:00
    4 2222    sleeping 2015-01-01 15:00:00 2015-01-01 15:59:59
    5 2222    sleeping 2015-01-01 17:00:00 2015-01-01 18:59:59
    6 2222    sleeping 2015-01-01 19:00:00 2015-01-01 21:00:00

If we don't specify the format, it will throw a warning:

``` r
df_collapsed <- collapse_ranges(df, 
                                groups = c("id", "diary"), 
                                start_var = "start_time", 
                                end_var = "end_time", 
                                dimension = "timestamp")
```

    Warning in collapse_ranges(df, groups = c("id", "diary"), start_var =
    "start_time", : Dimension 'timestamp' selected but format unchanged. Will
    try to convert to '%Y-%m-%d %H:%M:%OS'.

``` r
df_collapsed
```

        id       diary          start_time            end_time
    1 1111     reading 2014-01-01 14:00:00 2014-01-01 14:59:59
    2 1111 watching TV 2014-01-01 15:00:00 2014-01-01 19:00:00
    3 2222    sleeping 2015-01-01 15:00:00 2015-01-01 15:59:59
    4 2222    sleeping 2015-01-01 17:00:00 2015-01-01 21:00:00
