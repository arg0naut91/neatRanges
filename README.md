neatRanges
================

[![Travis build
status](https://travis-ci.org/arg0naut91/chronshape.svg?branch=master)](https://travis-ci.org/arg0naut91/chronshape)
[![codecov](https://codecov.io/gh/arg0naut91/chronshape/branch/master/graph/badge.svg)](https://codecov.io/gh/arg0naut91/chronshape)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The aim of `neatRanges` is to provide tools for working with date or
timestamp ranges, namely:

  - Collapsing,  
  - Partitioning,  
  - Expanding,  
  - Filling the gaps between ranges.

It uses `data.table` in order to speed up the operations.

You can install it *via*
`devtools::install_github("arg0naut91/neatRanges")`.

However, please note that:

  - Currently, `collapse_ranges`, `partition_ranges` (only for years and
    months) and `expand_times`/`expand_dates` functions are available.
    Additional developments coming up soon;

  - Even the before-mentioned functions are in their early state, and
    very much prone to bugs, especially when it comes to `POSIXct`
    formats.

## collapse\_ranges

This function can be useful when we want to collapse several consecutive
date or timestamp ranges.

Let’s say we have a data frame of different financial actors and their
credit ratings for different time ranges.

Now you can see that some of them are actually consecutive. Instead of 6
rows, we’d therefore like to get only 4.

``` 
    id rating start_date   end_date
1 1111     A+ 2014-01-01 2014-12-31
2 1111     AA 2015-01-01 2015-12-31
3 1111     AA 2016-01-01 2016-03-01
4 2222     B- 2017-01-01 2017-01-31
5 2222     B- 2018-01-01 2018-12-31
6 2222     B- 2019-01-01 2020-02-01
```

We can do this as below. A bit of explanation of some of the arguments:

  - `dimension` defines whether we are dealing with dates or timestamps;
    defaults to dates;

  - `max_gap` defines what is considered as consecutive. By default, it
    is set to 0, this means no gap whatsoever. If `dimension` is `date`,
    this means 0 days; in case of `timestamp`, it refers to 0 seconds. A
    gap of 1 day / 1 second would be expressed as `max_gap = 1L`;

  - `fmt` defines the format. By default, it uses `%Y-%m-%d` for dates,
    and `%Y-%m-%d %H:%M:%OS` for timestamps. If your ranges are in
    another format, you need to modify that accordingly.

<!-- end list -->

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

``` 
    id rating start_date   end_date
1 1111     A+ 2014-01-01 2014-12-31
2 1111     AA 2015-01-01 2016-03-01
3 2222     B- 2017-01-01 2017-01-31
4 2222     B- 2018-01-01 2020-02-01
```

We can address timestamps in a similar way, only now we need to specify
the `dimension` as `timestamp`.

Note that here two additional arguments are important:

  - `tz` defines the time zone - by default, it is set to `UTC`;

  - `origin` specifies the origin for indexing/converting the dates; by
    default, it is set to `1970-01-01`.

Let’s say that now we’re dealing with individuals and the way they spend
their time:

``` 
    id       diary          start_time            end_time
1 1111     reading 2014-01-01 14:00:00 2014-01-01 14:59:59
2 1111 watching TV 2014-01-01 15:00:00 2014-01-01 16:29:59
3 1111 watching TV 2014-01-01 16:30:00 2014-01-01 19:00:00
4 2222     working 2015-01-01 15:00:00 2015-01-01 15:59:59
5 2222     working 2015-01-01 17:00:00 2015-01-01 18:59:59
6 2222     working 2015-01-01 19:00:00 2015-01-01 21:00:00
```

If we don’t specify the format, it will throw a warning:

``` r
df_collapsed <- collapse_ranges(df, 
                                groups = c("id", "diary"), 
                                start_var = "start_time", 
                                end_var = "end_time", 
                                dimension = "timestamp",
                                tz = 'UTC',
                                origin = '1970-01-01')
```

    Warning in collapse_ranges(df, groups = c("id", "diary"), start_var =
    "start_time", : Dimension 'timestamp' selected but format unchanged. Will
    try to convert to '%Y-%m-%d %H:%M:%OS' ..

``` r
df_collapsed
```

``` 
    id       diary          start_time            end_time
1 1111     reading 2014-01-01 14:00:00 2014-01-01 14:59:59
2 1111 watching TV 2014-01-01 15:00:00 2014-01-01 19:00:00
3 2222     working 2015-01-01 15:00:00 2015-01-01 15:59:59
4 2222     working 2015-01-01 17:00:00 2015-01-01 21:00:00
```

## partition\_ranges

This function allows users to further split their ranges.

Currently, this allows only splitting of `Date` formats (partitioning by
either year or month).

Consider the following data frame:

``` 
  group      start        end
1     a 2017-05-01 2018-09-01
2     a 2019-04-03 2020-04-03
3     b 2011-03-03 2012-05-03
4     b 2014-05-07 2016-04-02
5     c 2017-02-01 2017-04-05
```

Partitioning by year (default mode) would look like:

``` r
part_by_year <- partition_ranges(df,
                                 start_var = "start",
                                 end_var = "end",
                                 partition_by = "year",
                                 vars_to_keep = "group")

head(part_by_year)
```

``` 
  group      start        end
1     a 2017-05-01 2017-12-31
2     a 2018-01-01 2018-09-01
3     a 2019-04-03 2019-12-31
4     a 2020-01-01 2020-04-03
5     b 2011-03-03 2011-12-31
6     b 2012-01-01 2012-05-03
```

On the other hand, partitioning by month would take the following
format:

``` r
part_by_month <- partition_ranges(df,
                                  start_var = "start",
                                  end_var = "end",
                                  partition_by = "month",
                                  vars_to_keep = "group")

head(part_by_month)
```

``` 
  group      start        end
1     a 2017-05-01 2017-05-31
2     a 2017-06-01 2017-06-30
3     a 2017-07-01 2017-07-31
4     a 2017-08-01 2017-08-31
5     a 2017-09-01 2017-09-30
6     a 2017-10-01 2017-10-31
```

Note that the `vars_to_keep` argument is optional and basically
specifies which columns you’d like to keep.

There is also `fmt` argument that specifies the `Date` format (set to
`%Y-%m-%d` by default).

## expand\_\* family

These are light-weight functions that allow the user to expand
date/timestamp range into a column of elements of the sequence.

Let’s say we have a data frame with `start` and `end` which we would
like to expand:

``` 
    id gender      start        end
1 1111      M 2018-01-01 2018-01-05
2 2222      F 2019-01-01 2019-01-07
3 3333      F 2020-01-01 2020-01-08
```

This can be expanded with calling the `expand_dates` function with the
following arguments:

  - `start_var` and `end_var` as column names of our range;

  - `name` as the future name of our column. This is optional and
    defaults to ‘Expanded’;

  - `vars_to_keep` as indicators of which columns we’d like to keep when
    expanding (optional);

  - `unit` which defines the unit for expansion - defaults to each day
    in the sequence.

Other optional argument is also `fmt` where you can set the format of
your dates - by default, it is `%Y-%m-%d`.

``` r
df_exp <- expand_dates(df,
                       start_var = "start",
                       end_var = "end",
                       name = "exp_seqs",
                       vars_to_keep = c("id", "gender"),
                       unit = "day"
                       )
head(df_exp)
```

``` 
    id gender   exp_seqs
1 1111      M 2018-01-01
2 1111      M 2018-01-02
3 1111      M 2018-01-03
4 1111      M 2018-01-04
5 1111      M 2018-01-05
6 2222      F 2019-01-01
```

You can also tackle *timestamp* formats in a similar way with
`expand_times`.

The arguments are pretty much the same, except that `unit` defaults to
`hour`, `fmt` to `%Y-%m-%d %H:%M:%OS`, and you can set an additional
argument `tz` (time zone; defaults to `UTC`).

``` 
    id gender               start                 end
1 1111      M 2018-01-01 15:00:00 2018-01-01 18:30:00
2 2222      F 2019-01-01 14:00:00 2019-01-01 17:30:00
3 3333      F 2020-01-01 19:00:00 2020-01-02 02:00:00
```

``` r
df_exp <- expand_times(df,
                       start_var = "start",
                       end_var = "end",
                       name = "exp_seqs",
                       vars_to_keep = c("id", "gender"),
                       unit = "hour"
                       )
head(df_exp)
```

``` 
    id gender            exp_seqs
1 1111      M 2018-01-01 15:00:00
2 1111      M 2018-01-01 16:00:00
3 1111      M 2018-01-01 17:00:00
4 1111      M 2018-01-01 18:00:00
5 2222      F 2019-01-01 14:00:00
6 2222      F 2019-01-01 15:00:00
```
