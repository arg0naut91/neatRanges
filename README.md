neatRanges
================

[![Travis build
status](https://travis-ci.org/arg0naut91/neatRanges.svg?branch=master)](https://travis-ci.org/arg0naut91/neatRanges)
[![codecov](https://codecov.io/gh/arg0naut91/chronshape/branch/master/graph/badge.svg)](https://codecov.io/gh/arg0naut91/chronshape)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The aim of `neatRanges` is to provide tools for working with date or
timestamp ranges, namely:

  - Collapsing,  
  - Partitioning,  
  - Combining,  
  - Expanding,  
  - Filling the gaps between ranges.

It uses `data.table` in order to speed up the operations.

You can install it *via*
`devtools::install_github("arg0naut91/neatRanges")`.

Below is a quick overview of all functions, followed by a detailed
description of each one of them.

| Function          | Description                                           | Supports dates   | Supports timestamps |
| ----------------- | ----------------------------------------------------- | ---------------- | ------------------- |
| collapse\_ranges  | Collapse consecutive ranges                           | Yes              | Yes                 |
| partition\_ranges | Split a range into multiple ranges (by month or year) | Months and years | No                  |
| expand\_\*        | Expand a range                                        | Yes              | Yes                 |
| combine\_ranges   | Combine ranges from multiple tables                   | Yes              | Yes                 |
| fill\_ranges      | Add missing ranges                                    | Yes              | Yes                 |

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

## combine\_ranges

This function is essentially a wrapper around `collapse_ranges` that
allows you to combine ranges scattered around different tables into one
table with non-redundant splits & rows.

Let’s say we have the following data frames `df1`, `df2` and `df3`:

``` 
       start        end group infoScores
1 2010-01-01 2010-08-05     a          0
2 2012-06-01 2013-03-03     a          3
3 2014-10-15 2015-01-01     b          2
```

``` 
         end group      start score
1 2012-04-05     b 2009-01-15     8
2 2014-06-09     a 2012-07-08     2
3 2009-02-01     b 2008-01-01     3
```

``` 
         end group      start scoreInfo
1 2011-04-05     a 2010-02-03         1
2 2014-12-09     b 2014-07-08         2
3 2009-02-01     c 2008-01-01         3
```

Note the column names: the range names (`start` and `end`) as well as
the grouping variables (`group`) are the same across all data frames.
This is a requirement for the function to run properly. However - as you
can see from the example -, there is no need for them to be in the same
order.

The arguments are almost identical to those of `collapse_ranges`, except
that at the beginning it is possible to pass as many data frames as you
would like to combine. They need to be enumerated and not represented in
a list or vector.

For instance, we can combine the above data frames as follows (we don’t
need to specify the `dimension` as it defaults to `date`):

``` r
df <- combine_ranges(df1, df2, df3, start_var = "start", end_var = "end", groups = "group")

df
```

``` 
  group      start        end
1     a 2010-01-01 2011-04-05
2     a 2012-06-01 2014-06-09
3     b 2008-01-01 2012-04-05
4     b 2014-07-08 2015-01-01
5     c 2008-01-01 2009-02-01
```

As you have probably noticed, the output contains only the relevant
columns: range variables & grouping variables.

## fill\_ranges

The function adds missing ranges to a table. It supports both `Date` and
`POSIXct` formats; by default, it assumes the range columns are dates.

``` 
  group      start        end cost score
1     a 2007-01-01 2008-02-05  143    99
2     a 2010-06-02 2013-04-05  144    33
3     b 2009-04-05 2009-06-03  105    44
4     b 2012-08-01 2013-02-17  153    22
5     b 2019-03-19 2021-04-21  124    33
6     c 2020-01-05 2020-01-09  105   105
7     d 2014-01-01 2014-12-31  153   153
8     d 2015-01-01 2016-12-31  124   124
```

The arguments are almost identical to those of `collapse_ranges`.

By `dimension` argument you indicate whether your data frame contains
`date` or `timestamp` values (defaults to dates).

The output based on the above data frame:

``` r
df <- fill_ranges(df, start_var = "start", end_var = "end", groups = "group")

df
```

``` 
   group      start        end cost score
1      a 2007-01-01 2008-02-05  143    99
2      a 2008-02-06 2010-06-01   NA    NA
3      a 2010-06-02 2013-04-05  144    33
4      b 2009-04-05 2009-06-03  105    44
5      b 2009-06-04 2012-07-31   NA    NA
6      b 2012-08-01 2013-02-17  153    22
7      b 2013-02-18 2019-03-18   NA    NA
8      b 2019-03-19 2021-04-21  124    33
9      c 2020-01-05 2020-01-09  105   105
10     d 2014-01-01 2014-12-31  153   153
11     d 2015-01-01 2016-12-31  124   124
```

As you can see, all the original variables are returned.

The rows corresponding to added ranges will by default have `NA` in the
columns that are not grouping variables (in the above case `cost` &
`score` variables).

You can change this behaviour by adjusting the `fill` parameter, like
below:

``` r
df <- fill_ranges(df, 
                  groups = "group", 
                  start_var = "start", 
                  end_var = "end", 
                  fill = "cost = 0, score = Missing"
                  )

df
```

``` 
   group      start        end cost   score
1      a 2007-01-01 2008-02-05  143      99
2      a 2008-02-06 2010-06-01    0 Missing
3      a 2010-06-02 2013-04-05  144      33
4      b 2009-04-05 2009-06-03  105      44
5      b 2009-06-04 2012-07-31    0 Missing
6      b 2012-08-01 2013-02-17  153      22
7      b 2013-02-18 2019-03-18    0 Missing
8      b 2019-03-19 2021-04-21  124      33
9      c 2020-01-05 2020-01-09  105     105
10     d 2014-01-01 2014-12-31  153     153
11     d 2015-01-01 2016-12-31  124     124
```

Note that this feature is somewhat experimental & currently the columns
to be filled are - for safety reasons - automatically converted to
`character`. Additional checks & more flexibility will be added in the
future.
