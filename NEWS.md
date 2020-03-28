neatRanges 0.1.3
================

### Minor changes

- Using `timestamp` as dimension produced unnecessarily warning due to checks; this has been fixed.
  
---

neatRanges 0.1.2
================

### Minor changes

- Unit tests have been rewritten so that they are compatible with the upcoming `stringsAsFactors` default configuration in `data.frame`.

---

neatRanges 0.1.1
================

### General changes

-   The conversion to `Date` or `POSIXct` is not forced anymore & depends on the initial class. This brings significant speed to all functions (provided the initial class is already date/timestamp);

-   There have been some other minor improvements in speed, e.g. applying basic transformations to columns through `data.table`'s `set` instead of `lapply` and `.SD`.  

### Specific changes

- The `collapse_ranges` function got two new arguments, i.e. `startAttr` and `endAttr` - see `?collapse_ranges`;

- Moreover, the core function has been re-written in `Rcpp` for some speed gains.  

---

neatRanges 0.1.0
================

-   First version on CRAN.
