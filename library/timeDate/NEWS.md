# timeDate 4021.106

- fix `whichFormat()` to accommodate a change in R-devel after which
  `as.character(Sys.time())` contains fractional seconds. (`format(Sys.time())`
  doesn't; before this change in R-devel both were dropping the fractional
  seconds). (see timeDate rev 6286)


# timeDate 4021.105

- the list returned by `holidaysNYSE()` was missing the special closing days of
  the New York stock exchange (NYSE). Now it should be complete (though there
  may be ommissions after 2011). This fixes issue #1356 reported by Corwin
  Joy. Thanks to him and Ian E for the insigthful discussion and useful links.

  See also below. Contributions for the other exchanges and corrections are
  welcome.

- `holidaysNYSE()` gets a new argument, `type`, to select what type of the
  exchange's closing days to return. The default is to return all days in the
  requested years when NYSE was closed for whatever reason. Use `type =
  "standard"` and `type = `special` to get the standard holidays and the special
  closings, respectively.

  Returning any closing day by default might be considered a breaking
  change. However, not returning all closing days was perceived as erroneous by
  users (eg issue #1356). In fact, the package itself calculates business days
  by dropping weekends and days returned by `holidayXXXX`.

  Note that `holiday()` returns the actual dates of the public holidays, while
  the corresponding days returned by `holidayXXXX` are the resulting non-weekend
  closing days, if any.

- `holidayTSX()` now correctly calculate Christmas and Boxing day closures when
   Christmas is on Monday.  Fixes the part (2) of issue #1288 reported by Stefan
   Wilhelm (part (1) was fixed in a previous release). The fix is really a patch
   for the specific issue, maybe the same should be done when Christmas is on
   Sunday, for example. Information/contribution on Canadian holidays is
   welcome.

- now `holiday()` accepts also a function or a list of functions for argument
  'Holiday'.

- `timeNthNdayInMonth` could return a value in the following month. Now
  fixed. This is bug #1463 reported with a fix by Manny C. Note that the bug was
  not present for dates in the first day of a month.

- `timeLastNdayInMonth` could return a value in the following month,
  e.g. '1996-06-04' for the last Tuesday in May 1996. Now fixed. The check of
  this function was prompted by the bug report for #1463 (see above) for
  `timeNthNdayInMonth` but the error was different.

- the `data.frame` methods for `kurtosis()` and `skewness()` now set attribute
  `method` as for the other methods and as documented.

- removed `.holidayList()` which had been replaced by `listHolidays()` a long
  time ago and was not exported in recent versions of `timeDate`.

- updated documentation files.

## Deprecation notes

- the `timeDate` method for `cut` has been discouraged in the sources for a long
  time with a recommendation to use \code{window} instead (just replace `cut(x,
  from = xx , to = yy)` with `window(x, start = xx, end = yy)`. The `cut` method
  will be deprecated in the next release and later removed or replaced by a
  method that is consistent with the methods for `cut` in base R.
  

# timeDate 4021.104

- new maintainer: Georgi N. Boshnakov.

- updated DESCRIPTION with links and moved all `Depends:` to `Imports:`.

- removed the line `LazyData: yes` from DESCRIPTION to fix the NOTE on CRAN.

- added the new US holiday, Juneteenth National Independence Day. Fixes #6755 by
  Ian E (ene100).

- `holidayTSX()` now includes the Labour Day. Fixes part (1) of issue #1288
  reported by Stefan Wilhelm.

- created a first version of `_pkgdown.yml` for more organised view of the large
  number of objects in the package. Unpack the tarball and run
  `pkgdown::build_site()` to build the site locally. Don't know if this could
  work directly off the R-forge repository.
  

# timeDate 3043.102 and older versions

  See file `ChangeLog` for changes before 4021.104.
