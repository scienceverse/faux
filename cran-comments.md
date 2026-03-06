## Resubmission due to failed check

Sorry! I was using code like below to add links to my pkgdown site. This doesn't mess up the help, but might mess up the PDF versions of the manual.

```
\href{../doc/codebook.html}{\code{vignette("codebook", package = "faux")}} 
```

All checks pass with just some NOTES:

* CRAN incoming feasibility": just mentions that faux is archived 
* My name is not misspelled, don't worry!
* "Author field differs from that derived from Authors@R" 
  - I only have Authors@R, not Author in the DESCRIPTION
  - this is described here and suggests it is erroneous: https://stackoverflow.com/questions/60188508/r-package-declaring-author-in-description


## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

❯ checking CRAN incoming feasibility ... [3s/16s] NOTE
  Maintainer: ‘Lisa DeBruine <debruine@gmail.com>’
  
  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2026-03-02 as issues were not corrected
      despite reminders.

❯ checking for future file timestamps ... NOTE
  unable to verify current time

## devtools::check_win_devel()

### R-oldrelease

* using log directory 'd:/RCompile/CRANguest/R-oldrelease/faux.Rcheck'
* using R version 4.4.3 Patched (2026-02-12 r89426 ucrt)
* using platform: x86_64-w64-mingw32
* R was compiled by
    gcc.exe (GCC) 13.3.0
    GNU Fortran (GCC) 13.3.0
* running under: Windows Server 2022 x64 (build 20348)
* using session charset: UTF-8


* checking CRAN incoming feasibility ... [21s] NOTE
Maintainer: 'Lisa DeBruine <debruine@gmail.com>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  DeBruine (27:186)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2026-03-02 as issues were not corrected
    despite reminders.

* checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Lisa DeBruine [aut, cre, cph] (ORCID: <https://orcid.org/0000-0002-7523-5539>), Anna Krystalli [ctb] (ORCID: <https://orcid.org/0000-0002-2378-4915>), Andrew Heiss [ctb] (ORCID: <https://orcid.org/0000-0002-3948-3914>)'
  Authors@R: 'Lisa DeBruine [aut, cre, cph] (<https://orcid.org/0000-0002-7523-5539>), Anna Krystalli [ctb] (<https://orcid.org/0000-0002-2378-4915>), Andrew Heiss [ctb] (<https://orcid.org/0000-0002-3948-3914>)'

Status: 2 NOTEs

### R-devel

* using log directory 'd:/RCompile/CRANguest/R-devel/faux.Rcheck'
* using R Under development (unstable) (2026-03-05 r89546 ucrt)
* using platform: x86_64-w64-mingw32
* R was compiled by
    gcc.exe (GCC) 14.3.0
    GNU Fortran (GCC) 14.3.0
* running under: Windows Server 2022 x64 (build 20348)
* using session charset: UTF-8
* current time: 2026-03-06 11:34:23 UTC


* checking CRAN incoming feasibility ... [14s] NOTE
Maintainer: 'Lisa DeBruine <debruine@gmail.com>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  DeBruine (27:186)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2026-03-02 as issues were not corrected
    despite reminders.
    
Status: 1 NOTE

### R-release

* using log directory 'd:/RCompile/CRANguest/R-release/faux.Rcheck'
* using R version 4.5.2 Patched (2026-02-13 r89426 ucrt)
* using platform: x86_64-w64-mingw32
* R was compiled by
    gcc.exe (GCC) 14.3.0
    GNU Fortran (GCC) 14.3.0
* running under: Windows Server 2022 x64 (build 20348)
* using session charset: UTF-8
* checking for file 'faux/DESCRIPTION' ... OK
* this is package 'faux' version '1.2.4'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... [16s] NOTE
Maintainer: 'Lisa DeBruine <debruine@gmail.com>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  DeBruine (27:186)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2026-03-02 as issues were not corrected
    despite reminders.
    
Status: 1 NOTE