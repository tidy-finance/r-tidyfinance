# Generate Pseudo CCM Links

Returns a pseudo CRSP-Compustat linking table with the same column
layout as
[`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md).
Every pseudo `permno` is linked to its corresponding `gvkey` for the
full sample horizon.

## Usage

``` r
download_data_pseudo_ccm_links(
  n_assets = 1000L,
  seed = 1234L,
  linktype = c("LU", "LC"),
  linkprim = c("P", "C")
)
```

## Arguments

- n_assets:

  Integer. Number of pseudo firms in the universe. Defaults to `1000`.

- seed:

  Integer. Random seed controlling the pseudo identifier universe;
  defaults to `1234`.

- linktype:

  Accepted for API compatibility with
  [`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md);
  ignored for pseudo data.

- linkprim:

  Accepted for API compatibility with
  [`download_data_wrds_ccm_links()`](https://package.tidy-finance.org/reference/download_data_wrds_ccm_links.md);
  ignored for pseudo data.

## Value

A tibble with columns `permno`, `gvkey`, `linkdt`, and `linkenddt`, one
row per pseudo firm.

## See also

Other pseudo functions:
[`download_data_pseudo_compustat()`](https://package.tidy-finance.org/reference/download_data_pseudo_compustat.md),
[`download_data_pseudo_crsp()`](https://package.tidy-finance.org/reference/download_data_pseudo_crsp.md)

## Examples

``` r
download_data_pseudo_ccm_links(n_assets = 10)
#> # A tibble: 10 × 4
#>    permno gvkey  linkdt     linkenddt 
#>     <int> <chr>  <date>     <date>    
#>  1      1 010001 1925-12-31 2099-12-31
#>  2      2 010002 1925-12-31 2099-12-31
#>  3      3 010003 1925-12-31 2099-12-31
#>  4      4 010004 1925-12-31 2099-12-31
#>  5      5 010005 1925-12-31 2099-12-31
#>  6      6 010006 1925-12-31 2099-12-31
#>  7      7 010007 1925-12-31 2099-12-31
#>  8      8 010008 1925-12-31 2099-12-31
#>  9      9 010009 1925-12-31 2099-12-31
#> 10     10 010010 1925-12-31 2099-12-31
```
