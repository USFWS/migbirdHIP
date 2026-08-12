# Find non-UTF-8 glyphs/characters in a field

The internal `glyphFinder` function pulls non-UTF-8 characters in a
field.

## Usage

``` r
glyphFinder(raw_data, field)
```

## Arguments

- raw_data:

  The tibble created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

- field:

  Field that should be checked for non-UTF-8 characters. One of the
  fields from the following list may be supplied:

  - title, firstname, middle, lastname, suffix, address, city, state,
    zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag,
    dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes,
    band_tailed_pigeon, brant, seaducks, registration_yr, email

## See also

[`shiftFinder()`](https://usfws.github.io/migbirdHIP/reference/shiftFinder.md)

Other glyph functions:
[`glyphCheck()`](https://usfws.github.io/migbirdHIP/reference/glyphCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
