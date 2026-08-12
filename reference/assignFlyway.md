# Assign flyway as new field

Internal helper function that assigns a new field for flyway name (e.g.,
"Atlantic Flyway") to a tibble containing an existing field for state
abbreviations (e.g., "MA").

## Usage

``` r
assignFlyway(x, state_field_name, flyway_field_name)
```

## Arguments

- x:

  Tibble containing a field with two-letter state abbreviation

- state_field_name:

  Name of field with two-letter state abbreviations

- flyway_field_name:

  Name to use for flyway column assignment

## Author

Abby Walter, <abby_walter@fws.gov>
