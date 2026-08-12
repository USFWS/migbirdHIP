# Find non-UTF-8 glyphs/characters in any field

Pull and view any non-UTF-8 characters in the raw data. This function
iterates
[`glyphFinder`](https://usfws.github.io/migbirdHIP/reference/glyphFinder.md)
over the entire tibble.

## Usage

``` r
glyphCheck(raw_data)
```

## Arguments

- raw_data:

  The object created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## See also

[`shiftCheck()`](https://usfws.github.io/migbirdHIP/reference/shiftCheck.md)

Other glyph functions:
[`glyphFinder()`](https://usfws.github.io/migbirdHIP/reference/glyphFinder.md)

## Author

Abby Walter, <abby_walter@fws.gov>
