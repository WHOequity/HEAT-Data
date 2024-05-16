# HEAT Data

## General

## Translations

The current prototype introduces three functions,

- `translate_mapping()`
- `translate_data()`
- `translate_subset()`

### `translate_mapping()`

This function reads in our translations dictionary, modifies column names, and
returns a tibble we can use to modify the raw heat data.

### `translate_data()`

This function uses a tibble translation dictionary (the result of 
`translate_mapping()` by default) to add extra translated columns to the raw
heat data.

Please load the package and try the following,

```R
dplyr::glimpse(translate_data())
```

You will notice there are three additional columns "setting_fr", "setting_pt",
and "setting_es". These are translations of the standard "setting" column.

### `translate_subset()`

This function is a work in progress. The function will be used to select a 
subset of the heat data based on a language abbreviation. That is, if the 
abbreviation is "es" then the columns will be pared down to only those columns
translated to Spanish (and any columns we do not translate, such as numeric 
columns).

Because selecting columns based on abbreviation is trickier than I first
imagined I have paused for now.

