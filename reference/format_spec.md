# format_spec

format_spec

format_spec

## Public fields

- `name`:

  format name

- `urn`:

  urn

- `title`:

  title

- `type`:

  tpe

- `column_specs`:

  column specifications

## Methods

### Public methods

- [`format_spec$new()`](#method-format_spec-new)

- [`format_spec$setName()`](#method-format_spec-setName)

- [`format_spec$setURN()`](#method-format_spec-setURN)

- [`format_spec$setTitle()`](#method-format_spec-setTitle)

- [`format_spec$setType()`](#method-format_spec-setType)

- [`format_spec$addColumnSpec()`](#method-format_spec-addColumnSpec)

- [`format_spec$getColumnSpecByName()`](#method-format_spec-getColumnSpecByName)

- [`format_spec$getColumnSpecByURN()`](#method-format_spec-getColumnSpecByURN)

- [`format_spec$getColumnSpecByAlias()`](#method-format_spec-getColumnSpecByAlias)

- [`format_spec$getColumnSpec()`](#method-format_spec-getColumnSpec)

- [`format_spec$validateStructure()`](#method-format_spec-validateStructure)

- [`format_spec$validateSeries()`](#method-format_spec-validateSeries)

- [`format_spec$validateContent()`](#method-format_spec-validateContent)

- [`format_spec$validate()`](#method-format_spec-validate)

- [`format_spec$display_as_handsontable()`](#method-format_spec-display_as_handsontable)

- [`format_spec$validate_and_display_as_handsontable()`](#method-format_spec-validate_and_display_as_handsontable)

- [`format_spec$standardizeStructure()`](#method-format_spec-standardizeStructure)

- [`format_spec$standardizeContent()`](#method-format_spec-standardizeContent)

- [`format_spec$createTemplate()`](#method-format_spec-createTemplate)

- [`format_spec$clone()`](#method-format_spec-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a format specification from a JSON list object

#### Usage

    format_spec$new(file = NULL, obj = NULL, json = NULL)

#### Arguments

- `file`:

  a file, either in `JSON` or `YAML` format

- `obj`:

  an object of class [list](https://rdrr.io/r/base/list.html)

- `json`:

  an object of class [list](https://rdrr.io/r/base/list.html).
  Deprecated. Use `obj` instead

------------------------------------------------------------------------

### Method `setName()`

Set name

#### Usage

    format_spec$setName(name)

#### Arguments

- `name`:

  name

------------------------------------------------------------------------

### Method `setURN()`

Set URN

#### Usage

    format_spec$setURN(urn)

#### Arguments

- `urn`:

  urn

------------------------------------------------------------------------

### Method `setTitle()`

Set title

#### Usage

    format_spec$setTitle(title)

#### Arguments

- `title`:

  title

------------------------------------------------------------------------

### Method `setType()`

Set type

#### Usage

    format_spec$setType(type)

#### Arguments

- `type`:

  type

------------------------------------------------------------------------

### Method `addColumnSpec()`

Adds a column specification

#### Usage

    format_spec$addColumnSpec(column_spec)

#### Arguments

- `column_spec`:

  an object of class
  [column_spec](https://fdiwg.github.io/vrule/reference/column_spec.md)

------------------------------------------------------------------------

### Method `getColumnSpecByName()`

Get column specification by name

#### Usage

    format_spec$getColumnSpecByName(name)

#### Arguments

- `name`:

  name

#### Returns

an object of class
[column_spec](https://fdiwg.github.io/vrule/reference/column_spec.md),
or `NULL` if no column specification is found

------------------------------------------------------------------------

### Method `getColumnSpecByURN()`

Get column specification by URN

#### Usage

    format_spec$getColumnSpecByURN(urn)

#### Arguments

- `urn`:

  urn

#### Returns

an object of class
[column_spec](https://fdiwg.github.io/vrule/reference/column_spec.md),
or `NULL` if no column specification is found

------------------------------------------------------------------------

### Method `getColumnSpecByAlias()`

Get column specification by alias

#### Usage

    format_spec$getColumnSpecByAlias(alias)

#### Arguments

- `alias`:

  alias

#### Returns

an object of class
[column_spec](https://fdiwg.github.io/vrule/reference/column_spec.md),
or `NULL` if no column specification is found

------------------------------------------------------------------------

### Method `getColumnSpec()`

Get column specification

#### Usage

    format_spec$getColumnSpec(column)

#### Arguments

- `column`:

  column name or alias

#### Returns

an object of class
[column_spec](https://fdiwg.github.io/vrule/reference/column_spec.md),
or `NULL` if no column specification is found

------------------------------------------------------------------------

### Method `validateStructure()`

Applies data structure validation

#### Usage

    format_spec$validateStructure(data)

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
  or [tibble](https://tibble.tidyverse.org/reference/tibble.html)

#### Returns

an object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
if any data structure validation issue is found (ERROR or WARNING), or
`NULL` if valid

------------------------------------------------------------------------

### Method `validateSeries()`

Applies data series validation

#### Usage

    format_spec$validateSeries(data)

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
  or [tibble](https://tibble.tidyverse.org/reference/tibble.html)

#### Returns

an object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
if any data series validation issue is found (ERROR or WARNING), or
`NULL` if valid

------------------------------------------------------------------------

### Method `validateContent()`

Applies data content validation

#### Usage

    format_spec$validateContent(
      data,
      mode = c("column", "pair"),
      parallel = FALSE,
      ...
    )

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
  or [tibble](https://tibble.tidyverse.org/reference/tibble.html)

- `mode`:

  validation mode, either "column" (default) or "pair"

- `parallel`:

  whether the validation should be run as parallel (default `FALSE`)

- `...`:

  any other arg

#### Returns

an object of class [data.frame](https://rdrr.io/r/base/data.frame.html)

------------------------------------------------------------------------

### Method `validate()`

Applies data validation

#### Usage

    format_spec$validate(data, mode = c("column", "pair"), parallel = FALSE, ...)

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
  or [tibble](https://tibble.tidyverse.org/reference/tibble.html)

- `mode`:

  validation mode, either "column" (default) or "pair"

- `parallel`:

  whether the validation should be run as parallel (default `FALSE`)

- `...`:

  any other arg

#### Returns

an object of class [data.frame](https://rdrr.io/r/base/data.frame.html)

------------------------------------------------------------------------

### Method `display_as_handsontable()`

Display data and validation report as Handsontable

#### Usage

    format_spec$display_as_handsontable(data, report, ...)

#### Arguments

- `data`:

  data

- `report`:

  report

- `...`:

  any other arg

#### Returns

an object of class
[rhandsontable](https://rdrr.io/pkg/rhandsontable/man/rhandsontable.html)

------------------------------------------------------------------------

### Method `validate_and_display_as_handsontable()`

Applies data validation

#### Usage

    format_spec$validate_and_display_as_handsontable(
      data,
      parallel = FALSE,
      read_only = TRUE,
      use_css_classes = FALSE,
      ...
    )

#### Arguments

- `data`:

  object of class [data.frame](https://rdrr.io/r/base/data.frame.html)
  or [tibble](https://tibble.tidyverse.org/reference/tibble.html)

- `parallel`:

  whether the validation should be run as parallel (default `FALSE`)

- `read_only`:

  read only

- `use_css_classes`:

  use css classes

- `...`:

  any other arg

#### Returns

an object of class
[rhandsontable](https://rdrr.io/pkg/rhandsontable/man/rhandsontable.html)

------------------------------------------------------------------------

### Method `standardizeStructure()`

Standardize structure

#### Usage

    format_spec$standardizeStructure(data, exclude_unused = TRUE)

#### Arguments

- `data`:

  data

- `exclude_unused`:

  exclude unsed columns

#### Returns

the standardized data structure

------------------------------------------------------------------------

### Method `standardizeContent()`

Standardize content

#### Usage

    format_spec$standardizeContent(data)

#### Arguments

- `data`:

  data

#### Returns

the standardized data

------------------------------------------------------------------------

### Method `createTemplate()`

Creates template based on the format specification, including a template
structure and eventual reference data files (codelists)

#### Usage

    format_spec$createTemplate(use_aliases = FALSE, dir)

#### Arguments

- `use_aliases`:

  use aliases?

- `dir`:

  directory where to save the template

#### Returns

the path of the output template (as ZIP file)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    format_spec$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
