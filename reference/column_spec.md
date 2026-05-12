# column_spec

column_spec

column_spec

## Public fields

- `name`:

  name

- `urn`:

  urn

- `dimension`:

  dimension

- `aliases`:

  aliases

- `required`:

  required

- `rules`:

  rules

## Methods

### Public methods

- [`column_spec$new()`](#method-column_spec-new)

- [`column_spec$setName()`](#method-column_spec-setName)

- [`column_spec$setURN()`](#method-column_spec-setURN)

- [`column_spec$isDimension()`](#method-column_spec-isDimension)

- [`column_spec$setAliases()`](#method-column_spec-setAliases)

- [`column_spec$setRequired()`](#method-column_spec-setRequired)

- [`column_spec$addRule()`](#method-column_spec-addRule)

- [`column_spec$validate()`](#method-column_spec-validate)

- [`column_spec$hasCodelist()`](#method-column_spec-hasCodelist)

- [`column_spec$clone()`](#method-column_spec-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes a column specification

#### Usage

    column_spec$new(json = NULL)

#### Arguments

- `json`:

  object of class [list](https://rdrr.io/r/base/list.html)

------------------------------------------------------------------------

### Method `setName()`

set name

#### Usage

    column_spec$setName(name)

#### Arguments

- `name`:

  name

------------------------------------------------------------------------

### Method `setURN()`

set URN

#### Usage

    column_spec$setURN(urn)

#### Arguments

- `urn`:

  urn

------------------------------------------------------------------------

### Method `isDimension()`

Set if the column is a dimension

#### Usage

    column_spec$isDimension(isDimension)

#### Arguments

- `isDimension`:

  isDimension

------------------------------------------------------------------------

### Method `setAliases()`

Set aliases

#### Usage

    column_spec$setAliases(aliases)

#### Arguments

- `aliases`:

  aliases

------------------------------------------------------------------------

### Method `setRequired()`

Set if the column is required

#### Usage

    column_spec$setRequired(required)

#### Arguments

- `required`:

  required

------------------------------------------------------------------------

### Method `addRule()`

Adds a validation rule

#### Usage

    column_spec$addRule(rule)

#### Arguments

- `rule`:

  rule

------------------------------------------------------------------------

### Method `validate()`

Method to validate column data

#### Usage

    column_spec$validate(values, rows)

#### Arguments

- `values`:

  values

- `rows`:

  rows

#### Returns

a validation report, object of class
[vrule_report](https://fdiwg.github.io/vrule/reference/vrule_report.md)

------------------------------------------------------------------------

### Method `hasCodelist()`

Indicates if column specification includes a codelist validation rule

#### Usage

    column_spec$hasCodelist()

#### Returns

`TRUE` if it includes codelist validation, `FALSE` otherwise

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    column_spec$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
