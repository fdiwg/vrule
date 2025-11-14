# format_spec

format_spec

format_spec

## Methods

### Public methods

- [`format_spec$new()`](#method-format_spec-new)

- [`format_spec$setName()`](#method-format_spec-setName)

- [`format_spec$setURN()`](#method-format_spec-setURN)

- [`format_spec$setTitle()`](#method-format_spec-setTitle)

- [`format_spec$addColumnSpec()`](#method-format_spec-addColumnSpec)

- [`format_spec$getColumnSpecByName()`](#method-format_spec-getColumnSpecByName)

- [`format_spec$getColumnSpecByURN()`](#method-format_spec-getColumnSpecByURN)

- [`format_spec$getColumnSpecByAlias()`](#method-format_spec-getColumnSpecByAlias)

- [`format_spec$getColumnSpec()`](#method-format_spec-getColumnSpec)

- [`format_spec$validateStructure()`](#method-format_spec-validateStructure)

- [`format_spec$validateContent()`](#method-format_spec-validateContent)

- [`format_spec$validate()`](#method-format_spec-validate)

- [`format_spec$display_as_handsontable()`](#method-format_spec-display_as_handsontable)

- [`format_spec$validate_and_display_as_handsontable()`](#method-format_spec-validate_and_display_as_handsontable)

- [`format_spec$standardizeStructure()`](#method-format_spec-standardizeStructure)

- [`format_spec$standardizeContent()`](#method-format_spec-standardizeContent)

- [`format_spec$clone()`](#method-format_spec-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    format_spec$new(json = NULL)

------------------------------------------------------------------------

### Method `setName()`

#### Usage

    format_spec$setName(name)

------------------------------------------------------------------------

### Method `setURN()`

#### Usage

    format_spec$setURN(urn)

------------------------------------------------------------------------

### Method `setTitle()`

#### Usage

    format_spec$setTitle(title)

------------------------------------------------------------------------

### Method `addColumnSpec()`

#### Usage

    format_spec$addColumnSpec(column_spec)

------------------------------------------------------------------------

### Method `getColumnSpecByName()`

#### Usage

    format_spec$getColumnSpecByName(name)

------------------------------------------------------------------------

### Method `getColumnSpecByURN()`

#### Usage

    format_spec$getColumnSpecByURN(urn)

------------------------------------------------------------------------

### Method `getColumnSpecByAlias()`

#### Usage

    format_spec$getColumnSpecByAlias(alias)

------------------------------------------------------------------------

### Method `getColumnSpec()`

#### Usage

    format_spec$getColumnSpec(column)

------------------------------------------------------------------------

### Method `validateStructure()`

#### Usage

    format_spec$validateStructure(data)

------------------------------------------------------------------------

### Method `validateContent()`

#### Usage

    format_spec$validateContent(data, parallel = FALSE, ...)

------------------------------------------------------------------------

### Method `validate()`

#### Usage

    format_spec$validate(data, parallel = FALSE, ...)

------------------------------------------------------------------------

### Method `display_as_handsontable()`

#### Usage

    format_spec$display_as_handsontable(data, report, ...)

------------------------------------------------------------------------

### Method `validate_and_display_as_handsontable()`

#### Usage

    format_spec$validate_and_display_as_handsontable(
      data,
      parallel = parallel,
      read_only = TRUE,
      use_css_classes = FALSE,
      ...
    )

------------------------------------------------------------------------

### Method `standardizeStructure()`

#### Usage

    format_spec$standardizeStructure(data, exclude_unused = TRUE)

------------------------------------------------------------------------

### Method `standardizeContent()`

#### Usage

    format_spec$standardizeContent(data)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    format_spec$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
