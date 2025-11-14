# vrule_codelist

vrule_codelist

vrule_codelist

## Super classes

`vrule::vrule_abstract` -\> `vrule::vrule_abstract_simple` -\>
`vrule_codelist`

## Methods

### Public methods

- [`vrule_codelist$new()`](#method-vrule_codelist-new)

- [`vrule_codelist$validate()`](#method-vrule_codelist-validate)

- [`vrule_codelist$clone()`](#method-vrule_codelist-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    vrule_codelist$new(
      ref_data_url = NULL,
      ref_data_column = "code",
      ref_data_column_alt = "label",
      allow_labels = FALSE,
      ref_meta_url = NULL
    )

------------------------------------------------------------------------

### Method `validate()`

#### Usage

    vrule_codelist$validate(value, ...)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    vrule_codelist$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
