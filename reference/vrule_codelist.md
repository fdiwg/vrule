# vrule_codelist

vrule_codelist

vrule_codelist

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\> `vrule_codelist`

## Public fields

- `ref_data_url`:

  ref data url

- `ref_data`:

  ref data

- `ref_data_column`:

  ref data column

- `ref_data_column_alt`:

  ref data alternate column

- `ref_meta_url`:

  ref metadata url

- `ref_meta`:

  ref metadata

- `allow_labels`:

  allow labels?

## Methods

### Public methods

- [`vrule_codelist$new()`](#method-vrule_codelist-new)

- [`vrule_codelist$validate()`](#method-vrule_codelist-validate)

- [`vrule_codelist$clone()`](#method-vrule_codelist-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a codelist validation rule

#### Usage

    vrule_codelist$new(
      ref_data_url = NULL,
      ref_data_column = "code",
      ref_data_column_alt = "label",
      allow_labels = FALSE,
      ref_meta_url = NULL,
      ...
    )

#### Arguments

- `ref_data_url`:

  ref data url

- `ref_data_column`:

  ref data column

- `ref_data_column_alt`:

  ref data alternate column

- `allow_labels`:

  Allow labels?

- `ref_meta_url`:

  ref metadata url

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates value with a codelist validation rule

#### Usage

    vrule_codelist$validate(value, ...)

#### Arguments

- `value`:

  value

- `...`:

  any other args

#### Returns

a validation report, object of class
[vrule_report](https://fdiwg.github.io/vrule/reference/vrule_report.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    vrule_codelist$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
