# vrule_mapping

vrule_mapping

vrule_mapping

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_complex`](https://fdiwg.github.io/vrule/reference/vrule_abstract_complex.md)
-\> `vrule_mapping`

## Public fields

- `ref_data_url`:

  ref data url

- `ref_data`:

  ref data

- `ref_source_term`:

  ref source term

- `ref_target_term`:

  ref target term

- `data_target_term`:

  data target term

- `ref_meta_url`:

  ref meta url

- `ref_meta`:

  ref meta

## Methods

### Public methods

- [`vrule_mapping$new()`](#method-vrule_mapping-new)

- [`vrule_mapping$validate()`](#method-vrule_mapping-validate)

- [`vrule_mapping$clone()`](#method-vrule_mapping-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a mapping-based validation rule

#### Usage

    vrule_mapping$new(
      ref_data_url = NULL,
      ref_source_term = NULL,
      ref_target_term = NULL,
      data_target_term = NULL,
      ref_meta_url = NULL,
      ...
    )

#### Arguments

- `ref_data_url`:

  ref data url

- `ref_source_term`:

  ref source term

- `ref_target_term`:

  ref target term

- `data_target_term`:

  data target term

- `ref_meta_url`:

  ref metadata url

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_mapping$validate(value, row)

#### Arguments

- `value`:

  value

- `row`:

  row

#### Returns

a validation report, object of class
[vrule_report](https://fdiwg.github.io/vrule/reference/vrule_report.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    vrule_mapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
