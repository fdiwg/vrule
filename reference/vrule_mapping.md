# vrule_mapping

vrule_mapping

vrule_mapping

## Super classes

`vrule::vrule_abstract` -\> `vrule::vrule_abstract_simple` -\>
`vrule_mapping`

## Methods

### Public methods

- [`vrule_mapping$new()`](#method-vrule_mapping-new)

- [`vrule_mapping$validate()`](#method-vrule_mapping-validate)

- [`vrule_mapping$clone()`](#method-vrule_mapping-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    vrule_mapping$new(
      ref_data_url = NULL,
      ref_source_term = NULL,
      ref_target_term = NULL,
      ref_meta_url = NULL
    )

------------------------------------------------------------------------

### Method `validate()`

#### Usage

    vrule_mapping$validate(value, target, ...)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    vrule_mapping$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
