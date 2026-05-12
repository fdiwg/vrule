# vrule_raw_codelist

vrule_raw_codelist

vrule_raw_codelist

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\> `vrule_raw_codelist`

## Public fields

- `ref_values`:

  ref values

## Methods

### Public methods

- [`vrule_raw_codelist$new()`](#method-vrule_raw_codelist-new)

- [`vrule_raw_codelist$validate()`](#method-vrule_raw_codelist-validate)

- [`vrule_raw_codelist$clone()`](#method-vrule_raw_codelist-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a raw codelist validation rule

#### Usage

    vrule_raw_codelist$new(ref_values = NULL, ...)

#### Arguments

- `ref_values`:

  the raw codelists values

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates value with a raw codelist validation rule

#### Usage

    vrule_raw_codelist$validate(value, ...)

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

    vrule_raw_codelist$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
