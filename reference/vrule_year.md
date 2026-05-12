# vrule_year

vrule_year

vrule_year

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\>
[`vrule::vrule_datatype`](https://fdiwg.github.io/vrule/reference/vrule_datatype.md)
-\>
[`vrule::vrule_integer`](https://fdiwg.github.io/vrule/reference/vrule_integer.md)
-\> `vrule_year`

## Methods

### Public methods

- [`vrule_year$new()`](#method-vrule_year-new)

- [`vrule_year$validate()`](#method-vrule_year-validate)

- [`vrule_year$clone()`](#method-vrule_year-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a year validation rule

#### Usage

    vrule_year$new(na_allowed = FALSE, ...)

#### Arguments

- `na_allowed`:

  `TRUE` if NA values are allowed, `FALSE otherwise`

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates a year

#### Usage

    vrule_year$validate(value, ...)

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

    vrule_year$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
