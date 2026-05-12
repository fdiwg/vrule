# vrule_date

vrule_date

vrule_date

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\>
[`vrule::vrule_datatype`](https://fdiwg.github.io/vrule/reference/vrule_datatype.md)
-\>
[`vrule::vrule_character`](https://fdiwg.github.io/vrule/reference/vrule_character.md)
-\> `vrule_date`

## Methods

### Public methods

- [`vrule_date$new()`](#method-vrule_date-new)

- [`vrule_date$validate()`](#method-vrule_date-validate)

- [`vrule_date$clone()`](#method-vrule_date-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a date validation rule

#### Usage

    vrule_date$new(na_allowed = FALSE, ...)

#### Arguments

- `na_allowed`:

  `TRUE` if NA values are allowed, `FALSE otherwise`

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates a date

#### Usage

    vrule_date$validate(value, ...)

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

    vrule_date$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
