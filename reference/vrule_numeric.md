# vrule_numeric

vrule_numeric

vrule_numeric

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\>
[`vrule::vrule_datatype`](https://fdiwg.github.io/vrule/reference/vrule_datatype.md)
-\> `vrule_numeric`

## Methods

### Public methods

- [`vrule_numeric$new()`](#method-vrule_numeric-new)

- [`vrule_numeric$validate()`](#method-vrule_numeric-validate)

- [`vrule_numeric$clone()`](#method-vrule_numeric-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a numeric data validation rule

#### Usage

    vrule_numeric$new(na_allowed = FALSE, ...)

#### Arguments

- `na_allowed`:

  is NA allowed?

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_numeric$validate(value, ...)

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

    vrule_numeric$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
