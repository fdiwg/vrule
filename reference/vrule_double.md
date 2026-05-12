# vrule_double

vrule_double

vrule_double

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\>
[`vrule::vrule_datatype`](https://fdiwg.github.io/vrule/reference/vrule_datatype.md)
-\> `vrule_double`

## Methods

### Public methods

- [`vrule_double$new()`](#method-vrule_double-new)

- [`vrule_double$validate()`](#method-vrule_double-validate)

- [`vrule_double$clone()`](#method-vrule_double-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a double data validation rule

#### Usage

    vrule_double$new(na_allowed = FALSE, ...)

#### Arguments

- `na_allowed`:

  is NA allowed?

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_double$validate(value, ...)

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

    vrule_double$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
