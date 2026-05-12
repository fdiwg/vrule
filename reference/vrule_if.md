# vrule_if

vrule_if

vrule_if

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_complex`](https://fdiwg.github.io/vrule/reference/vrule_abstract_complex.md)
-\> `vrule_if`

## Public fields

- `if_condition`:

  if expression

- `then_apply`:

  list of expressions to apply if condition is fulfilled

- `else_apply`:

  list of expressions to apply if condition is not fulfilled

## Methods

### Public methods

- [`vrule_if$new()`](#method-vrule_if-new)

- [`vrule_if$validate()`](#method-vrule_if-validate)

- [`vrule_if$clone()`](#method-vrule_if-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a conditionnal validation rule

#### Usage

    vrule_if$new(if_condition, then_apply = list(), else_apply = list(), ...)

#### Arguments

- `if_condition`:

  if condition

- `then_apply`:

  list of expressions to apply if condition is fulfilled

- `else_apply`:

  list of expressions to apply if condition is not fulfilled

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_if$validate(value, row)

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

    vrule_if$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
