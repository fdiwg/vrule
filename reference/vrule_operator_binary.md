# vrule_operator_binary

vrule_operator_binary

vrule_operator_binary

## Super class

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\> `vrule_operator_binary`

## Public fields

- `operator`:

  operator

- `expr`:

  expr

## Methods

### Public methods

- [`vrule_operator_binary$new()`](#method-vrule_operator_binary-new)

- [`vrule_operator_binary$validate()`](#method-vrule_operator_binary-validate)

- [`vrule_operator_binary$clone()`](#method-vrule_operator_binary-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a binary operator validation rule

#### Usage

    vrule_operator_binary$new(operator, expr, ...)

#### Arguments

- `operator`:

  operator

- `expr`:

  expr

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_operator_binary$validate(value, ...)

#### Arguments

- `value`:

  value

- `...`:

  any other arg

#### Returns

a validation report, object of class
[vrule_report](https://fdiwg.github.io/vrule/reference/vrule_report.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    vrule_operator_binary$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
