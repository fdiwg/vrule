# vrule_operator_relational

vrule_operator_relational

vrule_operator_relational

## Details

vrule_operator_relational

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_operator_binary`](https://fdiwg.github.io/vrule/reference/vrule_operator_binary.md)
-\> `vrule_operator_relational`

## Methods

### Public methods

- [`vrule_operator_relational$new()`](#method-vrule_operator_relational-new)

- [`vrule_operator_relational$validate()`](#method-vrule_operator_relational-validate)

- [`vrule_operator_relational$clone()`](#method-vrule_operator_relational-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a operator relational validation rule

#### Usage

    vrule_operator_relational$new(operator, expr, ...)

#### Arguments

- `operator`:

  operator

- `expr`:

  expr

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_operator_relational$validate(value, ...)

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

    vrule_operator_relational$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
