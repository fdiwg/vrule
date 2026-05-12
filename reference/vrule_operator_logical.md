# vrule_operator_logical

vrule_operator_logical

vrule_operator_logical

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_operator_binary`](https://fdiwg.github.io/vrule/reference/vrule_operator_binary.md)
-\> `vrule_operator_logical`

## Public fields

- `operator_fun`:

  operator function

- `rules`:

  rules

## Methods

### Public methods

- [`vrule_operator_logical$new()`](#method-vrule_operator_logical-new)

- [`vrule_operator_logical$validate()`](#method-vrule_operator_logical-validate)

- [`vrule_operator_logical$clone()`](#method-vrule_operator_logical-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a logical operator validation rule

#### Usage

    vrule_operator_logical$new(operator, ...)

#### Arguments

- `operator`:

  operator

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_operator_logical$validate(value, row)

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

    vrule_operator_logical$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
