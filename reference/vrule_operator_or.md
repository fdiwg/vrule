# vrule_operator_or

vrule_operator_or

vrule_operator_or

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_operator_binary`](https://fdiwg.github.io/vrule/reference/vrule_operator_binary.md)
-\>
[`vrule::vrule_operator_logical`](https://fdiwg.github.io/vrule/reference/vrule_operator_logical.md)
-\> `vrule_operator_or`

## Methods

### Public methods

- [`vrule_operator_or$new()`](#method-vrule_operator_or-new)

- [`vrule_operator_or$validate()`](#method-vrule_operator_or-validate)

- [`vrule_operator_or$clone()`](#method-vrule_operator_or-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a logical OR operator validation rule

#### Usage

    vrule_operator_or$new(...)

#### Arguments

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_operator_or$validate(value, row)

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

    vrule_operator_or$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
