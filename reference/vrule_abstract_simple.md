# vrule_abstract_simple

vrule_abstract_simple

vrule_abstract_simple

## Super class

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\> `vrule_abstract_simple`

## Methods

### Public methods

- [`vrule_abstract_simple$new()`](#method-vrule_abstract_simple-new)

- [`vrule_abstract_simple$validate()`](#method-vrule_abstract_simple-validate)

- [`vrule_abstract_simple$clone()`](#method-vrule_abstract_simple-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes an abstract simple validation rule

#### Usage

    vrule_abstract_simple$new(...)

#### Arguments

- `...`:

  args

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_abstract_simple$validate(value, ...)

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

    vrule_abstract_simple$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
