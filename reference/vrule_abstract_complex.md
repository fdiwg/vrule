# vrule_abstract_complex

vrule_abstract_complex

vrule_abstract_complex

## Super class

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\> `vrule_abstract_complex`

## Methods

### Public methods

- [`vrule_abstract_complex$new()`](#method-vrule_abstract_complex-new)

- [`vrule_abstract_complex$validate()`](#method-vrule_abstract_complex-validate)

- [`vrule_abstract_complex$clone()`](#method-vrule_abstract_complex-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes an abstract complex validation rule

#### Usage

    vrule_abstract_complex$new(...)

#### Arguments

- `...`:

  args

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_abstract_complex$validate(value, ...)

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

    vrule_abstract_complex$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
