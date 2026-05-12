# Abstract validation rule

Abstract validation rule

Abstract validation rule

## Methods

### Public methods

- [`vrule_abstract$new()`](#method-vrule_abstract-new)

- [`vrule_abstract$validate()`](#method-vrule_abstract-validate)

- [`vrule_abstract$getType()`](#method-vrule_abstract-getType)

- [`vrule_abstract$getCategory()`](#method-vrule_abstract-getCategory)

- [`vrule_abstract$getName()`](#method-vrule_abstract-getName)

- [`vrule_abstract$clone()`](#method-vrule_abstract-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes an abstract validation rule

#### Usage

    vrule_abstract$new(..., type = c("ERROR", "WARNING"))

#### Arguments

- `...`:

  args

- `type`:

  the type of rule either `ERROR` or `WARNING`

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_abstract$validate(value, ...)

#### Arguments

- `value`:

  value

- `...`:

  any other args

#### Returns

a validation report, object of class
[vrule_report](https://fdiwg.github.io/vrule/reference/vrule_report.md)

------------------------------------------------------------------------

### Method `getType()`

Get validation type

#### Usage

    vrule_abstract$getType()

#### Returns

`ERROR` or `WARNING`

------------------------------------------------------------------------

### Method `getCategory()`

Get validation rule category

#### Usage

    vrule_abstract$getCategory()

#### Returns

the validation rule category

------------------------------------------------------------------------

### Method `getName()`

Get validation rule name

#### Usage

    vrule_abstract$getName()

#### Returns

the validation rule name

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    vrule_abstract$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
