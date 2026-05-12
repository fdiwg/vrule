# vrule_logical

vrule_logical

vrule_logical

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\>
[`vrule::vrule_datatype`](https://fdiwg.github.io/vrule/reference/vrule_datatype.md)
-\> `vrule_logical`

## Methods

### Public methods

- [`vrule_logical$new()`](#method-vrule_logical-new)

- [`vrule_logical$validate()`](#method-vrule_logical-validate)

- [`vrule_logical$clone()`](#method-vrule_logical-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a logical data validation rule

#### Usage

    vrule_logical$new(na_allowed = FALSE, ...)

#### Arguments

- `na_allowed`:

  is NA allowed?

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_logical$validate(value, ...)

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

    vrule_logical$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
