# vrule_datatype

vrule_datatype

vrule_datatype

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\> `vrule_datatype`

## Public fields

- `datatype`:

  data type

- `na_allowed`:

  is NA allowed?

## Methods

### Public methods

- [`vrule_datatype$new()`](#method-vrule_datatype-new)

- [`vrule_datatype$validate()`](#method-vrule_datatype-validate)

- [`vrule_datatype$clone()`](#method-vrule_datatype-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a data type validation rule

#### Usage

    vrule_datatype$new(datatype, na_allowed = FALSE, ...)

#### Arguments

- `datatype`:

  data type

- `na_allowed`:

  is NA allowed?

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Abstract method to validate data

#### Usage

    vrule_datatype$validate(value, ...)

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

    vrule_datatype$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
