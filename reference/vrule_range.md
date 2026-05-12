# vrule_range

vrule_range

vrule_range

## Details

vrule_range

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\> `vrule_range`

## Public fields

- `minValue`:

  min value

- `maxValue`:

  max value

## Methods

### Public methods

- [`vrule_range$new()`](#method-vrule_range-new)

- [`vrule_range$validate()`](#method-vrule_range-validate)

- [`vrule_range$clone()`](#method-vrule_range-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a range validation rule

#### Usage

    vrule_range$new(minValue, maxValue, ...)

#### Arguments

- `minValue`:

  min value

- `maxValue`:

  max value

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates data based on a range

#### Usage

    vrule_range$validate(value, ...)

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

    vrule_range$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
