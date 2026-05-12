# vrule_threshold

vrule_threshold

vrule_threshold

## Details

vrule_threshold

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_operator_binary`](https://fdiwg.github.io/vrule/reference/vrule_operator_binary.md)
-\>
[`vrule::vrule_operator_relational`](https://fdiwg.github.io/vrule/reference/vrule_operator_relational.md)
-\> `vrule_threshold`

## Methods

### Public methods

- [`vrule_threshold$new()`](#method-vrule_threshold-new)

- [`vrule_threshold$validate()`](#method-vrule_threshold-validate)

- [`vrule_threshold$clone()`](#method-vrule_threshold-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a threshold validation rule

#### Usage

    vrule_threshold$new(operator, threshold, ...)

#### Arguments

- `operator`:

  operator

- `threshold`:

  threshold

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates data based on a threshold

#### Usage

    vrule_threshold$validate(value, ...)

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

    vrule_threshold$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
