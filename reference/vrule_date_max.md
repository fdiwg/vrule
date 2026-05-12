# vrule_date_max

vrule_date_max

vrule_date_max

## Details

vrule_date_max

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_operator_binary`](https://fdiwg.github.io/vrule/reference/vrule_operator_binary.md)
-\>
[`vrule::vrule_operator_relational`](https://fdiwg.github.io/vrule/reference/vrule_operator_relational.md)
-\> `vrule_date_max`

## Methods

### Public methods

- [`vrule_date_max$new()`](#method-vrule_date_max-new)

- [`vrule_date_max$validate()`](#method-vrule_date_max-validate)

- [`vrule_date_max$clone()`](#method-vrule_date_max-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a date max validation rule

#### Usage

    vrule_date_max$new(maxValue, ...)

#### Arguments

- `maxValue`:

  max value

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Validates data based on a max date threshold

#### Usage

    vrule_date_max$validate(value, ...)

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

    vrule_date_max$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
