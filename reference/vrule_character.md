# vrule_character

vrule_character

vrule_character

## Super classes

[`vrule::vrule_abstract`](https://fdiwg.github.io/vrule/reference/vrule_abstract.md)
-\>
[`vrule::vrule_abstract_simple`](https://fdiwg.github.io/vrule/reference/vrule_abstract_simple.md)
-\>
[`vrule::vrule_datatype`](https://fdiwg.github.io/vrule/reference/vrule_datatype.md)
-\> `vrule_character`

## Methods

### Public methods

- [`vrule_character$new()`](#method-vrule_character-new)

- [`vrule_character$validate()`](#method-vrule_character-validate)

- [`vrule_character$clone()`](#method-vrule_character-clone)

Inherited methods

- [`vrule::vrule_abstract$getCategory()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getCategory)
- [`vrule::vrule_abstract$getName()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getName)
- [`vrule::vrule_abstract$getType()`](https://fdiwg.github.io/vrule/reference/vrule_abstract.html#method-getType)

------------------------------------------------------------------------

### Method `new()`

Initializes a character data validation rule

#### Usage

    vrule_character$new(na_allowed = FALSE, ...)

#### Arguments

- `na_allowed`:

  is NA allowed?

- `...`:

  any other arg

------------------------------------------------------------------------

### Method `validate()`

Method to validate data

#### Usage

    vrule_character$validate(value, ...)

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

    vrule_character$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
