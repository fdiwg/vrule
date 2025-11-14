# getClassesInheriting

get the list of classes inheriting a given super class provided by its
name

## Usage

``` r
getClassesInheriting(classname, extended, pretty)
```

## Arguments

- classname:

  the name of the superclass for which inheriting sub-classes have to be
  listed

- extended:

  whether we want to look at user namespace for third-party sub-classes

- pretty:

  prettify the output as `data.frame`

## Examples

``` r
  getClassesInheriting("vrule_integer")
#> [1] "vrule_year"
```
