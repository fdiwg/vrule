# getVruleClasses

get the list of validation classes, ie classes extending vrule_abstract
super class, including classes eventually defined outside vrule. In case
the latter is on the search path, the list of validation classes will be
cached for optimized used by vrule encoder/decoder.

## Usage

``` r
getVruleClasses()
```

## Author

Emmanuel Blondel, <emmanuel.blondel1@gmail.com>

## Examples

``` r
  getVruleClasses()
#>  [1] "vrule_abstract_complex"    "vrule_abstract_simple"    
#>  [3] "vrule_character"           "vrule_codelist"           
#>  [5] "vrule_cross_column"        "vrule_datatype"           
#>  [7] "vrule_date"                "vrule_date_max"           
#>  [9] "vrule_date_min"            "vrule_date_range"         
#> [11] "vrule_date_threshold"      "vrule_double"             
#> [13] "vrule_if"                  "vrule_integer"            
#> [15] "vrule_logical"             "vrule_mapping"            
#> [17] "vrule_max"                 "vrule_min"                
#> [19] "vrule_numeric"             "vrule_operator_and"       
#> [21] "vrule_operator_binary"     "vrule_operator_logical"   
#> [23] "vrule_operator_or"         "vrule_operator_relational"
#> [25] "vrule_range"               "vrule_raw_codelist"       
#> [27] "vrule_threshold"           "vrule_year"               
```
