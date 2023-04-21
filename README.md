# Alpha Correction R Package

### `library(alpha.correction.bh)`

## Introduction
Provides the alpha-adjustment correction from Benjamini & Hochberg (1995) Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal statistical society: series B (Methodological), 57(1), 289-300. For researchers interested in using the exact mathematical formulas and procedures as used in the original paper.

For a sorted list containing _m_ p-values indexed from  _1_ to _m_, the alpha for each p-value _p_ is computed as:

                          alpha(i) = (p_value(i)/m)Q
where:

- _i_ is the index of the p-value in list _l_ (1 to m),
- _p_value(i)_ is the p_value at index i, and 
- Q is the false discovery rate, which is 0.05 by default.

## Installation

Install the package using dev tools directly from github.

`devtools::install_github('pcla-code/alpha.correction.bh')`

## Usage

Import the package:

`library(alpha.correction.bh)`

And call the get_alphas function, passing your p_values and, optionally, Q:

`get_alphas(p_values, Q)`

Use this function to calculate corrected values for a list of p-values and a given false discovery rate Q.

If you do not provide Q, a default value of 0.05 will be used.

The function will sort the list of p-values, but you are encouraged to pass a sorted list nonetheless, so you can readily compare the p-values your passed to the returned alphas.

### Example 1:
`get_alphas(list(0.01,0.08,0.039))`

_Output:_

[[1]] [1] 0.01666667

[[2]] [1] 0.03333333

[[3]] [1] 0.05


### Example 2:
`get_alphas(list(0.01,0.08,0.039), .07)`

_Output:_

[[1]] [1] 0.02333333

[[2]] [1] 0.04666667

[[3]] [1] 0.07

### Documentation

To read the documentation of the function, execute the following in R:

`?get_alphas`

You can also read the vignette [here](http://htmlpreview.github.io/?https://github.com/pcla-code/alpha.correction.bh/blob/develop/vignettes/alpha-correction.html).