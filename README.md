# Alpha Correction R Package

### `library(alpha-correction-bh)`

## Introduction
This package provides functions for calculating alpha corrections for a list of p-values according to the _Benjamini-Hochberg_ alpha correction.

Reference:
Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society: series B (Methodological), 57(1), 289-300.

For a sorted list containing _m_ p-values indexed from  _1_ to _m_, the alpha for each p-value _p_ is computed as:

                          alpha(i) = (p_value(i)/m)Q
where:

- _i_ is the index of the p-value in list _l_ (1 to m),
- _p_value(i)_ is the p_value at index i, and 
- Q is the false discovery rate, which is 0.05 by default.

## Installation

Install the package using dev-tools directly from github or from cran.

`devtools::install_github('pcla-code/edu.upenn.pcla.alpha.correction.bh')`

## Dependencies
This library uses *knitr* to render tables.

## Usage

Import the package:

`library(alpha-correction-bh)`

`library(knitr)`

And call the get_alphas_bh function, passing your p_values and, optionally,
Q:

`get_alphas_bh(p_values, Q)`

Use this function to calculate corrected values for a list of p-values
and a given false discovery rate Q.

If you do not provide Q, a default value of 0.05 will be used.

## Output Options

You can customize the output of the function using the following two
options:

1.  `output` - valid values are:
    -   *print* - print the data frame to the console only

    -   *data_frame* - return the data frame only

    -   *both* - print the data frame to the console and return it. This
        is the default behavior.
2.  `include_is_significant_column` - valid values are:
    -   *TRUE* - The *is significant?* column is included. This is the
        default behavior.
    -   *FALSE* - The *is significant?* column is not included.

#### Example 1:

`get_alphas_bh(list(0.08,0.01,0.039))`

Output:

|p-value |alpha |is significant? |
|:-------|:-----|:---------------|
|0.08    |0.05  |NO              |
|0.01    |0.017 |YES             |
|0.039   |0.033 |NO              |

#### Example 2:

`get_alphas_bh(list(0.08,0.01,0.039), .07)`

Output:

|p-value |alpha |is significant? |
|:-------|:-----|:---------------|
|0.08    |0.07  |NO              |
|0.01    |0.023 |YES             |
|0.039   |0.047 |YES             |

### Documentation

To read the documentation of the function, execute the following in R:

`?get_alphas_bh`

You can also read the vignette [here](https://htmlpreview.github.io/?https://github.com/pcla-code/alpha-correction-bh/blob/develop/doc/alpha-correction.html).
