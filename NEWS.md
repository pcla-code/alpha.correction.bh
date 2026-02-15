# alpha.correction.bh 0.1.0

* **Logic Correction**: Updated `get_alphas_bh` to correctly implement the _Benjamini-Hochberg_ Step-Up procedure.
* **Algorithm Update**: The function now identifies the largest index _i_ such that _p_value(i) <= alpha(i)_. Once this index is found, the p-value at index _i_ and all p-values with a smaller index are marked as significant in the *is significant?* column.
* **Bugfix**: Removed the previous "Step-Down" behavior where all p-values were marked non-significant as soon as the first check failed. This ensures that cases like **Example 2** in the README correctly return "YES" for p-value 0.039 when Q is 0.07.
* **Type Consistency**: Ensured the returned data frame contains numeric vectors for *p-value* and *alpha* to align with standard R data structures.
