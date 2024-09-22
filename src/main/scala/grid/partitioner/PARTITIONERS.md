# Partitioners

This document provides an overview of various partitioning strategies used in privacy-preserving data analysis, each with its own unique characteristics and privacy budget considerations.

## Uniform Partitioner

**Privacy budget:** `Epsilon`

The Uniform Partitioner implements a partitioning strategy that divides the data space into equal-sized cells. The number of cells generated is determined by the specified number of partitions. This partitioning technique follows the EUGkM Strategy proposed in [Differentially Private K-Means Clustering](https://dl.acm.org/doi/pdf/10.1145/2857705.2857708).

The formula for determining the number of partitions is as follows:

```
M = (Ñ * epsilon / θ)^(2d / 2 + d)
```

Where:
- `Ñ = N + Lap( 1 / epsilon )`
- `N` is the number of points
- `epsilon` is the privacy budget
- `θ` is experimentally set to 10 and calculated as `sqrt(alpha / (2 * beta^2))`
- `d` is the dimensionality of the data space

## Binary Partitioner

**Privacy budget:** `None`

The Binary Partitioner divides the data space into two equal-sized cells and has a fanout of 2. For instance, in a 2D space, it splits the space into 2 cells. After two iterations of this partitioner, the space will be divided into 4 cells, following a pattern similar to a quad-tree, and so on.

The Binary Partitioner does not require a privacy budget for a fixed depth. If a stopping condition is met before reaching the maximum depth, any remaining privacy budget will be allocated to the data release.

## Median Partitioner

**Privacy budget:** `Epsilon`

The Median Partitioner employs a KD-tree-like partitioning approach, with a fanout of 2. It divides the data space and selects medians using an exponential mechanism. The algorithm used for selecting medians is described in [Differentially Private Quantiles](https://proceedings.mlr.press/v139/gillenwater21a/gillenwater21a.pdf).

## Multi Quantile Partitioner

**Privacy budget:** `Epsilon`

The Multi Quantile Partitioner selects 'm' quantiles using an exponential mechanism, following the `JointExp` function as described in [Differentially Private Quantiles](https://proceedings.mlr.press/v139/gillenwater21a/gillenwater21a.pdf). This partitioning strategy is designed to provide privacy guarantees while segmenting the data into quantile-based partitions.

## Epsilon comparison

The following table provides a comparison of the privacy budget required by each partitioner to achieve the same level of privacy.

| Partitioner    | epsilon_release | epsilon_partition            | epsilon_threshold               |
|----------------|-----------------|------------------------------|---------------------------------|
| Uniform        | YES             | YES - Count query            | YES - Count query (ISSUE #1)    |
| Median         | YES             | YES  - Exponential Mechanism | YES             - Count query   |
| Multi Quantile | YES             | YES - Exponential Mechanism  | YES             - Count query   |
| Binary         | YES             | NO                           | YES               - Count query |

ISSUE #1: how are we going to allocate the privacy budget if the fanout is variable?
SOLUTION #1: We can use an upper bound for the fanout and allocate the privacy budget accordingly.
We can use the first level count to determine the fanout and then allocate the privacy budget accordingly.
The only problem could be that the fanout is too large and we end up allocating too much privacy budget to the leaf nodes.
