## Uniform Budget Allocation

Uniform budget allocation is a method that allocates the privacy budget uniformly across all nodes in the tree structure. This means that each node receives an equal share of the total privacy budget. The formula for uniform budget allocation is:

```
ε_i = ε / n
```

where `ε_i` is the privacy budget allocated to node `i`, `ε` is the total privacy budget, and `n` is the total number of nodes in the tree.

Uniform budget allocation is a simple and straightforward method, but it may not be the most effective for all scenarios. In some cases, it may be more beneficial to allocate more privacy budget to certain nodes in the tree that contain more sensitive information.

## Fibonacci Budget Allocation

Fibonacci budget allocation is a novel non-uniform allocation scheme for privacy budget that enhances data utility and privacy guarantee. In this method, different privacy budgets are allocated to each layer of the quadtree using Fibonacci series features. The formula for Fibonacci allocation is:

```
ε_i = ε * F_i / F_n
```

where `ε_i` is the privacy budget allocated to node `i`, `ε` is the total privacy budget, `F_i` is the ith Fibonacci number, and `F_n` is the nth Fibonacci number.

Fibonacci budget allocation is more complex than uniform allocation, but it has been shown to significantly improve data query accuracy in geospatial point data. By allocating more privacy budget to nodes that contain more sensitive information, this method can provide better privacy guarantees while still maintaining data utility.

## References

- [Differential Privacy for Geospatial Point Data via Quadtree-based Data Abstraction](https://ieeexplore.ieee.org/document/7464127)