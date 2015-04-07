# Scythe #

This package provides the libraries `Math.Scythe` and `Math.Graph`.

The intention of these libraries is to implement the `scythe` algorithm described by Justin Curry, Robert Ghrist and Vridit Nanda in the paper [Discrete Morse theory for computing cellular sheaf cohomology](http://arxiv.org/abs/1312.6454).

This, however, is just a preliminary version where we do not take into acount a sheaf ont the poset, therefore we merely reduce the poset to it's critical morse data. Such extensions are planned to be implemented in the (hopefully near) future.

---

The current version has two implementation of the Scythe algorithm, named `scythe` which employs the data type `HasseDiagram` (this was an initial line of developement that was dropped as it proved too inefficient, nevertheless the code is kept for now). And `scytheDG` which uses the data type `Graph` which proves to be way more efficient than the initial implementation, even though the code is much the same.

This version suffers from a problem of scalability as it quickly becomes unfeasible to run the algorithm for large data sets (haven't been able to run an instance of scythe on the delaunay triangulation of a set with 1000 data points in 3 dimensions, the reason is that, in order to run `scytheDG` we first have to generate the full simplicial complex which can have several tens of thousands of simplices and relations between them).

As such, a specialized version of the algorithm is under development to be applied to simplices.
