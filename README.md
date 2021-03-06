manifoldRNC
===========

This is my first Haskell package. It is not yet even remotely complete
or positively correct. This package contains an implementation of a
manifold learning algorithm discovered by Andres Brun et. al in their
2005 published academic work "Fast Manifold Learning Based on
Riemannian Normal Coordinates".  I still have to tweak the
interpolation step properly and find faster, yet sufficiently
accurate, algorithms/data structures for necessary speed boosts. This
will be updated around the clock and this document will reflect the
activity.

**Update** (Oct 29, 2015) : Added the PDF (brun2005fast.pdf) that outlines/informs this implementation.

**Update** (Nov 10, 2015) : I have made some major revisions to all the code in the source directory.
I can say with confidence that I have optimized the code to the best of my ability. Just be mindful that
the algorithm does have a certain limitation; to even get decent results, you have to feed the algorithm
a very large input  (~5,000 points) and you have to ensure that the graph is connected. This is unfortunately
very costly.

# Building

## Dependencies

### Ubuntu

This should catch everything Cairo related (guessing based on memory, didn't have time to test in Docker, please update if this is wrong)

```
sudo apt-get install libghc-gtk-dev
```

## Stack + GHC 7.10

```
stack build
```
