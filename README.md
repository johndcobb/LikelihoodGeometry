# LikelihoodGeometry

A Macaulay2 package for likelihood ideals and maximum likelihood degrees of discrete statistical models.

Keep `LikelihoodGeometry.m2` and the `LikelihoodGeometry/` directory together. The latter contains the code, documentation, and tests.

From this directory, run in a fresh Macaulay2 session:

```m2
installPackage "LikelihoodGeometry"
check("LikelihoodGeometry", Verbose => true)
```

Use `help "LikelihoodGeometry"` for the package documentation. The base `toricIdeal` method comes from Quasidegrees, and `toricPolytope` comes from SlackIdeals.
