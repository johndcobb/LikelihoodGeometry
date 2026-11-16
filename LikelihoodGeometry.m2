---------------------------------------------------------------------------
-- PURPOSE : Compute the likelihood geometry of discrete statistical models.
-- PROGRAMMERS : Dave Barnhill, John Cobb, Matthew Faust
--
-- Created 7 September 2023 at the IMSI workshop "Algebraic Statistics".
---------------------------------------------------------------------------
newPackage("LikelihoodGeometry",
    AuxiliaryFiles => true,
    Version => "0.2",
    Date => "16 November 2026",
    Authors => {
        {Name => "John Cobb", Email => "jdcobb3@gmail.com", HomePage => "https://johndcobb.github.io"},
        {Name => "Matthew Faust", Email => "mfaust@msu.edu", HomePage => "https://mattfaust.github.io"},
        {Name => "David Barnhill", Email => "barnhill@usna.edu", HomePage => "https://scholar.google.com/citations?user=0PLXk0AAAAAJ&hl=en"}
    },
    Headline => "Methods for computing likelihood geometry of discrete statistical models",
    HomePage => "https://github.com/johndcobb/LikelihoodGeometry",
    Keywords => {"Commutative Algebra", "Algebraic Statistics"},
    -- Graphs must follow SlackIdeals so its graph constructor takes precedence over EdgeIdeals.
    PackageExports => {"LLLBases", "FourTiTwo", "Quasidegrees", "SlackIdeals", "Graphs"},
    PackageImports => {"Elimination", "NormalToricVarieties"},
    DebuggingMode => false
    )

export{
    -- Types
    "DiscreteRandomVariable",
    -- Functions/Methods
    "discreteRandomVariable",
    "LCRing",
    "computeLC",
    "toricModel",
    "states",
    "mean",
    "sample",
    "variance",
    "MLdegree",
    -- Constructions
    "rationalNormalScroll",
    "makeLogLinearMatrix"
}
protect pmf

baseDirectory = LikelihoodGeometry#"source directory"
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
load(baseDirectory | "LikelihoodGeometry/Code.m2")

--------------------------------------------------------------------
----- DOCUMENTATION
--------------------------------------------------------------------
beginDocumentation()
load(baseDirectory | "LikelihoodGeometry/Documentation.m2")

--------------------------------------------------------------------
----- TESTS
--------------------------------------------------------------------
load(baseDirectory | "LikelihoodGeometry/Tests.m2")
end
