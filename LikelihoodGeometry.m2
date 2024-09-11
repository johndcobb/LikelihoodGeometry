---------------------------------------------------------------------------
-- PURPOSE : Create methods for computing facts relating to likelihood geometry --           of discrete statistical models.
--           
-- PROGRAMMERS : Dave Barnhill, John Cobb, Matthew Faust
--
-- UPDATE HISTORY : created 7 September 2023 as a part of the IMSI workshop --  --                  "Algebraic Statistics";
---------------------------------------------------------------------------
newPackage("LikelihoodGeometry",
    Version => "0.1",
    Date => "4 September 2024",
    Authors => {
	{Name => "John Cobb", Email => "jcobb2@wisc.edu", HomePage => "https://johndcobb.github.io"}
    --Add your information here.
    },
    Headline => "Methods for computing likelihood geometry of discrete statistical models",
    Keywords => {"Commutative Algebra, Algebraic Statistics, Discrete Statistical Models, Likelihood Geometry"},
    PackageExports => {"GraphicalModels"},
    PackageImports => {"Elimination", "Quasidegrees", "NormalToricVarieties"},
    DebuggingMode => true --turn to false when submitting
    )

export{
    -- Types
    "ToricModel",
    -- Functions/Methods
    "LCRing",
    "computeIndependenceLC",
    "computeToricLC",
    "computeLC",
    "computenwayIndependenceLC",
    "nwayindependenceMatrices",
    "computenwayindependenceModel",
    "computenwayToricMatrix",
    "computeToricLCwithToric",
    "computeToricLCwithToricSeq",
    -- Symbols
    "Start",
    "Symbols"
    -- Helper functions
}

--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
load "LikelihoodGeometry/legacycode.m2"
load "LikelihoodGeometry/Code.m2"


--- THINGS TO IMPLEMENT? -- 
-*

*-

--------------------------------------------------------------------
----- DOCUMENTATION
--------------------------------------------------------------------
beginDocumentation()
load "LikelihoodGeometry/Documentation.m2"

--------------------------------------------------------------------
----- TESTS
--------------------------------------------------------------------
load "LikelihoodGeometry/Tests.m2"
end

--------------------------------------------------------------------
----- SCRATCH SPACE
--------------------------------------------------------------------
uninstallPackage "LikelihoodGeometry";
restart
installPackage "LikelihoodGeometry"
check LikelihoodGeometry

debug needsPackage "LikelihoodGeometry";

A = matrix{{1,1,0,0},{0,0,1,1}, {1,0,1,0}, {0,1,0,1}}
toricModel A