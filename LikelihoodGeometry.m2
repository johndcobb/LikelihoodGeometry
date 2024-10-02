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
    "DiscreteRandomVariable",
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

load "LikelihoodGeometry/CodeMatt.m2"

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

--- Will need a better way to make a bunch of DiscreteRandomVariables.
a = discreteRandomVariable 2;
b = discreteRandomVariable 2;
c = discreteRandomVariable 2;
d = discreteRandomVariable 2;
-- these dont create distinct instances.... why???

G = graph{{a,b},{b,c},{c,d}}
findMaximalCliques G


generatingSubsets = {{0,1},{1,2}};
discreteRandomVariables = {2,2,2}; 
makeLogLinearMatrix({{0,1},{1,2}},{2,2,2})