---------------------------------------------------------------------------
-- PURPOSE : Create methods for computing facts relating to likelihood geometry --           of discrete statistical models.
--           
-- PROGRAMMERS : Dave Barnhill, John Cobb, Matthew Faust
--
-- UPDATE HISTORY : created 7 September 2023 as a part of the IMSI workshop --  --                  "Algebraic Statistics";
---------------------------------------------------------------------------
newPackage("LikelihoodGeometry",
    AuxiliaryFiles => true,
    Version => "0.1",
    Date => "4 September 2024",
    Authors => {
	{Name => "John Cobb", Email => "jcobb2@wisc.edu", HomePage => "https://johndcobb.github.io"}
    --Add your information here.
    },
    Headline => "Methods for computing likelihood geometry of discrete statistical models",
    Keywords => {"Commutative Algebra, Algebraic Statistics, Discrete Statistical Models, Likelihood Geometry"},
    PackageExports => {"GraphicalModels", "LLLBases"},
    PackageImports => {"Elimination", "NormalToricVarieties"},
    DebuggingMode => true --turn to false when submitting
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
    "MLdegree",
    "toricPolytope",
    -- Constructions
    "rationalNormalScroll",
    "makeLogLinearMatrix"
    -- Symbols
    -- Helper functions
}
protect pmf

baseDirectory = LikelihoodGeometry#"source directory"
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
load(baseDirectory | "LikelihoodGeometry/Code.m2")
--- THINGS TO IMPLEMENT? -- 
-*

*-

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

--------------------------------------------------------------------
----- SCRATCH SPACE
--------------------------------------------------------------------
uninstallPackage "LikelihoodGeometry";
restart
installPackage "LikelihoodGeometry"
--check LikelihoodGeometryf
debug needsPackage "LikelihoodGeometry";

a = discreteRandomVariable 2;
b = discreteRandomVariable 3;
c = discreteRandomVariable 4;

G = graph({{a,b}, {b,c}})
X = toricModel G
Mlist = computeLCJI(X)
I = computeLCJI(X) 
J = computeLC(X)
R = ring(I)
S = ring(J)

a = discreteRandomVariable 2;
b = discreteRandomVariable 2;
c = discreteRandomVariable 2;
S = {{a,b}, {b,c}};
L = {a,b,c};
makeLogLinearMatrix(S,L)

LCRing(toricModel G)

phi = map(R,S, vars R)
phi(J) == I


maxCliques = findMaximalCliques G
states maxCliques
makeLogLinearMatrix(maxCliques, vertices G)

S = rationalNormalScroll({1,2,3})

G = graph({1,2,3,4,5,6},{{2,3}, {4,5}, {5,6}, {6,4}})
C = connectedComponents G
isJointlyIndependent(G)


G = graph{{a,b},{b,c}}
findMaximalCliques G
X = toricModel G
X.cache.Graph