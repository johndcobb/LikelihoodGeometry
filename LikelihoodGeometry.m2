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
	{Name => "John Cobb", Email => "jdcobb3@gmail.com", HomePage => "https://johndcobb.github.io"},
    {Name => "Matthew Faust", Email => "mfaust@msu.edu", HomePage => "https://mattfaust.github.io"},
    {Name => "David Barnhill", Email => "barnhill@usna.edu", HomePage => " "}
    },
    Headline => "Methods for computing likelihood geometry of discrete statistical models",
    Keywords => {"Commutative Algebra, Algebraic Statistics, Discrete Statistical Models, Likelihood Geometry"},
    PackageExports => {"GraphicalModels", "LLLBases", "FourTiTwo"},
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
    "toricIdeal",
    "states",
    "mean",
    "sample",
    "variance",
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
debug installPackage "LikelihoodGeometry"
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
X.cache.Graphrestart
needsPackage "LikelihoodGeometry";
R = QQ[x,y,z];
f_1 = (random(R^1,R^{-2}))_(0,0)
f_2 = (random(R^1,R^{-3}))_(0,0)
f_3 = (random(R^1,R^{-2}))_(0,0)
g = f_1*f_2*f_3
I = ideal(g)

needsPackage "Divisor";
D = divisor(I)
isSNC(D, IsGraded => true)

S = R ** QQ[s_1,s_2,s_3]

fList = for f in factor g when not isConstant(f#0) list f#0
m  = #fList

n = #(flatten entries vars R)
Q1 = diagonalMatrix(fList) || matrix {toList(S_(n)..S_(n+m-1))}
Q2 = transpose jacobian(ideal(fList)) || matrix{ for f in fList list 0}
Q = (Q1 | Q2)_{1..2*m-1}
likelihoodIdeal = minors(min(m+n-1, m+1), Q) + ideal sum flatten for i from 1 to m list degree(fList_(i-1))*S_(n+i-1)

varList := for i from 0 to #(gens R) -1 list S_i
phi := map(S,R,varList);
varList2 = for i from 0 to #(gens R) -1 list 1_S;

J = (matrix {varList2}) || transpose(phi jacobian(I));
Q = minors(codim(I)+1,jacobian(I));

U = reshape(S^n,S^2,matrix{gens S});
pmat = transpose(matrix{U_0});
umat = transpose(matrix{U_1});
Jaug := umat || J*diagonalMatrix(varList);

H := ideal((sum flatten entries pmat)*(product flatten entries pmat));

L := saturate(phi(I) + minors(codim(I)+2,Jaug),H+(phi(Q)));

L == likelihoodIdeal -- why is this false?