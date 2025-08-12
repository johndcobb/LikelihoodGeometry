KK = QQ; --global base ring
--- kludge to access parts of the 'Core'
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";

--------------------------------------------------------------------
----- Internal helper functions
--------------------------------------------------------------------

-- Function to generate all states of a list of discrete random variables given a list of arities
cartesianProd = (A, B) -> (A**B);

makeLogLinearMatrix = method()
makeLogLinearMatrix(List, List) := Matrix => (generatingSubsets, discreteRandomVariables) -> (
    -- expecting the GeneratingSubsets to be a list containing sublists of discreteRandomVariables
    -- expecting discreteRandomVariables to be a list of DiscreteRandomVariables which describe the arity of the random variables -- e.g. (2,2,2,2) for 4 binary random variables

    -- Generate all possible states for the given discrete random variables
    allStates := states discreteRandomVariables;

    -- Initialize an empty list to store the rows of the log-linear matrix
    matrixList := {};

    -- Iterate over each generating subset
    for g in generatingSubsets do (
        -- Generate all possible states for the current subset of discrete random variables
        gStates := states g;
        if #g == 1 then gStates = pack(gStates,1); -- make it work for singletons
        gPositions := apply(g, x -> position(discreteRandomVariables, l -> l === x)); -- this exchanges g for their positions within the original list of random variables
        
        -- Iterate over each state of the current subset
        for gState in gStates do (
            -- Create a row for the log-linear matrix where each entry is 1 if the state matches the current subset state, otherwise 0
            gRow := apply(apply(allStates, state -> state_gPositions == gState), b -> if b then 1 else 0);
            
            -- Add the generated row to the matrix list
            matrixList = matrixList | {gRow};
        );
    );

    -- Convert the list of rows into a matrix
    matrix matrixList
)

makeLogLinearMatrix Graph := Matrix => G -> (makeLogLinearMatrix(findMaximalCliques G, vertices G))

-- Define a function to check if a set of vertices forms a clique
isClique = (G, vertices) -> (
    -- Check if all pairs of vertices are connected
    all(subsets(vertices, 2) / (pair -> any(edges G, edge -> edge == set pair)))
)


-- Define a method to find maximal cliques in a graph
findMaximalCliques = method()

-- Implementation of the method for Graph objects, returning a list of cliques
findMaximalCliques(Graph) := List => (G) -> (
    -- If the graph has no vertices, return an empty list
    if #(vertices G) == 0 then return {};

    -- Generate all possible cliques of size equal to the clique number of the graph
    possibleCliques := subsets(vertices G, cliqueNumber G);

    -- Initialize an empty list to store the cliques
    Cliques := {};

    -- Iterate over all possible cliques
    for possibleClique in possibleCliques do (
        -- Check if the current subset of vertices forms a clique
        if isClique(G, possibleClique) then (
            -- If it is a clique, add it to the list of cliques
            Cliques = Cliques | {possibleClique};
        );
    );

    -- Find vertices that are not part of any clique found so far
    remainingUnmatchedVertices := vertices G - union(Cliques / set);

    -- If there are unmatched vertices, recursively find cliques in the induced subgraph
    if #remainingUnmatchedVertices != 0 then (
        Cliques = Cliques | findMaximalCliques(inducedSubgraph(G, remainingUnmatchedVertices));
    );

    -- Return the list of cliques
    Cliques
)

isJointlyIndependent = method()
isJointlyIndependent(Graph) := Boolean => G -> (
    C := connectedComponents G;
    all(C, c -> isCompleteGraph(inducedSubgraph(G, c)))
)

isCompleteGraph = method()
isCompleteGraph(Graph) := Boolean => G -> (
    -- Check if the graph has no vertices
    if #(vertices G) == 0 then return false;

    -- Check if the graph has the maximum number of edges for its number of vertices
    #edges G == #vertices G * (#vertices G - 1) / 2
)

--------------------------------------------------------------------
----- Basic features of the toricModel constructor
--------------------------------------------------------------------

toricModel = method (
    TypicalValue => NormalToricVariety,
    Options => {
    	CoefficientRing   => KK,
    	MinimalGenerators => false,
    	Variable          => getSymbol "x"
	}
    )
toricModel Matrix := opts -> vertices -> (
    X := normalToricVariety vertices;
    X.cache#"mat" = vertices;
    X
)

toricModel Graph := NormalToricVariety => opts -> G -> (
    A := makeLogLinearMatrix G;
    X := normalToricVariety A;
    X.cache#"mat" = A;
    X.cache#"Graph" = G;
    X
)

toricModel Ideal := NormalToricVariety => opts -> I -> (
-- does not check if ideal is toric!!
    A := toricPolytope I;
    X := normalToricVariety A;
    X.cache#"mat" = A;
    X
)

-- This function implementation is from Quasidegrees.m2
toricIdeal = method()
toricIdeal(Matrix,Ring) := (A,R) -> (
    m := product gens R;
    saturate(sub(toBinomial(transpose(syz(A)),R),R),m)
    )
toricIdeal(Matrix) := A -> (
    numcol := numColumns(A);
    p := local p;
    R := KK[p_0..p_(numcol-1)];
    toricIdeal(A,R)
)
toricIdeal(NormalToricVariety, Ring) := Ideal => (X,R) -> (
    A := X.cache#"mat";
    toricIdeal(A,R)
)
toricIdeal(NormalToricVariety) := X -> (
    A := X.cache#"mat";
    numcol := numColumns(A);
    p := local p;
    R := X.cache.CoefficientRing[p_0..p_(numcol-1)];
    toricIdeal(A, R)
)
toricIdeal(Graph) := G -> toricIdeal(toricModel G)

-- The next two functions are from SlackIdeals.m2
fromBinomial = b -> (
    -- INPUT: binomial b
    -- OUTPUT: kernel vector of exponents of b
    -- does not check binomiality, if b has >2 terms the exponent vector of the first two will be returned
    E := exponents(b);
    E_0 - E_1
)

toricPolytope = method()
toricPolytope Ideal := Matrix => I -> (
    -- INPUT: toric ideal I
    -- OUTPUT: matrix whose columns represent the vector configuration with toric ideal I
    K := for f in flatten entries gens I list fromBinomial(f);
    transpose LLL syz matrix K
)

--------------------------------------------------------------------
----- Basic features of the DiscreteRandomVariable datatype
--------------------------------------------------------------------
DiscreteRandomVariable = new Type of MutableHashTable
DiscreteRandomVariable.synonym = "discrete random variable"
DiscreteRandomVariable.GlobalAssignHook = globalAssignFunction
DiscreteRandomVariable.GlobalReleaseHook = globalReleaseFunction

arity = method()
arity DiscreteRandomVariable := ZZ => X -> X.arity

expression DiscreteRandomVariable := X -> (
    if hasAttribute (X, ReverseDictionary) 
    then expression getAttribute (X, ReverseDictionary) else 
    (describe X)
)
texMath DiscreteRandomVariable := X -> texMath expression X
describe DiscreteRandomVariable := X -> (
    myOutput := {net "arity => " | net X.arity};
    horizontalJoin flatten ("{", stack myOutput, "}")
)
toString DiscreteRandomVariable := X -> toString expression X

net DiscreteRandomVariable := net @@ expression

discreteRandomVariable = method (
    TypicalValue => DiscreteRandomVariable
)

discreteRandomVariable(ZZ, FunctionClosure) := (d,f) -> (
    if d < 1 then error "--expected a positive integer";
    new DiscreteRandomVariable from {
        symbol arity => d,
        symbol cache => new CacheTable,
        symbol pmf => f
    }
)
discreteRandomVariable ZZ := d -> (
    if d < 1 then error "--expected a positive integer";
    g := x -> if x > 0 and x <= d then 1/d else 0;
    discreteRandomVariable(d,g)
)

isWellDefined DiscreteRandomVariable := Boolean => X -> (
    K := keys X;
    expectedKeys := set {symbol arity, symbol cache, symbol pmf};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList (K - expectedKeys);
	    missing := toList (expectedKeys - K);
	    if #added > 0 then 
	    << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	    << "-- missing keys(s): " << toString missing << endl
	    );	 
    	return false
    	);
    if not instance (X.cache, CacheTable) then (
    	if debugLevel > 0 then 
	    << "-- expected `X.cache' to be a CacheTable" << endl;
    	return false
	);
    pmfs := apply(states X, i -> X.pmf(i));
    if not sum(pmfs) == 1 then (
    	if debugLevel > 0 then 
        << "-- expected the sum of the pmf to be 1" << endl;
    	return false
    );
true)

states = method()
states DiscreteRandomVariable := List => X -> toList(1..X.arity)
states List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    if #L == 1 then return states L_0;
    valueSets := apply(L, x -> set states x);
    combinations := fold(cartesianProd,valueSets) / deepSplice / toList;
    rsort(toList(combinations))
)

sample = method()
sample DiscreteRandomVariable := ZZ => X -> (
        cdf := accumulate(plus, 0, apply(states X, i -> X.pmf(i)));
        randomRR := random(0.0,1.0);
        return position(cdf, x -> x > randomRR)+1
)
sample(DiscreteRandomVariable, ZZ) := ZZ => (X, n) -> (
    for i in 1..n list sample X
)
sample List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    apply(L, x -> sample x)
)
sample(List, ZZ) := (L, n) -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    for i in 1..n list sample L
)

mean = method()
mean DiscreteRandomVariable := RR => X -> (
    pmfs := apply(states X, i -> X.pmf(i));
    sum(pack(2,mingle(pmfs, states X)) / product)
)

mean List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    apply(L, x -> mean x)
)

variance = method()
variance DiscreteRandomVariable := RR => X -> (
    pmfs := apply(states X, i -> X.pmf(i));
    X2 := apply(states X, i -> i^2);
    sum(pack(2,mingle(pmfs, X2)) / product) - (mean X)^2
)

variance List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    apply(L, x -> variance x)
)

--------------------------------------------------------------------
----- Methods utilizing NormalToricVariety and DiscreteRandomVariables
--------------------------------------------------------------------

computeLC = method()
computeLC(Ideal, Ring) := Ideal => (I,S) -> ( -- this works for arbitrary ideals, but can be very slow
    U := local U;
    d := local d;
    u := local u;
    pmat := local pmat;
    umat := local umat;
    R := ring I;
    n := numgens R;
    varList := for i from 0 to #(gens R) -1 list S_i;
    varList2 := for i from 0 to #(gens R) -1 list 1_S;
    f := map(S,R,varList);
    J := (matrix {varList2}) || transpose(f jacobian(I));
    Q := minors(codim(I),jacobian(I));

    U = reshape(S^n,S^2,matrix{gens S});
    pmat = transpose(matrix{U_0});
    umat = transpose(matrix{U_1});
    Jaug := umat || J*diagonalMatrix(varList);

    H := ideal((sum flatten entries pmat)*(product flatten entries pmat));

    L := saturate(f(I) + minors(codim(I)+2,Jaug),H*(f(Q)));
    I.cache#"LC" = L;
    L)
computeLC(Ideal) := I -> computeLC(I, LCRing(I))
computeLC(NormalToricVariety, Ring) := Ideal => (X,R) -> (
    if member("LC", keys X.cache) then return X.cache#"LC"; -- check if its already been computed
    if member("Graph", keys X.cache) and isJointlyIndependent(X.cache#"Graph") then return computeLCJI(X); 
    p := local p;
    u := local u;
    -- Retrieve matrix A from the cache
    A := X.cache#"mat";
    if A === null then error "Matrix A is not defined in the cache of x";

    numcol := numColumns(A);
    toric := toricIdeal(A, R);
    M := reshape(R^numcol, R^2, matrix({gens R}));
    I := toric + minors(2, A * M);
    pprod := product entries M_0;
    psum := sum entries M_0;
    L := saturate(I, psum); --- need to further saturate by pprod if L ends up not being prime. We conjecture this does not happen.

    -- Cache the computed "LC" value
    X.cache#"LC" = L;

    -- Return the cached "LC" value
    L
)
computeLC(NormalToricVariety) := Ideal => X -> computeLC(X, LCRing(X))


--The following code computes the likelihood correspondence in the case that the ideal comes from a joint independence model
computeLCJI = method()
computeLCJI(NormalToricVariety) := Ideal => X -> (
    G := X.cache#"Graph";
    if isJointlyIndependent(G) != true then error "--expected the graph to be jointly independent";
    p := local p;
    u := local u;

    allStates := states vertices G;

    pvars := for state in allStates list p_(toSequence state);
    uvars := for state in allStates list u_(toSequence state);
    R := KK[pvars, uvars, Degrees => toList(join(#pvars:{1,0}, #uvars:{0,1})) ];

    matrixList := {};
    for c in connectedComponents G do (
        M := {};
        cstates := states c;
        if #c == 1 then cstates = pack(cstates,1); -- make it work for singletons
        cPositions := apply(c, x -> position(vertices G, l -> l === x));
        for cstate in cstates do (
            cRow := select(allStates, state -> state_cPositions == cstate);
            pList := for c in cRow list p_(toSequence c);
            uSum := sum for c in cRow list u_(toSequence c);
            M = M | {pList | {uSum}};
        );
        matrixList = matrixList | {M};
    );

    matrixList = matrixList / matrix;
    I := sum for M in matrixList list minors(2, M);
    X.cache#"LC" = I;
    I
)

LCRing = method()
LCRing(Ideal, Ring) := Ring => (I,R) -> (
    u := local u;
    n := numgens R;
    coefficientRing(R)[gens R, u_0..u_(n-1)]
)
LCRing(Ideal) := I -> LCRing(I, ring I)
LCRing(NormalToricVariety) := X -> ( --I should change the methods that use NormalToricVariety to utilize ring X, and to use ideal X, so that it embeds into the LCRing
    p := local p;
    u := local u;
    A := X.cache#"mat";
    if A === null then error "Matrix A is not defined in the cache of x";

    numcol := numColumns(A);
    X.cache.CoefficientRing[p_0..p_(numcol-1), u_0..u_(numcol-1), Degrees => {numcol:{1,0}, numcol:{0,1}}]
) 

MLdegree = method();
MLdegree(NormalToricVariety) := (X) -> (
    if member("MLD", keys X.cache) then return X.cache#"MLD"; -- check if its already been computed
    if not member("LC", keys X.cache) then (
        computeLC(X);
    );
    curLC := X.cache#"LC";
    
    -- Return the cached "LC" value
    curVars := vars ring curLC;
    numVars := numColumns(curVars);
    numVars2 := numVars//2;
    use ring curLC;
    randu := for i from 0 to (numVars2-1) list random(numVars2*20);
    useVars := for i from 0 to (numVars2-1) list curVars_i_0;
    curR := QQ[useVars];
    useVars3 := for i from 0 to (numVars2-1) list (vars curR)_i_0;
    specmap := map(curR, ring curLC, join(useVars3, randu));
    X.cache#"MLD" = degree (specmap curLC);

    X.cache#"MLD"
);
MLdegree(Ideal) := X -> (
    if member("MLD", keys X.cache) then return X.cache#"MLD"; -- check if its already been computed
    if not member("LC", keys X.cache) then (
        computeLC(X);
    );
    curLC := X.cache#"LC";
    
    -- Return the cached "LC" value
    curVars := vars ring curLC;
    numVars := numColumns(curVars);
    numVars2 := numVars//2;
    use ring curLC;
    randu := for i from 0 to (numVars2-1) list random(numVars2*20);
    useVars := for i from 0 to (numVars2-1) list curVars_i_0;
    curR := QQ[useVars];
    useVars3 := for i from 0 to (numVars2-1) list (vars curR)_i_0;
    specmap := map(curR, ring curLC, join(useVars3, randu));
    X.cache#"MLD" = degree (specmap curLC);

    X.cache#"MLD"
)

--------------------------------------------------------------------
----- Basic constructions
--------------------------------------------------------------------

rationalNormalScroll = method()
rationalNormalScroll List := NormalToricVariety => L -> (
    if all(L, x -> class x === ZZ) != true then error "--expected a list of integers";
    matrixList := {};
    i := 0;
    c := #L;
    for n in L do (
        Alist := {toList(n:1)};
        for k from 1 to i do (
            Alist = Alist | {toList(n:1)};
        );
        for k from 1 to c-i-1 do (
            Alist = Alist | {toList(n:0)};
        );
        Alist = Alist | {toList(1..n)};

        matrixList = matrixList | {Alist};
        i = i + 1;
    );
    A := matrix matrixList_0;
    for i from 1 to #matrixList-1 do (
        A = A | matrix matrixList_i;
    );
    toricModel A
)