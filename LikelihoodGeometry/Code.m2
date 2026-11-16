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

    if #discreteRandomVariables == 1 then allStates = pack(allStates,1);

    -- Construct the rows for each generating subset
    matrixList := flatten apply(generatingSubsets, g -> (
        gStates := states g;
        if #g == 1 then gStates = pack(gStates,1);
        gPositions := apply(g, x -> position(discreteRandomVariables, l -> l === x));
        apply(gStates, gState -> apply(allStates, state -> if state_gPositions == gState then 1 else 0))
    ));

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

-- The facets of the clique complex are all maximal cliques, including those of different sizes
findMaximalCliques(Graph) := List => G -> (
    if #vertices G == 0 then return {};
    F := apply(facets cliqueComplex G, m -> positions(first exponents m, e -> e != 0));
    apply(sort(F, f -> {-#f,f}), f -> (vertices G)_f)
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
toricModel Matrix := NormalToricVariety => opts -> vertices -> (
    X := normalToricVariety(vertices, opts);
    X.cache#"mat" = vertices;
    X
)

toricModel Graph := NormalToricVariety => opts -> G -> (
    A := makeLogLinearMatrix G;
    X := normalToricVariety(A, opts);
    X.cache#"mat" = A;
    X.cache#"Graph" = G;
    X
)

toricModel Ideal := NormalToricVariety => opts -> I -> (
-- does not check if ideal is toric!!
    A := toricPolytope I;
    X := normalToricVariety(A, opts);
    X.cache#"mat" = A;
    X
)

-- Extend the toricIdeal method exported by Quasidegrees with model constructors.
toricIdeal(Matrix) := Ideal => A -> (
    numcol := numColumns(A);
    p := local p;
    R := KK[p_0..p_(numcol-1)];
    toricIdeal(A,R)
)
toricIdeal(NormalToricVariety, Ring) := Ideal => (X,R) -> (
    A := X.cache#"mat";
    toricIdeal(A,R)
)
toricIdeal(NormalToricVariety) := Ideal => X -> (
    A := X.cache#"mat";
    numcol := numColumns(A);
    p := local p;
    R := X.cache.CoefficientRing[p_0..p_(numcol-1)];
    toricIdeal(A, R)
)
toricIdeal(Graph) := Ideal => G -> toricIdeal(toricModel G)

--------------------------------------------------------------------
----- Basic features of the DiscreteRandomVariable datatype
--------------------------------------------------------------------
DiscreteRandomVariable = new Type of MutableHashTable
DiscreteRandomVariable.synonym = "discrete random variable"
DiscreteRandomVariable.GlobalAssignHook = globalAssignFunction
DiscreteRandomVariable.GlobalReleaseHook = globalReleaseFunction

arity = method()
arity DiscreteRandomVariable := ZZ => X -> X.arity

expression DiscreteRandomVariable := Thing => X -> (
    if hasAttribute (X, ReverseDictionary) 
    then expression getAttribute (X, ReverseDictionary) else 
    (describe X)
)
texMath DiscreteRandomVariable := String => X -> texMath expression X
describe DiscreteRandomVariable := Net => X -> (
    myOutput := {net "arity => " | net X.arity};
    horizontalJoin flatten ("{", stack myOutput, "}")
)
toString DiscreteRandomVariable := String => X -> toString expression X

net DiscreteRandomVariable := Thing => net @@ expression

discreteRandomVariable = method (
    TypicalValue => DiscreteRandomVariable
)

discreteRandomVariable(ZZ, FunctionClosure) := DiscreteRandomVariable => (d,f) -> (
    if d < 1 then error "--expected a positive integer";
    new DiscreteRandomVariable from {
        symbol arity => d,
        symbol cache => new CacheTable,
        symbol pmf => f
    }
)
discreteRandomVariable ZZ := DiscreteRandomVariable => d -> (
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
    sort(toList(combinations))
)

sample = method()
sample DiscreteRandomVariable := ZZ => X -> (
        cdf := accumulate(plus, 0, apply(states X, i -> X.pmf(i)));
        randomRR := random(0.0,1.0);
        return position(cdf, x -> x > randomRR)+1
)
sample(DiscreteRandomVariable, ZZ) := List => (X, n) -> (
    for i in 1..n list sample X
)
sample List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    apply(L, x -> sample x)
)
sample(List, ZZ) := List => (L, n) -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    for i in 1..n list sample L
)

mean = method()
mean DiscreteRandomVariable := Number => X -> (
    pmfs := apply(states X, i -> X.pmf(i));
    sum(pack(2,mingle(pmfs, states X)) / product)
)

mean List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    apply(L, x -> mean x)
)

variance = method()
variance DiscreteRandomVariable := Number => X -> (
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
computeLC(Ideal, Ring) := Ideal => (I,S) -> ( -- Algorithm 6 of Hosten--Khetan--Sturmfels for a homogeneous prime ideal
    R := ring I;
    n := numgens R;
    if numgens S != 2*n then error "--expected a ring with probability variables followed by data variables";
    pvars := take(gens S,n);
    uvars := drop(gens S,n);
    f := map(S,R,pvars);
    Q := minors(codim I,jacobian I);
    J := (matrix{toList(n:1_R)} || transpose jacobian I)*diagonalMatrix(gens R);

    -- Compute the kernel over the coordinate ring, then lift its generators to R
    K := lift(syz sub(J,R/I),R);
    H := ideal((sum pvars)*(product pvars));
    L := saturate(f(I) + ideal(matrix{uvars}*f(K)),H*f(Q));
    I.cache#"LC" = L;
    L)
computeLC(Ideal) := Ideal => I -> computeLC(I, LCRing(I))
computeLC(NormalToricVariety, Ring) := Ideal => (X,R) -> (
    if member("LC", keys X.cache) and ring(X.cache#"LC") === R then return X.cache#"LC";
    if member("Graph", keys X.cache) and isJointlyIndependent(X.cache#"Graph") then return computeLCJI(X,R);
    p := local p;
    u := local u;
    -- Retrieve matrix A from the cache
    A := X.cache#"mat";
    if A === null then error "Matrix A is not defined in the cache of x";

    numcol := numColumns(A);
    if numgens R != 2*numcol then error "--expected a ring with probability variables followed by data variables";
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
computeLC(NormalToricVariety) := Ideal => X -> (
    if member("LC", keys X.cache) then X.cache#"LC" else computeLC(X, LCRing(X))
)


--The following code computes the likelihood correspondence in the case that the ideal comes from a joint independence model
computeLCJI = method()
computeLCJI(NormalToricVariety, Ring) := Ideal => (X,R) -> (
    G := X.cache#"Graph";
    if isJointlyIndependent(G) != true then error "--expected the graph to be jointly independent";
    allStates := states vertices G;
    if #vertices G == 1 then allStates = pack(allStates,1);
    n := #allStates;
    if numgens R != 2*n then error "--expected a ring with probability variables followed by data variables";

    matrixList := apply(connectedComponents G, c -> (
        cstates := states c;
        if #c == 1 then cstates = pack(cstates,1);
        cPositions := apply(c, x -> position(vertices G, l -> l === x));
        matrix apply(cstates, cstate -> (
            cRow := select(toList(0..n-1), i -> allStates_i_cPositions == cstate);
            pList := apply(cRow, i -> R_i);
            uSum := sum apply(cRow, i -> R_(n+i));
            pList | {uSum}
        ))
    ));

    I := sum for M in matrixList list minors(2, M);
    X.cache#"LC" = I;
    I
)

LCRing = method()
LCRing(Ideal, Ring) := Ring => (I,R) -> (
    u := local u;
    n := numgens R;
    coefficientRing(R)[gens R, u_0..u_(n-1), MonomialOrder => Lex]
)
LCRing(Ideal) := Ring => I -> LCRing(I, ring I)
LCRing(NormalToricVariety) := Ring => X -> (
    p := local p;
    u := local u;
    A := X.cache#"mat";
    if A === null then error "Matrix A is not defined in the cache of x";

    numcol := numColumns(A);
    X.cache.CoefficientRing[p_0..p_(numcol-1), u_0..u_(numcol-1), Degrees => {numcol:{1,0}, numcol:{0,1}}, MonomialOrder => Lex]
) 

MLdegree = method();
MLdegree(NormalToricVariety) := ZZ => (X) -> (
    -- Joint independence models have a unique generic maximum likelihood estimate
    if member("Graph", keys X.cache) and isJointlyIndependent(X.cache#"Graph") then (
        X.cache#"MLD" = 1;
        return 1
    );
    if member("MLD", keys X.cache) then return X.cache#"MLD"; -- check if its already been computed
    if not member("LC", keys X.cache) then (
        computeLC(X);
    );
    curLC := X.cache#"LC";
    
    -- Return the cached "LC" value
    numVars2 := numgens(ring curLC)//2;
    use ring curLC;
    randu := for i from 0 to (numVars2-1) list random(numVars2*20);
    useVars := take(gens ring curLC,numVars2);
    curR := QQ[useVars];
    useVars3 := gens curR;
    specmap := map(curR, ring curLC, join(useVars3, randu));
    X.cache#"MLD" = degree (specmap curLC);

    X.cache#"MLD"
);
MLdegree(Ideal) := ZZ => X -> (
    if member("MLD", keys X.cache) then return X.cache#"MLD"; -- check if its already been computed
    if not member("LC", keys X.cache) then (
        computeLC(X);
    );
    curLC := X.cache#"LC";
    
    -- Return the cached "LC" value
    numVars2 := numgens(ring curLC)//2;
    use ring curLC;
    randu := for i from 0 to (numVars2-1) list random(numVars2*20);
    useVars := take(gens ring curLC,numVars2);
    curR := QQ[useVars];
    useVars3 := gens curR;
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