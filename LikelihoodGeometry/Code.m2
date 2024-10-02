
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
    allStates = states discreteRandomVariables;

    -- Initialize an empty list to store the rows of the log-linear matrix
    matrixList = {};

    -- Iterate over each generating subset
    for g in generatingSubsets do (
        -- Generate all possible states for the current subset of discrete random variables
        gStates = states g;
        gPositions = apply(g, x -> position(discreteRandomVariables, l -> l === x)); -- this exchanges g for their positions within the original list of random variables
        
        -- Iterate over each state of the current subset
        for gState in gStates do (
            -- Create a row for the log-linear matrix where each entry is 1 if the state matches the current subset state, otherwise 0
            gRow = apply(apply(allStates, state -> state_gPositions == gState), b -> if b then 1 else 0);
            
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
    possibleCliques = subsets(vertices G, cliqueNumber G);

    -- Initialize an empty list to store the cliques
    Cliques = {};

    -- Iterate over all possible cliques
    for possibleClique in possibleCliques do (
        -- Check if the current subset of vertices forms a clique
        if isClique(G, possibleClique) then (
            -- If it is a clique, add it to the list of cliques
            Cliques = Cliques | {possibleClique};
        );
    );

    -- Find vertices that are not part of any clique found so far
    remainingUnmatchedVertices = vertices G - union(Cliques / set);

    -- If there are unmatched vertices, recursively find cliques in the induced subgraph
    if #remainingUnmatchedVertices != 0 then (
        Cliques = Cliques | findMaximalCliques(inducedSubgraph(G, remainingUnmatchedVertices));
    );

    -- Return the list of cliques
    Cliques
)


--------------------------------------------------------------------
----- Basic features of the ToricModels datatype
--------------------------------------------------------------------
ToricModel = new Type of NormalToricVariety
ToricModel.synonym = "toric model"
ToricModel.GlobalAssignHook = globalAssignFunction
ToricModel.GlobalReleaseHook = globalReleaseFunction
expression ToricModel := X -> if hasAttribute (X, ReverseDictionary) 
    then expression getAttribute (X, ReverseDictionary) else 
    (describe X)#0
texMath ToricModel := X -> texMath expression X
describe ToricModel := X -> Describe (expression toricVariety) (
    expression rays X, expression max X)

toricModel = method (
    TypicalValue => ToricModel,
    Options => {
    	CoefficientRing   => KK,
    	MinimalGenerators => false,
    	Variable          => getSymbol "x"
	}
    )
toricModel Matrix := opts -> vertices -> (
    new ToricModel from normalToricVariety vertices
)
toricModel Graph := opts -> G -> (
    X := new ToricModel from normalToricVariety makeLogLinearMatrix(G);
    X.cache.Graph = G;
    X
)


-- So far, this is literally the same as NormalToricVariety

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
    TypicalValue => DiscreteRandomVariable, 
    Options => {
    	Variable => getSymbol "X"
    }
)

discreteRandomVariable ZZ := opts -> d -> (
    if d < 1 then error "--expected a positive integer";
    new DiscreteRandomVariable from {
        symbol arity => d,
        symbol cache => new CacheTable,
        symbol pmf => "Uniform"
    }
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
true)
-- make sure pmf is probability density function
-- swap out "Uniform" text with just uniform distribution
-- make functions that display mean and variance and whatnot.

states = method()
states DiscreteRandomVariable := List => X -> toList(1..X.arity)
states List := List => L -> (
    if all(L, x -> class x === DiscreteRandomVariable) != true then error "--expected a list of DiscreteRandomVariables";
    valueSets := apply(L, x -> set states x);
    combinations := fold(cartesianProd,valueSets) / deepSplice / toList;
    rsort(toList(combinations))
)

sample = method()
sample DiscreteRandomVariable := ZZ => X -> (
    if X.pmf === "Uniform" then (
        return random(1,X.arity)
    )
    else (
        cdf := accumulate(plus, 0, apply(states X, i -> X.pmf(i)));
        randomRR := random(0.0,1.0);
        return position(cdf, x -> x > randomRR)
    );
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