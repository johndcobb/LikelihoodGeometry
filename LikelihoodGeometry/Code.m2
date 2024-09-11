
--- kludge to access parts of the 'Core'
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";

--------------------------------------------------------------------
----- Internal helper functions
--------------------------------------------------------------------

-- Function to generate all states of a list of discrete random variables given a list of arities
cartesianProd = (A, B) -> (A**B);
generateStates = (arities) -> (
    -- Generate the list of sets of all values each variable can take
    valueSets = apply(arities, a -> set (1..a));
    -- Compute the cartesian product of the value lists
    combinations = fold(cartesianProd,valueSets) / deepSplice / toList;
    rsort(toList(combinations))
);

makeLogLinearMatrix = method()
makeLogLinearMatrix(List, List) := Matrix => (generatingSubsets, discreteRandomVariables) -> (
    -- expecting the GeneratingSubsets to be a list containing sets of integers which index discreteRandomVariables
    -- expecting discreteRandomVariables to be a list of numbers which describe the arity of the random variables -- e.g. (2,2,2,2) for 4 binary random variables

    allStates = generateStates(discreteRandomVariables);
    matrixList = {};
    for g in generatingSubsets do (
        gStates = generateStates(discreteRandomVariables_g);
        for gState in gStates do (
            gRow = apply(apply(allStates, state -> state_g == gState), b -> if b then 1 else 0);
            matrixList = matrixList | {gRow};
        );
    );
    matrix matrixList
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

-- So far, this is literally the same as NormalToricVariety
