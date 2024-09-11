
--- kludge to access parts of the 'Core'
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";

--------------------------------------------------------------------
----- Internal helper functions
--------------------------------------------------------------------

-- Function to generate all states of a list of discrete random variables given a list of arities
generateStates = (arities) -> (
    cartesianProd = (A, B) -> (A**B);
    -- Generate the list of sets of all values each variable can take
    valueSets = apply(arities, a -> set (1..a));
    -- Compute the cartesian product of the value lists
    combinations = fold(cartesianProd,valueSets) / deepSplice / toList;
    rsort(toList(combinations))
);



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
