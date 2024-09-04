
--- kludge to access parts of the 'Core'
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";
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
