
-*
doc ///
  Key
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
  Caveat
  SeeAlso
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
  Item
Description
  Text
  Example
  CannedExample
  Code
  Pre
ExampleFiles
Contributors
References
Caveat
SeeAlso
///
*-

undocumented {
  (describe, DiscreteRandomVariable),
  (texMath, DiscreteRandomVariable),
  (expression, DiscreteRandomVariable),
  (net, DiscreteRandomVariable),
  (toString, DiscreteRandomVariable),
  (isWellDefined, DiscreteRandomVariable),
  fromBinomial
}


doc ///
Key
  LikelihoodGeometry
Headline
  Methods for computing likelihood geometry of discrete statistical models
Description
  Text
    This package provides methods for computing the likelihood geometry of discrete statistical models. It includes functions for constructing toric models from undirected graphical models, computing the likelihood correspondence, and finding the maximum likelihood degree of a toric model, as described in @HREF"https://arxiv.org/abs/2312.08501"@.
  Text
    The package also includes functions for constructing rational normal scrolls and hierarchical log-linear models as toric varieties. It provides methods for sampling from discrete random variables, computing their mean, and finding their possible states.
///

doc ///
Key 
  LCRing
  (LCRing, NormalToricVariety)
  (LCRing, Ideal)
  (LCRing, Ideal, Ring)
Headline
  Constructs the ambient ring of the likelihood correspondence
Usage 
  LCRing(X)
  LCRing(I)
Inputs
  X: NormalToricVariety
  I: Ideal
    the vanishing ideal of a toric model
Outputs
  R: Ring
    the ambient ring of the likelihood correspondence
Description
  Text
    The function LCRing constructs the ambient ring of the likelihood correspondence of a toric model $\mathcal{M}\subseteq \mathbb{P}^n_p$, i.e. the coordinate ring of $\mathbb{P}^n_p \times \mathbb{P}^n_u$. It chooses $p_0\dots p_n$ and $u_0 \dots u_n$ as the coordinates of $\mathbb{P}^n_p$ and $\mathbb{P}^n_u$, respectively.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    G = graph{{a,b}};
    X = toricModel G;
    LCRing(X)
SeeAlso
  toricModel
  computeLC
///

doc ///
Key
  computeLC
  (computeLC, NormalToricVariety)
  (computeLC, NormalToricVariety, Ring)
  (computeLC, Ideal)
  (computeLC, Ideal, Ring)
Headline
  Computes the likelihood correspondence of a toric model
Usage
  computeLC(X)
  computeLC(I)
Inputs
  X: NormalToricVariety
  I: Ideal
    the vanishing ideal of a toric model
Outputs
  L: Ideal
    the likelihood correspondence of the toric model
Description
  Text
    The function computeLC computes the likelihood correspondence of a toric model $\mathcal{M}\subseteq \mathbb{P}^n_p$, i.e. the vanishing ideal of the likelihood correspondence of $\mathcal{M}$.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    G = graph{{a,b}};
    X = toricModel G;
    computeLC(X)
  Text 
    computeLC also works more generally using the vanishing ideal of any discrete statistical model using a slower algorithm based off of Lagrange multipliers, see @HREF"https://arxiv.org/abs/math/0408270"@.
  Example
    R := QQ[x,y,z,w];
    I := ideal(x*z-y*w);
    computeLC(I)
SeeAlso
  LCRing
  toricModel
///

doc ///
Key 
  toricModel
  (toricModel, Graph)
  (toricModel, Matrix)
  (toricModel, Ideal)
  [toricModel, CoefficientRing]
  [toricModel, MinimalGenerators]
  [toricModel, Variable]
Headline  
  Constructs a toric model from a graph or a matrix
Usage
  toricModel(G)
  toricModel(A)
  toricModel(I)
Inputs
  G: Graph
    a graph representing an undirected graphical model, with vertices defined by @TO DiscreteRandomVariable@s.
  A: Matrix
    the defining matrix of the toric variety of the toric model; each column is the lattice vartex of the polytope.
  I: Ideal
    the vanishing ideal of a toric model
  MinimalGenerators => Boolean 
	    that specifies whether to compute minimal generators
  Variable => Symbol 
	    that specifies the @TO2(baseName, "base name")@ for the indexed
	    variables in the total coordinate ring
  CoefficientRing => Ring 
	    that specifies the coefficient ring of the 
	    @TO2((ring, NormalToricVariety), "total coordinate ring")@
Outputs
  X: NormalToricVariety
    the toric model defined by the graph or matrix
Description
  Text
    The function toricModel constructs a toric model from an undirected graphical models or a matrix where each column is the lattice vertex of the polytope of the toric model. 
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    G = graph{{a,b}};
    toricModel G
Caveat
  This method does not check if the ideal is a toric ideal.
SeeAlso
  computeLC
  LCRing
  toricIdeal
///

doc ///
Key
  discreteRandomVariable
  (discreteRandomVariable, ZZ)
  (discreteRandomVariable, ZZ, FunctionClosure)
Headline
  Constructs a discrete random variable
Usage
  discreteRandomVariable(n)
  discreteRandomVariable(n, p)
Inputs
  n: ZZ
    the number of states of the discrete random variable
  p: FunctionClosure
    a probability mass function of the discrete random variable
Outputs
  X: DiscreteRandomVariable
    the discrete random variable with $n$ states
Description
  Text
    The function discreteRandomVariable constructs a discrete random variable with $n$ states with probability mass function $p$. If $p$ is not provided, the probability mass function is uniform.
  Example
    discreteRandomVariable 2
SeeAlso
  (states, DiscreteRandomVariable)
  (sample, DiscreteRandomVariable)
  (mean, DiscreteRandomVariable)
///

doc ///
Key
  sample
  (sample, DiscreteRandomVariable)
  (sample, DiscreteRandomVariable, ZZ)
  (sample, List)
  (sample, List, ZZ)
Headline
  Take samples from a discrete random variable
Usage
  sample(X)
  sample(X, n)
  sample(L)
  sample(L, n)
Inputs
  X: DiscreteRandomVariable
  L: List
    a discrete random variable or a list of discrete random variables
  n: ZZ
    the number of samples to take
Outputs
  S: ZZ
Description
  Text
    The function sample takes samples from a discrete random variable or a list of discrete random variables. This is done according to the probability mass function attached to the discrete random variables, which is uniform by default. Adding on an integer $n$ will sample the discrete random variable $n$ times.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    sample(a)
    sample({a,b}, 5)
SeeAlso
  discreteRandomVariable
  (states, DiscreteRandomVariable)
  (mean, DiscreteRandomVariable)
///

doc ///
Key 
  mean
  (mean, DiscreteRandomVariable)
  (mean, List)
Headline
  Compute the mean of discrete random variables
Usage
  mean(X)
  mean(L)
Inputs
  X: DiscreteRandomVariable
  L: List
    a list of discrete random variables
Outputs
  m: RR
    the mean of the discrete random variable(s)
Description
  Text
    The function mean computes the mean of a @TO DiscreteRandomVariable@. Applying it to a list of discrete random variables will find the mean of each one separately. This is done according to the probability mass function attached to the discrete random variables, which is uniform by default.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    mean(a)
    mean({a,b})
SeeAlso
  discreteRandomVariable
  (states, DiscreteRandomVariable)
  (sample, DiscreteRandomVariable)
///

doc ///
Key 
  states
  (states, DiscreteRandomVariable)
  (states, List)
Headline
  Get a list of possible states of discrete random variables
Usage
  states(X)
  states(L)
Inputs
  X: DiscreteRandomVariable
  L: List
    a list of discrete random variables
Outputs
  S: List
    a list of possible states of the discrete random variable(s)
Description
  Text
    The function states returns a list of possible states of a discrete random variable. Applying it to a list of discrete random variables returns all possible joint states of the variables.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    states(b)
    states({a,b})
SeeAlso
  discreteRandomVariable
  (mean, DiscreteRandomVariable)
  (sample, DiscreteRandomVariable)
///

doc ///
Key
  rationalNormalScroll
  (rationalNormalScroll, List)
Headline 
  Constructs a rational normal scroll
Usage
  rationalNormalScroll(L)
Inputs
  L: List
    a list of integers specifying the type of rational normal scroll
Outputs
  X: NormalToricVariety
    the rational normal scroll defined by the list of integers
Description
  Text
    From $L=\{a_0,\dots, a_k}$, the function rationalNormalScroll constructs the rational normal scroll $S(a_0,\dots, a_k) \subseteq \mathbb{P}^N$ where $N=\sum_{i=0}^k (a_i) + k$. 
  Example
    rationalNormalScroll {2,3}
SeeAlso
  toricModel
///


doc ///
Key
  DiscreteRandomVariable
Headline
  the class of all discrete random variables
Description
  Text
    A discrete random variable $X$ is a random variable with a finite number of states. It carries $\texttt{arity}$ and $\texttt{pmf}$, which are the number of states and the probability mass function of $X$, respectively.
SeeAlso
  discreteRandomVariable
  (states, DiscreteRandomVariable)
  (sample, DiscreteRandomVariable)
  (mean, DiscreteRandomVariable)
///

doc ///
Key 
  makeLogLinearMatrix
  (makeLogLinearMatrix, Graph)
  (makeLogLinearMatrix, List, List)
Headline
  Constructs the defining matrix of a hierarchical log-linear model as a toric variety
Usage
  makeLogLinearMatrix(G)
  makeLogLinearMatrix(S, L)
Inputs
  G: Graph
    a graph representing a undirected graphical model
  S: List
    a set of subsets of L, the generators of the model
  L: List
    a list of discrete random variables
Outputs
  A: Matrix
    the defining matrix of the hierarchical log-linear model as a toric model
Description
  Text
    Hierarchical log--linear models are a class of toric models. Let $\mc{X} = [d_1] \times \cdots \times [d_n]$ denote the joint state space of the discrete random variables $X_1,\dots, X_n$. A \textit{hierarchical log--linear model} (or simply \textit{log--linear model}) is defined by a collection $S = \{G_1,\dots, G_g\}$ of non-empty subsets of $L = \{X_1,\dots, X_n\}$ called \textit{generators}. This function takes these two inputs and constructs the defining matrix of the hierarchical log--linear model as a toric variety, where the columns are the lattice vertices of the polytope of the toric variety.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 2; c = discreteRandomVariable 2;
    S = {{a,b}, {b,c}};
    L = {a,b,c};
    makeLogLinearMatrix(S,L)
  Text
    One can also pass in a graph $G$ to give the defining matrix for the corresponding undirected graphical model, which is the log--linear model on $X$ in which the generators are cliques (maximal complete subgraphs) of $\mc{G}$.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    G = graph{{a,b}};
    makeLogLinearMatrix(G)
SeeAlso
  toricModel
///

doc ///
Key
  MLdegree
  (MLdegree, NormalToricVariety)
  (MLdegree, Ideal)
Headline
  Computes the maximum likelihood degree of a toric model
Usage
  MLdegree(X)
Inputs
  X: NormalToricVariety
Outputs
  d: ZZ
    the maximum likelihood degree of the toric model
Description
  Text
    The function MLdegree computes the maximum likelihood degree of a toric model $\mathcal{M}\subseteq \mathbb{P}^n_p$. 
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    G = graph{{a,b}};
    X = toricModel G;
    MLdegree(X)
SeeAlso
  toricModel
///

doc ///
Key
  toricIdeal
  (toricIdeal, Matrix, Ring)
  (toricIdeal, Matrix)
  (toricIdeal, NormalToricVariety)
  (toricIdeal, NormalToricVariety, Ring)
  (toricIdeal, Graph)
Headline
  Constructs the vanishing ideal of a toric model
Usage
  toricIdeal(A, R)
  toricIdeal(A)
  toricIdeal(X)
  toricIdeal(G)
Inputs
  A: Matrix
    the defining matrix of the toric variety of the toric model; each column is the lattice vartex of the polytope.
  R: Ring
    the ambient ring of the toric model
  X: NormalToricVariety
    the toric model
  G: Graph
    a graph representing an undirected graphical model
Outputs
  I: Ideal
    the vanishing ideal of the toric model
Description
  Text
    The function toricIdeal constructs the vanishing ideal of a toric model $\mathcal{M}\subseteq \mathbb{P}^n_p$ from the defining matrix $A$ and the coefficient ring $R$. It also works with the toric model $X$ or the graph $G$ representing the undirected graphical model.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    G = graph{{a,b}};
    X = toricModel G;
    toricIdeal(X)
SeeAlso
  toricModel
  toricPolytope
///

doc /// 
Key
  toricPolytope
  (toricPolytope, Ideal)
Headline
  Constructs the toric polytope of a toric ideal (as a matrix)
Usage
  toricPolytope(I)
Inputs
  I: Ideal
    the vanishing ideal of a toric model
Outputs
  A: Matrix
    the toric polytope of the toric ideal
Description
  Text
    The function toricPolytope constructs the toric polytope of a toric ideal $\mathcal{I}$, i.e. the matrix whose columns are the lattice vertices of the polytope of $\mathcal{I}$.
  Example
    R = QQ[p_0,p_1,p_2];
    M = ideal(4*p_0*p_2-p_1^2);
    A = toricPolytope(M)
SeeAlso
  toricIdeal
  toricModel
///

doc ///
Key
  variance
  (variance, DiscreteRandomVariable)
  (variance, List)
Headline
  Compute the variance of discrete random variables
Usage
  variance(X)
  variance(L)
Inputs
  X: DiscreteRandomVariable
  L: List
    a list of discrete random variables
Outputs
  v: RR
    the variance of the discrete random variable(s)
Description
  Text
    The function variance computes the variance of a @TO DiscreteRandomVariable@. Applying it to a list of discrete random variables will find the variance of each one separately. This is done according to the probability mass function attached to the discrete random variables, which is uniform by default.
  Example
    a = discreteRandomVariable 2; b = discreteRandomVariable 3;
    variance(a)
    variance({a,b})
SeeAlso
  discreteRandomVariable
  (states, DiscreteRandomVariable)
  (sample, DiscreteRandomVariable)
  (mean, DiscreteRandomVariable)
///

