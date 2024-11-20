-- test 0
TEST ///  -* making a custom pmf *-
f = new HashTable from {
    1 => 0.2,
    2 => 0.3,
    3 => 0.5
};
g = x -> if f#?x then f#x else 0;

X = discreteRandomVariable 3;
assert isWellDefined X;
X.pmf = g;
sample X
///

-- test 1
TEST /// 
g = new HashTable from {
    1 => 0.5,
    2 => 0.3,
    3 => 0.2
};
f = x -> if g#?x then g#x else 0;
Y = discreteRandomVariable(3, f)
assert isWellDefined Y;
{states Y, mean Y, sample(Y,2), variance Y}
///

-- test 2
TEST ///
R = QQ[p_0,p_1,p_2];
M = ideal(4*p_0*p_2-p_1^2);
L = computeLC(M)
MLdegree(M)
///

-- test 3
TEST ///
R = QQ[p_0,p_1,p_2];
M = ideal(p_0*p_2-(p_0+p_1)*p_1);
P = toricModel(toricPolytope(M))
///

-- test 4
TEST ///
A = matrix{{1,1,1,1},{1,2,3,4}};
X = toricModel(A)
R = QQ[p_0,p_1,p_2,p_3];
M = minors(2, matrix{{p_0,p_1,p_2},{p_1,p_2,p_3}});
toricPolytope(M)
assert(M == toricIdeal(X,R))
///

-- test 5
TEST ///
a = discreteRandomVariable 2;
b = discreteRandomVariable 2;
c = discreteRandomVariable 2;
S = {{a,b}, {b,c}};
L = {a,b,c};
makeLogLinearMatrix(S,L)
///

-- test 6
TEST ///
a = discreteRandomVariable 2;
b = discreteRandomVariable 2;
c = discreteRandomVariable 2;
S = {{a,b}, {b,c}};
L = {a,b,c};
makeLogLinearMatrix(S,L)
///

-- test 7
TEST /// 
a = discreteRandomVariable(2); b = discreteRandomVariable(2); c = discreteRandomVariable(3); d = discreteRandomVariable(3); e = discreteRandomVariable(2); f = discreteRandomVariable(2);
G = graph({a,b,c,d,e,f}, matrix{{0,0,0,0,0,0},{0,0,1,0,0,0},{0,1,0,0,0,0},{0,0,0,0,1,1},{0,0,0,1,0,1},{0,0,0,1,1,0}});
X = toricModel(G);
R = LCRing(X)
L = computeLC(X,R);
///

-- test 8
TEST ///
S = rationalNormalScroll({1,2,3})
R = LCRing(S)
L = computeLC(S,LCRing(S))
assert(MLdegree(S) == 3)
///



