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