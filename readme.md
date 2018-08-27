# official scala language tutorial
https://docs.scala-lang.org/tour/tour-of-scala.html

Key concepts need to be understood

## variances

### co-variant
class AnyClazz[+A] 
// A covariant class implies that for two types A and B where A is a subtype (child type) of B, 
then AnyClazz[A] is a subtype of AnyClazz[B]. 

### contra-variant
class AnyClazz[-A] 
// A contravariant class implies that for two types A and B where A is a subtype of B, 
then AnyClazz[B] is a subtype of AnyClazz[A].

### in-variant
class Baz[A]  // An invariant class

## upper type bounds


## lower type bounds
