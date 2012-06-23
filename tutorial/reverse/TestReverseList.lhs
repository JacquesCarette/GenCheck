This is the simplest example of using the GenCheck framework 
to test an implementation against a specification.
This module contains a revList (reverse) function
that reverses the order of the elements in a list,
and two properties for this function:
reversing a list twice returns the original list,
and reversing a singleton list has no effect.

\begin{code}
module TestReverseList where

import Test.GenCheck.System.SimpleCheck
\end{code}

The properties that the revList function must satisfy:
\begin{itemize}
\item  propRevUnitEq - the reverse of a singleton list is the original
\item  propRevRevEq  - reversing the reverse is the original list (involutive)
\end{itemize}

Note that the first is a property of individual values and 
the second is a property of lists, so different test suites 
and test case generators are required.

\begin{code}
propRevUnitEq :: (Eq a) => Property a
propRevUnitEq x = (revList [x]) == [x]

propRevRevEq :: (Eq a) => Property [a]
propRevRevEq xs = (revList.revList) xs == xs
\end{code}

The implementation to test.

\begin{code}
revList :: [a] -> [a]
revList xs  = rev xs []
  where rev [] xs' = xs'
        rev (y:ys) xs' = rev ys (y:xs')

\end{code}

To evaluate whether the properties are satisfied,
we need to generate a collection of test cases,
evaluate them and report the results of the test.
To do this, we pick one of the test programs provided by GenCheck,
and the best place to start is the friendly sounding module SimpleCheck.
There are a number of test programs here that provide
variations on how test cases are generated and results are reported.
The name of the test program consists of three parts:

\begin{description}
\item[testsuite]{Describes the kind of test suite generated: std, deep, base }
\item[reporting]{Defines whether the first failure (Check), all failures (Test)
or all results (Report) are printed to the screen}
\item[arguments]{functions ending in Args have additional parameters; 
those not ending in Args provide default values for these parameters
and default test case generators (so must be instances of Testable,
more on this class coming up soon).}
\end{description}

The first property takes a value, puts it in a list,
and tests that reversing the list doesn't change the value.
This value could be of any Haskell type, but we have to
pick one to actually run the tests, so we'll start with Char.
Char is a ``base'' type (as opposed to a structure) so all 
of the elements of this type are the same size.
SimpleCheck's baseTest, baseCheck, and baseReport are all appropriate
as they don't distinguish test elements by size.
We'll pick baseTest so that all of the test cases are evaluated,
but only the failure cases are reported.
Char is an instance of the Testable class, so we don't need to 
use the parameterized versions (baseTestArgs, etc.) and will settle
for the default settings for now.

Note that the GenCheck test programs generally request 
the desired number of test cases as input, but then only
uses that as a guide in developing the test suite.
The actual number of test cases will vary according to
the number of possible test cases in the test domain.

\begin{code}
testUnitEq_1 = baseTest (propRevUnitEq :: Property Char) 100
\end{code}

The next property requires a list of values to reverse,
so baseTest is no longer appropriate as it is only for scalar values.
The test suite must have lists of a variety of different lengths;
in GenCheck parlance the size of a structure is referred to as it's ``rank''.
stdTest and deepTest both generate test suites for structures,
with deepTest including much higher ranked test cases, but fewer at each rank.
For lists, there is only one list structure for each rank,
so we'll use deepTest (deepReport and deepCheck would produce the same test cases).

Setting the property to a list of Ints seems a good starting point,
but in order to use the SimpleCheck test functions we need an instance of Testable.
Is |[Int]| an instance of Testable or do we need to do something?
Lists are instances of the Structure class, which provides the method
to populate a structure with elements from another generator,
and also an instance of Testable, so there are standard generators available.
A default instance of Testable is provided for lists of any Testable type,
and Ints are Testable, so we don't have to worry about how lists are generated - yet.

\begin{code}
testRevIdem_1 = deepTest (propRevRevEq :: Property [Int]) 100
\end{code}

The tests are passing, but somewhat unsatisfying that we can't see the test
cases.  So, we switch to the baseReport / deepReport functions that report all
of the results, flagging any failed test cases with ``FAILED: ''.  These are
the same cases that were used for baseTest / deepTest, so we already know they
will be successful, but using the Report versions will allow an evaluation of
the quality of the test suite.  The report dumps a lot of data onto the screen,
so it made sense to verify the properties worked first with the Test functions
so we didn't miss any of the FAILED flags.

\begin{code}
testUnitEq_2  = baseReport (propRevUnitEq :: Property Char) 100
testRevIdem_2 = deepReport (propRevRevEq :: Property [Int]) 100
\end{code}

What happens when one or more test cases fail?  Let's create a failing property,
such as comparing a reversed list with the original. The very first test case 
with two elements in the list fails.

\begin{code}
propRevEqNot :: (Eq a) => Property [a]
propRevEqNot xs =  revList xs == xs

testRevEqNot_1 =  deepTest (propRevEqNot :: Property [Int]) 100 -- should fail
\end{code}

We also see that *every* other case failed too - maybe we shouldn't print out
all the failures!  This is where using deepCheck comes in handy - it stops
after the first failure case.

\begin{code}
-- isn't implemented yet, so this is redundant
testRevEqNot_2 =  deepCheck (propRevEqNot :: Property [Int]) 100 
\end{code}

The SimpleCheck module provides a useful collection of simple test programs
that start with default test parameters and test case generators, but allow the
tester to make a few simple choices to customize the testing if desired.  The
simpleTest, simpleReport and simpleCheck functions are the glue to connect the
reporting, test execution and test suite options for a test program.
