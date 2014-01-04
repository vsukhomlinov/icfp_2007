import Test.HUnit
import Pattern

--[!0,([!4536645,([!800]),!2971964])]
--test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1, 3))
--test2 = TestCase (assertEqual "for (flatten [])," [] (flatten []::[FlatModel]))

--test3 = TestCase (assertEqual "addChar," [Pattern.SEQ_END "ICFP"] (addChar 'I' [Pattern.CHAR_SEQ "CFP"]))

tests = TestList [test1, test2, test3, test4]