open OUnit2
open Gamestate
open Player

let crd1 = {num = 3; suit = Diamond}
let crd2 = {num = 2; suit = Club}
let crd3 = {num = 10; suit = Club}
let crd4 = {num = 13; suit = Heart}
let crd5 = {num = 11; suit = Spade}
let crd6 = {num = 7; suit = Diamond}
let crd7 = {num = 9; suit = Spade}
let crd8 = {num = 9; suit = Heart}
let crd9 = {num = 10; suit = Spade}
let crd10 = {num = 4; suit = Club}
let crd11 = {num = 5; suit = Club}
let crd12 = {num = 2; suit = Club}
let crd13 = {num = 9; suit = Diamond}
let crd14 = {num = 6; suit = Diamond}
let crd15 = {num = 11; suit = Diamond}
let crd16 = {num = 2; suit = Diamond}
let crd17 = {num = 9; suit = Club}
(* straight flush *)
let crd18 = {num = 6; suit = Heart}
let crd19 = {num = 5; suit = Heart}
let crd20 = {num = 4; suit = Heart}
let crd21 = {num = 3; suit = Heart}
let crd22 = {num = 2; suit = Heart}
(* royal flush *)
let crd23 = {num = 10; suit = Heart}
let crd24 = {num = 11; suit = Heart}
let crd25 = {num = 12; suit = Heart}
let crd26 = {num = 13; suit = Heart}
let crd27 = {num = 14; suit = Heart}

let crdList0 = [crd4;crd5;crd3;crd7;crd6;crd1;crd2]
let crdList1 = [crd1;crd2;crd3;crd4;crd5;crd6;crd7]
let crdList2 = [crd7;crd2;crd4;crd3;crd5;crd1;crd6]
let crdList3 = [crd1;crd2;crd3;crd4;crd5]
let crdList4 = [crd1;crd2;crd3;crd4]
let crdList5 = [crd1;crd2;crd3]
let crdList6 = [crd1;crd2]
(* cardlists for pair testing *)
let crdList7 = [crd4;crd5;crd3;crd7;crd8;crd6;crd1]
let crdList8 = [crd7;crd8]
let crdList9 = [crd5;crd3;crd9;crd7;crd6;crd1;crd2]
let crdList10 = [crd3;crd9]
let crdList11 = [crd7;crd8;crd6;crd11;crd10;crd1;crd2]
let crdList12 = [crd7;crd6;crd11;crd10;crd1;crd2;crd12]
(* cardlists for two pair testing *)
let crdList13 = [crd5;crd9;crd3;crd7;crd8;crd1;crd2]
(* cardlists for three of a kind testing *)
let crdList14 = [crd4;crd5;crd13;crd7;crd8;crd6;crd11]
(* cardlists for straight testing *)
let crdList15 = [crd4;crd5;crd14;crd11;crd10;crd1;crd2]
let crdList16 = [crd4;crd6;crd14;crd11;crd10;crd1;crd2]
(* cardlists for flush testing *)
let crdList17 = [crd4;crd5;crd3;crd11;crd10;crd12;crd2]
let crdList18 = [crd4;crd3;crd13;crd6;crd14;crd1;crd2]
let crdList19 = [crd4;crd15;crd13;crd6;crd14;crd1;crd2]
let crdList20 = [crd15;crd13;crd6;crd14;crd1;crd2;crd12]
(* cardlists for fullhouse testing *)
let crdList21 = [crd4;crd5;crd13;crd7;crd8;crd2;crd12]
let crdList22 = [crd5;crd3;crd7;crd8;crd2;crd12;crd16]
let crdList23 = [crd4;crd5;crd3;crd8;crd2;crd12;crd16]
(* cardlists for four of a kind testing *)
let crdList24 = [crd7;crd8;crd13;crd17;crd6;crd11;crd1]
(* cardlists for straight flush testing *)
let crdList25 = [crd5;crd7;crd18;crd19;crd20;crd21;crd22]
let crdList26 = [crd8;crd7;crd18;crd19;crd20;crd21;crd22]
(* cardlists for royal flush testing *)
let crdList27 = [crd27;crd26;crd25;crd24;crd23;crd6;crd14]

let tests =
[
  (* testing calculate score functionaliry *)
  (* getIntList test cases *)
  "getIntlist0" >:: (fun _ -> assert_equal [] (getIntList [] []));
  "getIntlist1" >:: (fun _ -> assert_equal [13;11;10;9;7;3;2] (getIntList crdList1 []));
  "getIntList2" >:: (fun _ -> assert_equal [13;11;10;9;7;3;2] (getIntList crdList2 []));
  "getIntList3" >:: (fun _ -> assert_equal [13;11;10;3;2] (getIntList crdList3 []));
  "getIntList4" >:: (fun _ -> assert_equal [14] (getIntList [crd27] []));
  (* sortCards test cases *)
  "sortCards0" >:: (fun _ -> assert_equal [] (sortCards [] [] []));
  "sortCards1" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd7;crd6;crd1;crd2]
    (sortCards crdList1 (getIntList crdList1 []) []));
  "sortCards2" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd7;crd6;crd1;crd2]
    (sortCards crdList2 (getIntList crdList2 []) []));
  "sortCards3" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd1;crd2]
    (sortCards crdList3 (getIntList crdList3 []) []));
  "sortCards4" >:: (fun _ -> assert_equal crdList0
    (sortCards crdList0 (getIntList crdList0 []) []));
  "sortCards5" >:: (fun _ -> assert_equal [crd27]
                       (sortCards [crd27] (getIntList [crd27] []) []));
  "sortCards" >:: (fun _ -> assert_equal [crd15;crd9;crd6]
                      (sortCards [crd9;crd6;crd15] (getIntList [crd9;crd6;crd15] []) []));
  (* trimList test cases *)
  "trimList1" >:: (fun _ -> assert_equal [crd7;crd6] (trimList crdList1 crdList3 []));
  "trimList2" >:: (fun _ -> assert_equal [crd5;crd7;crd6] (trimList crdList1 crdList4 []));
  "trimList3" >:: (fun _ -> assert_equal [crd5;crd7;crd6] (trimList crdList2 crdList4 []));
  "trimList4" >:: (fun _ -> assert_equal [crd4;crd5;crd7;crd6] (trimList crdList2 crdList5 []));
  "trimList5" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd7;crd6] (trimList crdList1 crdList6 []));
  "trimList6" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd6;crd1] (trimList crdList7 crdList8 []));
  (* maxFive test cases *)
  "maxFive1" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd7;crd6] (maxFive crdList0 [] 0));
  (* findHighCard test cases *)
  "findHC1" >:: (fun _ -> assert_equal [crd4;crd5;crd3;crd7;crd6] (findHighCard crdList0));
  (* findPair test cases *)
  "findPair1" >:: (fun _ -> assert_equal [crd8;crd7;crd4;crd5;crd3] (findPair crdList7 [] crdList7));
  "findPair2" >:: (fun _ -> assert_equal [crd8;crd7;crd6;crd11;crd10] (findPair crdList11 [] crdList11));
  "findPair3" >:: (fun _ -> assert_equal [crd9;crd3;crd5;crd7;crd6] (findPair crdList9 [] crdList9));
  "findPair4" >:: (fun _ -> assert_equal [crd2;crd12;crd7;crd6;crd11] (findPair crdList12 [] crdList12));
  "findPair5" >:: (fun _ -> assert_equal [] (findPair crdList1 [] crdList1));
  (* findTwoPair test cases *)
  "findTwoPair1" >:: (fun _ -> assert_equal [crd3;crd9;crd8;crd7;crd5] (findTwoPair crdList13 [] [] crdList13));
  "findTwoPair2" >:: (fun _ -> assert_equal [] (findTwoPair crdList1 [] [] crdList1));
  "findTwoPair3" >:: (fun _ -> assert_equal [] (findTwoPair crdList12 [] [] crdList12));
  (* findThreeKind test cases *)
  "findThreeKind1" >:: (fun _ -> assert_equal [crd8;crd7;crd13] (findThreeKind crdList14 []));
  "findThreeKind2" >:: (fun _ -> assert_equal [] (findThreeKind crdList13 []));
  "findThreeKind3" >:: (fun _ -> assert_equal [crd16;crd12;crd2] (findThreeKind crdList23 []));
  "findThreeKind4" >:: (fun _ -> assert_equal [crd16;crd12;crd2] (findThreeKind crdList22 []));
  (* findStraight test cases *)
  "findStraight1" >:: (fun _ -> assert_equal [crd14;crd11;crd10;crd1;crd2] (findStraight crdList15 []));
  "findStraight2" >:: (fun _ -> assert_equal [crd6;crd14;crd11;crd10;crd1] (findStraight crdList16 []));
  "findStraight3" >:: (fun _ -> assert_equal [] (findStraight crdList10 []));
  (* findFlush test cases *)
  "findFlush1" >:: (fun _ -> assert_equal [] (findFlush crdList16 [] [] [] []));
  "findFlush2" >:: (fun _ -> assert_equal [crd3;crd11;crd10;crd12;crd2] (findFlush crdList17 [] [] [] []));
  "findFlush3" >:: (fun _ -> assert_equal [] (findFlush crdList18 [] [] [] []));
  "findFlush4" >:: (fun _ -> assert_equal [crd15;crd13;crd6;crd14;crd1] (findFlush crdList19 [] [] [] []));
  "findFlush5" >:: (fun _ -> assert_equal [crd15;crd13;crd6;crd14;crd1] (findFlush crdList20 [] [] [] []));
  (* findFullHouse test cases *)
  "findFullHouse1" >:: (fun _ -> assert_equal [crd8;crd7;crd13;crd2;crd12] (findFullHouse crdList21 []));
  "findFullHouse2" >:: (fun _ -> assert_equal [crd8;crd7;crd16;crd12;crd2] (findFullHouse crdList22 []));
  "findFullHouse3" >:: (fun _ -> assert_equal [] (findFullHouse crdList20 []));
  (* findFourOfAKind test cases *)
  "findFourKind1" >:: (fun _ -> assert_equal [crd17;crd13;crd8;crd7] (findFourKind crdList24 [] crdList24));
  "findFourKind2" >:: (fun _ -> assert_equal [] (findFourKind crdList22 [] crdList22));
  (* findStraightFlush test case *)
  "findSF1" >:: (fun _ -> assert_equal [crd18;crd19;crd20;crd21;crd22] (findStraightFlush crdList25 []));
  "findSF2" >:: (fun _ -> assert_equal [crd18;crd19;crd20;crd21;crd22] (findStraightFlush crdList26 []));
  "findSF3" >:: (fun _ -> assert_equal [] (findStraightFlush crdList16 []));
  (* findRoyalFlush test case *)
  "findRF1" >:: (fun _ -> assert_equal [] (findRoyalFlush crdList26));
  "findRF2" >:: (fun _ -> assert_equal [crd27;crd26;crd25;crd24;crd23] (findRoyalFlush crdList27));

  "findSF10" >:: (fun _ -> assert_equal [crd27;crd26;crd25;crd24;crd23] (findStraightFlush crdList27 []));

  "findStraight3" >:: (fun _ -> assert_equal [crd27;crd26;crd25;crd24;crd23] (findStraight crdList27 []));

  "besthand0" >:: (fun _ -> assert_equal (High_Card, []) (best_hand []));
  "besthand1" >:: (fun _ -> assert_equal (High_Card,[crd27;crd26])
                      (best_hand [crd27;crd26]));
  "besthand2" >:: (fun _ -> assert_equal (Pair, [crd8;crd7])
                      (best_hand [crd7;crd8]));
  "besthand3" >:: (fun _ -> assert_equal (Pair, [crd27;crd27])
                      (best_hand [crd27;crd27]));
]

let suite =
  "Gamestate test suite"
  >::: tests

let _ = run_test_tt_main suite
