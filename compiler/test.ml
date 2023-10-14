let%test "rev" =
  List.equal Int.equal (List.rev [ 3; 2; 12 ]) [ 1; 2; 3 ]4