(** Test suites for scalable power ml file using oUnit. *)

open OUnit2
open Scalable
open Test_scalable_templates

(*let () =  let t_list = [((from_int (-1), from_int 12), from_int (11) );
                        ((from_int (-1), from_int 11), from_int (10) );
                        ((from_int 0, from_int 2), from_int 2);
                        ((from_int 3, from_int 1), from_int 4);
                        ((from_int 5, from_int 0), from_int 5);
                        ((from_int (-2), from_int 2), from_int (0));
                        ((from_int (2), from_int (-3)), from_int (-1));
                        ((from_int 3, from_int (-5)), from_int (-2));
                        ((from_int 3, from_int 3), from_int 6)]
          in
          run_test template_2_1 "Bitarray Addition Function" add_b t_list
;;*)

let () =  let t_list = [((from_int (-1), from_int 12), from_int (-13) );
                        ((from_int (-1), from_int 11), from_int (-12) );
                        ((from_int 0, from_int 2), from_int (-2) );
                        ((from_int 3, from_int 1), from_int 2);
                        ((from_int 5, from_int 0), from_int 5);
                        ((from_int (-2), from_int 2), from_int (-4));
                        ((from_int (2), from_int (-3)), from_int (5));
                        ((from_int 3, from_int (-5)), from_int (8));
                        ((from_int 3, from_int 3), from_int 0)]
          in
          run_test template_2_1 "Bitarray Difference Function" diff_b t_list
;;

let () =  let t_list = [((from_int (-1), from_int 12), from_int (-12) );
                        ((from_int (-1), from_int 11), from_int (-11) );
                        ((from_int 0, from_int 2), from_int 0);
                        ((from_int 3, from_int 1), from_int 3);
                        ((from_int 5, from_int 0), from_int 0);
                        ((from_int (-2), from_int 2), from_int (-4));
                        ((from_int (2), from_int (-3)), from_int (-6));
                        ((from_int 3, from_int (-5)), from_int (-15));
                        ((from_int 3, from_int 3), from_int 9)]
          in
          run_test template_2_1 "Bitarray multiplication Function" mult_b t_list
;;
	   
let () =  let t_list = [ (from_int (-71), from_int (7) ), (from_int (-11),from_int(6)) ;
			((from_int (32), from_int (16) ), (from_int (2),from_int(0) ) );
			((from_int (33), from_int (16) ), (from_int (2),from_int(1) ) );
			((from_int (16), from_int (-2) ), (from_int (-8),from_int(0) ) );
			((from_int (-55), from_int (-7) ), (from_int (8),from_int(1) ) );
			((from_int (-123456789), from_int (123) ), (from_int (-1003714),from_int(33) ) );]
          in
          run_test template_2_2 "Bitarray Division Function" div_b t_list
;;
	   
