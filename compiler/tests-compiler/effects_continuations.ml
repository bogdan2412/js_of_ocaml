(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Util

let%expect_test "test-compiler/lib-effects/test1.ml" =
  let code =
    compile_and_parse
      ~effects:true
      {|
         let exceptions s =
           (* Compiled using 'try ... catch',
              and 'throw' within the try block *)
           let n = try int_of_string s with Failure _ -> 0 in
           let m =
             try if s = "" then raise Not_found else 7 with Not_found -> 0 in
           (* Uses caml_{push,pop}_trap. *)
           try
             if s = "" then raise Not_found;
             Some (open_in "toto", n, m)
            with Not_found ->
             None

         (* Conditional whose result is used *)
         let cond1 b =
           let ic = if b then open_in "toto" else open_in "titi" in
           (ic , 7)

         (* Conditional whose result is not used *)
         let cond2 b =
           if b then Format.eprintf "toto" else Format.eprintf "toto";
           7

         (* A dummy argument is used to call the continuation in the
            [then] clause *)
         let cond3 b =
           let x= ref 0 in if b then x := 1 else Format.eprintf "toto";
           !x

         (* Two continuation functions are created. One to bind [ic] before
            entering the loop, and one for the loop. We use a dummy argument
            to go back to the begining of the loop if [b] is false *)
         let loop1 b =
           let all = ref [] in
           let ic = open_in "/static/examples.ml" in
           while true do
             let line = input_line ic in
             all := line :: !all;
             if b then prerr_endline line
           done

         (* There is a single continuation for the loop since the result of
            [Format.eprintf] is ignored. *)
         let loop2 () =
           let all = ref [] in
           let ic = open_in "/static/examples.ml" in
           Format.eprintf "titi";
           while true do
             let line = input_line ic in
             all := line :: !all;
             prerr_endline line
           done
       |}
  in
  print_fun_decl code (Some "exceptions");
  print_fun_decl code (Some "cond1");
  print_fun_decl code (Some "cond2");
  print_fun_decl code (Some "cond3");
  print_fun_decl code (Some "loop1");
  print_fun_decl code (Some "loop2");
  [%expect
    {|

    function exceptions(s,cont)
     {try
       {var _x_=runtime.caml_int_of_string(s),n=_x_}
      catch(_B_)
       {_B_ = caml_wrap_exception(_B_);
        if(_B_[1] !== Stdlib[7])
         {var raise$1=caml_pop_trap();return caml_cps_exact_call1(raise$1,_B_)}
        var n=0,_s_=0}
      try
       {if(caml_string_equal(s,cst$0))throw Stdlib[8];var _w_=7,m=_w_}
      catch(_A_)
       {_A_ = caml_wrap_exception(_A_);
        if(_A_ !== Stdlib[8])
         {var raise$0=caml_pop_trap();return caml_cps_exact_call1(raise$0,_A_)}
        var m=0,_t_=0}
      runtime.caml_push_trap
       (function(_z_)
         {if(_z_ === Stdlib[8])return caml_cps_exact_call1(cont,0);
          var raise=caml_pop_trap();
          return caml_cps_exact_call1(raise,_z_)});
      if(caml_string_equal(s,cst))
       {var _u_=Stdlib[8],raise=caml_pop_trap();
        return caml_cps_exact_call1(raise,_u_)}
      var _v_=Stdlib[79];
      return caml_cps_call2
              (_v_,
               cst_toto,
               function(_y_)
                {caml_pop_trap();
                 return caml_cps_exact_call1(cont,[0,[0,_y_,n,m]])})}
    //end
    function cond1(b,cont)
     {function _r_(ic){return caml_cps_exact_call1(cont,[0,ic,7])}
      return b
              ?caml_cps_call2(Stdlib[79],cst_toto$0,_r_)
              :caml_cps_call2(Stdlib[79],cst_titi,_r_)}
    //end
    function cond2(b,cont)
     {function _p_(_q_){return caml_cps_exact_call1(cont,7)}
      return b
              ?caml_cps_call2(Stdlib_Format[137],_a_,_p_)
              :caml_cps_call2(Stdlib_Format[137],_b_,_p_)}
    //end
    function cond3(b,cont)
     {var x=[0,0];
      function _n_(_o_){return caml_cps_exact_call1(cont,x[1])}
      return b
              ?(x[1] = 1,caml_cps_exact_call1(_n_,0))
              :caml_cps_call2(Stdlib_Format[137],_c_,_n_)}
    //end
    function loop1(b,cont)
     {var all=[0,0],_j_=Stdlib[79];
      return caml_cps_call2
              (_j_,
               cst_static_examples_ml,
               function(ic)
                {function _k_(_m_)
                  {var _l_=Stdlib[83];
                   return caml_cps_call2
                           (_l_,
                            ic,
                            function(line)
                             {all[1] = [0,line,all[1]];
                              return b
                                      ?caml_cps_call2(Stdlib[53],line,_k_)
                                      :caml_cps_exact_call1(_k_,0)})}
                 return caml_cps_exact_call1(_k_,0)})}
    //end
    function loop2(param,cont)
     {var all=[0,0],_e_=Stdlib[79];
      return caml_cps_call2
              (_e_,
               cst_static_examples_ml$0,
               function(ic)
                {var _f_=Stdlib_Format[137];
                 function _g_(_i_)
                  {var _h_=Stdlib[83];
                   return caml_cps_call2
                           (_h_,
                            ic,
                            function(line)
                             {all[1] = [0,line,all[1]];
                              return caml_cps_call2(Stdlib[53],line,_g_)})}
                 return caml_cps_call2(_f_,_d_,_g_)})}
    //end |}]