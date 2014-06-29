(* camlp5r ./pa_html.cmo *)
(* $Id: allf.ml,v 1.00 2013-10-08 22:19:06 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value default_max_cnt = 2000;

(* tools *)

value string_start_with ini s =
  loop 0 0 where rec loop i1 i2 =
    if i1 = String.length ini then True
    else if i2 = String.length s then
      if ini.[i1] = '_' then loop (i1 + 1) i2 else False
    else if s.[i2] = ini.[i1] || s.[i2] = ' ' && ini.[i1] = '_' then
      loop (i1 + 1) (i2 + 1)
    else False
;

value combine_by_ini ini list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(k, s, cnt) :: list] -> do {
          let ini_k =
            if String.length k > String.length ini then
              String.sub k 0 (index_of_next_char k (String.length ini))
            else k ^ String.make (String.length ini + 1 - String.length k) '_'
          in
          for i = 0 to String.length ini_k - 1 do {
            if ini_k.[i] = ' ' then ini_k.[i] := '_' else ()
          };
          let new_list =
            if ini_k = "_" then new_list
            else
              match new_list with
              [ [] -> [(ini_k, [(s, cnt)])]
              | [(ini_k1, l) :: ll] ->
                  if ini_k1 = ini_k then [(ini_k1, [(s, cnt) :: l]) :: ll]
                  else [(ini_k, [(s, cnt)]); (ini_k1, l) :: ll] ]
          in
          loop new_list list
        } ]
  in
  List.fold_left (fun new_l (ini_k, l) -> [(ini_k, List.rev l) :: new_l]) []
    list
;

value combine_by_count list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(_, s, cnt) :: list] ->
          let new_list =
            match new_list with
            [ [] -> [(cnt, [s])]
            | [(cnt1, l) :: ll] ->
                if cnt1 = cnt then [(cnt1, [s :: l]) :: ll]
                else [(cnt, [s]); (cnt1, l) :: ll] ]
          in
          loop new_list list ]
  in
  List.fold_left (fun new_l (cnt, l) -> [(cnt, List.rev l) :: new_l]) [] list
;

value alphab_string conf base field s =
  if field = "surname" then
    if Mutil.utf_8_db.val then
      surname_end base s ^ surname_begin base s
    else old_surname_end s ^ old_surname_begin s
  else s
;

value lower_if_not_utf8 s =
  if Mutil.utf_8_db.val then s else Name.lower s
;

value capitalize_if_not_utf8 s =
  if Mutil.utf_8_db.val then s else String.capitalize s
;

value lowercase_if_not_utf8 s =
  if Mutil.utf_8_db.val then s else String.lowercase s
;

value new_name_key base s =
  let part = Util.get_particle base s in
  if part = "" then s
  else
    let i = String.length part in
    String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
;

value name_key_compatible base s =
  if Mutil.utf_8_db.val then new_name_key base s else Mutil.name_key s
;

(* print *)

value print_title conf base field ini len = do {
  if len >= 2 then
    match field with
    [ "surname" ->
        Wserver.wprint (fcapitale (ftransl conf "the %d surnames")) len
    | "firstname" ->
         Wserver.wprint (fcapitale (ftransl conf "the %d first names")) len
    | field ->
        Wserver.wprint (fcapitale (ftransl conf "the %d %s")) len field ]
  else
    match field with
    [ "surname" ->
        Wserver.wprint "%s" (capitale (transl_nth conf "surname/surnames" 0))
    | "firstname" ->
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0))
    | field ->
        Wserver.wprint (fcapitale (ftransl conf "the %d %s")) len field ];
  if ini <> "" then
    Wserver.wprint " %s %s" (transl conf "starting with")
      (capitalize_if_not_utf8 ini)
  else
    Wserver.wprint " (%d %s)" (nb_of_persons base)
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
};

value displayify s =
  if Mutil.utf_8_db.val then
    loop 0 0 where rec loop i len =
      if i = String.length s then Buff.get len
      else
        let nbc = Name.nbc s.[i] in
        if nbc < 0 || i + nbc > String.length s then
          Buff.get (Buff.mstore len "...")
        else loop (i + nbc) (Buff.gstore len s i nbc)
  else String.capitalize s
;

value tr c1 s2 s =
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if String.unsafe_get s i = c1 then loop (i + 1) (Buff.mstore len s2)
    else loop (i + 1) (Buff.store len (String.unsafe_get s i))
;

value print_alphabetic_big conf base field ini list len too_big = do {
  let title _ = print_title conf base field ini len in
  header conf title;
  print_link_to_welcome conf True;
  tag "p" begin
    List.iter
      (fun (ini_k, _) ->
         stagn "a" "href=\"%sm=LIST_FIELD;f=%s;tri=A;k=%s\"" (commd conf)
           field (Util.code_varenv ini_k)
         begin
           Wserver.wprint "%s" (tr '_' "&nbsp;" (displayify ini_k));
         end)
      list;
  end;
  if len <= default_max_cnt && not too_big then do {
    stagn "p" begin
      Wserver.wprint "%s:" (capitale (transl conf "the whole list"));
    end;
    tag "ul" begin
      stagn "li" begin
        stag "a" "href=\"%sm=LIST_FIELD;f=%s;tri=A;o=A;k=%s\""
          (commd conf) field ini
          begin
            Wserver.wprint "%s" (transl conf "long display");
          end;
      end;
      stagn "li" begin
        stag "a" "href=\"%sm=LIST_FIELD;f=%s;tri=S;o=A;k=%s\""
          (commd conf) field ini
          begin
            Wserver.wprint "%s" (transl conf "short display");
          end;
      end;
      stagn "li" begin
        stag "a" "href=\"%sm=LIST_FIELD;f=%s;tri=S;o=A;k=%s;cgl=on\""
          (commd conf) field ini
          begin
            Wserver.wprint "%s + %s" (transl conf "short display")
              (transl conf "cancel GeneWeb links");
          end;
      end;
    end;
  }
  else ();
  trailer conf;
};

value print_alphabetic_all conf base field ini list len = do {
  let title _ = print_title conf base field ini len in
  header conf title;
  print_link_to_welcome conf True;
  tag "p" begin
    List.iter
      (fun (ini_k, _) ->
         let ini = capitalize_if_not_utf8 ini_k in
         stagn "a" "href=\"#%s\"" ini begin
           Wserver.wprint "%s" (Mutil.tr '_' ' ' ini);
         end)
    list;
  end;
  tag "ul" begin
    List.iter
      (fun (ini_k, l) ->
         let ini = capitalize_if_not_utf8 ini_k in
         tag "li" begin
           stagn "a" "id=\"%s\"" ini_k begin
             Wserver.wprint "%s" (Mutil.tr '_' ' ' ini);
           end;
           tag "ul" begin
             List.iter
               (fun (s, cnt) ->
                  stagn "li" begin
                    let href =
                      "m=PERS_FIELD;f="^ field ^ ";v=" ^
                      code_varenv (lower_if_not_utf8 s)
                    in
                    wprint_geneweb_link conf href
                      (alphab_string conf base field s);
                    Wserver.wprint " (%d)" cnt;
                  end)
               l;
           end;
         end)
      list;
  end;
  trailer conf;
};

value print_alphabetic_small conf base field ini list len = do {
  let title _ = print_title conf base field ini len in
  header conf title;
  print_link_to_welcome conf True;
  if list = [] then ()
  else
    tag "ul" begin
      List.iter
        (fun (_, s, cnt) ->
           stagn "li" begin
             stag "a" "href=\"%sm=PERS_FIELD;f=%s;v=%s;\"" (commd conf)
               field (code_varenv (lower_if_not_utf8 s))
             begin
               Wserver.wprint "%s" (alphab_string conf base field s);
             end;
             Wserver.wprint " (%d)" cnt;
           end)
        list;
    end;
  trailer conf;
};

value print_frequency_any conf base field list len = do {
  let title _ = print_title conf base field "" len in
  let n = ref 0 in
  header conf title;
  print_link_to_welcome conf True;
  tag "ul" begin
    List.iter
      (fun (cnt, l) ->
         if n.val > default_max_cnt then ()
         else
           tag "li" begin
             Wserver.wprint "%d\n" cnt;
             tag "ul" begin
               List.iter
                 (fun s ->
                    stagn "li" begin
                      stag "a" "href=\"%sm=LIST_FIELD;f=%s;v=%s\""
                        (commd conf) field (code_varenv (Name.lower s))
                        begin
                          Wserver.wprint "%s"
                            (alphab_string conf base field s);
                        end;
                      incr n;
                    end)
                 l;
             end;
           end)
      list;
  end;
  trailer conf;
};

(* selection *)

value get_field field p =
  match field with
  [ "surname" -> get_surname p
  | "first_name" -> get_first_name p
  | "public_name" -> get_public_name p
  | "occupation" -> get_occupation p
  | "birth_place" -> get_birth_place p
  | "birth_src" -> get_birth_src p
  | "baptism_place" -> get_baptism_place p
  | "baptism_src" -> get_baptism_src p
  | "death_place" -> get_death_place p
  | "death_src" -> get_death_src p
  | "burial_place" -> get_burial_place p
  | "burial_src" -> get_burial_src p
  | "psources" -> get_psources p
  | _ -> failwith "get_field" ]
;

value select_names conf base field ini need_whole_list =
  let iii = persons_of_field base field in
  let (list, len) =
    let start_k = Mutil.tr '_' ' ' ini in
    match
      try Some (spi_first iii (capitalize_if_not_utf8 start_k)) with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr 0 [] where rec loop istr len list =
          let s = nominative (sou base istr) in
          let k = name_key_compatible base s in
          if string_start_with ini k then
            let (list, len) =
              if s <> "?" then
                let my_list = spi_find iii istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn = get_field field p in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in
                let cnt = List.length my_list in
                if cnt = 0 then (list, len)
                else
                  match list with
                  [ [(k1, s1, cnt1) :: list1] ->
                      if k = k1 then
                        ([(k1, s1, cnt1 + cnt) :: list1], len - 1)
                      else ([(k, s, cnt) :: list], len)
                  | [] -> ([(k, s, cnt)], len) ]
              else (list, len)
            in
            match
              try Some (spi_next iii istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr (len + dlen) list
            | None -> (list, len) ]
          else (list, len)
    | None -> ([], 0) ]
  in
  let (list, len) =
    let lim =
      match p_getint conf.env "atleast" with
      [ Some x -> x
      | None -> 0 ]
    in
    List.fold_left
      (fun (list, len) (k, s, cnt) ->
         if cnt >= lim then ([(k, s, cnt) :: list], len) else (list, len - 1))
      ([], len) list
  in
  (list, if Mutil.utf_8_db.val then False else True, len)
;

value compare2 s1 s2 =
  if Mutil.utf_8_db.val then Gutil.alphabetic_utf_8 s1 s2 else compare s1 s2
;

value print_frequency conf base field =
  let () = load_strings_array base in
  let (list, _, len) = select_names conf base field "" True in
  let list =
    List.sort
      (fun (k1, _, cnt1) (k2, _, cnt2) ->
         if cnt1 > cnt2 then -1
         else if cnt1 < cnt2 then 1
         else compare2 k1 k2)
      list
  in
  let list = combine_by_count list in
  print_frequency_any conf base field list len
;

value print_alphabetic conf base field =
  let ini =
    match p_getenv conf.env "k" with
    [ Some k -> lowercase_if_not_utf8 k
    | _ -> "" ]
  in
  let fast =
    p_getenv conf.base_env "fast_alphabetic" = Some "yes" && ini = ""
  in
  let _ =
    if fast || String.length ini < 2 then load_strings_array base else ()
  in
  let all =
    match p_getenv conf.env "o" with
    [ Some "A" -> True
    | _ -> False ]
  in
  let (list, len) =
    if fast then
      loop [] 0 'Z' where rec loop list len c =
        let list = [(String.make 1 c, "", 1) :: list] in
        if c = 'A' then (list, len)
        else loop list (len + 1) (Char.chr (Char.code c - 1))
    else
      let (list, sorted, len) = select_names conf base field ini all in
      let list =
        if sorted then list
        else List.sort (fun (k1, _, _) (k2, _, _) -> compare2 k1 k2) list
      in
      (list, len)
  in
  if fast then
    let list = List.map (fun (s, _, _) -> (s, 1)) list in
    print_alphabetic_big conf base field ini list 1 True
  else if len >= 50 || ini = "" then
    let list = combine_by_ini ini list in
    if all then
      if len > default_max_cnt then incorrect_request conf
      else print_alphabetic_all conf base field ini list len
    else print_alphabetic_big conf base field ini list len False
  else print_alphabetic_small conf base field ini list len
;

(* short print *)

value print_alphabetic_short conf base field ini list len = do {
  let title _ = print_title conf base field ini len in
  let need_ref = len >= 250 in
  header conf title;
  print_link_to_welcome conf True;
  if need_ref then
    tag "p" begin
      List.iter
        (fun (ini_k, _) ->
           let ini = capitalize_if_not_utf8 ini_k in
           stagn "a" "href=\"#%s\"" ini begin
             Wserver.wprint "%s" (Mutil.tr '_' ' ' ini);
           end)
      list;
    end
  else ();
  List.iter
    (fun (ini_k, l) ->
       let ini = capitalize_if_not_utf8 ini_k in
       tag "p" begin
         list_iter_first
           (fun first (s, cnt) -> do {
              let href =
                if not conf.cancel_links then
                  " href=\"" ^ commd conf ^ "m=PERS_FIELD;f=%s" ^ field ^
                    ";v=" ^ code_varenv (lower_if_not_utf8 s) ^ ";t=A\""
                else ""
              in
              let name =
                if first && need_ref then " id=\"" ^ ini ^ "\"" else ""
              in
              if not first then Wserver.wprint ",\n" else ();
              if href <> "" || name <> "" then
                Wserver.wprint "<a%s%s>" href name
              else ();
              Wserver.wprint "%s" (alphab_string conf base field s);
              if href <> "" || name <> "" then Wserver.wprint "</a>"
              else ();
              Wserver.wprint " (%d)" cnt;
            })
           l;
         Wserver.wprint "\n";
       end)
    list;
  trailer conf;
};


(* main *)

value persons_of_absolute_field conf base field x =
  let istrl = base_strings_of_field base field x in
  List.fold_right
    (fun istr l ->
       let str = sou base istr in
       if str = x then
         let iperl = spi_find (persons_of_field base field) istr in
         let iperl =
           List.fold_left
             (fun iperl iper ->
                if eq_istr (get_field field (pget conf base iper)) istr then
                  [iper :: iperl]
                else iperl)
             [] iperl
         in
         if iperl = [] then l else [(str, istr, iperl) :: l]
       else l)
    istrl []
;

value print_all_persons_of_field conf base field = do {
  let x =
    match p_getenv conf.env "v" with
    [ Some v -> v (* au decode près *)
    | None -> "" ]
  in
  let list = persons_of_absolute_field conf base field x in
  let title _ = Wserver.wprint "List of persons" in
  header conf title;
  print_link_to_welcome conf True;
  (* Construction de la table des sosa de la base *)
  let () = Perso.build_sosa_ht conf base in
  List.iter
    (fun (str, istr, ipl) -> do {
       Wserver.wprint "%s" str;
       tag "ul" begin
         List.iter
           (fun ip -> do {
             let p = poi base ip in
             html_li conf;
             Perso.print_sosa conf base p True;
             Wserver.wprint "\n%s" (referenced_person_text conf base p);
             Wserver.wprint "%s" (Date.short_dates_text conf base p);
             stag "em" begin
               specify_homonymous conf base p False;
             end;
          })
          ipl;
       end })
    list;
  trailer conf;
};

value print_field_values conf base field =
  match p_getenv conf.env "tri" with
  [ Some "F" -> print_frequency conf base field
  | _ -> print_alphabetic conf base field ]
;


(* template *)
type env 'a =
  [ Vother of 'a
  | Vnone ]
;

value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value print_lists conf base =
  if p_getenv conf.env "old" = Some "on" then ()
  else
    Hutil.interp conf base "listsmenu"
      {Templ.eval_var _ = raise Not_found;
       Templ.eval_transl _ = Templ.eval_transl conf;
       Templ.eval_predefined_apply _ = raise Not_found;
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach _ = raise Not_found}
      [] ()
;
