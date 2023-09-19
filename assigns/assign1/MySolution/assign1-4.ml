#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)
(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)
let helper_substring(cs: string) (index: int): char = 
  string_get_at(cs) (index)

let substring(cs:string): string =
  string_init(string_length(cs)-1)(fun i -> helper_substring(cs) (i+1))

let concat_helper(index: int) (s1: string) (s2: string): char =
  if index <= string_length(s1) - 1 then string_get_at (s1) (index) else string_get_at(s2) (index - string_length(s1))

let string_concatenate (s1: string) (s2: string): string =
  string_init(string_length(s1) + string_length(s2)) (fun i -> concat_helper(i)(s1)(s2))

let rec intrep_helper (ds1:string) (ds2:string) (len1: int) (len2:int) (carryover: int): string = 
  if len1 == -1 && len2 > -1 then intrep_helper (ds1) (ds2) (len1) (len2-1) (0) ^ str(char_of_digit(ord(string_get_at(ds2) (len2)) - 48 + carryover)) else
  if len1 > -1 && len2 == -1 then intrep_helper (ds1) (ds2) (len1-1) (len2) (0) ^ str(char_of_digit(ord(string_get_at(ds1) (len1)) - 48 + carryover)) else
  if len1 == - 1 && len2 == -1 then "" else
  if (ord(string_get_at(ds1) (len1)) - 48) + (ord(string_get_at (ds2) (len2)) - 48) + carryover >= 10 && len1 == 0 && len2 == 0
    then "1"^str(char_of_digit((digit_of_char(string_get_at(ds1) (len1)) + digit_of_char( string_get_at(ds2) (len2))+carryover) mod 10)) 
  else 
    if (ord(string_get_at(ds1) (len1)) - 48) + (ord(string_get_at (ds2) (len2)) - 48) + carryover >= 10 then
    intrep_helper(ds1) (ds2) (len1-1) (len2-1) (1) ^ str(char_of_digit((digit_of_char(string_get_at(ds1) (len1)) + digit_of_char(string_get_at(ds2) (len2))+carryover) mod 10))
    else
    intrep_helper(ds1) (ds2) (len1-1) (len2-1) (0) ^ str(char_of_digit(digit_of_char(string_get_at(ds1) (len1)) + digit_of_char(string_get_at(ds2) (len2)) + carryover))

let intrep_add(ds1:string) (ds2:string): string = 
  intrep_helper (ds1) (ds2) (string_length(ds1) - 1) (string_length(ds2) - 1) (0)

let test = intrep_add("")("");;