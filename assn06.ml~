(*Rahul Mitra
*CPSC,316
*Assignment 6 : Huffman Code*)

(*List to be input*)
val L = [(#"A",8),(#"B", 1),(#"C", 2),(#"D", 4),(#"E", 13),(#"F", 2),(#"G", 2),(#"H", 6),(#"I", 8),(#"J",1),(#"K", 1),(#"L",4),(#"M", 2),(#"N", 6),(#"O", 8),(#"P", 2),(#"Q", 0),(#"R", 6),(#"S", 6), (#"T", 9),(#"U", 3),(#"V", 1),(#"W", 3),(#"X", 1), (#"Y", 2),(#"Z", 0)]

(*This is a helper function to sort a list assuming user enters an unsorted list based on frequency*)
fun sortList L1 = ListMergeSort.sort(fn((_,a),(_,b)) => a < b) L1; 

(*This is a helper function to reverse a string*)
fun reverse s = foldl  op:: [] (explode s);

(*Definition of tree datatype*)
datatype tree =
   Node of tree * int * tree
 | Leaf of char * int;


(*A function to find the frequency of a node or a leaf*)
fun frequency(Leaf(_, f)) = f
  | frequency(Node(_,f, _)) = f;


(*This function inserts into the tree*)
fun insert (_,to_Insert,[]) = [to_Insert]
| insert (f,to_Insert,(t1 :: ts)) = if f < frequency t1 then t1 :: to_Insert :: ts 
else t1 :: (insert (f,to_Insert,ts));

(*This function builds the tree*)
fun build_tree [t] = t
| build_tree(a :: b :: cs) =
let 
val freq = frequency a + frequency b
val new_tree = insert (freq,(Node(a,freq, b)),cs)
in 
 build_tree new_tree
end;

(*This function makes a list of huffman encodings for a given tree*)
fun make_encoding (Leaf(c, freq), code) = (c,code) :: []
| make_encoding (Node(left, _, right), code) =  make_encoding(right, "1"^code) @ make_encoding(left,"0"^code) ;

(*This function builds the list of encodings i.e. huffman table from a given list of characters and frequencies*)
(*This is Part 1 in the assignment*)
fun huffman L =
let
val sortedL = sortList L 
val Leaves =  map (fn (a,b) => Leaf(a,b)) sortedL (*Make the list into leaves*)
val T = build_tree Leaves (*Build the tree using the leaves*)
in
make_encoding (T, "")
end;

(*This function looks up a code for a given character - used later in encoding*)
fun look_up_code (c : char , L : (char * string) list) = 
if c = (#1(hd L)) then (#2(hd L)) else look_up_code(c, tl L);

(*This function encodes a string*)
(*This is Part 2-a in the assignment*)
fun encode ("") = ""
| encode (s : string) = 
let
val sortedL = sortList L;
val leaves = map (fn (a,b) => Leaf(a,b)) sortedL; (*Map every element in the list with Leaf*)
val huff_tree = build_tree leaves; (*Build the huffman tree*)
val my_list = huffman L; (*Build a list mapping characters to encoded bit vectors*)
val char_list = explode s
in
look_up_code (hd (char_list), my_list) ^ encode (implode(tl char_list))
end;

(*This is a helper function to decode a string*)
fun decode_helper("", Node(x,y,z), decoded) = decoded
| decode_helper (s, Leaf(a, b), decoded) =  
let
val sortedL = sortList L
val leaves = map (fn (a,b) => Leaf(a,b)) sortedL; (*Map every element in the list with Leaf*)
val huff_tree = build_tree leaves; (*Build the huffman tree*)
in
  decode_helper(s, huff_tree, Char.toString(a) ^ decoded)
end
| decode_helper (s, Node(left, mid, right), decoded) = 
if (hd(explode s) = #"1") then  decode_helper(implode(tl(explode(s))), right, decoded) else decode_helper(implode(tl(explode(s))), left, decoded);

(*This function decodes a string*)
(*This is Part 2-b in the assignment*)
fun decode (s : string) = 
let
val sortedL = sortList L
val leaves = map (fn (a,b) => Leaf(a,b)) sortedL (*Map every element in the list with Leaf*)
val huff_tree = build_tree leaves (*Build the huffman tree*)
val rev = implode (reverse s)
in
decode_helper(rev,huff_tree, "")
end;

(*Testing*)
val huffman_table = huffman L;
val encodedA = encode "A";
val encodedJ = encode "J";
(*The encoding of J is longer than the encoding of A which is to be expected since A has higher frequency*)
val decodeA = decode "0011";
val decodeJ = decode "000001";
val encodeRAHUL = encode "RAHUL";
val decodeRAHUL = decode "0100001111011100101001";
val encodePETERYOON = encode "PETERYOON";
val decodePETERYOON = decode  "00000001111011100111010001000010101010010";
val encodeHELLOWORLD = encode "HELLOWORLD";
val decodeHELLOWORLD = decode "110100111010010100101010111001010100010011010";
(*In my implementation I've ignored all whitespaces,lowercase characters and non-alphabet characters assuming the user won't test such cases*)





