(* vim: set nowrap: -*- truncate-lines: t -*- *)

let text = {|O,b#kbU$f{@.;Fp#N m&UV6.Xp+`fi0F%'f&iD.ENI!#*\cU[O'A<P|bP9c{CloM QK;tKoyFxiB!1/3?6^dD8n:alk)f<E9r5y!ADGlUTer7Q54c1S!T3{_Xs_<:U`&Jce>YD9x#[17Q|YgGdG,p)IRVv1FQv;b'o:@B-+]-y6KC$/0,*"k~!i!J3^ur3'Jr`+_&#UQCA3!|8QkU<DSi&<mD1nI=pm?0'Q:/LTk@5HImf!Qd8MV9WR[w8e`poar\qp2]D4sy>`Y@/N>z|J\K?U1S)AZ>/S@P+2_%79M%6Xq8WmQ}Oq:/)PD5S\[SVq&nb_fm:D{v{gqdO/0$-kW[LBCFoRw:&2[[?@1q+TrjM]:24+Kkun,,Q7@hOE;{}p}9%_WeLv7)Z"0b!W"QWSq$;j,ateA{gi$9OW}hKP87.xnwMS#(_n:c2&QAxw&^GX8JlL/%Qm|\$"F5ntmmfe1.lJ)lL,Z!r0o*7Y'hgh^8]7=cpuzB"[yFc|}

let input = "1000010010001100110001010011011101101100110000110011010111001000110011001101110101101011001001100001101100110000101100011001000101011011101111101100010011011111110010010101011101000011011011101101010110111011011011001111100111111010010111111100110101111010101111000110110011110011110001010010111110110111011001100110101010101101110000111000011010111111010111111000100101011101111101001110000100101110000111111010111011001011001001010111100110101111000110011000010010100101111110011111101111000111100011011101001101111010001101100100010100110111001100001011000110010011010001100000011100110111001100001100110111110101101100010101101100110111000110101111101100010011011101011010110010011010011011000111000110101111101011001"

type huffman = Node of int * huffman * huffman | Leaf of int * char

let freqs str =
    let f seq = (Seq.length seq, Seq.uncons seq |> Option.get |> fst) in
    String.to_seq str |> List.of_seq
    |> List.sort compare |> List.to_seq
    |> Seq.group (=) |> Seq.map f
    |> List.of_seq |> List.stable_sort compare

let freq = function
    | Node (n, _, _) -> n
    | Leaf (n, _)    -> n

let combine t1 t2 = Node (freq t1 + freq t2, t1, t2)

let rec insert x = function
    | []    -> [x]
    | y::xs -> if freq x < freq y then x :: y :: xs
               else y :: insert x xs

let build_huffman freqs =
    let rec f = function
        | []       -> failwith "invalid forest"
        | [x]      -> x
        | x::y::xs -> combine x y |> Fun.flip insert xs |> f
    in List.map (fun (f, c) -> Leaf (f, c)) freqs |> f

let decode str tree =
    let f (xs, t) d =
        match t with
           | Leaf _    -> failwith "invalid code"
           | Node (_, l, r) -> match if d = '0' then l else r with
                               | Leaf (_, x) -> (x::xs, tree)
                               | t'          -> (xs, t')
    in String.to_seq str
       |> Seq.fold_left f ([], tree) |> fst
       |> List.rev |> List.to_seq |> String.of_seq

let answer = freqs text |> build_huffman |> decode input
