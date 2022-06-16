let rec lenc xs k=
    match xs with
    | [] -> k 0
    | x::xr -> lenc xr (fun v -> k(1+v));;

printf "The answer is ’% d’ \n" (lenc [2; 5; 7] id)
//(ii)
printf "The answer is ’% d’ \n" (lenc [2; 5; 7] (fun v -> 2*v))

//(iii)
let rec leni xs acc=
    match xs with
    | [] -> acc
    | x::xr -> leni xr (acc+1);;

printf "The answer is ’% d’ \n" (leni [2; 5; 7] 0)

//neither use stack, but lenc uses heap for building the closure
//also leni is lenc rewritten to use accumulating parameter



//11.2
let rec revc xs k =
    match xs with
    | [] -> k []
    | x::xr -> revc xr (fun v -> k(v@ [x]));;

(revc [2; 5; 7] id);;

//(ii)
(revc [2; 5; 7] (fun v -> v@v));;
//at end copy list [7,5,2,  7,5,2]


//(iii)
let rec revi xs acc =
    match xs with
    | [] -> acc
    | x::xr -> revi xr (acc@ [x]);;

(revi [2; 5; 7] []);;



//11.3
let rec prodc xs k =
    match xs with
    | [] -> k 1
    | x::xr -> prodc xr (fun v -> k(x*v));;


//11.4
let rec prodc2 xs k =
    match xs with
    | [] -> k 1
    | 0::_ -> 0
    | x::xr -> prodc2 xr (fun v -> k(x*v));;


let rec prodi xs acc =
    match xs with
    | [] -> acc
    | 0::_ -> 0
    | x::xr -> prodi xr (x * acc);;


printf "The answer is ’% d’ \n" (prodc [2; 5; 7; 0] id)
printf "The answer is ’% d’ \n" (prodc2 [2; 5; 7; 0] id)
printf "The answer is ’% d’ \n" (prodi [2; 5; 7; 0] 1)
printf "The answer is ’% d’ \n" (prodc [2; 5; 7] id)
printf "The answer is ’% d’ \n" (prodc2 [2; 5; 7] id)
printf "The answer is ’% d’ \n" (prodi [2; 5; 7] 1)







