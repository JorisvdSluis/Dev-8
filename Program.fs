// Learn more about F# at http://fsharp.org

open System
open System.Reflection.Metadata

let inc = fun x -> x + 1
let add = fun x y -> x + y
let addy = add 5
// result = fun y = 5 + y

let abs = 
    fun x ->
      if x > 0 then 
        x
      else
        -x
// use rec for recursion
let rec sumn = 
  fun n ->
    if n = 1 then 
      n
     else 
      n + (sumn (n-1))

// exercise 1 and 2 change else concatination
let rec allNumbers(n : int) : string = 
  if n = 0 then
    "0"
  else
   (string n) + " " + allNumbers(n - 1) 

//excercise 3
let rec allNumbersRange (lower : int) (upper : int) = 
  if lower = upper then 
    string lower
  else 
    allNumbersRange lower (upper - 1) + " " + (string upper)

//exercise 8
let rec toBinary (n : int) : string =
  if n = 0 then
    ""
  else 
    let currentDigit = n % 2
    (toBinary (n / 2) + string currentDigit)

// define a tuple in f#
let myTuple = 3,4,5,6, ""

// tuple with defined types
let (secondTuple : int * int * string) = 1,3, "hello"

// record with rebuild in method
type Car =
  {
    MaxSpeed : float
    Model : string
    Brand : string
  }
  with
    member this.GoFaster = 
      {
      MaxSpeed = this.MaxSpeed + 10.0
      Model = this.Model
      Brand = this.Brand
      }

// record with method rebuild whole record via shortcut this with
// constructor via static member
type Bike =
  {
    MaxSpeed : float
    Model : string
    Brand : string
  }
  with 
    static member Create(maxSpeed: float, model: string, brand: string)  =
      {
      MaxSpeed = maxSpeed
      Model = model
      Brand = brand
      }
    member this.GoFaster = 
      {
        this with
          MaxSpeed  = this.MaxSpeed + 10.0
      }
    
//generic type via apostroph

let foo (x : 'a) = 5

type Genericrecord<'a, 'b> =
  {
    A: 'a
    B: 'b
  }

//excercise 3
let rec allNumbersRange2 (lower: int) (upper: int) : string =
  if lower = upper then
    string(upper)
  else
  allNumbersRange2 lower (upper - 1 ) + " " + string(upper)
// excercise 5
let rec allEvenRange (lower: int) (upper: int) :string = 
  if lower = upper then
    if upper % 2 = 0 then
      string(upper)
    else 
     ""
  else 
    if upper % 2 = 0 then
      allEvenRange lower (upper  - 1) + " " + string (upper)
    else
      allEvenRange lower (upper  - 1) 
// excercise 6

let rec drawLine (length: int) : string = 
  if length = 1 then
    "*"
  else
    drawLine (length - 1) + "*"
//reader 2
//Excercies 1

type Point2D  =
  {
  Position : (float * float)
  X: float
  Y: float
  }
with
  static member Create(x: float, y: float)=
    {
     X = x
     Y = y
     Position = (x, y)
    }
   static member Random(min: float, max:float) = 
      let r = Random()
      let rand = r.NextDouble()
      let min2 = min  + rand * (max - min)
      let max2 = max  - rand * (max - min)   
      if min2 < max2 then 
      {
         X = min2
         Y = max2
         Position = (min2, max2)
      }
      else 
      {
        X = max2
        Y = min2
        Position = (max2, min2)
      }
  
      
      // Excercise 2
let distance(point1 : Point2D, point2: Point2D) : float  = 
      
      let x3 = (point1.X - point2.X) ** 2.0
      let y3 = (point1.Y - point2.Y) ** 2.0
      let total = x3 + y3
      Math.Sqrt(total)


let r = Random()
let nextMove = r.Next(0,3)
let distance2 = r.Next(-50,50)

type Blob  =
  {
  Position : Point2D
  Speed: int
  }
  with  
    static member Create() =
     {
      Position =  Point2D.Create(float(r.Next(-50, 50)),float(r.Next(-50, 50)))
      Speed = r.Next(1,5)
     }
      member this.Move() = 
            if nextMove < 2 then
              if float(distance2) + this.Position.X < -50.0 then
                {
                 Position = Point2D.Create(-50.0, this.Position.Y)
                 Speed = this.Speed 
                }
              else if float(distance2) + this.Position.X > 50.0 then
                {
                 Position = Point2D.Create(50.0, this.Position.Y)
                 Speed = this.Speed 
                }
              else 
                {
                 Position = Point2D.Create(this.Position.X + float(distance2), this.Position.Y)
                 Speed = this.Speed 
                }
            else
              if float(distance2) + this.Position.Y < -50.0 then
                {
                 Position = Point2D.Create(this.Position.X, -50.0)
                 Speed = this.Speed 
                }
              else if float(distance2) + this.Position.Y > 50.0 then
                {
                 Position = Point2D.Create(this.Position.X,50.0)
                 Speed = this.Speed 
                }
              else 
                {
                 Position = Point2D.Create(this.Position.X, this.Position.Y + float(distance2))
                 Speed = this.Speed 
                }
let num = 5
type World = 
  {
    Ticks: int
    Ditto1: Blob
    Ditto2: Blob
  }
  with
    static member Create(ticks: int, ditto1: Blob, ditto2: Blob) =
      {
        Ticks = ticks
        Ditto1 = ditto1
        Ditto2 = ditto2
      }
    member this.Run() = 
    
     while num < 0 do
      num - 1 
      printfn "Tick!"
        
        
//reader 3
//excercise 1

let rec last (l : List<'a>) : Option<'a> =
  match l with
  | [] -> None
  | [x] -> Some x
  | x :: xs -> 
    last xs

//excercise 2
// @ appends something to a list
let rec rev (l : List<'a>) : List<'a> = 
  match l with
  | [] -> []
  | x :: xs -> 
    (rev xs) @ [x]

//excercise 3
let append(l1 : List<'a>)(l2 : List<'a>) : List<'a> =
  match l2 with
  | [] -> []
  | x :: xs ->
    l1 @ l2

//Excercise 4
let rec nth (n:int)(l:List<'a>) : Option<'a> =
  if n = 0 then
    Some l.Head
  elif l = [] then
    None
  else
    nth(n-1) l.Tail
//excercise 5
let palindrome (l: List<'a>) : bool =
  if l = rev(l) then
    true
  else 
    false

//excercise 6 not working
let rec compress (l: List<'a>) : List<'a> =
  match l with
  | x :: xs when xs = [] -> [x]
  | x :: xs when x = xs.Head  -> compress(xs)
  | x :: xs -> x :: compress(xs)
  | [] -> []

//excercise 7
let rec caesarCypher (l: List<char>) ( shift : int) : List<char> =
  match l with
  |x :: xs -> char(int(x) + shift)  :: (caesarCypher(xs) shift)
  | [] -> []
  
// excercise 8
let rec splitAt (i: int) (l: List<'a>) : List<'a> * List<'a> =
  match  l with
  | [] -> [],[]
  | x::xs -> 
    if i = 0 then
      [], x:: xs
    else 
      let left,right = splitAt(i-1) xs
      x :: left,right


let rec merge (l1 : List<'a>)(l2: List<'a>) : List<'a> =
  match l1, l2 with
  |[], l
  |l, [] -> l
  |x ::xs, y::ys -> 
    if x<=y then
      let newlist = merge xs (y::ys)
      x :: newlist
    else
      let newlist = merge l1 ys
      y :: newlist


//reader 4
//maplist f = foldlist [] (fun x xs -> f x::xs)
//exercise 1
let mapfold (f: 'a -> 'b)(l:List<'a>) : List<'b> =
  l |> List.fold (fun x xs -> x @ [f xs]) []
// let compose (f: 'a -> 'b) (g : 'b -> 'c) : 'a -> 'c = // first you call f with an input and then you pass the output to g and you return the result
//   fun (x : 'a) -> g(f(x))
// prinfn "%A" (compose(fun x -> x * 2) (fun x -> string x)5)
// fsharp has this function build in >>
//// prinfn "%A" (((fun x -> x * 2) >> (fun x -> string x)5))

// let rec repeat (n : int) (f : 'a -> 'a) : 'a -> 'a =
//   if n = 0 then
//     id // does fun x-> x so input will be output
//   else
//     f >> (repeat(n-1) f) 
    
    // repeat 3 (fun x -> x * 2)
    // repeat 1 f -> f >> (repeat 0 f)  = f >> id
    // repeat 2 f -> f >> (repeat 1 f)
    // repeat 3 f -> f >> (repeat 2 f)

let filterfold (f : 'a -> bool)(l : List<'a>):List<'a> =
  l |> List.fold(fun p x-> 
                     if f x then
                      x :: p
                     else
                      p
                  ) []
let flatten (l : List<List<'a>>) : List<'a> =
  l |> List.fold( fun newlist l -> newlist @ l) [] 


let rec last2 (l : List<'a>) : Option<'a> =
  match l with
  | [] -> None
  | x :: xs -> 
    if xs = [] then 
      Some x
    else 
      last2 xs
let rec rev2 (l : List<'a>) : List<'a> =
  match l with 
  | [] -> []
  | [x] -> [x]
  | x :: xs -> rev2(xs) @ [x]

let rec append2 (l1 : List<'a>)(l2 : List<'a>) : List<'a> =
  match l1, l2 with
  | [], l2 -> l2
  | x :: xs, l2 -> x :: (append2 xs l2)

let rec splitAt2 (i : int) (l : List<'a>) : List<'a> * List<'a> =
  match l with
  | [] -> [],[]
  | x :: xs -> 
      if i = 0 then
        [], l
      else
        let left,right = splitAt2 (i - 1) xs
        x :: left,right

let rec merge2 (l1 : List<'a>) ( l2: List<'a>) : List<'a> = 
  match l1, l2 with
  | [], [] -> []
  | _, [] -> l1
  | [], _ -> l2
  | x :: xs, y :: ys -> 
    if x < y then
      x :: merge2 xs l2
    else
      y :: merge2 l1 ys
let rec compress2 (l : List<'a>) : List<'a> =
  match l with
  | [] -> []
  |[x] -> [x]
  | x :: y :: ys ->
    if x = y then
      compress2( y :: ys)
    else  
      x :: (compress2( y :: ys))
let rec map2 (f : 'a-> 'b -> 'c)(l1 : List<'a>)(l2 : List<'b>) : Option<list<'c>> =
  match l1, l2 with
  | [], [] -> Some []
  | [] , _ -> None
  | _ ,[] -> None
  | x :: xs, y :: ys -> 
    let lopt = map2 f xs ys
    match lopt with
      | None -> None
      | Some l -> Some((f x y) :: l)

let rec fold2 (f : 'state -> 'a -> 'b -> 'state )(init: 'state)(l1 : List<'a>)(l2 : List<'b>) : Option<'state> =
  match l1,l2  with 
  | [], [] -> Some init
  | [], _ -> None
  |_, [] -> None
  | x :: xs, y :: ys ->
   fold2 f (f init x y) xs ys

let rec append20 (l1 : List<'a>) (l2 : List<'a>) : List<'a> = 
  l1 @ l2

let mapfold2(f : 'a -> 'b)(l: List<'a>) : List<'b> =
  l |> List.fold(fun newlist item-> newlist @ [f item])[]

let rec splitAt3 (i : int)(l : List<'a>) : List<'a>*List<'a> =
  match i, l with
   | _, [] -> [],[]
   | i, x :: xs -> 
      if i = 0 then
        [], x :: xs
      else
        let left,rigth = splitAt3 (i - 1) xs
        x :: left,rigth
 
let rec compress3 (l : List<'a>) : List<'a> =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::xs -> 
    if xs.Head = x then
      compress3( xs)
    else 
      x :: compress3(xs)

let rec bla (lower: int)(upper : int) : string =
  if lower <= upper then
    string lower + " " + bla (lower + 1) upper
  else
    ""


let rec vraag1 (length : int)(d : int) : string =
  if length > 0 then
    if (length % d ) = 0 then
      vraag1 (length - 1 ) d + " "
    else
      vraag1 (length - 1) d + "*"
  else
    ""

type Tree<'a>=
| Empty
| Node of 'a *  List<Tree<'a>>
  member this.Map(f : 'a -> 'b) =
    match this with
    | Empty -> Empty
    | Node(x, sub) ->
        Node(f x, sub |> List.map(fun t -> t.Map f))
  
  member this.Fold(f : 'state -> 'a -> 'state)(state: 'state) : 'state =
    match this with
    | Empty -> Empty
    | Node(x,sub) ->
        let state1 = f state x
        sub |> List.fold (fun s tree -> tree.Fold f s) state1

type Entry<'K,'v> =
  {
    Key: 'K
    Value: 'v
  }
  with
  static member Create(key : 'k, value : 'v) =
    {
      Key = key
      Value = value
    }

let rec last3 (l: List<'a>) : Option<'a> = 
  match l with
  | [] -> None
  | x :: xs ->
    if xs = [] then
      Some(x)
    else
      last3 xs

let rec rev3 (l : List<'a>) : List<'a> =
  match l with
  | [] -> []
  | x :: xs -> 
     rev3 xs  @ [x]
    
    
let rec append3 (l1 : List<'a>)(l2 : List<'a>) : List<'a> =
  match l1 with
  | [] -> l2
  | x :: xs -> x :: append3 xs l2
  // 1 :: append3 [2;3;]  l2
  // 1; 2; append3 [3;] l2
  // 1;2;3; append3 [] l2
  // [1;2;3; l2;]

let rec nth3 (n : int) (l : List<'a>) : Option<'a> =
  match l with
  | []-> None
  | x :: xs -> 
    if n = 0 then
      Some x
    else 
      nth3 (n - 1) (xs )

let palindrome3 (l : List<'a> ) : bool = 
    let rec reverse =
      match l with
      | [] -> []
      | x :: xs -> 
         rev3 xs  @ [x]
    l = reverse

    
//excercise 1
//(Lx y -> ((Lz -> z)x))0
//t[x-> u] = Ly -> ((Lz -> z)0)
  
//excercise 2
//((Lyx -> yx)O)((Lx -> x)K)
//(OX)(K)
//v = OK

//excercise 3
//let foo (x : int -> string)(y : int) : string = x y
// let (f : ...) = foo(fun x : int) -> string x
// f = int -> string


//excercise 4
//let map (f : 'a -> 'b) -> (l : List<'a>) : List<'b> = []
//let (x : ...) = map((fun x: int) -> (string x) + "1")
// x = list<int> -> List<string>

// exc 5

// let curry (f: 'a * 'b -> 'c) : 'a -> 'b -> 'c = ...
// let add (x : int, y : int) : int =

// practicum
// excercise 1

//let rec vraag1 (length : int)(d : int) : string =
  // if length > 0 then
  //   if (length % d ) = 0 then
  //     vraag1 (length - 1 ) d + " "
  //   else
  //     vraag1 (length - 1) d + "*"
  // else
  //   ""

// excercise 2
let rec concat (l1 : List<'a> ) (l2 : List<'a>) : List<'a> = 
  match l1 with
  | [] -> l2
  | x :: xs -> x :: concat xs l2

// excercise 3
// let rec repeat(x : 'a)(n : int) : List<'a> =
//   match n with
//   | 0 -> []
//   |_ -> x :: repeat x (n - 1)

//   match l with
//   | [] -> []
//   | x :: xs -> 
//     match x with
//     | corrupted -> Error "error message"
//     | compressed(x , n) -> repeat x n | > List.map(fun e => Element e)
//     | uncompressed(x) -> Element x
//     | nested (l_xs) -> l_xs |> List.map(fun x -> uncompress x) |> List.concat

// excercise 4
//type Either<'a, 'b> =
//| Left of 'a
//| right of 'b
// let rec length (l : 'a list) = 
// match l with 
// | [] -> 0
// | x :: xs -> l + length xs

// let mapchoiceaux
// (l : List < 'a>)
// (functions : List<Either<('a -> 'b),('a -> 'c)>>) : Option < List< Either<'b, 'c>>> =
//macth l, functions with
// [], [] -> []
// | x ::xs, y :: ys -> 
  // match y with
  // | left('a) -> a x :: mapchoiceaux xs ys
  // right(b) -> b x :: mapchoiceaux xs ys

// let mapchoice
// (l : List < 'a>)
// (functions : List<Either<('a -> 'b),('a -> 'c)>>) : Option < List< Either<'b, 'c>>> =
// if (length l != length functions) then None
// else
// Some(mapchoiceaux l functions)

// excercise 5
// let rec search ....
// match fs with 
// | Empty -> []
// | Directory (files, directoryfiles) -> 
//   let filtered_files : List<File> = files  |> List.filter(fun f -> 
//                                                       match option with
//                                                       | ByName n-> n = f.ByName.contains(n)
//                                                       | Byextension e -> e = f.Extension )
//   let files_from_subdir : List<List<File>> = 
//     sub_dir |> List.map(fun d -> search option d) |> List.concat
//     filtered_files @  files_from_subdir


[<EntryPoint>]
let main argv =
   let point = Point2D.Create(2.0, 5.0)
   let pointR = Point2D.Create(5.0, 9.0)
   let length = string(distance(point, pointR))
   let Ditto = Blob.Create()
   let Ditto2 = Blob.Create()
   let World3 = World.Create(5, Ditto, Ditto2)
   let cl = [[3;1;4];[5;0;1];[2];[4;5];[1;3;2]]
   World3.Run()
  //  printfn "%A" (rev[1;2;2;])
  //  printfn "%A" (append3 [1;2;3] [4;5;6])
  //  printfn "%A" (palindrome [1;2;3;2;1])
  //  printfn "%A" (caesarCypher ['a'; 'z'] 3)
  //  printfn "%A" (merge2 [1;3;6;9] [2;4;5;8])
  //  printfn "%A" (compress3 ['a';'a';'a';'x';'x';'a';])
  //  printfn "%A" (rev3 [1;3;4])
  //  printfn "%A" (flatten [[1];[2]])
  //  printfn "%A" (append20  [1;2;3;] [4;5;6;])
  //  printfn "%A" (last3 [1;2;3;4;])
   printfn "%A" (palindrome3 ['a'; 'b'; 'a'])
   0

   // return an integer exit code
