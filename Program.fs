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
   match l with
   | x :: xs  when 


let compose (f: 'a -> 'b) (g : 'b -> 'c) : 'a -> 'c = // first you call f with an input and then you pass the output to g and you return the result
  fun (x : 'a) -> g(f(x))
// prinfn "%A" (compose(fun x -> x * 2) (fun x -> string x)5)
// fsharp has this function build in >>
//// prinfn "%A" (((fun x -> x * 2) >> (fun x -> string x)5))

let rec repeat (n : int) (f : 'a -> 'a) : 'a -> 'a =
  if n = 0 then
    id // does fun x-> x so input will be output
  else
    f >> (repeat(n-1) f) 
    
    // repeat 3 (fun x -> x * 2)
    // repeat 1 f -> f >> (repeat 0 f)  = f >> id
    // repeat 2 f -> f >> (repeat 1 f)
    // repeat 3 f -> f >> (repeat 2 f)


[<EntryPoint>]
let main argv =
   let point = Point2D.Create(2.0, 5.0)
   let pointR = Point2D.Create(5.0, 9.0)
   let length = string(distance(point, pointR))
   let Ditto = Blob.Create()
   let Ditto2 = Blob.Create()
   let World3 = World.Create(5, Ditto, Ditto2)
   World3.Run()
   printfn "%A" (rev[1;2;2;])
   printfn "%A" (append [1;2;3] [4;5;6])
   printfn "%A" (palindrome [1;2;3;2;1])
   printfn "%A" (caesarCypher ['a'; 'b'] 3)
   0
   // return an integer exit code
