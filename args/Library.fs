namespace Args
open Microsoft.FSharp.Reflection
open System.Reflection
open System

module Array =
    /// Builds a new array that contains the elements of the given array, excluding the first N elements.
    /// If the array length is less then N then it returns an empty array.
    let trySkip i arr =
        let rec trySkipInner n arr =
            match arr with
            | [||] -> [||]
            | arr ->    
                if i = n then
                    arr
                else
                    arr
                    |> Array.skip 1
                    |> trySkipInner (n+1)
        trySkipInner 0 arr

module Parser =
    let isOption (typ: Type) =
        typ.IsGenericType &&
        typ.GetGenericTypeDefinition() = typedefof<Option<_>>

    let isList (typ: Type) = 
        typ.IsGenericType &&
        typ.GetGenericTypeDefinition() = typedefof<List<_>>

    type ArgsError =
        | NotImplmented

    type Token =
        | DoubleDashed of string
        | Undashed of string

    let tokenize (argv: string array) =
        argv
        |> Array.map (fun a ->
            if a.StartsWith "--" then
                DoubleDashed (a.Substring 2)
            else
                Undashed a)

    let getValuesAsString tokens (field: PropertyInfo) =
        // printfn "%s" field.Name
        // printfn "%A" tokens
        // printfn "-----"
        tokens
        |> Array.skipWhile (fun t ->
            match t with
            | DoubleDashed k -> k <> field.Name.ToLowerInvariant()
            | _ -> true)
        |> Array.trySkip 1
        |> Array.fold (fun (cont, values) t ->
            match t with
            | DoubleDashed _ -> (false, values)
            | Undashed v ->
                if cont then
                    let values = Array.append values [|v|]
                    let cont = isList field.PropertyType
                    (cont, values)
                else
                    (false, values))
            (true, Array.empty)
        |> snd
    
    let rec parseAsType typ (values: string array) : obj array =
        // TODO: return Result.
        if typ = typeof<string> then
            values |> Array.map (fun v -> v :> obj)
        elif typ = typeof<int> then
            values |> Array.map (fun v -> (int v) :> obj)
        elif typ = typeof<bool> then
            values |> Array.map (fun v -> (bool.Parse v) :> obj)
        elif typ = typeof<Option<string>> then
            values
            |> Array.map (Some >> (fun v -> v :> obj))
        elif typ = typeof<Option<int>> then
            values
            |> parseAsType typeof<int>
            |> Array.map ((fun v -> v :?> int) >> Some >> (fun v -> v :> obj))
        elif typ = typeof<Option<bool>> then
            values
            |> parseAsType typeof<bool>
            |> Array.map ((fun v -> v :?> bool) >> Some >> (fun v -> v :> obj))
        elif typ = typeof<List<string>> then
            let values =
                values
                |> List.ofArray
            [|values :> obj|]
        elif typ = typeof<List<int>> then
            let values =
                values
                |> parseAsType typeof<int> 
                |> Array.map (fun v -> v :?> int)
                |> List.ofArray
            [|values :> obj|]
        elif typ = typeof<List<bool>> then
            let values =
                values
                |> parseAsType typeof<bool> 
                |> Array.map (fun v -> v :?> bool)
                |> List.ofArray
            [|values :> obj|]
        else
            values |> Array.map (fun v -> v :> obj)
    
    let createRecordType typ tokens =
        let fields = FSharpType.GetRecordFields typ
        let values =
            fields
            |> Array.map (fun f -> (f, getValuesAsString tokens f))
            |> Array.map (fun (f, values) -> (f, parseAsType f.PropertyType values))
            |> Array.map (fun (f, values) ->
                match values with
                | [||] ->
                    if isOption f.PropertyType then
                        None :> obj
                    elif (tokens |> Array.contains (DoubleDashed (f.Name.ToLowerInvariant()))) then
                        true :> obj
                    else
                        false :> obj
                | [|i|] -> i
                | values -> (List.ofArray values) :> obj)

        // printfn "%A" values
        FSharpValue.MakeRecord (typ, values)


    let parse<'a> argv : Result<'a, ArgsError> =
        let tokens = argv |> tokenize
        let t = typeof<'a>
        if FSharpType.IsRecord t then
            let res = (createRecordType t tokens) :?> 'a
            Ok res
        else
            Error NotImplmented
