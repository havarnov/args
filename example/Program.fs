// Learn more about F# at http://fsharp.org

type ShowCaseArgs = {
    String: string;
    Int: int;
    Bool: bool;
    Optional: string option;
    List: string list;
}

[<EntryPoint>]
let main argv =

    let args: Result<ShowCaseArgs, _> =
        Args.Parser.parse
            [|
                "--string"; "somestring";
                "--int"; "42";
                "--bool";
                "--optional"; "someoptionalstring";
                "--list"; "a"; "b"; "c"
            |]
    printfn "%A" args

    0
