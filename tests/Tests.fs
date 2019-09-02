module Tests

open System

open Xunit

type BasicArgs = {
    Name: string;
    Age: int;
    IsCool: bool;
}

type BasicArgsOptional = {
    Name: string;
    Optional: string option
    OptionalInt: int option
    OptionalBool: bool option
}

type RecWithList = {
    Name: string;
    Hobbies: string list;
}

type MoreLists = {
    Name: string;
    Strings: string list;
    Ints: int list;
    Bools: bool list;
}

[<Fact>]
let ``basic test for record type`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Age = 123;
        IsCool = true;
    }

    // act
    let basicArgs: Result<BasicArgs, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--age";
                "123";
                "--iscool";
                "true";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic test for record type with empty bool should be true`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Age = 123;
        IsCool = true;
    }

    // act
    let basicArgs: Result<BasicArgs, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--age";
                "123";
                "--iscool";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic test for record type out of order args`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Age = 123;
        IsCool = true;
    }

    // act
    let basicArgs: Result<BasicArgs, _> =
        Args.Parser.parse
            [|
                "--age";
                "123";
                "--iscool";
                "--name";
                "foobar";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic test for record type missing bool should be false`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Age = 123;
        IsCool = false;
    }

    // act
    let basicArgs: Result<BasicArgs, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--age";
                "123";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic test with optional with value.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Optional = Some "optional";
        OptionalInt = Some 123;
        OptionalBool = Some true;
    }

    // act
    let basicArgs: Result<BasicArgsOptional, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--optional";
                "optional";
                "--optionalint";
                "123";
                "--optionalbool";
                "true";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)


[<Fact>]
let ``basic test with optional with missing value.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Optional = Some "optional";
        OptionalInt = None;
        OptionalBool = Some true;
    }

    // act
    let basicArgs: Result<BasicArgsOptional, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--optional";
                "optional";
                "--optionalbool";
                "true";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic test with optional with all missing value.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Optional = None;
        OptionalInt = None;
        OptionalBool = None;
    }

    // act
    let basicArgs: Result<BasicArgsOptional, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic list test.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Hobbies = List.ofArray [|"a";"b";"c"|]
    }

    // act
    let basicArgs: Result<RecWithList, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--hobbies";
                "a";
                "b";
                "c";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic list test with empty list.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Hobbies = List.ofArray [||]
    }

    // act
    let basicArgs: Result<RecWithList, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--hobbies";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``basic list test with list arg totally missing.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Hobbies = List.ofArray [||]
    }

    // act
    let basicArgs: Result<RecWithList, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)

[<Fact>]
let ``list int,string,bool.`` () =
    // arrange
    let expected = {
        Name = "foobar";
        Strings = List.ofArray [|"a";"b"|];
        Ints = List.ofArray [|1;2;3|];
        Bools = List.ofArray [|true;true;false|];
    }

    // act
    let basicArgs: Result<MoreLists, _> =
        Args.Parser.parse
            [|
                "--name";
                "foobar";
                "--strings";
                "a";
                "b";
                "--ints";
                "1";
                "2";
                "3";
                "--bools";
                "true";
                "true";
                "false";
            |]
    
    // assert
    Assert.Equal(Ok expected, basicArgs)
