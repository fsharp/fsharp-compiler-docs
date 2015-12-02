namespace TestTP

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

module Helper =
    let doNothing() = ()
    let doNothingOneArg(x:int) = ()
    let doNothingGeneric(x:'T) = ()
    type C() = 
        static member DoNothing() = ()
        static member DoNothingOneArg(x:int) = ()
        static member DoNothingTwoArg(c:C, x:int) = ()
        static member DoNothingGeneric(x:'T) = ()
        member __.InstanceDoNothing() = ()
        member __.InstanceDoNothingOneArg(x:int) = ()
        member __.InstanceDoNothingTwoArg(c:C, x:int) = ()
        member __.InstanceDoNothingGeneric(x:'T) = ()

    type G<'U>() = 
        static member DoNothing() = ()
        static member DoNothingOneArg(x:int) = ()
        static member DoNothingTwoArg(c:C, x:int) = ()
        static member DoNothingGeneric(x:'T) = ()
        member __.InstanceDoNothing() = ()
        member __.InstanceDoNothingOneArg(x:int) = ()
        member __.InstanceDoNothingTwoArg(c:C, x:int) = ()
        member __.InstanceDoNothingGeneric(x:'U) = ()

[<TypeProvider>]
type BasicProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "ErasedWithConstructor.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)

        let ctor = ProvidedConstructor([], InvokeCode = fun args -> <@@ "My internal state" :> obj @@>)
        myType.AddMember(ctor)

        let ctor2 = ProvidedConstructor(
                        [ProvidedParameter("InnerState", typeof<string>)],
                        InvokeCode = fun args -> <@@ (%%(args.[0]):string) :> obj @@>)
        myType.AddMember(ctor2)

        let innerState = ProvidedProperty("InnerState", typeof<string>,
                            GetterCode = fun args -> <@@ (%%(args.[0]) :> obj) :?> string @@>)
        myType.AddMember(innerState)

        let someMethod = ProvidedMethod("DoNothing", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.doNothing() @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("DoNothingOneArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.doNothingOneArg(3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("DoNothingGeneric", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.doNothingGeneric(3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassDoNothing", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C.DoNothing() @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassDoNothingGeneric", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C.DoNothingGeneric(3) @@>)

        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassDoNothingOneArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C.DoNothingOneArg(3) @@>)

        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassDoNothingTwoArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C.DoNothingTwoArg(Helper.C(), 3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassInstanceDoNothing", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C().InstanceDoNothing() @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassInstanceDoNothingGeneric", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C().InstanceDoNothingGeneric(3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassInstanceDoNothingOneArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C().InstanceDoNothingOneArg(3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("ClassInstanceDoNothingTwoArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.C().InstanceDoNothingTwoArg(Helper.C(), 3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("GenericClassDoNothing", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.G<int>.DoNothing() @@>)
        myType.AddMember(someMethod)

        // These do not seem to compile correctly when used in provided expressions:
        //Helper.G<int>.DoNothingGeneric(3)

        // These do not seem to compile correctly when used in provided expressions:
        //Helper.G<int>().InstanceDoNothingGeneric(3)
                                                         
        let someMethod = ProvidedMethod("GenericClassDoNothingOneArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.G<int>.DoNothingOneArg(3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("GenericClassDoNothingTwoArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.G<int>.DoNothingTwoArg(Helper.C(), 3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("GenericClassInstanceDoNothing", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.G<int>().InstanceDoNothing() @@>)
        myType.AddMember(someMethod)


        let someMethod = ProvidedMethod("GenericClassInstanceDoNothingOneArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.G<int>().InstanceDoNothingOneArg(3) @@>)
        myType.AddMember(someMethod)

        let someMethod = ProvidedMethod("GenericClassInstanceDoNothingTwoArg", [], typeof<unit>,
                            InvokeCode = fun args -> <@@ Helper.G<int>().InstanceDoNothingTwoArg(Helper.C(), 3) @@>)
        myType.AddMember(someMethod)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()