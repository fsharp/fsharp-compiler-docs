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
                            InvokeCode = fun args -> <@@ Helper.doNothing();
                                                         Helper.doNothingOneArg(3)
                                                         Helper.doNothingGeneric(3)
                                                         Helper.C.DoNothing()
                                                         Helper.C.DoNothingGeneric(3)
                                                         Helper.C.DoNothingOneArg(3)
                                                         Helper.C.DoNothingTwoArg(Helper.C(), 3)
                                                         Helper.C().InstanceDoNothing()
                                                         Helper.C().InstanceDoNothingGeneric(3)
                                                         Helper.C().InstanceDoNothingOneArg(3)
                                                         Helper.C().InstanceDoNothingTwoArg(Helper.C(), 3)
                                                         Helper.G<int>.DoNothing()
                                                         Helper.G<int>.DoNothingGeneric(3)
                                                         Helper.G<int>.DoNothingOneArg(3)
                                                         Helper.G<int>.DoNothingTwoArg(Helper.C(), 3)
                                                         Helper.G<int>().InstanceDoNothing()
                                                         Helper.G<int>().InstanceDoNothingGeneric(3)
                                                         Helper.G<int>().InstanceDoNothingOneArg(3)
                                                         Helper.G<int>().InstanceDoNothingTwoArg(Helper.C(), 3) @@>)

        myType.AddMember(someMethod)

        [myType]

    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()