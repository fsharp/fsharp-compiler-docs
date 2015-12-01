namespace TestProject

type T = ErasedWithConstructor.Provided.MyType

type Class1() = 
    member this.X = T().DoNothing()
