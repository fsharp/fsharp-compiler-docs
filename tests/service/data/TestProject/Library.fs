namespace TestProject

type T = ErasedWithConstructor.Provided.MyType

type Class1() = 
    member this.X1 = T().DoNothing()
    member this.X2 = T().DoNothingGeneric()
    member this.X3 = T().DoNothingOneArg()
    member this.X4 = T().ClassDoNothing()
    member this.X5 = T().ClassDoNothingGeneric()
    member this.X6 = T().ClassDoNothingOneArg()
    member this.X7 = T().ClassDoNothingTwoArg()
    member this.X8 = T().ClassInstanceDoNothing()
    member this.X9 = T().ClassInstanceDoNothingGeneric()
    member this.X10 = T().ClassInstanceDoNothingOneArg()
    member this.X11 = T().ClassInstanceDoNothingTwoArg()
    member this.X12 = T().GenericClassDoNothing()
    member this.X13 = T().GenericClassDoNothingOneArg()
    member this.X14 = T().GenericClassDoNothingTwoArg()



