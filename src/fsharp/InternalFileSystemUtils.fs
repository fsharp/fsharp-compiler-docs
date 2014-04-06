// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

// This module related to bugs 4577 and 4651.

// Briefly, there are lots of .Net APIs that take a 'string filename' and if the 
// filename is 'relative', they'll use Directory.GetCurrentDirectory() to 'base' the file.
// Since that involves mutable global state, it's anathema to call any of these methods
// with a non-absolute filename inside the VS process, since you never know what the 
// current working directory may be.

// Thus the idea is to replace all calls to e.g. File.Exists() with calls to File.SafeExists,
// which asserts that we are passing an 'absolute' filename and thus not relying on the 
// (unreliable) current working directory.

// At this point, we have done 'just enough' work to feel confident about the behavior of 
// the product, but in an ideal world (perhaps with 4651) we should ensure that we never
// call unsafe .Net APIs and always call the 'safe' equivalents below, instead.

namespace Internal.Utilities.FileSystem

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System
open System.IO
open System.Diagnostics

