/*
An example of using the Genifer.Logic.dll .NET library.
To build that library, see the docs at the top of Logic.cobra.

To run this program:

On Mono:

	gmcs -debug -reference:Genifer.Logic.dll UseLogic.cs
	mono --debug UseLogic.exe

On .NET+Windows:
	(untested)
	
	Command line:
		csc /d /ref:Genifer.Logic.dll UseLogic.cs
		UseLogic.exe
	
	Visual Studio:
		Create a C# Console project
		Add this C# code over the default Main.cs file
			or add this file and make adjustments.
		Add a reference to the DLL
		Run (Ctrl+F5) or Debug (F5)
*/


using System;
using Genifer.Logic;


class Program {

	public static void Main() {
		var t = new ImpliesOpApp(new NotOpApp(new NotOpApp(new Variable("x"))), new Variable("x"));
		Console.WriteLine("term: {0}", t);
		Console.WriteLine("variables:");
		foreach(var variable in t.Variables()) {
			Console.WriteLine("  {0}", variable);
		}
		Console.WriteLine("done.");
	}

}
