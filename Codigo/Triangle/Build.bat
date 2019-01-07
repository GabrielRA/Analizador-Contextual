@echo off

mkdir temp
copy Misc\*.* temp
copy ContextualAnalyzer\*.* temp
copy SyntacticAnalyzer\*.* temp
copy CodeGenerator\*.* temp

cd temp

ocamlc -c ErrorReporter.mli
ocamlc -c RuntimeEntity.mli
ocamlc -c Ast.mli
ocamlc -c IdentificationTable.mli
ocamlc -c TreeDrawer.mli
ocamlc -c Parser.mli
ocamlc -c Scanner.mli
ocamlc -c Checker.mli
ocamlc -c Encoder.mli

ocamlc -c ErrorReporter.ml
ocamlc -c IdentificationTable.ml
ocamlc -c TreeDrawer.ml
ocamlc -c Parser.ml
ocamlc -c Scanner.ml
ocamlc -c Checker.ml
ocamlc -c Encoder.ml
ocamlc -c -pp camlp4o Compiler.ml

ocamlc ErrorReporter.cmo IdentificationTable.cmo TreeDrawer.cmo Parser.cmo Scanner.cmo Checker.cmo Encoder.cmo Compiler.cmo -o Triangle.exe

move Triangle.exe ..
cd ..
rd /s /q temp