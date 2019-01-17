@echo off

mkdir temp
copy Code\Misc\ErrorReporter\*.* temp
copy Code\Misc\Printers\*.* temp
copy Code\SyntacticAnalyzer\*.* temp
copy Code\CodeGenerator\*.* temp
copy Code\ContextualAnalyzer\*.* temp
copy Code\Compiler.ml temp

rd /s /q Docs

REM Generate fields to save docs

mkdir Docs
cd Docs

mkdir "ErrorReporter(mli)"
mkdir "RuntimeEntity(mli)"
mkdir "Ast(mli)"
mkdir "TreeDrawer(mli)"
mkdir "Token(mli)"
mkdir "Parser(mli)"
mkdir "Scanner(mli)"
mkdir "IdentificationTable(mli)"
mkdir "Checker(mli)"
mkdir "TokenPrinter_Pipe(mli)"
mkdir "TokenPrinter_XML(mli)"
mkdir "TokenPrinter_HTML(mli)"

mkdir "ErrorReporter(ml)"
mkdir "TreeDrawer(ml)"
mkdir "Parser(ml)"
mkdir "Scanner(ml)"
mkdir "IdentificationTable(ml)"
mkdir "Checker(ml)"
mkdir "TokenPrinter_Pipe(ml)"
mkdir "TokenPrinter_XML(ml)"
mkdir "TokenPrinter_HTML(ml)"
mkdir "Compiler(ml)"

REM Generate fields to save docs

cd ..

cd temp

ocamlc -c ErrorReporter.mli
ocamlc -c RuntimeEntity.mli
ocamlc -c Ast.mli
ocamlc -c TreeDrawer.mli
ocamlc -c Token.mli
ocamlc -c Parser.mli
ocamlc -c Scanner.mli
ocamlc -c IdentificationTable.mli
ocamlc -c Checker.mli
ocamlc -c TokenPrinter_Pipe.mli
ocamlc -c TokenPrinter_XML.mli
ocamlc -c TokenPrinter_HTML.mli

ocamlc -c ErrorReporter.ml
ocamlc -c TreeDrawer.ml
ocamlc -c Parser.ml
ocamlc -c Scanner.ml
ocamlc -c IdentificationTable.ml
ocamlc -c Checker.ml
ocamlc -c TokenPrinter_Pipe.ml
ocamlc -c TokenPrinter_XML.ml
ocamlc -c TokenPrinter_HTML.ml
ocamlc -c -pp camlp4o Compiler.ml

REM Create docs

ocamldoc ErrorReporter.mli -d "..\Docs\ErrorReporter(mli)" -html
ocamldoc RuntimeEntity.mli -d "..\Docs\RuntimeEntity(mli)" -html
ocamldoc Ast.mli -d "..\Docs\Ast(mli)" -html
ocamldoc TreeDrawer.mli -d "..\Docs\TreeDrawer(mli)" -html
ocamldoc Token.mli -d "..\Docs\Token(mli)" -html
ocamldoc Parser.mli -d "..\Docs\Parser(mli)" -html
ocamldoc Scanner.mli -d "..\Docs\Scanner(mli)" -html
ocamldoc IdentificationTable.mli -d "..\Docs\IdentificationTable(mli)" -html
ocamldoc Checker.mli -d "..\Docs\Checker(mli)" -html
ocamldoc TokenPrinter_Pipe.mli -d "..\Docs\TokenPrinter_Pipe(mli)" -html
ocamldoc TokenPrinter_XML.mli -d "..\Docs\TokenPrinter_XML(mli)" -html
ocamldoc TokenPrinter_HTML.mli -d "..\Docs\TokenPrinter_HTML(mli)" -html

ocamldoc ErrorReporter.ml -d "..\Docs\ErrorReporter(ml)" -html
ocamldoc TreeDrawer.ml -d "..\Docs\TreeDrawer(ml)" -html
ocamldoc Parser.ml -d "..\Docs\Parser(ml)" -html
ocamldoc Scanner.ml -d "..\Docs\Scanner(ml)" -html
ocamldoc IdentificationTable.ml -d "..\Docs\IdentificationTable(ml)" -html
ocamldoc Checker.ml -d "..\Docs\Checker(ml)" -html
ocamldoc TokenPrinter_Pipe.ml -d "..\Docs\TokenPrinter_Pipe(ml)" -html
ocamldoc TokenPrinter_XML.ml -d "..\Docs\TokenPrinter_XML(ml)" -html
ocamldoc TokenPrinter_HTML.ml -d "..\Docs\TokenPrinter_HTML(ml)" -html
ocamldoc Compiler.ml -d "..\Docs\Compiler(ml)" -html

REM Create docs

ocamlc ErrorReporter.cmo TreeDrawer.cmo TokenPrinter_Pipe.cmo TokenPrinter_XML.cmo TokenPrinter_HTML.cmo Parser.cmo Scanner.cmo IdentificationTable.cmo Checker.cmo Compiler.cmo -o Triangle.exe

move Triangle.exe ..

cd ..

rd /s /q temp