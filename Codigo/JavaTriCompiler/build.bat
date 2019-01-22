@echo off

javac .\Triangle\AbstractSyntaxTrees\*.java .\Triangle\CodeGenerator\*.java .\Triangle\ContextualAnalyzer\*.java .\Triangle\Printers\*.java .\Triangle\SyntacticAnalyzer\*.java .\Triangle\TreeDrawer\*.java .\Triangle\*.java .\TAM\*.java

jar cfm Triangle.jar MANIFEST.MF .\Triangle\AbstractSyntaxTrees\*.class .\Triangle\CodeGenerator\*.class .\Triangle\ContextualAnalyzer\*.class .\Triangle\Printers\*.class .\Triangle\SyntacticAnalyzer\*.class .\Triangle\TreeDrawer\*.class .\Triangle\*.class .\TAM\*.class