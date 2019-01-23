@echo off

rd /s /q "Parser\ASTPrinter(XML)"
rd /s /q "Parser\TokensPrinter(HTML)"
rd /s /q "Parser\TokensPrinter(Pipe)"
rd /s /q "Parser\TokensPrinter(XML)"

mkdir "Parser\ASTPrinter(XML)"
mkdir "Parser\TokensPrinter(HTML)"
mkdir "Parser\TokensPrinter(Pipe)"
mkdir "Parser\TokensPrinter(XML)"

rd /s /q "Scanner\TokensPrinter(HTML)"
rd /s /q "Scanner\TokensPrinter(Pipe)"
rd /s /q "Scanner\TokensPrinter(XML)"

mkdir "Scanner\TokensPrinter(HTML)"
mkdir "Scanner\TokensPrinter(Pipe)"
mkdir "Scanner\TokensPrinter(XML)"

powershell -Command "foreach ($g in $(ls .\Scanner\Tests -n)) {.\..\Triangle.exe .\Scanner\Tests\$g -tpp Scanner\'TokensPrinter(Pipe)'\$g.txt -tpx Scanner\'TokensPrinter(XML)'\$g.xml -tph Scanner\'TokensPrinter(HTML)'\$g.html}"


powershell -Command "foreach ($g in $(ls .\Parser\Tests -n)) {.\..\Triangle.exe .\Parser\Tests\$g -xt Parser\'ASTPrinter(XML)'\$g.xml -tpp Parser\'TokensPrinter(Pipe)'\$g.txt -tpx Parser\'TokensPrinter(XML)'\$g.xml -tph Parser\'TokensPrinter(HTML)'\$g.html}"