#!/bin/bash

rm -r 'Parser/ASTPrinter(XML)'
rm -r 'Parser/TokensPrinter(HTML)'
rm -r 'Parser/TokensPrinter(Pipe)'
rm -r 'Parser/TokensPrinter(XML)'

mkdir 'Parser/ASTPrinter(XML)'
mkdir 'Parser/TokensPrinter(HTML)'
mkdir 'Parser/TokensPrinter(Pipe)'
mkdir 'Parser/TokensPrinter(XML)'

rm -r 'Scanner/TokensPrinter(HTML)'
rm -r 'Scanner/TokensPrinter(Pipe)'
rm -r 'Scanner/TokensPrinter(XML)'

mkdir 'Scanner/TokensPrinter(HTML)'
mkdir 'Scanner/TokensPrinter(Pipe)'
mkdir 'Scanner/TokensPrinter(XML)'

for files in ./Scanner/Tests/*
do
	file=$(basename -- "$files")

	./../Triangle "Scanner/Tests/$file" -tpp "Scanner/TokensPrinter(Pipe)/$file".txt
	./../Triangle "Scanner/Tests/$file" -tpx "Scanner/TokensPrinter(XML)/$file".xml
	./../Triangle "Scanner/Tests/$file" -tph "Scanner/TokensPrinter(HTML)/$file".html
done

for files in ./Parser/Tests/*
do
	file=$(basename -- "$files")

	./../Triangle "Parser/Tests/$file" -xt  "Parser/ASTPrinter(XML)/$file".xml
	./../Triangle "Parser/Tests/$file" -tpp "Parser/TokensPrinter(Pipe)/$file".txt
	./../Triangle "Parser/Tests/$file" -tpx "Parser/TokensPrinter(XML)/$file".xml
	./../Triangle "Parser/Tests/$file" -tph "Parser/TokensPrinter(HTML)/$file".html
done