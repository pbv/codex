#!/usr/bin/tclsh

foreach file $argv {
    puts $file
    set output [file rootname $file].pdf
    if [file exists $output] {
	puts "output file $output exists; skipping"
	continue
    }
    
    exec pandoc -s -f markdown --pdf-engine=xelatex -o $output header.md $file 
}


