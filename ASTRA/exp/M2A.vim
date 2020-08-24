function! M2A()
	" All on a single line
	execute normal "JJJJJJJ"
	" Remove whitespaces
	execute "%s/\s\+/\r /g"
	" Put a whitespace in front of the fisrt line
	execute "s/^/ /"
	" Change to scientific format
	execute "%s/^\s*[0-9]\.[0-9]*/\0E+00/g"
	execute "%s/^\(\s*\)1\([0-9]\)\.\([0-9]*\)/\11.\2\3E+01/g"
endfunction
