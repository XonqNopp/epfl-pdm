function var = loadascii( filename )
% var = loadascii( filename )
%

var = load( filename, '-ascii' );
%ifind = strfind(filename,'.');
%tmpvarname = strrep(filename(1:ifind(end)-1),'.','_');
%if ~isempty(strfind(tmpvarname,'/'))
	%ifind2 = strfind(tmpvarname,'/');
	%tmpvarname = tmpvarname(ifind2(end)+1:end);
%end
%eval( [ 'var = ' tmpvarname ';' ] );

end
