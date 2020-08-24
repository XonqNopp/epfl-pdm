function fns_out = change_fns( all_fns, field_plus )
% fns_out = change_fns( all_fns, field_plus )
%    creates a structure from all_fns with the directory and the fields from field_plus
%

fns_out.dir = all_fns.dir;

if iscell( field_plus )
	for ii = 1:length(field_plus)
		eval( [ 'fns_out.' field_plus{ii} ' = all_fns.' field_plus{ii} ';' ] );
	end
else
	eval( [ 'fns_out.' field_plus ' = all_fns.' field_plus ';' ] );
end

end
