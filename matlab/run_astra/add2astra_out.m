function astra_out = add2astra_out( astra_in, items )
% astra_out = add2astra_out( astra_out, items )
%    items : structure which each field will be field of
%            astra_out with the same content
%

astra_out = astra_in;
news = fieldnames( items );
for ii = 1:length(news)
	eval( [ 'astra_out.' news{ii} ' = items.' news{ii} ';' ] );
end

end
