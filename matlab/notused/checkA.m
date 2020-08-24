function [ out_ok out_bad out_miss ]  = checkA( astra_out, ffs )
% [out_ok out_bad out_miss]=checkA(astra_out,ffs)
%   Tells which are the OK, bad and missing fields in ffs
%

L = length( ffs );
ii_ok = 1;
ii_bad = 1;
for ii = 1:L
	eval( [ 'miss.' ffs{ii} ' = 1;' ] );
	if isfield( astra_out, ffs{ii} )
		out_ok{ii_ok} = ffs{ii};
		ii_ok = ii_ok + 1;
	else
		out_bad{ii_bad} = ffs{ii};
		ii_bad = ii_bad + 1;
	end
end
fastra = fieldnames( astra_out );
La = length(fastra);
clear ii
ii_miss = 1;
for ii = 1:La
	if ~isfield( miss, fastra{ii} )
		out_miss{ii_miss} = fastra{ii};
		ii_miss = ii_miss + 1;
	end
end
out_ok   = sort( out_ok );
out_bad  = sort( out_bad );
out_miss = sort( out_miss );

end
