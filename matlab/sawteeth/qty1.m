function q1 = qty1( astra_out, qty, range )
% q1 = qty1( astra_out, qty, range )
%   Gives back the quantity 'qty' (ASTRA name) of the
%   q = 1 surface for a 'range' of indices of time given

times = eval( [ 'astra_out.t(' range ')' ] );
N = length( times );
q = eval( [ 'astra_out.q( :,' range ')' ] );
quantity = eval( [ 'astra_out.' qty '( :,' range ')' ] );
q1 = zeros( N, 1 );
for i = 1:N
	qi = q( :, i );
	idx = iround( qi, 1 );
	if numel( idx ) > 1
		% More than 1 q=1 surface, take the outermost
		idx = idx( end );
	elseif isempty( idx )
		idx = 1;
	end
	qtyi = quantity(:,i);
	q1( i ) = qtyi( idx );
end
end
