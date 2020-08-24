function we = weAstra( astra_out, varargin )
% we = weAstra( astra_out, varargin )
%   Computes the energy of the electrons [W]
%     varargin{ 1 } : time of simulation (default is end)
%             { 2 } : range of rhovol [rho_start rho_end]
%

if size( varargin, 2 ) >= 1 && ~isempty( varargin{1} )
	it = int2str( iround( astra_out.t, varargin{1} ) );
else
	it = 'end';
end
if size( varargin, 2 ) >= 2 && ~isempty( varargin{2} ) && numel( varargin{2} ) == 2
	rhovol = eval( [ 'astra_out.rhovol(:,' it ');' ] );
	i_start = iround( rhovol, varargin{2}(1) );
	i_stop  = iround( rhovol, varargin{2}(2) );
	range = [ int2str( i_start ) ':' int2str( i_stop ) ];
else
	range = ':';
end

kev2j = 1.602e-16;
volum = eval( [ 'astra_out.volum(' range ', ' it ' );' ] );
ne = eval( [ '1e19 .* astra_out.ne( ' range ', ' it ' );' ] );
te = eval( [ 'astra_out.te( ' range ', ' it ' );' ] );
we_a = 1.5 .* ne .* te .* kev2j;
[ ai1 ai2 ai3 we_int ] = interpos( volum, we_a );
clear ai1 ai2 ai3;
we = we_int( end );

end
