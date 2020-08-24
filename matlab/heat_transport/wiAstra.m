function wi = wiAstra( astra_out, varargin )
% wi = wiAstra( astra_out, varargin )
%   Computes the energy of the ions [W]
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
ni = eval( [ '1e19 .* astra_out.ni( ' range ', ' it ' );' ] );
ti = eval( [ 'astra_out.ti( ' range ', ' it ' );' ] );
wi_a = 1.5 .* ni .* ti .* kev2j;
[ ai1 ai2 ai3 wi_int ] = interpos( volum, wi_a );
clear ai1 ai2 ai3;
wi = wi_int( end );

end
