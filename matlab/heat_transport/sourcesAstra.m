function se = sourcesAstraExp( astra_out, varargin )
% se = sourcesAstra( astra_out, varargin )
%   Computes the source of the electron energy [W]
%     varargin{ 1 } : time of simulation (default is end)
%

%             { 2 } : range of rhovol [rho_start rho_end]
if size( varargin, 2 ) >= 1 && ~isempty( varargin{1} )
	it = int2str( iround( astra_out.t, varargin{1} ) );
else
	it = 'end';
end
%if size( varargin, 2 ) >= 2 && ~isempty( varargin{2} ) && numel( varargin{2} ) == 2
	%rhovol = eval( [ 'astra_out.rhovol(:,' it ');' ] );
	%i_start = iround( rhovol, varargin{2}(1) );
	%i_stop  = iround( rhovol, varargin{2}(2) );
	%range = [ int2str( i_start ) ':' int2str( i_stop ) ];
%else
	range = ':';
%end
%%% Astra data %%%
volum = eval( [ 'astra_out.volum( ' range ', ' it ');' ] );
poh_a = eval( [ '1e6 .* astra_out.poh( ' range ', ' it ');' ] );
ecrh_a = eval( [ '1e6 .* astra_out.ecrh( ' range ', ' it ');' ] );
peicl_a = eval( [ '1e6 .* astra_out.peicl( ' range ', ' it ');' ] );

% Powers
[ ai1 ai2 ai3 poh_int ] = interpos( volum, poh_a );
[ ai1 ai2 ai3 ecrh_int ] = interpos( volum, ecrh_a );
% Collisions
[ ai1 ai2 ai3 coll_int ] = interpos( volum, peicl_a );
clear ai1 ai2 ai3;
poh = poh_int(end);
ecrh = ecrh_int(end);
coll = coll_int(end);
% Sum
se = poh + ecrh - coll;
%keyboard
end
