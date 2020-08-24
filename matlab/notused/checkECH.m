function ecrh = checkECH( astra_out, varargin )
% ecrh = checkECH( astra_out, varargin )
%    varargin{ 1 } : STRING of the simulation single time you want
%                    (default is 'end')
%

% Checking varargin
if size( varargin, 2 ) >= 1 && ~isempty( varargin{ 1 } )
	t = varargin{ 1 };
else
	t = 'end';
end
disp( [ ' Results will be token at simulation time ''' t '''' ] );

shot = astra_out.shot;
time = astra_out.t0(1);
ecrh.t = time;
% Doing ASTRA
disp( [ '  Computing the integrated ECH power from ASTRA for shot ' int2str( shot ) '...' ] );
volum  = eval( [ 'astra_out.volum(:,' t ');' ] );
ecrh_a = eval( [ '1e6 .* astra_out.ecrh(:,' t ');' ] );
[ ai1 ai2 ai3 ecrh_v ] = interpos( volum, ecrh_a );
clear ai1 ai2 ai3;
ecrh.astra = ecrh_v( end );

% From nodes
mdsopen( shot );
pg = tdi( '\results::toray.input.p_gyro' );
pint = tdi( '\results::toray.output_x:pint' );
mdsclose;
p_gyro = pg.data( :, 10 );
ratio = reshape( pint.data( end, 10, : ), 1, size( pint.data, 3 ) );
time_toray = pint.dim{3};

% Mapping p_gyro on TORAY time
p_gyro_toray = interpos( pg.dim{1}, 1e3 .* p_gyro, time_toray )';
p_absorbed = p_gyro_toray .* ratio;
it = iround( time_toray, time );
ecrh.p_gyro = p_gyro_toray( it );
ecrh.p_absorbed = p_absorbed( it );
ecrh.ratio = ratio( it );

end
