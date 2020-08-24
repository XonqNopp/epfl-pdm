function out = addTaueScaling( in )
% out = addTaueScaling( in )
%

% out
out = in;
% Getting basic info
shot = in.shot;
t0 = in.t0;
% Fetching MDS data
mdsopen( shot );
taue_tdi = tdi( '\results::conf:taue100' );
hh_tdi   = tdi( '\results::conf:h_scal' );
if all( size( hh_tdi.data ) == [ 1 1 ] ) && hh_tdi.data == 0
	hh_tdi = tdi( '\results::conf:h_scal:trial' );
end
mdsclose;
% Getting some more parameters
it = iround( taue_tdi.dim{1}, t0 );
scales = { 'RLW', 'IAEA-TCV', 'ITER98L', 'ITER98P', 'ITER98(y,2)' };
which_scale = 1;
% Computing
taue    = taue_tdi.data( it );
hh_data = squeeze( hh_tdi.data( it, which_scale, : ) );
if size( hh_data, 1 ) == 1
	if isnan( hh_data )
		error( ' Scaling is NaN...' );
	end
else
	inans = find( isnan( hh_data ) );
	if isempty( inans )
		disp( '   * Many trial index for scaling law, taking the fisrt' );
		hh = hh_data( 1 );
	else
		shh   = size( hh_data, 1 );
		list = [];
		jj = 0;
		for ii = 1:shh
			if ~any( ismember( inans, ii ) )
				jj = jj + 1;
				list(jj) = ii;
			end
		end
		if isempty( list )
			error( ' Scaling are NaNs...' );
		end
		if length( list ) == 1
			hh = hh_data( list );
		else
			disp( '   * Many trial index for scaling law, taking the fisrt non-NaN' );
			hh = hh_data( list( 1 ) );
		end
	end
end
%keyboard
% Adding to ASTRA
disp( [ ' ZRD39 : taue_scaling (' scales{which_scale} ')' ] );
out.ZRD39 = taue / hh;

end
