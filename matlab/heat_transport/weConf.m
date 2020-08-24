function we = weConf( shot, t0, varargin )
% we = weConf( shot, t0, varargin )
%   varargin{ 1 } : range on rhovol [rho_start rho_end]
%           { 2 } : we input (to be saved only)
%

rhovol_ped = 0.8; % most simple solution
if size( varargin, 2 ) > 1 && ~isempty( varargin{2} )
	we = varargin{2};
end

for ii = 1:numel(shot)
	t1 = floor( t0(ii ) );
	t2 = floor( 10*( t0(ii) - t1 ) );
	tsave = [ 't' int2str( t1 ) int2str( t2 ) ];
	shotsave = [ 's' int2str( shot(ii) ) ];
	mdsopen(shot(ii));
	rhovol_tdi = tdi( '\results::conf:rhovol' );
	it = iround( rhovol_tdi.dim{2}, t0(ii) );
	rhovol = rhovol_tdi.data(:,it);
	irhovol_ped = iround( rhovol, rhovol_ped );
	if size( varargin, 2 ) > 0 && ~isempty( varargin{1} )
		if numel(varargin{1}) == 1 & varargin{1} == 0
			range = [];
		else
			i_start = iround( rhovol, varargin{1}(1) );
			i_stop  = iround( rhovol, varargin{1}(2) );
			range = [ int2str( i_start ) ':' int2str( i_stop ) ];
		end
	else
		range = ':';
	end

	ev2j = 1.602e-19;

	te_tdi = tdi( '\results::conf:te' );
	ne_tdi = tdi( '\results::conf:ne' );
	vol_tdi = tdi( '\results::conf:vol' );
	mdsclose;
	if ~isempty( range )
		te = eval( [ 'te_tdi.data(' range ',it)' ] );
		ne = eval( [ 'ne_tdi.data(' range ',it)' ] );
		pe = ne .* te .* ev2j;
		volum = eval( [ 'vol_tdi.data(' range ',it)' ] );

		dwedvol = 1.5 .* pe;
		[ ai1 ai2 ai3 we_int ] = interpos( volum, dwedvol );
		eval( [ 'we.' shotsave '.' tsave ' = we_int(end);' ] );
	else
		% Core
		te = te_tdi.data(1:irhovol_ped,it);
		ne = ne_tdi.data(1:irhovol_ped,it);
		pe = ne .* te .* ev2j;
		volum = vol_tdi.data(1:irhovol_ped,it);

		dwedvol = 1.5 .* pe;
		[ ai1 ai2 ai3 we_int ] = interpos( volum, dwedvol );
		eval( [ 'we.' shotsave '.' tsave '.core = we_int(end);' ] );
		clear we_int dwedvol te ne pe volum;
		% Pedestal
		te = te_tdi.data(irhovol_ped:end,it);
		ne = ne_tdi.data(irhovol_ped:end,it);
		pe = ne .* te .* ev2j;
		volum = vol_tdi.data(irhovol_ped:end,it);

		dwedvol = 1.5 .* pe;
		[ ai1 ai2 ai3 we_int ] = interpos( volum, dwedvol );
		eval( [ 'we.' shotsave '.' tsave '.ped = we_int(end);' ] );
	end
end

end
