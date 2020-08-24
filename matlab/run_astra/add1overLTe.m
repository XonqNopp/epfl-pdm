function out = add1overLTe( in, t0, varargin )
% out = add1overLTe( in, varargin )
%   varargin{ 1 } : 1 for plot, default 0
%

out = in;
shot = in.shot;
%t0 = in.t0;
R0 = 0.88;
% 1 / L_{n_e}
mdsopen( shot );
rlte_conf = tdi( '\results::conf:r_lte' );
grho1_conf = tdi( '\results::conf:grho1' );
rhovol_conf =  tdi( '\results::conf:rhovol' );
time = rlte_conf.dim{2};
mdsclose;
% time (all comes from conf nodes thus on same time scale)
it = iround( rlte_conf.dim{ 2 }, t0(1) );
% Compute
%grho1 = grho1_conf.data( :, it );
grho1 = grho1_conf.data;
%rlte_bis = rlte_conf.data( :, it ) ./ ( R0 .* grho1 );
rlte_bis = rlte_conf.data ./ ( R0 .* grho1 );
% ASTRA works with rho_vol
rhovol = rhovol_conf.data( :, it );
%rhovol = rhovol_conf.data;

%% OLD STUFF %%
%rhopsi = rhovol_conf.dim{ 1 };
%[ bb drhopsi_drhovol ] = interpos( rhovol, rhopsi );
%clear bb;
% IS THIS WELL DONE?????????
%rlte_ter = interpos( rhopsi, rlte_bis, rhovol );
%rlte_data = rlte_ter .* drhopsi_drhovol;
%% OLD STUFF %%

rlte_data = rlte_bis;
if size( varargin,2)>0 && ~isempty(varargin{1}) && varargin{1}>0
	figure;
	set( gca, 'fontsize', 16 );
	plot( rhovol, rlte_data, '-b', 'markersize', 15, 'linewidth', 2 );
	xlabel( '\rho_{Vol}' );
	ylabel( '1/L_{Te}' );
	grid( 'on' );
	zoom( 'on' );
end

%keyboard

% Creating structure
rlte.data = rlte_data;
rlte.rgrid = rhovol;
rlte.tgrid = time;
rlte.gridtype = 14;% gridtype specifies the radial variable. See table 5.5 p. 131
rlte.comment = '1 / L_{Te}';
rlte = select_times_G( rlte, t0 );
out.CAR1 = rlte;
disp( ' CAR1 : 1/LTe profile' );

end

