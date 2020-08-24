function out = add1overLne( in, t0, varargin )
% out = add1overLne( in, varargin )
%   varargin{ 1 } : 1 for plot, default 0
%

out = in;
shot = in.shot;
%t0 = in.t0;
R0 = 0.88;
% 1 / L_{n_e}
mdsopen( shot );
rlne_conf = tdi( '\results::conf:r_lne' );
grho1_conf = tdi( '\results::conf:grho1' );
rhovol_conf =  tdi( '\results::conf:rhovol' );
time = rlne_conf.dim{2};
mdsclose;
% time (all comes from conf nodes thus on same time scale)
it = iround( rlne_conf.dim{ 2 }, t0(1) );
% Compute
%grho1 = grho1_conf.data( :, it );
grho1 = grho1_conf.data;
%rlne_bis = rlne_conf.data( :, it ) ./ ( R0 .* grho1 );
rlne_bis = rlne_conf.data ./ ( R0 .* grho1 );
% ASTRA works with rho_vol
rhovol = rhovol_conf.data( :, it );
%rhovol = rhovol_conf.data;

%% OLD STUFF %%
%rhopsi = rhovol_conf.dim{ 1 };
%[ bb drhopsi_drhovol ] = interpos( rhovol, rhopsi );
%clear bb;
% IS THIS WELL DONE?????????
%rlne_ter = interpos( rhopsi, rlne_bis, rhovol );
%rlne_data = rlne_ter .* drhopsi_drhovol;
%% OLD STUFF %%

rlne_data = rlne_bis;
%if size( varargin, 2 ) >= 1 && ~isempty( varargin{ 1 } ) && varargin{ 1 } > 0
	%figure;
	%set( gca, 'fontsize', 16 );
	%plot( rhovol, rlne_data, '-b', 'markersize', 15, 'linewidth', 2 );
	%xlabel( '\rho_{Vol}' );
	%ylabel( '1/L_{ne}' );
	%grid( 'on' );
	%zoom( 'on' );
%end

%keyboard

% Creating structure
rlne.data = rlne_data;
rlne.rgrid = rhovol;
rlne.tgrid = time;
rlne.gridtype = 14;% gridtype specifies the radial variable. See table 5.5 p. 131
rlne.comment = '1 / L_{ne}';
rlne = select_times_G( rlne, t0 );
out.CAR10 = rlne;
disp( ' CAR10 : 1/Lne profile' );

end
