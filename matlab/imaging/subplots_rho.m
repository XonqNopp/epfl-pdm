function subplots_rho( astra_out, quantities, xlab, x2coor, varargin )
% subplots_rho( astra_out, quantities, xlab, x2coor, varargin )
%

t = astra_out.t;
rhovol = astra_out.rhovol( :, end );
if isempty( xlab ) || strcmp( xlab, 't' )
	x_plot = t;
	s_it = ':';
	s_irho = int2str( iround( rhovol, x2coor ) );
	x2 = 'rhovol';
else
	x_plot = rhovol;
	s_it = int2str( iround( t, x2coor ) );
	s_irho = ':';
	x2 = 't';
end
numplots = length( quantities );
figure;
for ii = 1:numplots
	y_plot = eval( [ 'astra_out.' quantities{ii} '( ' s_irho ', ' s_it ' )' ] );
	ax(ii) = subplot( numplots, 1, ii );
	plot( x_plot, y_plot );%, ##, 'markersize', 15, 'linewidth', 2 );
	if ii == 1
		title( [ x2 ' = ' num2str( x2coor ) ] );
	end
	%hold on;
	xlabel( xlab );
	ylabel( quantities{ii} );
	grid( 'on' );
end
linkaxes( ax, 'x' );
zoom( 'on' );
%print( '-dpsc', [ '##.ps' ] );


end
