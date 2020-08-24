function plots1( astra_out, varargin )
% plots1( astra_out, varargin )
%     varargin{ 1 } : 1 for double plot, 2 for plot on existing figure
%             { 2 } : range of time indices

dd = 3;
if length( varargin ) >= 2 && ~isempty( varargin{ 2 } ) && numel( varargin{ 2 } ) == 2 && varargin{2}(1) > 0 && varargin{2}(2) > 0
	range = [ int2str( varargin{2}(1) - dd ) ':' int2str( varargin{2}(2) + dd ) ];
else
	range = ':';
end

times = eval( [ 'astra_out.t(' range ')' ] );
s1 = qty1( astra_out, 'shear', range );
%keyboard
if length( varargin ) >= 1 & ~isempty( varargin{ 1 } )
	if varargin{ 1 } == 1
		doubleplot = 1;
		existplot = 0;
	elseif varargin{ 1 } == 2
		doubleplot = 0;
		existplot = 1;
	else
		doubleplot = 0;
		existplot = 0;
	end
else
	doubleplot = 0;
	existplot = 0;
end
if ~existplot
	figure;
end
if doubleplot
	subplot(2,1,1);
end
set( gca, 'fontsize', 16 );
plot( times, s1, '-b', 'markersize', 15, 'linewidth', 2 );
xlabel( 'time' );
ylabel( 's1' );
grid( 'on' );
axis( [ times(1) times(end) 0.95*min(s1) 1.05*max(s1) ] );
if doubleplot
	subplot(2,1,2);
	set( gca, 'fontsize', 16 );
	plot( times, s1, '-b', 'markersize', 15, 'linewidth', 2 );
	xlabel( 't' );
	ylabel( 's1' );
	grid( 'on' );
end
zoom( 'on' );
%print( '-dpsc', '##.ps' );

end
