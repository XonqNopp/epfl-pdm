function plot_traces( astra_out, qty, varargin )
% plot_traces( astra_out, qty, varargin )
%   varargin{ 1 } : whatever to add rhotor
%

shot = astra_out.shot;

figure;
set( gca, 'fontsize', 16 );
if size( varargin, 2 ) > 0
	plot( astra_out.t, astra_out.rhotor+eval( [ 'astra_out.' qty ] ) );
else
	plot( astra_out.t, eval( [ 'astra_out.' qty ] ) );
end
xlabel( 't' );
ylabel( qty );
grid( 'on' );
zoom( 'on' );
print( '-dpsc', [ 'pics/' int2str( shot ) '_traces_' qty '.ps' ] );

end
