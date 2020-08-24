function plotdd( rho, qq, varargin )
% plotdd( rho, quantity, varargin )
%   Plots the quantity together with its spatial derivative
%     varargin{ 1 } : label for quantity

if size( varargin, 2 ) >= 1 && ~isempty( varargin{1} )
	ylab = varargin{1};
else
	ylab = '?';
end

[aa dqq] = interpos( rho, qq );
clear aa;

figure;
set( gca, 'fontsize', 16 );
% Subplot 1
ax(1) = subplot( 2, 1, 1 );
plot( rho, qq );%, ##, 'markersize', 15, 'linewidth', 2 );
xlabel( '\rho_{V}' );
ylabel( ylab );
grid( 'on' );
% Subplot 2
ax(2) = subplot( 2, 1, 2 );
plot( rho, dqq );%, ##, 'markersize', 15, 'linewidth', 2 );
xlabel( '\rho_{V}' );
ylabel( [ 'd\_' ylab ] );
grid( 'on' );
% Common features
linkaxes( ax, 'x' );
zoom( 'on' );

end
