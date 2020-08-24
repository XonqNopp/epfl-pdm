function a_cn()
% a_cn()
%  Displays the auto Vn built in ASTRA

rho = [0:0.05:1];
x0 = 0.87;
dx = 0.2;
alpha = 10;
cst = 1.5;
min_req = zeros( size( rho ) );
min_req( rho > x0 ) = 1;

%%% PLOT %%%
figure;
set( gca, 'fontsize', 16 );
plot( [rho(1) rho(end)], [cst cst], 'linewidth', 2 );
hold on;
plot( rho, cst+a_fcn(rho,x0,dx,alpha,min_req), '--r', 'linewidth', 2 );
xlabel( '\rho_{ASTRA}' );
ylabel( '-V_n' );
%legend( 'std', 'H-mode', 'location', 'northwest' );
%% MEMO %%
legend( 'CRAD3', '+CRAD2 e\^( -(CRAD1-x)^2 / (CRAD4/2)^2 )', 'location', 'northwest' );
title( 'Taking only ascending part of gaussian' );
%%
grid( 'on' );
%zoom( 'on' );
print( '-dpsc', [ 'pics/a_cn.ps' ] );

end
