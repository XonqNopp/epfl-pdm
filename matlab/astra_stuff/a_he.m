function a_he()
% a_he()
%  Displays the auto chie built in ASTRA

rho = [0:0.02:1];
alpha = 6;
expo = 3;
cst = 2;
x0 = 0.9;
heavi0 = 0.82;
dx = 0.2;
ampli = a_fheL(1,alpha,expo,cst)-cst/2;
min_req = zeros( size( rho ) );
min_req( rho > x0 ) = 1;
min2 = zeros( size( rho ) );
min2( rho > 1-dx ) = 0.1;
min3 = 1000 .* ones( size( rho ) );
min3( rho > 1-dx ) = 0.1;

%%% PLOT %%%
figure;
set( gca, 'fontsize', 16 );
plot( rho, a_fheL(rho,alpha,expo,cst), 'linewidth', 2 );
grid( 'on' );
xlabel( '\rho_{ASTRA}' );
ylabel( '\chi_e' );
axis([0 1 -8 8]);
hold on;
plot(rho,max(a_fheL(rho,alpha,expo,cst).*(1-heaviside(rho-heavi0)),0.1),'-m');
print('-dpsc','pics/a_he_1bis.ps');
set(gcf,'paperpositionmode','auto');
print('-dpsc','pics/a_he_1bis_ppm.ps');

clf;
set( gca, 'fontsize', 16 );
plot( rho, a_fheL(rho,alpha,expo,cst), 'linewidth', 2 );
grid( 'on' );
xlabel( '\rho_{ASTRA}' );
ylabel( '\chi_e' );
axis([0 1 -8 8]);
print('-dpsc','pics/a_he_1.ps');
set(gcf,'paperpositionmode','auto');
print('-dpsc','pics/a_he_1_ppm.ps');
set(gcf,'paperpositionmode','manual');
hold on;
plot( rho, -a_fheH(rho,x0,dx,min_req,ampli), '-r', 'linewidth', 2 );
grid( 'on' );
xlabel( '\rho_{ASTRA}' );
ylabel( '\chi_e' );
print('-dpsc','pics/a_he_2.ps');
set(gcf,'paperpositionmode','auto');
print('-dpsc','pics/a_he_2_ppm.ps');
set(gcf,'paperpositionmode','manual');
pp = plot( rho, min(max(a_fheL(rho,alpha,expo,cst)-a_fheH(rho,x0,dx,min_req,ampli),min2),min3), '-m', 'linewidth', 2 );
xlabel( '\rho_{ASTRA}' );
ylabel( '\chi_e' );
print('-dpsc','pics/a_he_3.ps');
set(gcf,'paperpositionmode','auto');
print('-dpsc','pics/a_he_3_ppm.ps');
set(gcf,'paperpositionmode','manual');
set(pp,'linestyle','--');
legend( 'L-mode \chi_e', 'exp decay', 'H-mode \chi_e', 'location', 'southwest' );
%legend( 'cst+alpha  x\^expo', 'ampli e\^( -(x0-x)^2/(dx/2)^2)', '1 + 2', 'location', 'southwest' );

%% MEMO FOR ME %%
%legend( 'CHI4+CNB1  x\^CNB2', 'ampli e\^( -(CNB3-x)^2/(CHE4/2)^2)', '1 + 2', 'location', 'southwest' );
%title( 'lower boundary CNB4' );
%%

grid( 'on' );
%zoom( 'on' );
print( '-dpsc', [ 'pics/a_he.ps' ] );

end
