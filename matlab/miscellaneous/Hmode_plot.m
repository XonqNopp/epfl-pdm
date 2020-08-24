function Hmode_plot(shot,t0,Nplot,deriv)
% Hmode_plot(shot,t0,Nplot,deriv)
%

if deriv > 5
	deriv = 5;
elseif deriv < 0
	deriv = 0;
end
mdsopen(shot);
qq_tdi = tdi('\results::conf:ne');
rhovol_tdi = tdi('\results::conf:rhovol');
mdsclose;
it = iround(rhovol_tdi.dim{2},t0);
rhovol = rhovol_tdi.data(:,it);
qq = qq_tdi.data(:,it);
[aa dqq   ddqq  ] = interpos(rhovol,  qq,rhovol,-1);
[aa dddqq ddddqq] = interpos(rhovol,ddqq,rhovol,-1);
clear aa;
rho_zero = zeros( size( rhovol ) );

maxNplotx = 3;
Nplotx = Nplot;
Nploty = 1;
while Nplotx > maxNplotx
	Nploty = Nploty + 1;
	Nplotx = ceil( Nplot / Nploty );
end

figpos = [ 3 132 1265 820 ];
f0 = figure;
set(f0,'outerposition',figpos);
set( gca, 'fontsize', 16 );
title(['Looking for the minimum of the ' int2str(deriv) ' derivative']);

% SP1 %
sp(1) = subplot(Nplotx,Nploty,1);
set( gca, 'fontsize', 16 );
plot( rhovol, qq );
xlabel( '\rho_V' );
%ylabel( 'electron pressure' );
grid( 'on' );
if Nplot > 1
	% SP2 %
	sp(2) = subplot(Nplotx,Nploty,2);
	set( gca, 'fontsize', 16 );
	plot(rhovol,dqq);
	hold on;
	plot(rhovol,rho_zero,'--r');
	hold off;
	xlabel('\rho_V');
	ylabel('d / d\rho');
	grid( 'on' );
end
if Nplot > 2
	% SP3 %
	sp(3) = subplot(Nplotx,Nploty,3);
	set( gca, 'fontsize', 16 );
	plot(rhovol,ddqq);
	hold on;
	plot(rhovol,rho_zero,'--r');
	hold off;
	xlabel('\rho_V');
	ylabel('d^2 / d\rho^2');
	grid( 'on' );
end
if Nplot > 3
	% SP4 %
	sp(4) = subplot(Nplotx,Nploty,4);
	set( gca, 'fontsize', 16 );
	plot(rhovol,dddqq);
	hold on;
	plot(rhovol,rho_zero,'--r');
	hold off;
	xlabel('\rho_V');
	ylabel('d^3 / d\rho^3');
	grid( 'on' );
end
if Nplot > 4
	% SP5 %
	sp(5) = subplot(Nplotx,Nploty,5);
	set( gca, 'fontsize', 16 );
	plot(rhovol,ddddqq);
	hold on;
	plot(rhovol,rho_zero,'--r');
	hold off;
	xlabel('\rho_V');
	ylabel('d^4 / d\rho^4');
	grid( 'on' );
end

% END SP %
linkaxes(sp,'x');
zoom( 'on' );
disp( ' Prepare the figure to point at it...' );
pause;

xl = xlim;
xlim( [ xl(1) min(xl(2),1) ] );
ylim('auto');
disp( ' Choose range to find rho_ped' );
[xx yy] = ginput(2);
clear yy;
if xx(1) > xx(2)
	xx = flipud(xx);
end
for ii = 1:Nplot
	subplot(Nplotx,Nploty,ii);
	xlim(xx);
end
i1 = iround(rhovol,xx(1));
i2 = iround(rhovol,xx(2));
Nd = 'dddddddddd';
findmin = eval( [ Nd(1:deriv-1) 'qq(i1:i2)' ] );
irho = i1 - 1 + find(min(findmin)==findmin);
rhoped = rhovol(irho);
qqped  = qq(irho);
%keyboard
for ii = 1:Nplot
	subplot(Nplotx,Nploty,ii);
	grid( 'off' );
	xlim('auto');
	y_range = ylim;
	hold on;
	plot([rhoped rhoped],y_range,'--k');
	hold off;
end
pause;
close(f0);

figure;
%set(gcf,'outerposition',figpos);
set(gca,'fontsize',18);
plot(rhovol,qq,'linewidth',2);
hold on;
plot([rhoped rhoped],[0 qqped*1.15],'--k','linewidth',2);
text(rhoped,qqped*1.25,'\rho_{ped}','fontsize',16,'horizontalalignment','center')
hold off;
xlabel('\rho_V');
ylabel( qq_tdi.units );
%N = 6;
%set(gca,'xtick',[0 linspace(rhoped/N,rhoped,N) 1]);
%labs = {};
%for ii = 1:N
	%labs{ii+1} = '';
%end
%labs{1} = '0';
%labs{1+N} = 'rho_ped';
%labs{1+N+1} = '1';
%iN = floor(N/2);
%labs{1+iN} = 'Core';
%set(gca,'xticklabel',labs);
grid( 'on' );
zoom( 'on' );
title( [ '#' int2str(shot) ', t=' num2str(t0) ] );
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_rhoped.ps' ] );
set(gcf,'paperpositionmode','auto');
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_rhoped_ppm.ps' ] );
end
