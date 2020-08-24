function out = draw_psistar(astra_out,t_crash)
% out = draw_psistar(astra_out,t_crash)
%

figpos = [ 3 132 1265 820 ];
R0 = 0.88;
%shot = astra_out.shot;
t = astra_out.t;
it = iround(t,t_crash);
rhovol = astra_out.rhovol(:,it-1:it);
ne = astra_out.ne(:,it-1:it);
te = astra_out.te(:,it-1:it);
q = astra_out.q(:,it-1:it);
r = astra_out.ametr(:,it-1:it);
rra(:,1) = r(:,1) ./ r(end,1);
rra(:,2) = r(:,2) ./ r(end,2);
r2 = r.^2;
Btor = astra_out.btor(:,it-1:it);

to_int = ( 1./q - 1 );
for ii = 1:size(to_int,2)
	[aa aa1 aa2 psistar(:,ii)] = interpos(r2(:,ii),to_int(:,ii));
	psistar(:,ii) = psistar(:,ii) - psistar(end,ii);
	%psistar(:,ii) = ( ( 2 .* Btor(ii) ) ./ R0 ) .* psistar(:,ii);
end
max_psistar = psistar(1,end);
psistar0    = psistar(1,1);
r_max       = rra(end,1);
iold_maxs   = (find(psistar(:,1)==max(psistar(:,1))));
iold_max    = min(iold_maxs);
rold_max    = rra(iold_max,1);
dpst        = diff(psistar(:,1));
imax        = min(find(dpst<0));
irmix       = imax - 1 + min(find(abs(psistar(imax:end,1)-psistar0)<0.0001));
rmix        = rra(irmix,1);

out.psistar = psistar;
%out.rmix = rmix;

figure;
set(gca,'fontsize',16);
set(gcf,'name','psi_star');
set(gcf,'outerposition',figpos);
plot( rra, psistar );
hold on;
plot([0 1],              [max_psistar max_psistar],'--k','linewidth',1);
plot([0 1],              [psistar0 psistar0],'--k','linewidth',1);
plot([rmix rmix],        [0 max_psistar],'--k','linewidth',1);
plot([rold_max rold_max],[0 max_psistar],'--k','linewidth',1);
xlabel( 'r/r_a' );
ylabel( '\psi_*' );
grid( 'on' );
zoom( 'on' );
%xlim([0 r(end,1)]);
print( '-dpsc', [ 'emil/psi_star.ps' ] );

figure;
set(gca,'fontsize',16);
set(gcf,'name','te');
plot( rra, te );
hold on;
plot([rmix rmix],        [min(min(te)) max(max(te))],'--k','linewidth',1);
plot([rold_max rold_max],[min(min(te)) max(max(te))],'--k','linewidth',1);
xlabel( 'r/r_a' );
ylabel( 'T_e' );
grid( 'on' );
zoom( 'on' );
print( '-dpsc', [ 'emil/te.ps' ] );

figure;
set(gca,'fontsize',16);
set(gcf,'name','ne');
plot( rra, ne );
hold on;
plot([rmix rmix],        [min(min(ne)) max(max(ne))],'--k','linewidth',1);
plot([rold_max rold_max],[min(min(ne)) max(max(ne))],'--k','linewidth',1);
xlabel( 'r/r_a' );
ylabel( 'n_e' );
grid( 'on' );
zoom( 'on' );
print( '-dpsc', [ 'emil/ne.ps' ] );

figure;
set(gca,'fontsize',16);
set(gcf,'name','q');
plot( rra,q );
hold on;
plot([rmix rmix],        [0 max(max(q))],'--k','linewidth',1);
plot([rold_max rold_max],[0 max(max(q))],'--k','linewidth',1);
xlabel( 'r/r_a' );
ylabel( 'q' );
grid( 'on' );
zoom( 'on' );
print( '-dpsc', [ 'emil/q.ps' ] );


%keyboard

% Prepare to write file
% Structure will be r/ra-pre r/ra-post psi-pre psi-post te-pre te-post ne-pre ne-post q-pre q-post
towrite(:,1) =rra(:,1);
towrite(:,2) =rra(:,2);
towrite(:,3) =psistar(:,1);
towrite(:,4) =psistar(:,2);
towrite(:,5) =te(:,1);
towrite(:,6) =te(:,2);
towrite(:,7) =ne(:,1);
towrite(:,8) =ne(:,2);
towrite(:,9) =q(:,1);
towrite(:,10)=q(:,2);
save('emil/r_psistar_te_ne_q.txt','towrite','-ascii');

end
