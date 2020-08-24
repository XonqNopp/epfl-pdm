function rhos = choose_rhos( astra_out, quantities, t_i, t_crash, deltaELM, addname0, rhos0 )
% rhos = choose_rhos( astra_out, quantities, t_i, t_crash, deltaELM, addname0, rhos0 )
%    astra_out
%    quantities
%    t_i
%    t_crash
%    deltaELM
%    addname0
%    rhos0 : rhos from previous computation
%

if ~strcmp( addname0, '' )
	addname = [ '_' addname0 ];
else
	addname = '';
end
rhos = rhos0;

savepics = 0;
shot = astra_out.shot;
rhovol = astra_out.rhovol(:,end); % assuming almost constant
t0 = astra_out.t0(1);
filename = [ 'datafiles/ti_' int2str( shot ) '_' num2str(t0) addname '.mat' ];
figs = plot_chosen_profiles( astra_out, quantities, t_i, t_crash, deltaELM, addname0, savepics );
for ii = figs
	figure(ii);
	zoom on;
	disp( ' Dispose the figure ready to point on it.' );
	pause;
	disp( ' Please choose the rho you are interested in...' );
	figure(ii);
	grid off;
	get_ax = axis;
	y_range = [ get_ax(3) get_ax(4) ];
	hold on;
	for jkl = 1:length(rhos)
		plot( [rhos(jkl) rhos(jkl)],y_range,':k','linewidth',1);
	end
	hold off;
	[xx yy]=ginput;
	close(ii);
	for kl = 1:length(xx)
		irho = iround( rhovol, xx(kl) );
		if ~any( abs( rhos - rhovol(irho) ) <= 0.01 )
			rhos = [ rhos rhovol(irho) ];
			rhos = sort( rhos );
		end
	end
	disp([' > You now have ' int2str(length(rhos)) ' stored...']);
end
filename_rhos = [ 'datafiles/rhos_' int2str(shot) '_' num2str(t0) '.mat' ];
fprintf( [ '  Saving rhos in ASCII in ' filename_rhos '...' ] );
save( filename_rhos, 'rhos', '-ascii' );
fprintf( '  Done !\n' );

end
