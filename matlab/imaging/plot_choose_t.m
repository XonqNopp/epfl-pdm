function t_i = plot_choose_t( astra_out, quantities, t_crash, t_i0, varargin )
% t_i = plot_choose_t( astra_out, quantities, t_crash, t_i0, varargin )
%   quantities is a structure which fieldnames are the quantities to plot
%      the value of each field defines how to plot it
%        0 : ask the user
%        1 : max of qty within range given by user
%        2 : same but max of gradient of qty
%   t_i0 : t_i from previous plots (leave empty if there isn't)
%   varargin{ 1 } : additional name to save
%

if size(varargin,2)>0 && ~isempty(varargin{1}) && ~strcmp(varargin{1},'')
	addname = [ '_' varargin{1}];
else
	addname = '';
end
default_lw = 1;

% Global needed parameters
shot   = astra_out.shot;
t0     = astra_out.t0(1);
rhovol = astra_out.rhovol(:,end);% Assuming rhovol is almost not varying
volum  = astra_out.volum;
t      = astra_out.t;
R0 = 0.88;
t_i = t_i0;

qqs = fieldnames( quantities );
for ii = 1:length(qqs)
	this_crash = t_crash;
	qq_cont = eval( [ 'quantities.' qqs{ii} ] );
	back = check_compute_needed( astra_out, qqs{ii} );
	qq = back.data;

	L = size( qq, 2 );
	shorter = [ 1:10:L ];

	figpos = [ 3 132 1265 820 ];
	f0 = figure;
	set( gca, 'fontsize', 16 );
	set(f0,'outerposition',figpos);
	plot( rhovol, qq(:,shorter), 'linewidth', default_lw );
	xlabel( '\rho_V' );
	ylabel( [ back.name ' ' back.units ] );
	grid( 'on' );
	zoom( 'on' );

	disp( ' Position the figure (and zoom if you want) then hit enter...' );
	pause;
	%if qq_cont == 0
		disp( '  Please choose the rhovol interval on which I will plot the time traces.' );
		figure(f0);
		[x1 yy]=ginput(2);
		if x1(2) < x1(1)
			x1 = flipud(x1);
		end
		irho1 = iround( rhovol, x1(1) );
		irho2 = iround( rhovol, x1(2) );

		figure(f0);
		clf;
		set( gca, 'fontsize', 16 );
		if any( max(qq) > 1e11 )
			q2q = qq(irho1:irho2,:);
			for uli = 1:size(q2q,1)% for each trace
				q2q(uli,:) = q2q(uli,:) ./ max(q2q(uli,:));
			end
			plot( t, q2q, 'linewidth', default_lw );
		else
			plot( t, qq(irho1:irho2, : ), 'linewidth', default_lw );
		end
		ylabel( [ back.name ' ' back.units ] );
	%elseif qq_cont == 1
		%% Max of qty
		%disp( '  Please select a left boundary for thr range within I will plot the max.' );
		%figure(f0);
		%[x1 yy]=ginput(1);
		%irho1 = iround( rhovol, x1(1) );
%
		%figure(f0);
		%clf;
		%set( gca, 'fontsize', 16 );
		%plot( t, max(qq(irho1:end, : ) ), 'linewidth', default_lw );
		%ylabel( [ 'max of ' qqs{ii} ] );
	%else
		%% Max of grad qty
		%disp( '  Please select a left boundary for thr range within I will plot the max of grad.' );
		%figure(f0);
		%[x1 yy]=ginput(1);
		%irho1 = iround( rhovol, x1(1) );
		%for jk = 1:size( qq, 2)
			%[ aa gradqq(:,jk) ] = interpos( volum(irho1:end, jk ), qq(irho1:end, jk ) );
		%end

		%f1 = figure;
		%set( gca, 'fontsize', 16 );
		%plot( rhovol(irho1:end), gradqq );
		%xlabel( '\rho_V' );
		%ylabel( [ 'grad ' qqs{ii} ] );
		%grid( 'on' );
		%zoom( 'on' );
		%disp( [ '    Check if grad ' qqs{ii} ' seems plausible and hit enter.' ] );
		%pause;
		%close(f1);
		%for kl = 1:size( qq, 2 )
			%iis = iround( abs(gradqq(:,kl)), max(abs(gradqq(:,kl))) );
			%dgq = abs( abs(gradqq(iis,kl) ) - max( abs( gradqq(iis,kl) ) ) );
			%iis2 = find( dgq == min(dgq) );
			%if numel(iis2) > 1
				%iis2 = iis2(1);
			%end
			%% Now iis2 has only 1 element at each time step
			%iis_ok = iis(iis2);
			%qplot(kl) = qq(iis_ok,kl);
		%end

		%figure(f0);
		%clf;
		%set( gca, 'fontsize', 16 );
		%%plot( t, qq(iis) );
		%plot( t, qplot, 'linewidth', default_lw );
		%ylabel( [ qqs{ii} ' at max of grad' ] );
	%end

	xlabel( 't [s]' );
	grid( 'on' );
	zoom( 'on' );
	disp( '   Zoom on the zone of interest and hit the enter key...' );
	pause;

	disp( '  Please choose the times at which you want to see the profile (end by enter).' );
	figure(f0);
	% Plotting already-stored times
	grid off;
	get_ax = axis;
	y_range = [ get_ax(3) get_ax(4) ];
	if ~isempty( t_i )
		hold on;
		for et = 1:length(t_i)
			plot(this_crash+[t_i(et) t_i(et)],y_range,':k','linewidth',default_lw);
		end
		hold off;
	end
	[x2 yy] = ginput;
	for jj = 1:length(x2)
		it = iround( t, x2(jj) );
		storing = 0;
		if t(it) >= this_crash% ask user
			storing = 1;
		else
			storing = input( [ ' Chosen time is before crash (' num2str( 1000 * abs( t(it) - this_crash ) ) 'ms before), do you want to use it ? [0/1] ' ] );
		end
		if storing > 0
			if ~any(abs(t_i-(t(it)-this_crash)) < 0.000001 )% at 1us precision
			%if ~any(its==it)
				%its = [ its it ];
				%its = sort( its );
				t_i = [ t_i; t(it)-this_crash ];
				t_i = sort( t_i );
			end
		end
	end
	%disp( [ '   @ You have now ' int2str(numel(its)) ' chosen times.' ] );
	disp( [ '   @ You have now ' int2str(numel(t_i)) ' chosen times.' ] );
	%if numel(its)>20 & numel(its)<=28
	if numel(t_i)>20 & numel(t_i)<=28
		disp( '     Be careful not to choose too many times of interest if you want to see something...' );
	%elseif numel(its)>28
	elseif numel(t_i)>28
		warning( ' There are more than 28 time chosen (display problem)' );
	end
	close(f0);
	clear f0 qq x1 x2 yy get_ax y_range qplot gradqq dgq irho1 irho2 q2q L;
	fprintf('\n');
end

filename = [ 'datafiles/ti_' int2str( shot ) '_' num2str(t0) addname '.mat' ];
disp( [ '  Saving times into ' filename ] );
save( filename, 't_i', '-ascii' );
end
