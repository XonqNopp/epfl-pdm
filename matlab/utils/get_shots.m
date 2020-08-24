function shots_used = get_shots(varargin)
% shots_used = get_shots(varargin)
%   varargin{ 1 } : 1 for plot (default 0)
% Gives the variable shots_used with the shots, t0 and Zeff. Plots with tcvview.
shots_used.s29892.t0 = [ 0.53 0.7 1.0 1.3 ];
shots_used.s29892.zeff.ohmic = 2.2;
shots_used.s29892.zeff.ECH = 3.5;
shots_used.s39874.t0 = [ 0.5 1.3 ];
shots_used.s39874.zeff.ECH = 2.0;
shots_used.s40894.t0 = [ 0.85 ];
shots_used.s40894.zeff.ECH = 2.9;
shots_used.s40346.t0 = [ 1.255 ];
shots_used.s40346.zeff.ECH = 3.0;
shots_used.s40378.t0 = [ 1.253 ];
shots_used.s40378.zeff.ECH = 2.9;
%shots_used.s40350.t0 = [ 1.255 ];
%shots_used.s40350.zeff.ECH = [];
%shots_used.s40103.t0 = [ 1.1 ];% Ti available
%shots_used.s40103.zeff.ECH = 2.7;
shots_used.s40080.t0 = [ 0.8 ];% Ti available
shots_used.s40080.zeff = 3;
shots_used.s39863.t0 = [ 0.86 ];
shots_used.s39863.zeff.ECH = 1.7;
%shots_used.s39875.t0 = [ ];
%shots_used.s39875.zeff.ECH = [ ];
shots_used.s39857.t0 = [ 0.6 1.044 ];
shots_used.s39857.zeff.ECH = 1.55;
shots_used.s40045.t0 = [ 1.099 ];
shots_used.s40045.zeff.ECH = 1.8;
shots_used = orderfields( shots_used );

if size( varargin, 2 ) > 0
	the_shots = fieldnames( shots_used );
	Nshots = length(the_shots);
	%Ntot   = Nshots + 4;
	Ntot = 0;
	for ii = 1:Nshots
		Ntot = Ntot + eval(['length(shots_used.' the_shots{ii} '.t0)']);
	end
	%jj=1;
	%ii=1;
	%while ii <= Nshots
	for ii = 1:Nshots
		this_shot = str2num(the_shots{ii}(2:end));
		ts = eval(['shots_used.s' int2str(this_shot) '.t0']);
		if length(ts) > 1
			figure;
			set( gca, 'fontsize', 16 );
			for jj = 1:length(ts)
				subplot(1,length(ts),jj);
				tcvview('pvGt',this_shot,ts(jj));
				pause( 0.001 );
			end
			zoom( 'on' );
			print( '-dpsc', [ 'pics/' int2str(this_shot) '.ps' ] );
		elseif length(ts) == 1
			figure;
			set( gca, 'fontsize', 16 );
			tcvview('pvGt',this_shot,ts(1));
			pause( 0.001 );
			zoom( 'on' );
			print( '-dpsc', [ 'pics/' int2str(this_shot) '.ps' ] );
		end
	end

	%clear Nshots ii the_shots jj this_shot ts Ntot;
end

% Hmode_plot(40346,1.255,5,5);

% ICDBS
%zeff = 3;
%output=icdbseval_to_nodes(shot,1,1,[],[],[],zeff,[],['standard run, sets ok_trial=1, zeff=' num2str(zeff)],[],1);

% Proffit
%params.shot_type='hmode';
%params.zeff=zeff
%[status,data]=proffit_to_nodes(shot,1,params);

% Chie_tcv
%initialize_conf(shot);
%output=chie_tcv_to_nodes(shot,1,1,[],[1 -1 1],[],[],[],[],[],[],1,[],'standard run, use power deposition profiles from toray, sets ok_trial=1');

