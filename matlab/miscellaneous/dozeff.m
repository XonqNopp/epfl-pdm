function dozeff( shot, ecrh )
% dozeff( shot, ecrh )
%     Used to write to nodes with the right Zeff
%   ecrh : 1 if there is, 0 if ohmic shot
%

% Run proffit and write first
proffit_to_nodes(shot,1,'plot_opt',[1 0]);% trial_index should be 0 but that does not work...

% Loop on icdbs_eval to find the appropriate Zeff
thisz = 3;
zeff = 3;
while thisz > 0,
	zeff = thisz;
	icdbseval_to_nodes(shot,-1,[],[],[],[],zeff);
	disp( '' );
	disp( '' );
	disp( '  Choose a better Zeff (put 0 if the last one is the best, negative if you don t want to save) :' );
	thisz = input( [ '   (last one was ' num2str( zeff ) ' ) ' ] );
end

% Run all with the good Zeff
if thisz == 0
	% icdbs_eval
	icdbseval_to_nodes(shot,1,1,[],[],[],zeff,[],'standard run, sets ok_trial=1',[],1);

	% proffit
	proffit_to_nodes(shot,1,'plot_opt',[1 0],'zeff',zeff);

	% write_pgyro
	if ecrh
		write_pgyro(shot,1,[],[],[],[],[],1);
	end

	% chie_tcv
	if ecrh
		warning( 'ECH power not averaged' );
		chie_tcv_to_nodes(shot,1,1,[],[1 1],[],[],[],[],zeff,[],1,[],'standard run, use power deposition profiles from toray, sets ok_trial=1');
	else
		chie_tcv_to_nodes(shot,1,1,[],-1,[],[],[],[],zeff,[],1,[],'standard run, sets ok_trial=1');
	end
	disp( [ '   Done! Zeff used : ' num2str( zeff ) ] );
end
end
