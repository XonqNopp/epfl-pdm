function launch_astra_tcv_G( astra_args, modelfile, filename )
% launch_astra_tcv_G( astra_args, modelfile, filename )
% The following variable has to be a structure and MUST have the starred fields:
%     astra_args : shot (*)
%                  t0 (*)
%                  ECH (*)     : 1 if ECH, 0 if ohmic-only
%                  equ (*)     : 0 to have TeExp, 1 to have TE:EQ, 2 for twin shot 
%                                8 for H-mode
%                  ntimes (*)  : number of times to start the run
%                  delta_t     : time step in conf nodes (default: 50ms)
%                  HD          : suffix for model file for HD run
%                  rerun       : 1 to force rerun (0 default)
%                  store       : 1 to store the results (default)
%                  rewrite     : 1 to force rewrite the EXP file (0 default)
%                  resdir      : directory you save your astra_out
%                  t_offset    : t_offset
%                  TeDivider   : initial divider for the H-mode model file
%                  mix         : structure for twin shot run
%                               struct( shot, what, ntimes, t0, zeff )
%    modelfile : name without path
%    filename  : name of EXP file without path. Will be used to name the file containing astra_out
% The following variable has to be structure and can have the following fields:
%
% standard use: launch_astra_tcv_G(astra_args,modelfile,filename);
%

%%% Getting info from astra_args %%%
astra_args_required_fields = { 'shot', 't0', 'ECH', 'equ', 'ntimes' };
for ii = 1:length( astra_args_required_fields )
	if ~isfield( astra_args, astra_args_required_fields{ ii } )
		error( [ 'astra_args must contain fields ''' astra_args_required_fields{ii} ''', see help launch_astra_tcv_G.' ] );
	end
	eval( [ astra_args_required_fields{ii} ' = astra_args.' astra_args_required_fields{ii} ';' ] );
end
% delta_t (for more than one time)
if isfield( astra_args, 'delta_t' )
	delta_t = astra_args.delta_t;
else
	delta_t = 0.05;
end
% Store
if isfield( astra_args, 'store' )
	store = astra_args.store;
else
	store = 1;
end
% HD
if isfield( astra_args, 'HD' ) && ~strcmp( astra_args.HD, '' )
	%HD = [ '_HD_' astra_args.HD ];
	HD = astra_args.HD;
	fprintf('\n');
	if store
		fprintf( [ ' Saving astra_out as HD (case ' HD ').\n' ] );
	else
		warning( ' HD run without saving... :-S Setting HD=''''' );
		HD='';
	end
else
	HD = '';
end
fprintf('\n');
%%% Getting info from astra_args %%%
% rewrite
if isfield( astra_args, 'rewrite' )
	rewrite = astra_args.rewrite;
else
	rewrite = 0;
end
if isfield( astra_args, 'resdir' )
	astra_dir = astra_args.resdir;
else
	astra_dir = 'datafiles';
end
% t_offset
if isfield( astra_args, 't_offset' )
	t_offset = astra_args.t_offset;
else
	%t_offset = 0.3;
	t_offset = astra_args.t0;
end
% TeDivider
if isfield( astra_args, 'TeDivider' )
	TeDivider = astra_args.TeDivider;
else
	TeDivider = 1;% Needed since initial TE=TEX/TeDivider
end
% mix
if isfield( astra_args, 'mix' )
	mix = astra_args.mix;
else
	if equ == 2
		disp( ' To run twin shot (astra_args.equ=2), you must provide a structure containing' );
		error( '   the information about the twin shot in astra_args.mix.' );
	end
end
%%% End of getting info

% Time interval
if ntimes <= 1 | numel( t0 ) > 1
	timeshort = t0;
else
	timeshort = [ t0 ( t0 + ntimes * delta_t ) ];% Er... :-S
end
fprintf( [ '   + MODEL : ' modelfile '\n' '   + EXP   : ' filename '_EXP\n\n' ] );
%keyboard

if ~exist( [ '~/astra/exp/' filename '_EXP' ], 'file' ) | rewrite
	% Sources
	sources.Ip_source = 'liuqe';
	sources.mu_source = 'manual';
	if equ == 2
		sources.chie_source = 'twin shot';
	else
		sources.chie_source = 'conf nodes';
	end
	%sources.Te_ne_Ti_source = 'conf nodes';
	sources.Te_ne_source = 'conf nodes';
	sources.Ti_source = 'CXRS';
	if isempty( ECH ) || ECH <= 0
		sources.ECH_source = 'none';
		sources.Pgyro_source = 'none';
	else
		sources.ECH_source = 'toray nodes';
		%sources.Pgyro_source = 'toray nodes';
		sources.Pgyro_source = 'conf nodes';
	end
	sources.ECCD_source = 'none';
	sources.Bphi_source = 'tree';
	sources.Bnd_source = 'liuqe params';
	sources.Zeff_source = 'nodes';
	% TCV_to_ASTRA
	if equ == 2
		in = tcv_to_astra_exp(shot,timeshort,sources,t_offset,mix);
	else
		in = tcv_to_astra_exp(shot,timeshort,sources,t_offset);
	end

	% Adding 1 / Lne
	%in = add1overLne( in, t0 );
	% Adding the initial Tex divider
	in = addTeDivider( in, TeDivider );
	disp( ' ' );

	plot_astra_exp(in)

	%keyboard
	itp = input( '' );
	if ~isempty( itp )
		error( 'Astra exp file NOT written...' );
	end
	% write for ASTRA
	write_astra_exp(filename,in);
end
% Run ASTRA
astra_tcv(filename,astra_dir,modelfile,store,t0(1),t_offset,'-p 0');
if store
	storefile_prev = fullfile( astra_dir, [ filename 'ASTRA_RES'] );
	if strcmp( HD, '' )
		storefile_next = fullfile( astra_dir, [ filename 'ASTRA_RES.mat'] );
	else
		storefile_next = fullfile( astra_dir, [ filename HD '.mat'] );
	end
	% Load the astra_out
	load( storefile_prev, '-mat' );
	% Store some additional parameters in astra_out and add .mat extension
	items.shot = shot;
	items.t0 = t0;
	astra_out = add2astra_out( astra_out, items );
	unix( [ 'rm ' storefile_prev ] );
	save( storefile_next,'astra_out');
	disp( [ ' astra_out saved in ' storefile_next ] );
	% Copy the model file to keep the input data
	unix( [ 'cp ~/astra/equ/' modelfile    ' ~/astra/files_bckup/' filename '_' modelfile HD '_EQU' ] );
	unix( [ 'cp ~/astra/exp/' filename '_EXP ~/astra/files_bckup/' filename               HD '_EXP' ] );
	disp( ' Model and exp files backed up to the files_bckup directory.' );
end
end
