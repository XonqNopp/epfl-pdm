function astra_out = runAstra( astra_args )
% astra_out = runAstra( astra_args )
%
%   The following variable has to be a structure and MUST have the starred fields:
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
%                                struct( shot, what, ntimes, t0, zeff )
%
% standard : astra_out=runAstra(astra_args);

% Fetching information from astra_args, astra_args and astra_args
%%% astra_args %%%
astra_args_required_fields = { 'shot', 't0', 'ECH', 'equ', 'ntimes' };
for ii = 1:length( astra_args_required_fields )
	if ~isfield( astra_args, astra_args_required_fields{ ii } )
		error( [ 'astra_args must contain fields ''' astra_args_required_fields{ii} ''', see help runAstra.' ] );
	end
	eval( [ astra_args_required_fields{ii} ' = astra_args.' astra_args_required_fields{ii} ';' ] );
end
% results directory
if isfield( astra_args, 'resdir' ) && ~strcmp( astra_args.resdir, '' )
	resdir = astra_args.resdir;
else
	resdir = 'datafiles';
end
% HD
if isfield( astra_args, 'HD' ) && ~strcmp( astra_args.HD, '' )
	if ~isempty(strfind(astra_args.HD,' '))
		astra_args.HD = strrep(astra_args.HD,' ','_');
		disp( [ ' Found spaces in HD name, replacing them: ' astra_args.HD ] );
	end
	if strcmp( astra_args.HD(1:2), 'LD' )
		astra_args.HD = [ '_' astra_args.HD ];
	else
		astra_args.HD = [ '_HD_' astra_args.HD ];
	end
	HD = astra_args.HD;
else
	HD = '';
end
% rerun
if isfield( astra_args, 'rerun' )
	rerun = astra_args.rerun;
else
	rerun = 0;
end
% mix
if isfield( astra_args, 'mix' )
	mix = astra_args.mix;
else
	mix = [];
end
% End of input variables checks

% Choose the filename
% Is equ?
if equ == 0
	file2 = 'X';
else
	file2 = '';
end
if ECH == 0
	file3 = '_OH';
else
	file3 = '';
end
% filename => rootfname, resfname
if ~isempty( mix )
	fname = [ mix.what '_' int2str( mix.shot ) ];
else
	if ntimes == 1
		s_ntimes = '';
	else
		s_ntimes = [ int2str(ntimes) '_' ];
	end
	if numel( t0 ) == 1
		fname = [ file2 s_ntimes num2str( t0, '%0.2g' ) file3 ];
	else
		fname = [ file2 s_ntimes num2str( t0(1), '%0.2g' ) '_' num2str( t0(end), '%0.2g' ) file3 ];
	end
end
rootfname = [ int2str( shot ) '_' fname ];
if strcmp(HD,'')
	resfname = fullfile( resdir, [ rootfname 'ASTRA_RES.mat' ] );
else
	resfname = fullfile( resdir, [ rootfname HD '.mat' ] );
end
%keyboard

%% Modelfile name
if equ == 0
	modelfile = 'TCV_EXP';
elseif abs( equ ) == 1 | equ == 2
	modelfile = 'TCV_T_EQ';
elseif equ >= 8
	modelfile = 'TCV_H';
end
%keyboard

% Load filename (create it if needed or asked)
if ~exist( resfname ) | rerun
	launch_astra_tcv_G( astra_args, modelfile, rootfname );
end
if isfield(astra_args,'store') && astra_args.store == 0
	astra_out = [];
	disp( ' No data written, no file loaded...' );
else
	if nargout > 0
		load_fname = resfname;
		load( load_fname );
		disp( [ '  + ' load_fname ' loaded.' ] );
	else
		astra_out = [];
	end
end
end
