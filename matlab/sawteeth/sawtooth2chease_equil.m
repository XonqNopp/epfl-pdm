function sawtooth2chease_equil( astra_out_filename, astra_time_indices, dir_name )
% sawtooth2chease_equil( astra_out_filename, astra_time_indices, dir_name )
%     dir_name : directory name in datafiles/CHEASE

[ aaa basename ccc ] = fileparts( astra_out_filename );
clear aaa ccc;
load( astra_out_filename );
shot = int2str( astra_out.shot );
%clear astra_out;
cheasedir = [ 'datafiles/CHEASE/' dir_name ];
datadir = 'datafiles';
%tmpdir = '/tmp/tp41';
[ s uname ] = unix( 'whoami' );
clear s;
tmpdir = [ '/tmp/' uname( ~isspace( uname ) ) ];
timefile = [ 'times_idx_' shot ] ;
for lk = 1:numel( astra_time_indices )
	timefile = [ timefile '_' int2str( astra_time_indices( lk ) ) ];
end
timefile = [ timefile '.txt' ];
timefile = fullfile( cheasedir, timefile );

for ii = 1:numel(astra_time_indices)
	newname = [ shot '_' int2str( astra_time_indices( ii ) ) ];
	fprintf( [ '\n   ## Running for itime = ' int2str( astra_time_indices( ii ) ) ' (' int2str(ii) ' of ' int2str( length( astra_time_indices ) ) ') ##\n' ] );
	astra2chease_equil( astra_out_filename, astra_time_indices( ii ) );
	% Move from tmp to here o., o.cols, EXPEQ.OUT, EXPTNZ.OUT, EQDSK.OUT
	files_old = { [ 'o.chease.' basename ], [ 'o.chease.' basename '.cols' ], [ 'EXPTNZ.OUT_' basename ], [ 'EXPEQ.OUT_' basename ], [ 'EQDSK.OUT_' basename ] };
	files_new = { [ 'o.chease.' newname ], [ 'o.chease.' newname '.cols' ], [ 'EXPTNZ.OUT_' newname ], [ 'EXPEQ.OUT_' newname ], [ 'EQDSK.OUT_' newname ] };
	for jk = 1:length( files_old )
		oldfile = fullfile( tmpdir, files_old{ jk } );
		newfile = fullfile( cheasedir, files_new{ jk } );
		if exist( newfile, 'file' )
			unix( [ 'rm ' newfile ] );
		end
		unix( [ 'mv -v ' oldfile ' ' newfile ] );
	end
	% Move EXPEQ
	expname = fullfile( cheasedir, [ 'EXPEQ_' newname ] );
	exptomove = fullfile( datadir, [ 'EXPEQ_' basename ] );
	if exist( expname, 'file' )
		unix( [ 'rm ' expname ] );
	end
	unix( [ 'mv -v ' exptomove ' ' expname ] );
	% Move EXPTNZ
	tnzname = fullfile( cheasedir, [ 'EXPTNZ_' newname ] );
	tnztomove = fullfile( datadir, [ 'EXPTNZ_' basename ] );
	if exist( tnzname, 'file' )
		unix( [ 'rm ' tnzname ] );
	end
	unix( [ 'mv -v ' tnztomove ' ' tnzname ] );
	% MOVE namelist
	namelistname = fullfile( cheasedir, [ 'astra_chease_namelist_' shot ] );
	namelisttomove = fullfile( tmpdir, 'astra_chease_namelist' );
	if exist( namelistname, 'file' );
		unix( [ 'rm -v ' namelistname ] );
	end
	unix( [ 'mv -v ' namelisttomove ' ' namelistname ] );
end
fid = fopen( timefile, 'wt' );
if fid < 0
	error( 'Cannot write the file with time indices.' );
end
fprintf( fid, [ 'Time of shot where profiles have been token to ASTRA EXP file:\n' num2str( astra_out.t0( 1 ), '%0.4g' ) 's\n' ] );
fprintf( fid, 'ASTRA time from indices\n' );
for ii = 1:numel( astra_time_indices )
	fprintf( fid, [ int2str( astra_time_indices( ii ) ) ' : ' num2str( astra_out.t( astra_time_indices( ii ) ), '%0.5g' ) 's\n' ] );
end
fclose( fid );
end
