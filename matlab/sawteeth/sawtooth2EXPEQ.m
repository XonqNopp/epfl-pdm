function sawtooth2EXPEQ( astra_out_filename, astra_time_indices, dir_name, varargin )
% sawtooth2EXPEQ( astra_out_filename, astra_time_indices, dir_name, varargin )
%     dir_name : directory name in datafiles/CHEASE
%     varargin{ 1 } : 1 for rewrite EXPEQ and EXPTNZ files (default is 0)

load( astra_out_filename );
shot = astra_out.shot;
t = astra_out.t';
clear astra_out;
[ chease_dir nn ee ] = fileparts( astra_out_filename );
chease_dir = fullfile( chease_dir, [ 'CHEASE/' dir_name ] );
for ii = 1:numel(astra_time_indices)
	suffix = [ int2str( shot ) '_' int2str( astra_time_indices( ii ) ) ];
	eqname = [ 'EXPEQ_' suffix ];
	tnzname = [ 'EXPTNZ_' suffix ];
	if ~exist( fullfile( chease_dir, eqname ), 'file' ) | ~exist( fullfile( chease_dir, tnzname ), 'file' ) | ( size( varargin, 2 ) >= 1 & ~isempty( varargin{ 1 } ) & varargin{ 1 } == 1 )
		disp( [ '   ## Running shot #' int2str( shot ) ' for itime = ' int2str( astra_time_indices( ii ) ) ' (' int2str(ii) ' of ' int2str( length( astra_time_indices ) ) ') ##' ] );
		astra2EXPEQ( astra_out_filename, astra_time_indices( ii ), [ int2str( shot ) '_' int2str( astra_time_indices( ii ) ) ], chease_dir );
	else
		disp( [ '   + EXPEQ and EXPTNZ files already exist for shot #' int2str( shot ) ' it = ' int2str( astra_time_indices( ii ) ) ] );
	end
end
f1=figure;
subplot('position',[0.1,0.1,0.35,0.85]);
%set(gca,'fontsize',16);
grid( 'on' );
hold on;
subplot('position',[0.6,0.1,0.35,0.35])
%set(gca,'fontsize',16);
grid( 'on' );
hold on;
subplot('position',[0.6,0.6,0.35,0.35])
%set(gca,'fontsize',16);
grid( 'on' );
hold on;
f2=figure;
subplot( 2, 2, 1 );
%set(gca,'fontsize',16);
hold on;
subplot( 2, 2, 2 );
%set(gca,'fontsize',16);
hold on;
subplot( 2, 2, 3 );
%set(gca,'fontsize',16);
hold on;
subplot( 2, 2, 4 );
%set(gca,'fontsize',16);
hold on;
%colors = get( 0, 'defaultaxescolororder' );
colors = { 'c', 'r', 'g', 'm', 'k', 'b' };
for ii = 1:numel(astra_time_indices)
	%col = colors( mod( ii, size( colors, 1 ) ) + 1, : );
	col = colors{ mod( ii, length( colors ) ) + 1 };
	figure(f1);
	plot_EXPEQ( fullfile( chease_dir, [ 'EXPEQ_' int2str( shot ) '_' int2str( astra_time_indices( ii ) ) ] ), col );%, 'markersize', 15, 'linewidth', 2 );
	figure(f2);
	plot_exptnz( fullfile( chease_dir, [ 'EXPTNZ_' int2str( shot ) '_' int2str( astra_time_indices( ii ) ) ] ), col );%, 'markersize', 15, 'linewidth', 2 );
end
%keyboard
figure(f1);
zoom( 'on' );
%hold off;
subplot('position',[0.1,0.1,0.35,0.85]);
%grid( 'on' );
legend( [ repmat( 't = ', length(astra_time_indices), 1 ) num2str( t( astra_time_indices ) ) ], 'Location', 'Best' );
%subplot('position',[0.6,0.1,0.35,0.35])
%grid( 'on' );
%subplot('position',[0.6,0.6,0.35,0.35])
%grid( 'on' );
figure(f2);
zoom( 'on' );
%hold off;
subplot( 2, 2, 4 );
legend( [ repmat( 't = ', length(astra_time_indices), 1 ) num2str( t( astra_time_indices ) ) ], 'Location', 'NorthEast' );
%
%print( '-dpsc', '##.ps' );

load( astra_out_filename );
shot = int2str( astra_out.shot );
timefile = [ 'times_idx_' shot ] ;
for lk = 1:numel( astra_time_indices )
	timefile = [ timefile '_' int2str( astra_time_indices( lk ) ) ];
end
timefile = [ timefile '.txt' ];
timefile = fullfile( chease_dir, timefile );
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
