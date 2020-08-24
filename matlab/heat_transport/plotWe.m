function f0 = plotWe( s, varargin )
% f0 = plotWe( s )
%

names = fieldnames( s );
for ii = 1:length(names)
	shot = names{ii}(2:end);
	shots(ii) = str2num( shot );
	times = fieldnames( eval( [ 's.' names{ii} ] ) );
	for jj = 0:length(times)-1
		t0s(ii+jj) = str2num( times{jj+1}(2) ) + str2num( times{jj+1}(3) )/10;
		xplot( ii+jj ) = eval( [ 's.' names{ii} '.' times{jj+1} '.ped' ] ) ./ 1000;
		yplot( ii+jj ) = eval( [ 's.' names{ii} '.' times{jj+1} '.core' ] ) ./ 1000;
	end
end

%if size( varargin, 2 ) > 0 && ~isempty( varargin{1} ) && varargin{1} ~= 0
	%f0 = varargin{1};
	%figure(f0);
	%hold on;
%else
	f0 = figure;
%end
set( gca, 'fontsize', 16 );
%plot( xplot, yplot, 'xb' );%##, ##, ##, 'markersize', 15, 'linewidth', 2 );
%hold on;
ok_shots = [ 39838 39729 39856 39857 39844 ];
for ii = 1:length(names)
	times = fieldnames( eval( [ 's.' names{ii} ] ) );
	if size(varargin,2)==0 | any( shots(ii) == ok_shots )
		for jj = 0:length(times)-1
			%th = text(xplot(ii+jj),yplot(ii+jj),int2str(shots(ii)));
			th = text(xplot(ii+jj),yplot(ii+jj),[int2str(shots(ii)) ',' num2str(t0s(ii+jj))]);
			set( th, 'fontsize', 10, 'horizontalalignment', 'center' );
		end
	end
end
xlabel( 'W_{ped} [kJ]' );
ylabel( 'W_{core} [kJ]' );
axis( [ 0 max(xplot) 0 max(yplot) ] );
%legend( ##, ##, 'Location', 'Best' );
grid( 'on' );
zoom( 'on' );
hold off;
%print( '-dpsc', [ '##.ps' ] );


end
