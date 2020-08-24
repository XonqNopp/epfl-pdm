function get_ST(shot,t0,plusminus)
% get_ST(shot,t0,plusminus)
%

mdsopen(shot);
dmpx_tdi = tdi('\atlas::dt100_northeast_001:channel_001');
mdsclose;

figure;
set( gca, 'fontsize', 16 );
plot( dmpx_tdi.dim{1}, dmpx_tdi.data );
xlabel( 't [s]' );
ylabel( 'DMPX signal [V]' );
xlim([t0-plusminus t0+plusminus]);
grid( 'on' );
zoom( 'on' );
title(['#' int2str(shot)]);
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_DMPX.ps' ] );

end
