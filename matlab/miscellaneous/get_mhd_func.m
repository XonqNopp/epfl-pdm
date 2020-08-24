function [climmhd_prev varargout]=get_mhd_func(shot,varargin)
% [climmhd_prev varargout]=get_mhd_func(shot,varargin)
%     varargin{ 1 } : nfft (default 512)
%             { 2 } : onlyspecfig (0), 1 for only spectrogram figure
%             { 3 } : climmhd
%
%     varargout{ 1 } : [ himagesc hbarmhd ] % I don't know what could be useful
%
% standard use : climmhd_prev = get_mhd_func(shot);
%

mdsopen(shot);
mhd=mdsvalue('mhdval(''LFS_DBPOL_002'',1)');
tmhd=mdsvalue('dim_of(mhdval(''LFS_DBPOL_002'',1))');
tmhd=tmhd; %used to have -0.04, probably

% varargin{ 1 } is nfft
if size( varargin, 2 ) >= 1 && ~isempty( varargin{ 1 } )
  nfft = varargin{ 1 };
else
  nfft = 512;
end
l=nfft;

% varargin{ 2 } is onlyspecfig
if size( varargin, 2 ) >= 2 && ~isempty( varargin{ 2 } )
  onlyspecfig = varargin{ 2 };
else
  onlyspecfig = 0;
end

% varargin{ 3 } is climmhd
if size( varargin, 2 ) >= 3 && ~isempty( varargin{ 3 } )
  climmhd = varargin{ 3 };
end

mhdx=max(reshape(mhd(1:l*fix(length(mhd)/l)),l,fix(length(mhd)/l)));
tmhdm=mean(reshape(tmhd(1:l*fix(length(tmhd)/l)),l,fix(length(tmhd)/l)));

[B,F,T]=specgram(mhd,nfft,1/mean(diff(tmhd)),hanning(nfft),nfft/2);
if ~onlyspecfig
  %%% 1 %%% Magntitude vs time
  figure;
  plot(tmhdm,mhdx);
  xlabel('time [s]');
  ylabel('magnitude of LFS_DBPOL_002(1)');
  title(['#' int2str(shot)]);
  %%% 2 %%% Frequency vs time
  figure;
  imagesc(T+tmhdm(1),F,20*log10(abs(B)));
  axis xy;
  colormap jet;
  title(['#' num2str(shot) ' Intensity of poloidal magnetic field flux']);
  xlabel('time [s]');
  ylabel('Mode frequency [Hz]');
  colorbar;
  [mb imb]=max(abs(B));
  TF=T(mb>0.2*max(mb))+tmhdm(1);
  FF=F(imb(mb>0.2*max(mb)));
  hold on;
  plot(TF(FF>0),FF(FF>0),'.');
  zoom on;
  disp('for better display, try a new nfft');% and then' );
%  disp( 'spectrogram(mhd,nfft,1/mean(diff(tmhd)),hanning(nfft),nfft/2);' );
  %%% 3 %%% Mode frequency vs time
  figure;
  plot(TF(FF>0),FF(FF>0),'.');
  title(['#' num2str(shot)]);
  xlabel('time [s]');
  ylabel('Mode frequency [Hz]');

end

%%% 4 %%% Frequency vs time
figure;
set(gcf,'paperpositionmode','auto');

if exist('climmhd')
  himagesc=imagesc(T+tmhdm(1),F,20*log10(abs(B)),climmhd);
  axis xy;
  colormap jet;
  hbarmhd=colorbar;
  climmhd_prev=get(hbarmhd,'ylim');
else
  himagesc=imagesc(T+tmhdm(1),F,20*log10(abs(B)));
  axis xy;
  colormap jet;
  hbarmhd=colorbar;
  climmhd_prev=get(hbarmhd,'ylim');
  disp('to use specific limits for colormap, set');
  disp('climmhd = climmhd_prev;');
  disp('to use previous');% or climmhd = [min max];' );
end
title(['#' num2str(shot) ' Intensity of poloidal magnetic field flux']);
xlabel('time [s]');
ylabel('Mode frequency [Hz]');

disp(['for wiki pages, upload file after: print -djpeg100 ' num2str(shot) '_mhdspec.jpg']);

if nargout > 1
	varargout{ 1 } = [ himagesc hbarmhd ];
end
end
