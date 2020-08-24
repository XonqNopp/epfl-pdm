function Grun(shot,ef,varargin)
% Grun(shot,exp_file)

bi.chietcv=9;
bi.ntimes=1;
mf='TCV_H_HALFTEX';
ew.rewrite=0;
launch_astra_tcv_G(shot,0,1,bi,mf,ef,ew,0);
end
