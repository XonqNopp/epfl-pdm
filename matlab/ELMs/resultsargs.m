function results_args = resultsargs(instruct,varargin)
% results_args = resultsargs(instruct,varargin)
%   instruct has following fields (starred are required)
%      filenames (*)
%      tELM (*)
%      deltaELM (*)
%      tauELM (*)
%      rhos (*)
%      study (*)
%      tis
%      quantities
%      dont_close
%      nonew_rhos
%      save_pics
%      addnote
%      addname
% varargin{ 1 } : value for the quantities fields (default 0)
%

q_val = 0;
if size(varargin,2)>0 && ~isempty(varargin{1})
	q_val = varargin{1};
end

required_fields = {'filenames','tELM','rhos','deltaELM','tauELM','study'};
optional_fields = {'tis','quantities','dont_close','nonew_rhos','save_pics','addnote','addname'};
for ii = 1:length(required_fields)
	if ~isfield(instruct,required_fields{ii})
		error( [ ' Missing field ' required_fields{ii} ' as input' ] );
	end
	eval( [ required_fields{ii} ' = instruct.' required_fields{ii} ';' ] );
end
% Default values:
%deltaELM = 20;
%tauELM   = 100;
tis = [];
quantities.te = q_val;
quantities.lte = q_val;
quantities.ne = q_val;
quantities.lne = q_val;
quantities.p_e = q_val;
quantities.gradp = q_val;
quantities.ti = q_val;
quantities.itot = q_val;
quantities.ibs = q_val;
quantities.ibsped = q_val + 0.78;
quantities.shear = q_val;
quantities.q = q_val;
quantities.upl = q_val;
dont_close = 1;
nonew_rhos = 1;
save_pics = 1;
addnote = { 'Top of ne ped', 'max \nabla p (equil)' };
addname='';
% Checking if provided by user
for ii = 1:length(optional_fields)
	if isfield(instruct,optional_fields{ii})
		eval( [ optional_fields{ii} ' = instruct.' optional_fields{ii} ';' ] );
	end
end

%results_args.deltaELM = deltaELM;
%results_args.tauELM   = tauELM;
results_args.study = study;

results_args.astradir = filenames.dir;
filenames = rmfield( filenames, 'dir' );

results_args.tis = tis;

results_args.rhos0 = rhos;

%results_args.quantities = {'te','ne','p_e','ti','itot','ibsped','shear','q','upl'};
results_args.quantities = quantities;
clear quantities;

results_args.dont_close = dont_close;

results_args.nonew_rhos = nonew_rhos;

results_args.save_pics = save_pics;

results_args.addnote = addnote;

results_args.addname = addname;

fn=fieldnames(filenames);
for kk = 1:length(fn)
	if ~strcmp(eval(['filenames.' fn{kk}]),'')
		if ~isfield(tELM,fn{kk}) | ~isfield(deltaELM,fn{kk}) | ~isfield(tauELM,fn{kk})
			error( [ ' Missing ELM time for ' fn{kk} ] );
		end
		eval( [ 'results_args.' fn{kk} '.fn        = filenames.' fn{kk} ';' ] );
		eval( [ 'results_args.' fn{kk} '.t         = tELM.'      fn{kk}  ';' ] );
		eval( [ 'results_args.' fn{kk} '.deltaELM  = deltaELM.'  fn{kk}  ';' ] );
		eval( [ 'results_args.' fn{kk} '.tauELM    = tauELM.'    fn{kk}  ';' ] );
	end
end

end
