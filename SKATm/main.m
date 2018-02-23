%% SKAT function
%   ARGUMENTS:
%       Z, 
%       obj, 
%       kernel, 
%       method, 
%       weight_beta, 
%       weights, 
%       impute_method, 
%       r_corr, 
%       is_check_genotype, 
%       is_dosage, 
%       missing_cuttoff, 
%       max_maf, 
%       estimate_MAF

Y = dlmread('../inputFiles/y.c.tsv');
X = dlmread('../inputFiles/X.tsv')
Z = dlmread('../inputFiles/Z.tsv');

SKAT()