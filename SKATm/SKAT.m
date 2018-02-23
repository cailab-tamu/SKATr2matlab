function [re] = SKAT (Z, obj, kernel, method, weight_beta, weights, impute_method, r_corr, is_check_genotype, is_dosage, missing_cuttoff, max_maf, estimate_MAF)
%% Setting default values for function as in R code
if ~exist('kernel','var')
    kernel = 'linear_weigthed';
end
if ~exist('method','var')
    method = 'davies';
end
if ~exist('weight_beta','var')
    [weight_beta] = [1,25];
end
if ~exist('weights','var')
    weights = [];
end
if ~exist('impute_method','var')
    impute_method = 'fixed';
end
if ~exist('r_corr','var')
    r_corr = 0;
end
if ~exist('is_check_genotype','var')
    is_check_genotype = true;
end
if ~exist('is_dosage','var')
    is_dosage = false;
end
if ~exist('missing_cutoff','var')
    missing_cutoff = 0.15;
end
if ~exist('max_maf','var')
    max_maf = 1;
end
if ~exist('estimate_MAF','var')
    estimate_MAF = 1;
end
%% 
end