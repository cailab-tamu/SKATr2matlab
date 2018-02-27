load example_data.mat y X Z
% SKAT_Null_Model Function
mdl=fitlm(X,y);
res=mdl.Residuals.Raw;
s2=mdl.RMSE^2;
X1=x2fx(X);

maf=mean(Z)/2;

% Beta Weights function
weights=betapdf(maf,1,25);

%% KMTest_Linear_Linear
Z=Z.*weights;
Q_temp = res' * Z;
Q = Q_temp * Q_temp'/s2/2;

%%
W_1 = Z' * Z - (Z' * X1) * inv(X1' * X1) * (X1' * Z);


% Denote Y as the n-vector of binary outcomes, X a n × p matrix of covariates (intercept included) to be adjusted, and 
% G the n × m matrix of genotype scores for m variants measured in a gene region. 
% Denote the n-vector of residuals Z = Y – Ŷ, where Ŷ are the fitted probabilities under the null model: log[Pr(Y = 1)/Pr(Y = 0)] = Xα. 
% Denote the n × n null covariance matrix V = Cov(Z), and 
% a m × m diagonal weight matrix W, which is typically determined by the variant MAF.
% The SKAT statistic is defined as Q = Z′GWWG′Z. Under the null hypothesis Q asymptotically follows the mixture of one degree of freedom (1-DF) χ2 
% distributions with the mixing coefficients being the positive eigen values of K = V1/2GWWG′V1/2 (Wu et al., 2010, 2011).
% -- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4761292/



%% Get_Davies_PVal

% The Davies’ method has been implemented in the ‘CompQuadForm’ R package (Lafaye De Micheaux, 2013).
K = W_1/2;

%% Get Lambda
lambda1 = eig(K);
IDX1 = find(eig(K) > 0);
IDX2 = find(lambda1 > mean(lambda1(IDX1))/100000);
lambda = lambda1(IDX2);

%% Get_PValue
A1 = W_1/2;
A2 = A1 * A1;
C1 = [sum(diag(A1)) sum(diag(A2)) sum(sum(A1.*A2')) sum(sum(A2.*A2'))];
muQ = C1(1);
sigmaQ = sqrt(2*C1(2));
s1 = C1(3) / C1(2)^(3/2);
s2 = C1(4) / C1(2)^2;
beta1 = sqrt(8)*s1;
beta2 = 12*s2;
type1 = 0;
if s1^2 > s2
    a = 1/(s1 - sqrt(s1^2 - s2));
    d = s1 *a^3 - a^2;
    l = a^2 - 2*d;
else
    type1 =1;
    a = 1/s1;
    d = 0;
    l = 1/s1^2;
end
muX = l+d;
sigmaX = sqrt(2) *a;
%%
Q_Norm = (Q - muQ)/sigmaQ;
Q_Norm1 = Q_Norm * sigmaX + muX;

pValue = 1-ncx2cdf(Q_Norm1,l,d)
