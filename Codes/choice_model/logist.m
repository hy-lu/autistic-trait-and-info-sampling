function p = logist(x)
%logist A simple logistic function
%   Return the probability of stopping drawing with a logistic function
p = 1 ./ (1 + exp(-x));
end

