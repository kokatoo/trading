function returns()
    compound30Years();
    bondsStocks();
end

function compound30Years()
    mu = .085;
    sigma = .15;
    initial = 1000;
    
    rets = normrnd(mu, sigma, 100, 30);
    finalReturns = initial * prod(1+rets, 2);

    hist(finalReturns, 20)
    title('30-Year Compounding');
    xlabel('30-Year Value');
    ylabel('Counts');
    
    fprintf('\nInitial amount: %d', initial);
    fprintf('\nMean return: %.2f', mu);
    fprintf('\nStd Dev: %.2f', sigma);
    fprintf('\nFinal Mean: %.2f', mean(finalReturns));
    fprintf('\nFinal Std Dev: %.2f', mean(finalReturns));
    fprintf('\n\n');
end

function bondsStocks()
    mu = [.085, .03];
    correlation = -.05;
    N = 5000;
    weights = [.5, .5];
    initial = 1000;
    
    sigma = [mu(1)^2, correlation*prod(mu); correlation*prod(mu), mu(2)^2];
    
    stockReturns = ones(N, 1);
    bondReturns = ones(N, 1);
    
    for i = 1:30
        scenarios = mvnrnd(mu, sigma, N);
        stockReturns = stockReturns.*(1+scenarios(:, 1));
        bondReturns = bondReturns.*(1+scenarios(:, 2));
    end
    
    finalReturns = initial.*(weights(1).*stockReturns + weights(2).*bondReturns);
    hist(finalReturns, 20);
    title('Stocks And Bonds');
    xlabel('30-Year Value');
    ylabel('Counts');
    
    fprintf('\nInitial amount: %d', initial);
    fprintf('\nMean return:\n');
    disp(mu);
    fprintf('\nCovariance Matrix:\n');
    disp(sigma);
    fprintf('\nWeights:\n');
    disp(weights);
    fprintf('\nFinal Mean: %.2f', mean(finalReturns));
    fprintf('\nFinal Std Dev: %.2f', mean(finalReturns));
    fprintf('\n\n');
end
