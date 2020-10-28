import React from 'react';
import { ThemeProvider } from 'styled-components';
import Router from './router/router';
import { theme } from './styles/theme';
import { EthProvider } from './contexts/EthContext';
import { UseWalletProvider } from 'use-wallet'
import GlobalStyle from './styles/globalStyle';

const App = () => {
  return (
    <ThemeProvider theme={theme}>
      <UseWalletProvider
        chainId={1}
        connectors={{
          // This is how connectors get configured
          portis: { dAppId: 'my-dapp-id-123-xyz' },
        }}
      >
        <EthProvider>
          <GlobalStyle />
          <Router />
        </EthProvider>
      </UseWalletProvider>
    </ThemeProvider>
  );
};

export default App;
