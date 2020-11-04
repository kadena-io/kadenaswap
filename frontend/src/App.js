import React from 'react';
import { ThemeProvider } from 'styled-components';
import Router from './router/router';
import { theme } from './styles/theme';
import { EthProvider } from './contexts/EthContext';
import Web3ContextProvider from './contexts/Web3Context'
import { UseWalletProvider } from 'use-wallet'
import GlobalStyle from './styles/globalStyle';


const App = () => {
  return (
    <ThemeProvider theme={theme}>
      <Web3ContextProvider>
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
      </Web3ContextProvider>
    </ThemeProvider>
  );
};

export default App;
