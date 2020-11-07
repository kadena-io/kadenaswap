import React from 'react';
import { ThemeProvider } from 'styled-components';
import Router from './router/router';
import { theme } from './styles/theme';
import { EthProvider } from './contexts/EthContext';
import { Web3ReactProvider, useWeb3React } from '@web3-react/core'
import { Web3Provider } from '@ethersproject/providers'
import GlobalStyle from './styles/globalStyle';



const App = () => {

  const getLibrary = (provider) => {
    const library = new Web3Provider(provider)
    library.pollingInterval = 12000
    return library
  }

  return (
    <ThemeProvider theme={theme}>
      <Web3ReactProvider getLibrary={getLibrary}>
        <EthProvider>
          <GlobalStyle />
          <Router />
        </EthProvider>
      </Web3ReactProvider>
    </ThemeProvider>
  );
};

export default App;
