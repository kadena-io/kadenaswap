import React from 'react';
import { ThemeProvider } from 'styled-components';
import { theme } from './styles/theme';
import GlobalStyle from './styles/globalStyle';
import { PactProvider, PactContext } from './contexts/PactContext';
import NotificationContent from './components/notification/NotificationContent';
import NotificationRender from './components/notification/NotificationRender';
import Header from './components/layout/header/Header'

export const Wallet = () => {
  return (
    <ThemeProvider theme={theme}>
      <NotificationRender>
        <PactProvider>
          <GlobalStyle />
          <Header />
        </PactProvider>
      </NotificationRender>
    </ThemeProvider>
  );
};

export const WalletProvider =  PactProvider;
export const WalletContext = PactContext;
