import React from 'react';
import { ThemeProvider } from 'styled-components';
import { theme } from './styles/theme';
import GlobalStyle from './styles/globalStyle';
import { WalletProvider, WalletContext } from './contexts/WalletContext';
import NotificationContent from './components/notification/NotificationContent';
import NotificationRender from './components/notification/NotificationRender';
import Header from './components/layout/header/Header'

export const Wallet = () => {
  return (
    <ThemeProvider theme={theme}>
      <NotificationRender>
        <WalletProvider>
          <GlobalStyle />
          <Header />
        </WalletProvider>
      </NotificationRender>
    </ThemeProvider>
  );
};
