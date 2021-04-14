import React from 'react';
import Home from "./components/Home";
import { PactProvider } from "./contexts/PactContext";

import { ThemeProvider } from 'styled-components';
import { theme } from './wallet/styles/theme';
import GlobalStyle from './wallet/styles/globalStyle';
import { WalletProvider, WalletContext } from './wallet/contexts/WalletContext';
import NotificationContent from './wallet/components/notification/NotificationContent';
import NotificationRender from './wallet/components/notification/NotificationRender';
import Layout from './wallet/components/layout/Layout'

function App() {

  return (
    <ThemeProvider theme={theme}>
      <NotificationRender>
        <WalletProvider>
          <GlobalStyle />
          <Layout>
            <PactProvider>
              <Home/>
            </PactProvider>
          </Layout>
        </WalletProvider>
      </NotificationRender>
    </ThemeProvider>
  );
}

export default App;
