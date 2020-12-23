import React from 'react';
import { ThemeProvider } from 'styled-components';
import Router from './router/router';
import { theme } from './styles/theme';
import GlobalStyle from './styles/globalStyle';
import { PactProvider } from './contexts/PactContext';
import NotificationContent from './components/notification/NotificationContent';
import NotificationRender from './components/notification/NotificationRender';

const App = () => {
  return (
    <ThemeProvider theme={theme}>
      <NotificationRender>
        <PactProvider>
          <GlobalStyle />
          <Router />
        </PactProvider>
      </NotificationRender>
    </ThemeProvider>
  );
};

export default App;
