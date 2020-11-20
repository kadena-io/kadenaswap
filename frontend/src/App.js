import React from 'react';
import { ThemeProvider } from 'styled-components';
import Router from './router/router';
import { theme } from './styles/theme';
import GlobalStyle from './styles/globalStyle';
import { PactProvider } from './contexts/PactContext';

const App = () => {
  return (
    <ThemeProvider theme={theme}>
      <PactProvider>
        <GlobalStyle />
        <Router />
      </PactProvider>
    </ThemeProvider>
  );
};

export default App;
