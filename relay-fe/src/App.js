import React from 'react';
import Home from "./components/Home";
import { PactStore } from "./contexts/PactContext";
import { WalletProvider, WalletContext } from './components/wallet/contexts/WalletContext';

function App() {

  return (
    <PactStore>
      <WalletProvider>
        <Home/>
      </WalletProvider>
    </PactStore>
  );
}

export default App;
