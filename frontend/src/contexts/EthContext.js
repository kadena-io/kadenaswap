import React, { useState, createContext, useEffect } from 'react';
import { Connectors } from 'web3-react'
import Web3 from 'web3';
import { useWallet, UseWalletProvider } from 'use-wallet'

export const EthContext = createContext();

export const EthProvider = (props) => {

  const wallet = useWallet()

  const [test, setTest] = useState("hi");
  const [web3, setWeb3] = useState(null);


  useEffect(() => {
    getEth()
  }, []);

  const getEth = () => {
    try {
      let web3 = new Web3(window.ethereum)
    } catch (e) {
      console.log(e)
    }
  }

  const connectMetaMask = () => {
    //default connect is with metamask
    wallet.connect();
  }

  const disconnectWallet = () => {
    wallet.reset();
  }

  return (
    <EthContext.Provider
      value={{
        test,
        connectMetaMask,
        disconnectWallet
      }}
    >
      {props.children}
    </EthContext.Provider>
  );
};

export const EthConsumer = EthContext.Consumer;

export const withEthContext = (Component) => (props) => (
  <EthConsumer>{(providerProps) => <Component {...props} sessionContextProps={providerProps} />}</EthConsumer>
);
