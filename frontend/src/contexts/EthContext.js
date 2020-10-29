import React, { useState, createContext, useEffect } from 'react';
import { Connectors } from 'web3-react'
import Web3 from 'web3';
import { useWallet } from 'use-wallet'

export const EthContext = createContext();

export const EthProvider = (props) => {

  const wallet = useWallet()

  const [web3, setWeb3] = useState(null);
  const [accts, setAccts] = useState([]);
  const [loading, setLoading] = useState(false);


  useEffect(() => {
    (async function _getEth() {
      await getEth();
    })();
    getAccounts();
  }, [accts]);

  const getEth = async () => {
    try {
      let web3Inst = new Web3(window.ethereum)
      await setWeb3(web3Inst);
      await getAccounts();
      console.log(accts)
    } catch (e) {
      console.log(e)
    }
  }

  const getAccounts = async () => {
    try {
      const as = await web3.eth.getAccounts();
      setAccts(as);
    } catch (e) {
      setAccts([]);
      console.log(e)
    }
  }

  const connectMetaMask = async () => {
    await wallet.connect();
    await getAccounts();
    console.log(accts);
  }

  const disconnectWallet = async () => {
    await wallet.reset();
  }

  return (
    <EthContext.Provider
      value={{
        accts,
        web3,
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
