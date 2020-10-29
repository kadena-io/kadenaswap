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
    (async function _getEthEnv() {
      try {
        let web3Inst = await new Web3(window.ethereum)
        const as = await web3Inst.eth.getAccounts();
        await setAccts(as);
        await setWeb3(web3Inst);
      } catch (e) {
        console.log('did not find web3 instance')
        await setAccts([])
      }
    })();
  }, []);

  const getEth = async () => {
    try {
      let web3Inst = await new Web3(window.ethereum)
      await setWeb3(web3Inst);
      console.log(accts)
    } catch (e) {
      console.log(e)
    }
  }

  const getAccounts = async () => {
    try {
      const as = await web3.eth.getAccounts();
      await setAccts(as);
    } catch (e) {
      // setAccts([]);
      console.log(e)
    }
  }

  const connectMetaMask = async () => {
    await wallet.connect();
    console.log(wallet.status)
    await getAccounts();
    console.log(accts);
  }

  const disconnectWallet = async () => {
    await wallet.reset();
    console.log(wallet.status)
    //VERIFY WALLET IS DISCONNECTED
    await setAccts([])
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
